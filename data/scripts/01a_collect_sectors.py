"""Thu thập và chuẩn hóa dữ liệu 10 chỉ số ngành HOSE.

Thứ tự ưu tiên nguồn:
1. File thô người dùng đặt trong data/00_raw/equity/sectors/.
2. CafeF Ajax API.
3. SSI iBoard UDF charting API.
4. TCBS UDF API dự phòng.

Script chỉ ghi dữ liệu đã làm sạch vào data/01_processed/sectors/ và metadata
nhẹ phục vụ kiểm tra nguồn. Không sửa hoặc ghi đè dữ liệu thô.
"""

from __future__ import annotations

import json
import sys
import time
import urllib.parse
import urllib.request
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

import pandas as pd

from config import METADATA_DIR, SECTOR_CODES, SECTOR_PROCESSED_DIR, SECTOR_RAW_DIR


CAFEF_URL = "https://s.cafef.vn/Ajax/PageNew/Data_LichSuGiaoDich.ashx"
SSI_URL = "https://iboard-api.ssi.com.vn/statistics/charts/history"
TCBS_URL = "https://apipublish.tcbs.com.vn/api/v1/stock/history-resolution"
START_DATE = "01/01/2015"
PAGE_SIZE = 1000
USER_AGENT = (
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
    "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125 Safari/537.36"
)


if hasattr(sys.stdout, "reconfigure"):
    sys.stdout.reconfigure(encoding="utf-8", errors="replace")
if hasattr(sys.stderr, "reconfigure"):
    sys.stderr.reconfigure(encoding="utf-8", errors="replace")


def ensure_directories() -> None:
    SECTOR_RAW_DIR.mkdir(parents=True, exist_ok=True)
    SECTOR_PROCESSED_DIR.mkdir(parents=True, exist_ok=True)
    METADATA_DIR.mkdir(parents=True, exist_ok=True)


def request_json(url: str, params: dict[str, Any], timeout: int = 20) -> Any:
    query = urllib.parse.urlencode(params)
    referer = "https://iboard.ssi.com.vn/" if "ssi.com.vn" in url else "https://s.cafef.vn/"
    origin = "https://iboard.ssi.com.vn" if "ssi.com.vn" in url else "https://s.cafef.vn"
    req = urllib.request.Request(
        f"{url}?{query}",
        headers={
            "User-Agent": USER_AGENT,
            "Accept": "application/json,text/plain,*/*",
            "Origin": origin,
            "Referer": referer,
        },
    )
    with urllib.request.urlopen(req, timeout=timeout) as response:
        content = response.read().decode("utf-8-sig", errors="replace")
    return json.loads(content)


def parse_number(value: Any) -> float | None:
    if pd.isna(value):
        return None
    if isinstance(value, (int, float)):
        return float(value)
    text = str(value).strip().replace("\u00a0", "").replace(" ", "")
    if not text or text in {"-", "--", "null", "None"}:
        return None
    text = text.replace("%", "")
    if "," in text and "." in text:
        if text.rfind(",") > text.rfind("."):
            text = text.replace(".", "").replace(",", ".")
        else:
            text = text.replace(",", "")
    elif "," in text:
        left, right = text.rsplit(",", 1)
        text = left.replace(",", "") + right if len(right) == 3 else left.replace(",", "") + "." + right
    try:
        return float(text)
    except ValueError:
        return None


def normalize_frame(records: list[dict[str, Any]], source: str) -> pd.DataFrame | None:
    if not records:
        return None

    rows: list[dict[str, Any]] = []
    date_keys = ["Ngay", "Date", "date", "TradingDate", "time", "t"]
    close_keys = ["GiaDongCua", "Close", "close", "c", "Index", "IndexValue", "Price"]

    for rec in records:
        date_value = next((rec.get(k) for k in date_keys if k in rec), None)
        close_value = next((rec.get(k) for k in close_keys if k in rec), None)
        if date_value is None or close_value is None:
            continue
        if isinstance(date_value, (int, float)) and date_value > 10_000:
            date = pd.to_datetime(date_value, unit="s", errors="coerce")
        else:
            date = pd.to_datetime(date_value, dayfirst=True, errors="coerce")
        close = parse_number(close_value)
        if pd.notna(date) and close is not None and close > 0:
            rows.append({"date": pd.Timestamp(date).normalize(), "close": close, "source": source})

    if not rows:
        return None

    df = pd.DataFrame(rows)
    df = df.drop_duplicates("date", keep="last").sort_values("date").reset_index(drop=True)
    return df


def extract_records(payload: Any) -> list[dict[str, Any]]:
    if isinstance(payload, list):
        return [x for x in payload if isinstance(x, dict)]
    if not isinstance(payload, dict):
        return []

    candidates = [
        payload.get("data"),
        payload.get("Data"),
        payload.get("rows"),
        payload.get("Items"),
        payload.get("items"),
    ]
    for item in candidates:
        if isinstance(item, list):
            return [x for x in item if isinstance(x, dict)]
        if isinstance(item, dict):
            nested = extract_records(item)
            if nested:
                return nested
    return []


def fetch_from_cafef(symbol: str) -> pd.DataFrame | None:
    records: list[dict[str, Any]] = []
    end_date = datetime.now().strftime("%d/%m/%Y")

    for page in range(1, 50):
        params = {
            "Symbol": symbol,
            "StartDate": START_DATE,
            "EndDate": end_date,
            "PageIndex": page,
            "PageSize": PAGE_SIZE,
            "Type": 1,
        }
        try:
            payload = request_json(CAFEF_URL, params)
        except Exception as exc:
            if page == 1:
                print(f"  [Cảnh báo] CafeF không phản hồi cho {symbol}: {exc}")
            break
        page_records = extract_records(payload)
        if not page_records:
            break
        records.extend(page_records)
        if len(page_records) < PAGE_SIZE:
            break
        time.sleep(0.2)

    return normalize_frame(records, "CafeF Ajax API")


def fetch_from_ssi(symbol: str) -> pd.DataFrame | None:
    start_ts = int(datetime(2015, 1, 1, tzinfo=timezone.utc).timestamp())
    end_ts = int(time.time())
    params = {"symbol": symbol, "resolution": "D", "from": start_ts, "to": end_ts}
    try:
        payload = request_json(SSI_URL, params)
    except Exception as exc:
        print(f"  [Cảnh báo] SSI iBoard không phản hồi cho {symbol}: {exc}")
        return None
    if not isinstance(payload, dict):
        return None
    chart_data = payload.get("data", payload)
    status = payload.get("status")
    if isinstance(chart_data, dict):
        status = status or chart_data.get("s")
    if status not in {"ok", "OK", "SUCCESS", None}:
        return None
    if isinstance(chart_data, dict) and "t" in chart_data and "c" in chart_data:
        records = [{"t": t, "c": c} for t, c in zip(chart_data["t"], chart_data["c"])]
    else:
        records = extract_records(chart_data)
    return normalize_frame(records, "SSI iBoard Statistics API")


def fetch_from_tcbs(symbol: str) -> pd.DataFrame | None:
    start_ts = int(datetime(2015, 1, 1, tzinfo=timezone.utc).timestamp())
    end_ts = int(time.time())
    params = {"symbol": symbol, "resolution": "D", "from": start_ts, "to": end_ts}
    try:
        payload = request_json(TCBS_URL, params)
    except Exception as exc:
        print(f"  [Cảnh báo] TCBS không phản hồi cho {symbol}: {exc}")
        return None
    if not isinstance(payload, dict) or payload.get("s") not in {"ok", "OK", None}:
        return None
    if "t" in payload and "c" in payload:
        records = [{"t": t, "c": c} for t, c in zip(payload["t"], payload["c"])]
    else:
        records = extract_records(payload)
    return normalize_frame(records, "TCBS UDF API")


def read_local_table(file_path: Path) -> pd.DataFrame:
    if file_path.suffix.lower() in {".xlsx", ".xls"}:
        return pd.read_excel(file_path)
    for encoding in ("utf-8-sig", "utf-8", "cp1258", "latin1"):
        try:
            return pd.read_csv(file_path, encoding=encoding)
        except UnicodeDecodeError:
            continue
    return pd.read_csv(file_path)


def find_and_parse_local_file(symbol: str) -> pd.DataFrame | None:
    if not SECTOR_RAW_DIR.exists():
        return None

    date_candidates = {
        "ngày",
        "ngay",
        "ngày giao dịch",
        "ngay giao dich",
        "date",
        "tradingdate",
        "time",
    }
    close_candidates = {
        "đóng cửa",
        "dong cua",
        "giá đóng cửa",
        "gia dong cua",
        "close",
        "price",
        "chỉ số",
        "chi so",
        "index",
        "index_value",
        "indexvalue",
    }

    for file_path in SECTOR_RAW_DIR.iterdir():
        if not file_path.is_file() or symbol.lower() not in file_path.name.lower():
            continue
        try:
            raw = read_local_table(file_path)
        except Exception as exc:
            print(f"  [Lỗi] Không đọc được file {file_path.name}: {exc}")
            continue

        normalized_cols = {str(c).strip().lower(): c for c in raw.columns}
        date_col = next((normalized_cols[c] for c in date_candidates if c in normalized_cols), None)
        close_col = next((normalized_cols[c] for c in close_candidates if c in normalized_cols), None)

        if date_col is None or close_col is None:
            print(f"  [Cảnh báo] File {file_path.name} thiếu cột ngày hoặc đóng cửa: {list(raw.columns)}")
            continue

        records = [
            {"date": date_value, "close": close_value}
            for date_value, close_value in zip(raw[date_col], raw[close_col])
        ]
        df = normalize_frame(records, f"File thô cục bộ: {file_path.name}")
        if df is not None and not df.empty:
            return df
    return None


def update_inventory(summary: list[dict[str, Any]]) -> None:
    inventory_path = METADATA_DIR / "data_inventory.csv"
    new_rows = []
    for row in summary:
        if row["status"] != "ok":
            continue
        new_rows.append(
            {
                "dataset": f"sector_{row['symbol'].lower()}",
                "status": "ok",
                "rows": row["rows"],
                "date_start": row["date_start"],
                "date_end": row["date_end"],
                "path": f"data/01_processed/sectors/{row['symbol'].lower()}_clean.csv",
                "source": row["source"],
                "notes": "HOSE VNAllshare sector index cleaned for sector extension.",
            }
        )

    if not new_rows:
        return

    new_df = pd.DataFrame(new_rows)
    if inventory_path.exists():
        old = pd.read_csv(inventory_path)
        key_col = "dataset" if "dataset" in old.columns else old.columns[0]
        old = old[~old[key_col].astype(str).str.startswith("sector_")]
        combined = pd.concat([old, new_df], ignore_index=True, sort=False)
    else:
        combined = new_df
    combined.to_csv(inventory_path, index=False, encoding="utf-8-sig")


def main() -> None:
    print("=" * 72)
    print("BẮT ĐẦU: THU THẬP VÀ CHUẨN HÓA DỮ LIỆU CHỈ SỐ NGÀNH HOSE")
    print("=" * 72)
    ensure_directories()

    status_summary: list[dict[str, Any]] = []

    for code in SECTOR_CODES:
        print(f"\nĐang xử lý ngành: {code}")
        attempts = [
            ("local", find_and_parse_local_file),
            ("cafef", fetch_from_cafef),
            ("ssi", fetch_from_ssi),
            ("tcbs", fetch_from_tcbs),
        ]
        df = None
        for _, func in attempts:
            df = func(code)
            if df is not None and not df.empty:
                break

        if df is None or df.empty:
            print(f"  [Thất bại] Chưa lấy được dữ liệu cho {code}.")
            status_summary.append(
                {
                    "symbol": code,
                    "status": "missing",
                    "source": "N/A",
                    "rows": 0,
                    "date_start": "N/A",
                    "date_end": "N/A",
                    "retrieved_at": datetime.now().isoformat(timespec="seconds"),
                    "notes": f"Cần import {code.lower()}.xlsx/csv vào data/00_raw/equity/sectors/ hoặc kiểm tra API.",
                }
            )
            continue

        source = str(df["source"].dropna().iloc[0]) if "source" in df.columns else "unknown"
        df_out = df[["date", "close"]].copy()
        out_path = SECTOR_PROCESSED_DIR / f"{code.lower()}_clean.csv"
        df_out.to_csv(out_path, index=False, encoding="utf-8-sig")

        min_date = df_out["date"].min().strftime("%Y-%m-%d")
        max_date = df_out["date"].max().strftime("%Y-%m-%d")
        print(f"  [Thành công] Nguồn: {source}")
        print(f"  Lưu tại: {out_path.relative_to(out_path.parents[2])}")
        print(f"  Số dòng: {len(df_out)}, khoảng thời gian: {min_date} đến {max_date}")

        status_summary.append(
            {
                "symbol": code,
                "status": "ok",
                "source": source,
                "rows": len(df_out),
                "date_start": min_date,
                "date_end": max_date,
                "retrieved_at": datetime.now().isoformat(timespec="seconds"),
                "notes": "",
            }
        )

    summary_df = pd.DataFrame(status_summary)
    status_path = SECTOR_PROCESSED_DIR / "collection_status.csv"
    summary_df.to_csv(status_path, index=False, encoding="utf-8-sig")
    update_inventory(status_summary)

    print(f"\nĐã xuất báo cáo thu thập: {status_path.relative_to(status_path.parents[2])}")
    print("=" * 72)


if __name__ == "__main__":
    main()
