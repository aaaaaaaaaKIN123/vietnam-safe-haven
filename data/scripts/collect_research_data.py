from __future__ import annotations

import json
import math
import re
import sys
from datetime import date, datetime, timezone
from pathlib import Path
from urllib.error import HTTPError, URLError
from urllib.parse import quote, urlencode
from urllib.request import Request, urlopen

import pandas as pd


ROOT = Path(__file__).resolve().parents[2]
DATA = ROOT / "data"
RAW = DATA / "raw"
PROCESSED = DATA / "processed"
META = DATA / "metadata"

START_DATE = "2010-01-01"
END_DATE = date.today().isoformat()

USER_AGENT = (
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
    "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125 Safari/537.36"
)


def ensure_dirs() -> None:
    for path in [
        RAW / "vnstock_kbs",
        RAW / "yahoo",
        RAW / "giavang_now",
        RAW / "tradingeconomics",
        PROCESSED,
        META,
    ]:
        path.mkdir(parents=True, exist_ok=True)


def fetch_text(url: str, timeout: int = 45) -> str:
    req = Request(url, headers={"User-Agent": USER_AGENT, "Accept": "*/*"})
    with urlopen(req, timeout=timeout) as response:
        return response.read().decode("utf-8-sig", errors="replace")


def write_json(path: Path, payload) -> None:
    path.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")


def write_text(path: Path, text: str) -> None:
    path.write_text(text, encoding="utf-8")


def status_record(dataset, source, path, status, rows=None, start=None, end=None, notes=""):
    return {
        "dataset": dataset,
        "source": source,
        "path": str(path.relative_to(ROOT)) if path else "",
        "status": status,
        "rows": rows,
        "start": start,
        "end": end,
        "notes": notes,
        "collected_at_utc": datetime.now(timezone.utc).isoformat(),
    }


def parse_kbs_ohlc(payload: dict, data_key: str = "data_day") -> pd.DataFrame:
    records = payload.get(data_key, [])
    df = pd.DataFrame(records)
    if df.empty:
        return df
    df = df.rename(
        columns={
            "t": "date",
            "o": "open",
            "h": "high",
            "l": "low",
            "c": "close",
            "v": "volume",
        }
    )
    df["date"] = pd.to_datetime(df["date"], errors="coerce").dt.date
    for col in ["open", "high", "low", "close", "volume"]:
        if col in df:
            df[col] = pd.to_numeric(df[col], errors="coerce")
    df = df.dropna(subset=["date"]).sort_values("date").drop_duplicates("date")
    return df[["date", "open", "high", "low", "close", "volume"]]


def collect_vnindex_kbs() -> tuple[pd.DataFrame | None, dict]:
    start_dmy = datetime.strptime(START_DATE, "%Y-%m-%d").strftime("%d-%m-%Y")
    end_dmy = datetime.strptime(END_DATE, "%Y-%m-%d").strftime("%d-%m-%Y")
    url = (
        "https://kbbuddywts.kbsec.com.vn/iis-server/investment/index/"
        f"VNINDEX/data_day?{urlencode({'sdate': start_dmy, 'edate': end_dmy})}"
    )
    raw_path = RAW / "vnstock_kbs" / "vnindex_kbs_raw.json"
    csv_path = RAW / "vnstock_kbs" / "vnindex_daily_ohlcv.csv"
    try:
        payload = json.loads(fetch_text(url))
        write_json(raw_path, payload)
        df = parse_kbs_ohlc(payload)
        df.to_csv(csv_path, index=False, encoding="utf-8-sig")
        return df, status_record(
            "vnindex_daily_ohlcv",
            "KBS endpoint used by vnstock Quote.history",
            csv_path,
            "ok",
            len(df),
            str(df["date"].min()) if not df.empty else None,
            str(df["date"].max()) if not df.empty else None,
            "VNIndex OHLCV; direct endpoint avoids package installation.",
        )
    except Exception as exc:  # noqa: BLE001
        return None, status_record(
            "vnindex_daily_ohlcv",
            "KBS endpoint used by vnstock Quote.history",
            csv_path,
            "failed",
            notes=repr(exc),
        )


def unix_ts(iso_date: str) -> int:
    return int(datetime.strptime(iso_date, "%Y-%m-%d").replace(tzinfo=timezone.utc).timestamp())


YAHOO_SYMBOLS = {
    "gold_futures_gc": "GC=F",
    "brent_futures_bz": "BZ=F",
    "wti_futures_cl": "CL=F",
    "usd_vnd": "USDVND=X",
    "us_10y_yield_tnx": "^TNX",
    "bitcoin_usd": "BTC-USD",
    "ethereum_usd": "ETH-USD",
    "vix": "^VIX",
}


def parse_yahoo_chart(payload: dict) -> pd.DataFrame:
    chart = payload.get("chart", {})
    result = chart.get("result") or []
    if not result:
        return pd.DataFrame()
    item = result[0]
    timestamps = item.get("timestamp") or []
    quote_data = (item.get("indicators", {}).get("quote") or [{}])[0]
    adj_data = (item.get("indicators", {}).get("adjclose") or [{}])[0]
    df = pd.DataFrame({"timestamp": timestamps})
    if df.empty:
        return df
    df["date"] = pd.to_datetime(df["timestamp"], unit="s", utc=True).dt.date
    for col in ["open", "high", "low", "close", "volume"]:
        values = quote_data.get(col)
        df[col] = values if values is not None else None
    df["adjclose"] = adj_data.get("adjclose") if adj_data.get("adjclose") is not None else df["close"]
    for col in ["open", "high", "low", "close", "adjclose", "volume"]:
        df[col] = pd.to_numeric(df[col], errors="coerce")
    return df[["date", "open", "high", "low", "close", "adjclose", "volume"]].dropna(
        subset=["date"]
    )


def collect_yahoo_symbol(name: str, symbol: str) -> tuple[pd.DataFrame | None, dict]:
    period1 = unix_ts(START_DATE)
    period2 = unix_ts(END_DATE) + 24 * 60 * 60
    encoded = quote(symbol, safe="")
    url = (
        f"https://query1.finance.yahoo.com/v8/finance/chart/{encoded}?"
        + urlencode(
            {
                "period1": period1,
                "period2": period2,
                "interval": "1d",
                "events": "history",
                "includeAdjustedClose": "true",
            }
        )
    )
    raw_path = RAW / "yahoo" / f"{name}_raw.json"
    csv_path = RAW / "yahoo" / f"{name}_daily.csv"
    try:
        payload = json.loads(fetch_text(url))
        write_json(raw_path, payload)
        df = parse_yahoo_chart(payload)
        df = df.sort_values("date").drop_duplicates("date")
        df.to_csv(csv_path, index=False, encoding="utf-8-sig")
        return df, status_record(
            name,
            f"Yahoo Finance chart API ({symbol})",
            csv_path,
            "ok" if len(df) else "empty",
            len(df),
            str(df["date"].min()) if not df.empty else None,
            str(df["date"].max()) if not df.empty else None,
        )
    except Exception as exc:  # noqa: BLE001
        return None, status_record(
            name,
            f"Yahoo Finance chart API ({symbol})",
            csv_path,
            "failed",
            notes=repr(exc),
        )


def collect_giavang_now() -> tuple[pd.DataFrame | None, dict]:
    url = "https://giavang.now/api/prices?type=SJL1L10&days=30"
    raw_path = RAW / "giavang_now" / "sjc_9999_last30_raw.json"
    csv_path = RAW / "giavang_now" / "sjc_9999_last30.csv"
    try:
        payload = json.loads(fetch_text(url))
        write_json(raw_path, payload)
        if "history" in payload:
            records = []
            for item in payload.get("history") or []:
                prices = (item.get("prices") or {}).get("SJL1L10") or {}
                records.append(
                    {
                        "date": item.get("date"),
                        "type_code": "SJL1L10",
                        "name": prices.get("name"),
                        "buy": prices.get("buy"),
                        "sell": prices.get("sell"),
                        "day_change_buy": prices.get("day_change_buy"),
                        "day_change_sell": prices.get("day_change_sell"),
                        "updates": prices.get("updates"),
                    }
                )
        else:
            data = payload.get("data", [])
            if isinstance(data, dict):
                records = data.get("history") or data.get("prices") or data.get("data") or []
                if not records:
                    records = [data]
            else:
                records = data
        df = pd.DataFrame(records)
        if "update_time" in df:
            df["date"] = pd.to_datetime(df["update_time"], unit="s", errors="coerce").dt.date
        elif "time" in df:
            df["date"] = pd.to_datetime(df["time"], errors="coerce").dt.date
        elif "date" in df:
            df["date"] = pd.to_datetime(df["date"], errors="coerce").dt.date
        for col in ["buy", "sell", "change_buy", "change_sell"]:
            if col in df:
                df[col] = pd.to_numeric(df[col], errors="coerce")
        if "date" in df:
            df = df.sort_values("date").drop_duplicates("date")
        df.to_csv(csv_path, index=False, encoding="utf-8-sig")
        return df, status_record(
            "sjc_9999_last30",
            "giavang.now API, type=SJL1L10, days=30",
            csv_path,
            "ok" if len(df) else "empty",
            len(df),
            str(df["date"].min()) if "date" in df and not df.empty else None,
            str(df["date"].max()) if "date" in df and not df.empty else None,
            "Free API only documents history up to 30 days; not sufficient for main 2010-2026 model.",
        )
    except Exception as exc:  # noqa: BLE001
        return None, status_record(
            "sjc_9999_last30",
            "giavang.now API, type=SJL1L10, days=30",
            csv_path,
            "failed",
            notes=repr(exc),
        )


def collect_tradingeconomics_snapshot() -> dict:
    url = "https://tradingeconomics.com/vietnam/government-bond-yield"
    raw_path = RAW / "tradingeconomics" / "vietnam_10y_bond_yield_page.html"
    csv_path = RAW / "tradingeconomics" / "vietnam_10y_bond_yield_snapshot.csv"
    try:
        html = fetch_text(url)
        write_text(raw_path, html)
        actual = None
        match = re.search(r"Vietnam 10Y\s*</[^>]+>\s*([0-9.]+)", html)
        if not match:
            match = re.search(r"held steady at\s+([0-9.]+)%", html)
        if match:
            actual = float(match.group(1))
        df = pd.DataFrame(
            [
                {
                    "date_collected": END_DATE,
                    "country": "Vietnam",
                    "tenor": "10Y",
                    "yield_percent": actual,
                    "source_note": "Current page snapshot only; not historical CSV.",
                }
            ]
        )
        df.to_csv(csv_path, index=False, encoding="utf-8-sig")
        return status_record(
            "vietnam_10y_government_bond_yield_snapshot",
            "Trading Economics public page",
            csv_path,
            "ok" if actual is not None else "partial",
            len(df),
            END_DATE,
            END_DATE,
            "Snapshot only. Historical download requires a licensed data source or a different public CSV.",
        )
    except Exception as exc:  # noqa: BLE001
        return status_record(
            "vietnam_10y_government_bond_yield_snapshot",
            "Trading Economics public page",
            csv_path,
            "failed",
            notes=repr(exc),
        )


def safe_log_return(series: pd.Series) -> pd.Series:
    values = pd.to_numeric(series, errors="coerce")
    return 100 * (values.apply(lambda x: math.log(x) if pd.notna(x) and x > 0 else pd.NA).diff())


def build_processed(dataframes: dict[str, pd.DataFrame]) -> list[dict]:
    reports = []
    merged = None
    close_map = {
        "vnindex_daily_ohlcv": "vnindex_close",
        "gold_futures_gc": "global_gold_gc_close_usd_oz",
        "brent_futures_bz": "brent_close_usd",
        "wti_futures_cl": "wti_close_usd",
        "usd_vnd": "usd_vnd_close",
        "us_10y_yield_tnx": "us_10y_yield_percent_proxy",
        "bitcoin_usd": "btc_usd_close",
        "ethereum_usd": "eth_usd_close",
        "vix": "vix_close",
    }
    for key, out_col in close_map.items():
        df = dataframes.get(key)
        if df is None or df.empty or "close" not in df:
            continue
        part = df[["date", "close"]].copy().rename(columns={"close": out_col})
        merged = part if merged is None else merged.merge(part, on="date", how="outer")
    if merged is None:
        return reports
    merged["date"] = pd.to_datetime(merged["date"])
    merged = merged.sort_values("date").drop_duplicates("date")
    if {"global_gold_gc_close_usd_oz", "usd_vnd_close"}.issubset(merged.columns):
        merged["global_gold_vnd_per_oz"] = (
            merged["global_gold_gc_close_usd_oz"] * merged["usd_vnd_close"]
        )
        # 1 troy oz = 0.829426 tael (luong) approximately.
        merged["global_gold_vnd_per_tael"] = merged["global_gold_vnd_per_oz"] / 0.829426
    out_path = PROCESSED / "merged_daily_core.csv"
    merged.to_csv(out_path, index=False, encoding="utf-8-sig")
    reports.append(
        status_record(
            "merged_daily_core",
            "processed from raw sources",
            out_path,
            "ok",
            len(merged),
            str(merged["date"].min().date()),
            str(merged["date"].max().date()),
        )
    )

    returns = merged[["date"]].copy()
    for col in [c for c in merged.columns if c != "date"]:
        if "yield_percent" in col or "vix" in col:
            returns[f"d_{col}"] = pd.to_numeric(merged[col], errors="coerce").diff()
        else:
            returns[f"r_{col}"] = safe_log_return(merged[col])
    ret_path = PROCESSED / "returns_daily_core.csv"
    returns.to_csv(ret_path, index=False, encoding="utf-8-sig")
    reports.append(
        status_record(
            "returns_daily_core",
            "processed from merged_daily_core",
            ret_path,
            "ok",
            len(returns),
            str(returns["date"].min().date()),
            str(returns["date"].max().date()),
        )
    )

    sjc = dataframes.get("sjc_9999_last30")
    if sjc is not None and not sjc.empty and "date" in sjc and "sell" in sjc:
        premium = sjc[["date", "buy", "sell"]].copy()
        premium["date"] = pd.to_datetime(premium["date"])
        premium = premium.merge(
            merged[["date", "global_gold_vnd_per_tael"]]
            if "global_gold_vnd_per_tael" in merged
            else merged[["date"]],
            on="date",
            how="left",
        )
        if "global_gold_vnd_per_tael" in premium:
            premium["sjc_sell_minus_global_vnd_per_tael"] = (
                premium["sell"] - premium["global_gold_vnd_per_tael"]
            )
            premium["sjc_log_premium"] = premium.apply(
                lambda row: (
                    math.log(row["sell"]) - math.log(row["global_gold_vnd_per_tael"])
                    if pd.notna(row.get("sell"))
                    and pd.notna(row.get("global_gold_vnd_per_tael"))
                    and row["sell"] > 0
                    and row["global_gold_vnd_per_tael"] > 0
                    else pd.NA
                ),
                axis=1,
            )
        prem_path = PROCESSED / "gold_premium_sjc_last30.csv"
        premium.to_csv(prem_path, index=False, encoding="utf-8-sig")
        reports.append(
            status_record(
                "gold_premium_sjc_last30",
                "processed from giavang.now and Yahoo GC=F/USDVND",
                prem_path,
                "partial",
                len(premium),
                str(premium["date"].min().date()) if len(premium) else None,
                str(premium["date"].max().date()) if len(premium) else None,
                "Only last 30 days of SJC are available from the free API endpoint.",
            )
        )
    return reports


def write_metadata(inventory: list[dict]) -> None:
    inv = pd.DataFrame(inventory)
    inv_path = META / "data_inventory.csv"
    inv.to_csv(inv_path, index=False, encoding="utf-8-sig")

    readme = f"""# Research Data Folder

Generated on: {datetime.now().isoformat()}

## Folder structure

- `raw/vnstock_kbs/`: VNIndex OHLCV from the KBS endpoint used by vnstock.
- `raw/yahoo/`: international market series from Yahoo Finance chart API.
- `raw/giavang_now/`: SJC/domestic gold data available from giavang.now.
- `raw/tradingeconomics/`: Vietnam 10Y government bond yield snapshot.
- `processed/`: merged panel and return series for econometric work.
- `metadata/`: inventory and collection notes.
- `scripts/`: reproducible collection script.

## Main processed files

- `processed/merged_daily_core.csv`: daily panel with VNIndex, global gold, oil, USD/VND, US10Y, BTC/ETH and VIX.
- `processed/returns_daily_core.csv`: log returns for price series and first differences for yield/VIX-type series.
- `processed/gold_premium_sjc_last30.csv`: short-window SJC premium file if the SJC API returns history.

## Important limitations

1. Domestic SJC gold: the free `giavang.now` API documents history only up to 30 days. This is useful for testing premium construction, but not sufficient for the full 2010-2026 ADCC-GARCH model.
2. Vietnam government bond: this run stores a current 10Y yield snapshot. A daily historical Vietnam government bond return/yield series still needs a licensed or dedicated historical source such as AsianBondsOnline/Bloomberg/Refinitiv/FiinPro/HNX/VBMA/Investing.com export.
3. Bond hedging effectiveness should not be estimated from raw yield changes unless you convert yields into approximate bond returns using a transparent duration assumption.

## Suggested next step

Use `processed/merged_daily_core.csv` and `processed/returns_daily_core.csv` for preliminary GARCH/DCC tests. Add a proper long-history SJC gold series and Vietnam government bond return/yield series before estimating the final safe-haven ranking.
"""
    write_text(META / "README_data.md", readme)

    report_lines = ["# Data Collection Report", ""]
    for item in inventory:
        report_lines.append(
            f"- **{item['dataset']}**: {item['status']} | rows={item.get('rows')} | "
            f"{item.get('start')} to {item.get('end')} | `{item.get('path')}`"
        )
        if item.get("notes"):
            report_lines.append(f"  - Notes: {item['notes']}")
    write_text(META / "collection_report.md", "\n".join(report_lines) + "\n")


def main() -> int:
    ensure_dirs()
    inventory: list[dict] = []
    dataframes: dict[str, pd.DataFrame] = {}

    vnindex, rec = collect_vnindex_kbs()
    inventory.append(rec)
    if vnindex is not None:
        dataframes["vnindex_daily_ohlcv"] = vnindex

    for name, symbol in YAHOO_SYMBOLS.items():
        df, rec = collect_yahoo_symbol(name, symbol)
        inventory.append(rec)
        if df is not None:
            dataframes[name] = df

    sjc, rec = collect_giavang_now()
    inventory.append(rec)
    if sjc is not None:
        dataframes["sjc_9999_last30"] = sjc

    inventory.append(collect_tradingeconomics_snapshot())
    inventory.extend(build_processed(dataframes))
    write_metadata(inventory)

    ok = sum(1 for item in inventory if item["status"] in {"ok", "partial"})
    print(f"Collected/created {ok}/{len(inventory)} datasets/artifacts.")
    print(f"Inventory: {META / 'data_inventory.csv'}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
