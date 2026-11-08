"""Xây dựng dữ liệu daily/weekly cho phần mở rộng chỉ số ngành HOSE.

Quy trình:
1. Đọc core daily panel và core daily returns.
2. Ghép sector index levels vào lịch giao dịch VNIndex.
3. Forward-fill tối đa 3 ngày giao dịch và ghi audit riêng.
4. Tìm ngày bắt đầu chung của toàn bộ ngành có sẵn.
5. Tính log returns ngành và ghép biến lõi/kiểm soát.
6. Tạo weekly returns từ weekly levels theo week-ending Friday.
"""

from __future__ import annotations

import sys

import numpy as np
import pandas as pd

from config import MODEL_INPUT_DIR, SECTOR_CODES, SECTOR_INPUT_DIR, SECTOR_PROCESSED_DIR


FILL_LIMIT = 3


if hasattr(sys.stdout, "reconfigure"):
    sys.stdout.reconfigure(encoding="utf-8", errors="replace")
if hasattr(sys.stderr, "reconfigure"):
    sys.stderr.reconfigure(encoding="utf-8", errors="replace")


def log_return(series: pd.Series) -> pd.Series:
    clean = pd.to_numeric(series, errors="coerce")
    clean = clean.where(clean > 0)
    return 100 * np.log(clean / clean.shift(1))


def audit_forward_fill(before: pd.Series, after: pd.Series, code: str, dates: pd.Series) -> pd.DataFrame:
    filled_mask = before.isna() & after.notna()
    if not filled_mask.any():
        return pd.DataFrame(
            columns=["date", "sector", "filled_value", "fill_run_id", "fill_run_length"]
        )

    audit = pd.DataFrame(
        {
            "date": dates.loc[filled_mask].to_numpy(),
            "sector": code,
            "filled_value": after.loc[filled_mask].to_numpy(),
        }
    )
    run_break = (filled_mask != filled_mask.shift(fill_value=False)).cumsum()
    run_ids = run_break.loc[filled_mask].to_numpy()
    audit["fill_run_id"] = run_ids
    run_lengths = audit.groupby("fill_run_id")["date"].transform("count")
    audit["fill_run_length"] = run_lengths
    return audit


def main() -> None:
    print("=" * 72)
    print("BẮT ĐẦU: XÂY DỰNG DỮ LIỆU ĐẦU VÀO CHO MÔ HÌNH NGÀNH")
    print("=" * 72)

    core_panel_path = MODEL_INPUT_DIR / "core" / "final_daily_panel.csv"
    core_returns_path = MODEL_INPUT_DIR / "core" / "final_daily_returns.csv"
    core_weekly_returns_path = MODEL_INPUT_DIR / "core" / "final_weekly_returns.csv"

    if not core_panel_path.exists() or not core_returns_path.exists():
        print(f"  [Lỗi] Không tìm thấy core input trong {MODEL_INPUT_DIR / 'core'}")
        print("  Hãy chạy `python data/scripts/02_build_model_input.py` trước.")
        return

    core_panel = pd.read_csv(core_panel_path)
    core_panel["date"] = pd.to_datetime(core_panel["date"])
    core_returns = pd.read_csv(core_returns_path)
    core_returns["date"] = pd.to_datetime(core_returns["date"])

    merged_panel = core_panel.copy()
    available_sectors: list[str] = []
    fill_audits: list[pd.DataFrame] = []

    for code in SECTOR_CODES:
        clean_path = SECTOR_PROCESSED_DIR / f"{code.lower()}_clean.csv"
        if not clean_path.exists():
            print(f"  [Bỏ qua] Thiếu dữ liệu ngành {code}: {clean_path.name}")
            continue

        sector_df = pd.read_csv(clean_path)
        sector_df["date"] = pd.to_datetime(sector_df["date"])
        sector_col = code.lower()
        sector_df = sector_df[["date", "close"]].rename(columns={"close": sector_col})
        merged_panel = merged_panel.merge(sector_df, on="date", how="left")

        before_fill = merged_panel[sector_col].copy()
        merged_panel[sector_col] = merged_panel[sector_col].ffill(limit=FILL_LIMIT)
        fill_audits.append(
            audit_forward_fill(before_fill, merged_panel[sector_col], sector_col, merged_panel["date"])
        )
        available_sectors.append(sector_col)

    if not available_sectors:
        print("  [Lỗi] Không có dữ liệu ngành nào đã được chuẩn bị.")
        return

    valid = merged_panel.dropna(subset=available_sectors)
    if valid.empty:
        print("  [Lỗi] Không có khoảng thời gian chung cho các ngành có sẵn.")
        return

    sector_start_date = valid["date"].min()
    sector_panel_daily = merged_panel[merged_panel["date"] >= sector_start_date].reset_index(drop=True)
    print(f"  Ngành có dữ liệu: {', '.join(s.upper() for s in available_sectors)}")
    print(f"  Ngày bắt đầu chung: {sector_start_date.strftime('%Y-%m-%d')}")

    sector_returns_daily = pd.DataFrame({"date": sector_panel_daily["date"]})
    core_returns_filtered = core_returns[core_returns["date"] >= sector_start_date].reset_index(drop=True)
    sector_returns_daily = sector_returns_daily.merge(core_returns_filtered, on="date", how="left")

    sector_ret_cols = [f"r_{col}" for col in available_sectors]
    for col in available_sectors:
        sector_returns_daily[f"r_{col}"] = log_return(sector_panel_daily[col])

    cols = list(sector_returns_daily.columns)
    idx = cols.index("r_vnindex")
    for col in sector_ret_cols:
        cols.remove(col)
    sector_returns_daily = sector_returns_daily[cols[: idx + 1] + sector_ret_cols + cols[idx + 1 :]]
    sector_returns_daily = sector_returns_daily.dropna(subset=["r_vnindex", *sector_ret_cols]).reset_index(drop=True)

    sector_panel_weekly = (
        sector_panel_daily.set_index("date").resample("W-FRI").last().dropna(how="all").reset_index()
    )
    sector_returns_weekly = pd.DataFrame({"date": sector_panel_weekly["date"]})
    for col in available_sectors:
        sector_returns_weekly[f"r_{col}"] = log_return(sector_panel_weekly[col])

    if core_weekly_returns_path.exists():
        core_weekly = pd.read_csv(core_weekly_returns_path)
        core_weekly["date"] = pd.to_datetime(core_weekly["date"])
        core_weekly = core_weekly[core_weekly["date"] >= sector_start_date].reset_index(drop=True)
        sector_returns_weekly = sector_returns_weekly.merge(core_weekly, on="date", how="left")
        w_cols = list(sector_returns_weekly.columns)
        w_idx = w_cols.index("r_vnindex")
        for col in sector_ret_cols:
            if col in w_cols:
                w_cols.remove(col)
        sector_returns_weekly = sector_returns_weekly[
            w_cols[: w_idx + 1] + sector_ret_cols + w_cols[w_idx + 1 :]
        ]

    sector_returns_weekly = sector_returns_weekly.dropna(subset=["r_vnindex", *sector_ret_cols]).reset_index(drop=True)

    SECTOR_INPUT_DIR.mkdir(parents=True, exist_ok=True)
    daily_out = SECTOR_INPUT_DIR / "final_sector_daily_returns.csv"
    weekly_out = SECTOR_INPUT_DIR / "final_sector_weekly_returns.csv"
    fill_audit_out = SECTOR_INPUT_DIR / "sector_fill_audit.csv"

    sector_returns_daily.to_csv(daily_out, index=False, encoding="utf-8-sig")
    sector_returns_weekly.to_csv(weekly_out, index=False, encoding="utf-8-sig")
    fill_audit = pd.concat(fill_audits, ignore_index=True) if fill_audits else pd.DataFrame()
    fill_audit.to_csv(fill_audit_out, index=False, encoding="utf-8-sig")

    print(f"\n  [Thành công] Daily sector returns: {daily_out.relative_to(daily_out.parents[2])}")
    print(f"  [Thành công] Weekly sector returns: {weekly_out.relative_to(weekly_out.parents[2])}")
    print(f"  [Thành công] Forward-fill audit: {fill_audit_out.relative_to(fill_audit_out.parents[2])}")
    print("=" * 72)


if __name__ == "__main__":
    main()
