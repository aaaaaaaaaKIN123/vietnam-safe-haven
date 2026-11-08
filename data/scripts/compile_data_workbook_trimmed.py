"""
compile_data_workbook_trimmed.py
=================================
Same as compile_data_workbook.py but TRIMS each dataset to only include
observations within the actual research sample period used in the paper.

Research sample periods (from manuscript & metadata):
  - Core:           2010-01-04  →  2026-05-27  (~4,086 obs)
  - Crypto ext:     2017-11-09  →  2026-05-27  (~2,130 obs)
  - Sector ext:     2016-01-29  →  2026-05-27  (~2,575 obs)
  - Bond term str:  2007-07-03  →  2026-05-27  (~4,469 obs with full 1Y/5Y/10Y)

Usage:
    python data/scripts/compile_data_workbook_trimmed.py

Output:
    data/data_summary_trimmed.xlsx   (8 sheets, research sample only)
"""

import os
import sys
import pandas as pd
from pathlib import Path
from datetime import datetime
from openpyxl import Workbook
from openpyxl.styles import (
    Font, PatternFill, Alignment, Border, Side,
)
from openpyxl.utils import get_column_letter

# ── paths ────────────────────────────────────────────────────────────────────
PROJECT_ROOT = Path(__file__).resolve().parents[2]
DATA_DIR = PROJECT_ROOT / "data"
MODEL_INPUT = DATA_DIR / "02_model_input"
OUTPUT_FILE = DATA_DIR / "data_summary_trimmed.xlsx"

# ── research sample date ranges ─────────────────────────────────────────────
# These match the actual model estimation windows from the paper.
CORE_START = "2010-01-04"
CORE_END = "2026-05-27"

CRYPTO_START = "2017-11-09"
CRYPTO_END = "2026-05-27"

SECTOR_START = "2016-01-29"
SECTOR_END = "2026-05-27"

BOND_TS_START = "2007-07-03"
BOND_TS_END = "2026-05-27"

# ── styling constants ────────────────────────────────────────────────────────
HEADER_FONT = Font(name="Times New Roman", bold=True, color="FFFFFF", size=11)
HEADER_FILL = PatternFill(start_color="1F4E79", end_color="1F4E79",
                          fill_type="solid")
HEADER_ALIGN = Alignment(horizontal="center", vertical="center",
                          wrap_text=True)

BODY_FONT = Font(name="Times New Roman", size=11)
BODY_ALIGN_LEFT = Alignment(horizontal="left", vertical="center")
BODY_ALIGN_RIGHT = Alignment(horizontal="right", vertical="center")
BODY_ALIGN_CENTER = Alignment(horizontal="center", vertical="center")

ALT_ROW_FILL = PatternFill(start_color="D6E4F0", end_color="D6E4F0",
                            fill_type="solid")
THIN_BORDER = Border(
    left=Side(style="thin", color="B0B0B0"),
    right=Side(style="thin", color="B0B0B0"),
    top=Side(style="thin", color="B0B0B0"),
    bottom=Side(style="thin", color="B0B0B0"),
)

DATE_FMT = "YYYY-MM-DD"
NUM_FMT_INT = "#,##0"
NUM_FMT_DEC4 = "#,##0.0000"


# ── helper functions ─────────────────────────────────────────────────────────

def style_header(ws, ncols):
    for col_idx in range(1, ncols + 1):
        cell = ws.cell(row=1, column=col_idx)
        cell.font = HEADER_FONT
        cell.fill = HEADER_FILL
        cell.alignment = HEADER_ALIGN
        cell.border = THIN_BORDER


def style_body(ws, nrows, ncols, date_col_indices=None, int_col_indices=None):
    if date_col_indices is None:
        date_col_indices = set()
    if int_col_indices is None:
        int_col_indices = set()
    for row_idx in range(2, nrows + 1):
        is_alt = (row_idx % 2 == 0)
        for col_idx in range(1, ncols + 1):
            cell = ws.cell(row=row_idx, column=col_idx)
            cell.font = BODY_FONT
            cell.border = THIN_BORDER
            if is_alt:
                cell.fill = ALT_ROW_FILL
            if col_idx in date_col_indices:
                cell.alignment = BODY_ALIGN_CENTER
                cell.number_format = DATE_FMT
            elif col_idx in int_col_indices:
                cell.alignment = BODY_ALIGN_RIGHT
                cell.number_format = NUM_FMT_INT
            elif isinstance(cell.value, (int, float)):
                cell.alignment = BODY_ALIGN_RIGHT
                cell.number_format = NUM_FMT_DEC4
            else:
                cell.alignment = BODY_ALIGN_LEFT


def auto_column_width(ws, ncols, max_width=40, min_width=10):
    for col_idx in range(1, ncols + 1):
        max_len = 0
        col_letter = get_column_letter(col_idx)
        for row in ws.iter_rows(min_col=col_idx, max_col=col_idx,
                                min_row=1, max_row=min(ws.max_row, 100)):
            for cell in row:
                if cell.value is not None:
                    cell_len = len(str(cell.value))
                    if cell_len > max_len:
                        max_len = cell_len
        width = min(max(max_len + 2, min_width), max_width)
        ws.column_dimensions[col_letter].width = width


def add_autofilter_and_freeze(ws, ncols):
    last_col = get_column_letter(ncols)
    ws.auto_filter.ref = f"A1:{last_col}{ws.max_row}"
    ws.freeze_panes = "A2"


def trim_df(df, start, end):
    """Trim dataframe to date range [start, end]."""
    df["date"] = pd.to_datetime(df["date"])
    mask = (df["date"] >= start) & (df["date"] <= end)
    df = df.loc[mask].copy()
    df["date"] = df["date"].dt.strftime("%Y-%m-%d")
    return df


def write_df_to_sheet(wb, sheet_name, df):
    """Write a DataFrame to a new sheet with professional formatting."""
    ws = wb.create_sheet(sheet_name)
    headers = list(df.columns)
    ws.append(headers)

    date_col_indices = set()
    int_col_indices = set()
    for i, col in enumerate(headers, 1):
        if col == "date":
            date_col_indices.add(i)
        elif col.startswith("stress_"):
            int_col_indices.add(i)

    for _, row in df.iterrows():
        row_values = []
        for col in headers:
            val = row[col]
            if pd.isna(val):
                row_values.append(None)
            elif col == "date":
                try:
                    row_values.append(
                        datetime.strptime(str(val), "%Y-%m-%d").date()
                    )
                except (ValueError, TypeError):
                    row_values.append(str(val))
            elif col.startswith("stress_"):
                row_values.append(int(val))
            else:
                try:
                    row_values.append(float(val))
                except (ValueError, TypeError):
                    row_values.append(str(val))
        ws.append(row_values)

    ncols = len(headers)
    nrows = ws.max_row
    style_header(ws, ncols)
    style_body(ws, nrows, ncols,
               date_col_indices=date_col_indices,
               int_col_indices=int_col_indices)
    auto_column_width(ws, ncols, max_width=22, min_width=12)
    add_autofilter_and_freeze(ws, ncols)
    return nrows - 1, ncols


# ═══════════════════════════════════════════════════════════════════════════════
# SHEET 1: Data Source Overview
# ═══════════════════════════════════════════════════════════════════════════════
def build_sheet_data_sources(wb):
    ws = wb.active
    ws.title = "1. Nguon du lieu"

    headers = [
        "STT", "Nhom tai san", "Ten file goc", "Nguon thu thap",
        "Ticker / Ma", "Tan suat", "Ngay bat dau", "Ngay ket thuc",
        "So quan sat", "Ghi chu"
    ]

    rows = [
        [1, "Co phieu", "chi_so_vnindex_vietnam.xlsx", "TradingView / KBS",
         "VNINDEX", "Ngay", CORE_START, CORE_END, 4086,
         "Chi so VNIndex OHLCV - mau loi"],
        [2, "Vang noi dia", "gia_vang_sjc_lich_su.xlsx", "Investing.com",
         "SJC", "Ngay", CORE_START, CORE_END, 4086,
         "Gia vang mieng SJC ban ra (trieu VND/luong)"],
        [3, "Vang quoc te", "xau_usd_historical_data.csv", "Investing.com",
         "XAU/USD", "Ngay", CORE_START, CORE_END, 4086,
         "Gia vang giao ngay USD"],
        [4, "Vang ky han", "gold_futures_gc_daily.csv", "Yahoo Finance",
         "GC=F", "Ngay", CORE_START, CORE_END, "",
         "Gold futures (kiem dinh ben vung)"],
        [5, "TPCP VN 10Y", "trai_phieu_chinh_phu_viet_nam_10n.xlsx",
         "Investing.com / TradingView", "VN10Y", "Ngay",
         CORE_START, CORE_END, 4086, "Loi suat TPCP 10 nam"],
        [6, "TPCP VN 5Y", "trai_phieu_chinh_phu_viet_nam_5n.xlsx",
         "Investing.com / TradingView", "VN5Y", "Ngay",
         CORE_START, CORE_END, 4086, "Loi suat TPCP 5 nam"],
        [7, "TPCP VN 1Y", "vn_gbond_1y_yield_raw.xlsx", "Investing.com",
         "VN01Y", "Ngay", BOND_TS_START, BOND_TS_END, 4469,
         "Loi suat TPCP 1 nam (bond term structure)"],
        [8, "TPCP TRI 10Y", "chi_so_bond_total_return_vn.xlsx",
         "Mo phong", "-", "Ngay", CORE_START, CORE_END, 4086,
         "Chi so tong thu nhap TPCP 10Y (mo phong)"],
        [9, "Bien dong yield", "yield_volatility_10yr_lcy_bonds.csv",
         "ADB / AsianBondsOnline", "LCY 10Y", "Ngay",
         CORE_START, CORE_END, "", "Bien dong loi suat TPCP 10 nam"],
        [10, "Ty gia", "ty_gia_usd_vnd_vn.xlsx", "TradingView / IDC",
         "USDVND", "Ngay", CORE_START, CORE_END, 4086, "Ty gia USD/VND"],
        [11, "Lai suat lien NH", "lai_suat_lien_ngan_hang_vn.xlsx",
         "Investing.com", "VN Interbank", "Ngay",
         CORE_START, CORE_END, "", "Lai suat lien ngan hang VN"],
        [12, "Lai suat dieu hanh", "lai_suat_dieu_hanh_vn.xlsx", "NHNN",
         "SBV Rate", "Su kien", "-", "-", "",
         "Lai suat dieu hanh NHNN (forward-fill)"],
        [13, "Dau Brent", "brent_futures_bz_daily.csv", "Yahoo Finance",
         "BZ=F", "Ngay", CORE_START, CORE_END, 4086, "Brent oil futures"],
        [14, "Dau WTI", "wti_futures_cl_daily.csv", "Yahoo Finance",
         "CL=F", "Ngay", CORE_START, CORE_END, "",
         "WTI oil futures (kiem dinh ben vung)"],
        [15, "Bitcoin", "bitcoin_usd_daily.csv", "Yahoo Finance",
         "BTC-USD", "Ngay", CRYPTO_START, CRYPTO_END, 2130,
         "Gia BTC/USD (mo rong crypto)"],
        [16, "Ethereum", "ethereum_usd_daily.csv", "Yahoo Finance",
         "ETH-USD", "Ngay", CRYPTO_START, CRYPTO_END, 2130,
         "Gia ETH/USD (mo rong crypto)"],
        [17, "VIX", "vix_daily.csv", "Yahoo Finance / CBOE",
         "^VIX", "Ngay", CORE_START, CORE_END, 4086,
         "Chi so bien dong toan cau"],
        [18, "Sector VNFIN", "vnfin_clean.csv", "SSI iBoard API",
         "VNFIN", "Ngay", SECTOR_START, SECTOR_END, 2575,
         "Chi so nganh Tai chinh"],
        [19, "Sector VNREAL", "vnreal_clean.csv", "SSI iBoard API",
         "VNREAL", "Ngay", SECTOR_START, SECTOR_END, 2575,
         "Chi so nganh Bat dong san"],
        [20, "Sector VNIND", "vnind_clean.csv", "SSI iBoard API",
         "VNIND", "Ngay", SECTOR_START, SECTOR_END, 2575,
         "Chi so nganh Cong nghiep"],
        [21, "Sector VNIT", "vnit_clean.csv", "SSI iBoard API",
         "VNIT", "Ngay", SECTOR_START, SECTOR_END, 2575,
         "Chi so nganh CNTT"],
        [22, "Sector VNHEAL", "vnheal_clean.csv", "SSI iBoard API",
         "VNHEAL", "Ngay", SECTOR_START, SECTOR_END, 2575,
         "Chi so nganh Y te"],
        [23, "Sector VNUTI", "vnuti_clean.csv", "SSI iBoard API",
         "VNUTI", "Ngay", SECTOR_START, SECTOR_END, 2575,
         "Chi so nganh Tien ich"],
        [24, "Sector VNCONS", "vncons_clean.csv", "SSI iBoard API",
         "VNCONS", "Ngay", SECTOR_START, SECTOR_END, 2575,
         "Chi so nganh Tieu dung thiet yeu"],
        [25, "Sector VNCOND", "vncond_clean.csv", "SSI iBoard API",
         "VNCOND", "Ngay", SECTOR_START, SECTOR_END, 2575,
         "Chi so nganh Tieu dung khong thiet yeu"],
        [26, "Sector VNMAT", "vnmat_clean.csv", "SSI iBoard API",
         "VNMAT", "Ngay", SECTOR_START, SECTOR_END, 2575,
         "Chi so nganh Nguyen vat lieu"],
        [27, "Sector VNENE", "vnene_clean.csv", "SSI iBoard API",
         "VNENE", "Ngay", SECTOR_START, SECTOR_END, 2575,
         "Chi so nganh Nang luong"],
    ]

    ws.append(headers)
    for row in rows:
        ws.append(row)

    ncols = len(headers)
    nrows = ws.max_row
    style_header(ws, ncols)
    style_body(ws, nrows, ncols, int_col_indices={1, 9})
    auto_column_width(ws, ncols)
    add_autofilter_and_freeze(ws, ncols)
    print(f"  Sheet 1: {nrows - 1} rows")


# ═══════════════════════════════════════════════════════════════════════════════
# SHEET 2: Variable Dictionary
# ═══════════════════════════════════════════════════════════════════════════════
def build_sheet_variable_dict(wb):
    ws = wb.create_sheet("2. Tu dien bien so")

    headers = ["STT", "Ten bien", "Don vi", "Y nghia kinh te",
               "Cong thuc / Ghi chu"]

    variables = [
        [1, "date", "YYYY-MM-DD", "Ngay giao dich",
         "Lich giao dich VNIndex"],
        [2, "r_vnindex", "% (log return x100)", "Suat sinh loi VNIndex",
         "100 x ln(Pt/Pt-1)"],
        [3, "r_sjc_gold", "% (log return x100)",
         "Suat sinh loi vang SJC noi dia",
         "100 x ln(SJCt/SJCt-1)"],
        [4, "r_xau_vnd", "% (log return x100)",
         "Suat sinh loi vang quoc te quy VND",
         "100 x ln(XAU_VNDt/XAU_VNDt-1)"],
        [5, "r_gold_premium", "log ratio", "Phan bu vang noi dia",
         "ln(SJC_VND/luong) - ln(XAU_VND/luong)"],
        [6, "r_bond_tri", "% (log return x100)",
         "Suat sinh loi TPCP 10Y (TRI)",
         "100 x ln(TRIt/TRIt-1); TRI mo phong"],
        [7, "r_brent", "% (log return x100)", "Suat sinh loi dau Brent",
         "100 x ln(Brentt/Brentt-1)"],
        [8, "r_wti", "% (log return x100)", "Suat sinh loi dau WTI",
         "100 x ln(WTIt/WTIt-1); kiem dinh ben vung"],
        [9, "r_btc", "% (log return x100)", "Suat sinh loi Bitcoin",
         "100 x ln(BTCt/BTCt-1); mo rong crypto"],
        [10, "r_eth", "% (log return x100)", "Suat sinh loi Ethereum",
         "100 x ln(ETHt/ETHt-1); mo rong crypto"],
        [11, "d_vn10y_yield", "Diem phan tram",
         "Thay doi loi suat TPCP 10Y", "yt - yt-1"],
        [12, "d_vn5y_yield", "Diem phan tram",
         "Thay doi loi suat TPCP 5Y", "yt - yt-1"],
        [13, "d_interbank_rate", "Diem phan tram",
         "Thay doi lai suat lien ngan hang", "Sai phan bac 1"],
        [14, "policy_rate", "% (muc)", "Lai suat dieu hanh NHNN",
         "Forward-fill tu su kien chinh sach"],
        [15, "d_policy_rate", "Diem phan tram",
         "Thay doi lai suat dieu hanh",
         "Sai phan bac 1 sau forward-fill"],
        [16, "d_vix", "Diem", "Thay doi chi so VIX", "VIXt - VIXt-1"],
        [17, "interbank_rate", "% (muc)", "Lai suat lien ngan hang VN",
         "Muc lai suat"],
        [18, "vn10y_yield", "% (muc)", "Loi suat TPCP 10 nam",
         "Muc loi suat"],
        [19, "vn5y_yield", "% (muc)", "Loi suat TPCP 5 nam",
         "Muc loi suat"],
        [20, "vn10y_yield_volatility", "He so",
         "Bien dong loi suat TPCP 10Y",
         "Tu ADB/AsianBondsOnline"],
        [21, "vix", "Diem (muc)", "Chi so bien dong VIX", "Muc VIX"],
        [22, "stress_vnindex_q10", "Dummy (0/1)", "Stress tail Q10",
         "1 neu r_vnindex <= quantile 10%"],
        [23, "stress_vnindex_q05", "Dummy (0/1)", "Stress tail Q05",
         "1 neu r_vnindex <= quantile 5%"],
        [24, "stress_vnindex_q01", "Dummy (0/1)", "Stress tail Q01",
         "1 neu r_vnindex <= quantile 1%"],
        [25, "r_vnfin ... r_vnene", "% (log return x100)",
         "Suat sinh loi 10 sector HOSE",
         "Tuong tu r_vnindex; mo rong nganh"],
        [26, "y_1y / y_5y / y_10y", "% (muc)",
         "Loi suat TPCP theo ky han",
         "Muc loi suat 1/5/10 nam"],
        [27, "d_y_1y_bps / d_y_5y_bps / d_y_10y_bps",
         "Diem co ban (bps)", "Thay doi loi suat",
         "100 x (yt - yt-1)"],
        [28, "bond_slope_10y_1y", "Diem phan tram",
         "Do doc duong cong loi suat 10Y-1Y", "y_10y - y_1y"],
        [29, "bond_slope_10y_5y", "Diem phan tram",
         "Do doc duong cong loi suat 10Y-5Y", "y_10y - y_5y"],
        [30, "bond_curvature_1_5_10", "Diem phan tram",
         "Do cong duong cong loi suat",
         "y_10y - 2*y_5y + y_1y"],
        [31, "bond_tri_1y / bond_tri_5y", "Chi so",
         "TRI mo phong TPCP 1Y/5Y",
         "Mo phong tu yield; khong phai chi so chinh thuc"],
        [32, "r_bond_tri_1y / r_bond_tri_5y / r_bond_tri_10y",
         "% (log return x100)", "Suat sinh loi TRI theo ky han",
         "100 x ln(TRIt/TRIt-1)"],
    ]

    ws.append(headers)
    for row in variables:
        ws.append(row)

    ncols = len(headers)
    nrows = ws.max_row
    style_header(ws, ncols)
    style_body(ws, nrows, ncols, int_col_indices={1})
    auto_column_width(ws, ncols, max_width=60)
    add_autofilter_and_freeze(ws, ncols)
    print(f"  Sheet 2: {nrows - 1} rows")


# ═══════════════════════════════════════════════════════════════════════════════
# MAIN
# ═══════════════════════════════════════════════════════════════════════════════
def main():
    print("=" * 70)
    print("  COMPILE TRIMMED DATA WORKBOOK (research sample only)")
    print(f"  Output: {OUTPUT_FILE.name}")
    print("=" * 70)

    wb = Workbook()

    # Sheet 1
    print("\n[1/8] Nguon du lieu...")
    build_sheet_data_sources(wb)

    # Sheet 2
    print("\n[2/8] Tu dien bien so...")
    build_sheet_variable_dict(wb)

    # Sheet 3: Core daily returns - trimmed to 2010-01-04 .. 2026-05-27
    print(f"\n[3/8] Core Returns (Daily) [{CORE_START} -> {CORE_END}]...")
    df = pd.read_csv(MODEL_INPUT / "core" / "final_daily_returns.csv",
                     low_memory=False)
    df = trim_df(df, CORE_START, CORE_END)
    r, c = write_df_to_sheet(wb, "3. Core Returns (Daily)", df)
    print(f"  -> {r} rows x {c} cols")

    # Sheet 4: Core weekly returns - trimmed
    print(f"\n[4/8] Core Returns (Weekly) [{CORE_START} -> {CORE_END}]...")
    df = pd.read_csv(MODEL_INPUT / "core" / "final_weekly_returns.csv",
                     low_memory=False)
    df = trim_df(df, CORE_START, CORE_END)
    r, c = write_df_to_sheet(wb, "4. Core Returns (Weekly)", df)
    print(f"  -> {r} rows x {c} cols")

    # Sheet 5: Core daily panel - trimmed
    print(f"\n[5/8] Core Panel (Daily) [{CORE_START} -> {CORE_END}]...")
    df = pd.read_csv(MODEL_INPUT / "core" / "final_daily_panel.csv",
                     low_memory=False)
    df = trim_df(df, CORE_START, CORE_END)
    r, c = write_df_to_sheet(wb, "5. Core Panel (Daily)", df)
    print(f"  -> {r} rows x {c} cols")

    # Sheet 6: Crypto extension - trimmed to 2017-11-09 .. 2026-05-27
    print(f"\n[6/8] Crypto Extension [{CRYPTO_START} -> {CRYPTO_END}]...")
    df = pd.read_csv(
        MODEL_INPUT / "crypto_extension" / "final_crypto_daily_returns.csv",
        low_memory=False)
    df = trim_df(df, CRYPTO_START, CRYPTO_END)
    r, c = write_df_to_sheet(wb, "6. Crypto Extension", df)
    print(f"  -> {r} rows x {c} cols")

    # Sheet 7: Sector extension - trimmed to 2016-01-29 .. 2026-05-27
    print(f"\n[7/8] Sector Extension [{SECTOR_START} -> {SECTOR_END}]...")
    df = pd.read_csv(
        MODEL_INPUT / "sector_extension" / "final_sector_daily_returns.csv",
        low_memory=False)
    df = trim_df(df, SECTOR_START, SECTOR_END)
    r, c = write_df_to_sheet(wb, "7. Sector Extension", df)
    print(f"  -> {r} rows x {c} cols")

    # Sheet 8: Bond term structure - trimmed to 2007-07-03 .. 2026-05-27
    print(f"\n[8/8] Bond Term Structure [{BOND_TS_START} -> {BOND_TS_END}]...")
    df = pd.read_csv(
        MODEL_INPUT / "bond_term_structure"
        / "final_bond_term_structure_daily.csv",
        low_memory=False)
    df = trim_df(df, BOND_TS_START, BOND_TS_END)
    r, c = write_df_to_sheet(wb, "8. Bond Term Structure", df)
    print(f"  -> {r} rows x {c} cols")

    # Save
    print(f"\nSaving to {OUTPUT_FILE}...")
    wb.save(str(OUTPUT_FILE))

    file_size_mb = OUTPUT_FILE.stat().st_size / (1024 * 1024)
    print(f"\n{'=' * 70}")
    print(f"  DONE! File: {OUTPUT_FILE}")
    print(f"  Size: {file_size_mb:.2f} MB")
    print(f"  Sheets: {len(wb.sheetnames)}")
    for name in wb.sheetnames:
        print(f"    - {name}")
    print(f"{'=' * 70}")


if __name__ == "__main__":
    main()
