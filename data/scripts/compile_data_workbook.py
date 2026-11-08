"""
compile_data_workbook.py
========================
Compile all model-input CSVs and metadata into a single, professionally
formatted Excel workbook (data_summary_workbook.xlsx) for submission.

Usage:
    python data/scripts/compile_data_workbook.py

Output:
    data/data_summary_workbook.xlsx   (8 sheets)

Requirements:
    pip install openpyxl pandas
"""

import os
import sys
import csv
import pandas as pd
from pathlib import Path
from openpyxl import Workbook
from openpyxl.styles import (
    Font, PatternFill, Alignment, Border, Side, numbers
)
from openpyxl.utils import get_column_letter

# ── paths ────────────────────────────────────────────────────────────────────
PROJECT_ROOT = Path(__file__).resolve().parents[2]  # ktltcnc_ck/
DATA_DIR = PROJECT_ROOT / "data"
MODEL_INPUT = DATA_DIR / "02_model_input"
METADATA = DATA_DIR / "metadata"
OUTPUT_FILE = DATA_DIR / "data_summary_workbook.xlsx"

# ── styling constants ────────────────────────────────────────────────────────
HEADER_FONT = Font(name="Times New Roman", bold=True, color="FFFFFF", size=11)
HEADER_FILL = PatternFill(start_color="1F4E79", end_color="1F4E79", fill_type="solid")
HEADER_ALIGN = Alignment(horizontal="center", vertical="center", wrap_text=True)

BODY_FONT = Font(name="Times New Roman", size=11)
BODY_ALIGN_LEFT = Alignment(horizontal="left", vertical="center")
BODY_ALIGN_RIGHT = Alignment(horizontal="right", vertical="center")
BODY_ALIGN_CENTER = Alignment(horizontal="center", vertical="center")

ALT_ROW_FILL = PatternFill(start_color="D6E4F0", end_color="D6E4F0", fill_type="solid")
THIN_BORDER = Border(
    left=Side(style="thin", color="B0B0B0"),
    right=Side(style="thin", color="B0B0B0"),
    top=Side(style="thin", color="B0B0B0"),
    bottom=Side(style="thin", color="B0B0B0"),
)

DATE_FORMAT = "YYYY-MM-DD"
NUM_FORMAT_INT = "#,##0"
NUM_FORMAT_DEC4 = "#,##0.0000"
NUM_FORMAT_DEC6 = "#,##0.000000"


def style_header(ws, ncols):
    """Apply header styling to row 1."""
    for col_idx in range(1, ncols + 1):
        cell = ws.cell(row=1, column=col_idx)
        cell.font = HEADER_FONT
        cell.fill = HEADER_FILL
        cell.alignment = HEADER_ALIGN
        cell.border = THIN_BORDER


def style_body(ws, nrows, ncols, date_col_indices=None, int_col_indices=None):
    """Apply body styling with alternating row colors."""
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
                cell.number_format = DATE_FORMAT
            elif col_idx in int_col_indices:
                cell.alignment = BODY_ALIGN_RIGHT
                cell.number_format = NUM_FORMAT_INT
            elif isinstance(cell.value, (int, float)):
                cell.alignment = BODY_ALIGN_RIGHT
                # Use 4 decimal places for most numeric data
                cell.number_format = NUM_FORMAT_DEC4
            else:
                cell.alignment = BODY_ALIGN_LEFT


def auto_column_width(ws, ncols, max_width=40, min_width=10):
    """Auto-fit column widths based on content."""
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
    """Add auto-filter and freeze the header row."""
    last_col = get_column_letter(ncols)
    ws.auto_filter.ref = f"A1:{last_col}{ws.max_row}"
    ws.freeze_panes = "A2"


# ═══════════════════════════════════════════════════════════════════════════════
# SHEET 1: Tổng quan nguồn dữ liệu (Data Source Overview)
# ═══════════════════════════════════════════════════════════════════════════════
def build_sheet_data_sources(wb):
    """Build the data source overview sheet from curated inventory."""
    ws = wb.active
    ws.title = "1. Nguồn dữ liệu"

    # Curated data source table
    headers = [
        "STT", "Nhóm tài sản", "Tên file gốc", "Nguồn thu thập",
        "Ticker / Mã", "Tần suất", "Ngày bắt đầu", "Ngày kết thúc",
        "Số quan sát", "Ghi chú"
    ]

    rows = [
        [1, "Cổ phiếu", "chi_so_vnindex_vietnam.xlsx", "TradingView / KBS", "VNINDEX", "Ngày", "2006-05-08", "2026-05-27", 5001, "Chỉ số VNIndex OHLCV"],
        [2, "Vàng nội địa", "gia_vang_sjc_lich_su.xlsx", "Investing.com", "SJC", "Ngày", "2009-10-01", "2026-05-27", "", "Giá vàng miếng SJC bán ra (triệu VND/lượng)"],
        [3, "Vàng quốc tế", "xau_usd_historical_data.csv", "Investing.com", "XAU/USD", "Ngày", "2000-01-04", "2026-05-26", 4270, "Giá vàng giao ngay USD"],
        [4, "Vàng kỳ hạn", "gold_futures_gc_daily.csv", "Yahoo Finance", "GC=F", "Ngày", "2010-01-04", "2026-05-26", 4129, "Gold futures (kiểm định bền vững)"],
        [5, "TPCP VN 10 năm", "trai_phieu_chinh_phu_viet_nam_10n.xlsx", "Investing.com / TradingView", "VN10Y", "Ngày", "2007-07-03", "2026-05-27", 4397, "Lợi suất TPCP 10 năm"],
        [6, "TPCP VN 5 năm", "trai_phieu_chinh_phu_viet_nam_5n.xlsx", "Investing.com / TradingView", "VN5Y", "Ngày", "2007-07-02", "2026-05-27", 4507, "Lợi suất TPCP 5 năm"],
        [7, "TPCP VN 1 năm", "vn_gbond_1y_yield_raw.xlsx", "Investing.com", "VN01Y", "Ngày", "2007-07-03", "2026-06-19", 4671, "Lợi suất TPCP 1 năm"],
        [8, "TPCP TRI 10Y", "chi_so_bond_total_return_vn.xlsx", "Mô phỏng", "—", "Ngày", "2010-03-24", "2026-05-27", "", "Chỉ số tổng thu nhập TPCP 10Y (mô phỏng)"],
        [9, "Biến động yield", "yield_volatility_10yr_lcy_bonds.csv", "ADB / AsianBondsOnline", "LCY 10Y", "Ngày", "2006-01-01", "2026-05-27", "", "Biến động lợi suất TPCP 10 năm"],
        [10, "Tỷ giá", "ty_gia_usd_vnd_vn.xlsx", "TradingView / IDC", "USDVND", "Ngày", "2000-04-17", "2026-05-27", 4278, "Tỷ giá USD/VND"],
        [11, "Lãi suất liên NH", "lai_suat_lien_ngan_hang_vn.xlsx", "Investing.com", "VN Interbank", "Ngày", "2009-10-01", "2026-05-27", "", "Lãi suất liên ngân hàng VN"],
        [12, "Lãi suất điều hành", "lai_suat_dieu_hanh_vn.xlsx", "NHNN", "SBV Rate", "Sự kiện", "—", "—", 78, "Lãi suất điều hành NHNN (forward-fill)"],
        [13, "Dầu Brent", "brent_futures_bz_daily.csv", "Yahoo Finance", "BZ=F", "Ngày", "2010-01-04", "2026-05-26", 4129, "Brent oil futures"],
        [14, "Dầu WTI", "wti_futures_cl_daily.csv", "Yahoo Finance", "CL=F", "Ngày", "2010-01-04", "2026-05-26", 4129, "WTI oil futures (kiểm định bền vững)"],
        [15, "Bitcoin", "bitcoin_usd_daily.csv", "Yahoo Finance", "BTC-USD", "Ngày", "2014-09-17", "2026-05-26", 4270, "Giá BTC/USD"],
        [16, "Ethereum", "ethereum_usd_daily.csv", "Yahoo Finance", "ETH-USD", "Ngày", "2017-11-09", "2026-05-26", 3121, "Giá ETH/USD"],
        [17, "VIX", "vix_daily.csv", "Yahoo Finance / CBOE", "^VIX", "Ngày", "2010-01-04", "2026-05-26", 4277, "Chỉ số biến động toàn cầu"],
        [18, "Sector VNFIN", "vnfin_clean.csv", "SSI iBoard API", "VNFIN", "Ngày", "2016-01-28", "2026-06-19", 2586, "Chỉ số ngành Tài chính"],
        [19, "Sector VNREAL", "vnreal_clean.csv", "SSI iBoard API", "VNREAL", "Ngày", "2016-01-28", "2026-06-19", 2586, "Chỉ số ngành Bất động sản"],
        [20, "Sector VNIND", "vnind_clean.csv", "SSI iBoard API", "VNIND", "Ngày", "2016-01-28", "2026-06-19", 2586, "Chỉ số ngành Công nghiệp"],
        [21, "Sector VNIT", "vnit_clean.csv", "SSI iBoard API", "VNIT", "Ngày", "2016-01-28", "2026-06-19", 2586, "Chỉ số ngành CNTT"],
        [22, "Sector VNHEAL", "vnheal_clean.csv", "SSI iBoard API", "VNHEAL", "Ngày", "2016-01-28", "2026-06-19", 2586, "Chỉ số ngành Y tế"],
        [23, "Sector VNUTI", "vnuti_clean.csv", "SSI iBoard API", "VNUTI", "Ngày", "2016-01-28", "2026-06-19", 2586, "Chỉ số ngành Tiện ích"],
        [24, "Sector VNCONS", "vncons_clean.csv", "SSI iBoard API", "VNCONS", "Ngày", "2016-01-28", "2026-06-19", 2586, "Chỉ số ngành Tiêu dùng thiết yếu"],
        [25, "Sector VNCOND", "vncond_clean.csv", "SSI iBoard API", "VNCOND", "Ngày", "2016-01-28", "2026-06-19", 2586, "Chỉ số ngành Tiêu dùng không thiết yếu"],
        [26, "Sector VNMAT", "vnmat_clean.csv", "SSI iBoard API", "VNMAT", "Ngày", "2016-01-28", "2026-06-19", 2586, "Chỉ số ngành Nguyên vật liệu"],
        [27, "Sector VNENE", "vnene_clean.csv", "SSI iBoard API", "VNENE", "Ngày", "2016-01-28", "2026-06-19", 2586, "Chỉ số ngành Năng lượng"],
    ]

    ws.append(headers)
    for row in rows:
        ws.append(row)

    ncols = len(headers)
    nrows = ws.max_row
    style_header(ws, ncols)
    style_body(ws, nrows, ncols,
               int_col_indices={1, 9})  # STT, Số quan sát
    auto_column_width(ws, ncols)
    add_autofilter_and_freeze(ws, ncols)

    print(f"  Sheet 1: {nrows - 1} rows")


# ═══════════════════════════════════════════════════════════════════════════════
# SHEET 2: Từ điển biến số (Variable Dictionary)
# ═══════════════════════════════════════════════════════════════════════════════
def build_sheet_variable_dict(wb):
    """Build variable dictionary sheet."""
    ws = wb.create_sheet("2. Từ điển biến số")

    headers = ["STT", "Tên biến", "Đơn vị", "Ý nghĩa kinh tế", "Công thức / Ghi chú"]

    variables = [
        [1, "date", "YYYY-MM-DD", "Ngày giao dịch", "Lịch giao dịch VNIndex"],
        [2, "r_vnindex", "% (log return ×100)", "Suất sinh lợi VNIndex", "100 × ln(Pₜ/Pₜ₋₁)"],
        [3, "r_sjc_gold", "% (log return ×100)", "Suất sinh lợi vàng SJC nội địa", "100 × ln(SJCₜ/SJCₜ₋₁)"],
        [4, "r_xau_vnd", "% (log return ×100)", "Suất sinh lợi vàng quốc tế quy VND", "100 × ln(XAU_VNDₜ/XAU_VNDₜ₋₁)"],
        [5, "r_gold_premium", "log ratio", "Phần bù vàng nội địa", "ln(SJC_VND/lượng) − ln(XAU_VND/lượng)"],
        [6, "r_bond_tri", "% (log return ×100)", "Suất sinh lợi TPCP 10Y (TRI)", "100 × ln(TRIₜ/TRIₜ₋₁); TRI mô phỏng"],
        [7, "r_brent", "% (log return ×100)", "Suất sinh lợi dầu Brent", "100 × ln(Brentₜ/Brentₜ₋₁)"],
        [8, "r_wti", "% (log return ×100)", "Suất sinh lợi dầu WTI", "100 × ln(WTIₜ/WTIₜ₋₁); kiểm định bền vững"],
        [9, "r_btc", "% (log return ×100)", "Suất sinh lợi Bitcoin", "100 × ln(BTCₜ/BTCₜ₋₁); mở rộng crypto"],
        [10, "r_eth", "% (log return ×100)", "Suất sinh lợi Ethereum", "100 × ln(ETHₜ/ETHₜ₋₁); mở rộng crypto"],
        [11, "d_vn10y_yield", "Điểm phần trăm", "Thay đổi lợi suất TPCP 10Y", "yₜ − yₜ₋₁"],
        [12, "d_vn5y_yield", "Điểm phần trăm", "Thay đổi lợi suất TPCP 5Y", "yₜ − yₜ₋₁"],
        [13, "d_interbank_rate", "Điểm phần trăm", "Thay đổi lãi suất liên ngân hàng", "Sai phân bậc 1"],
        [14, "policy_rate", "% (mức)", "Lãi suất điều hành NHNN", "Forward-fill từ sự kiện chính sách"],
        [15, "d_policy_rate", "Điểm phần trăm", "Thay đổi lãi suất điều hành", "Sai phân bậc 1 sau forward-fill"],
        [16, "d_vix", "Điểm", "Thay đổi chỉ số VIX", "VIXₜ − VIXₜ₋₁"],
        [17, "interbank_rate", "% (mức)", "Lãi suất liên ngân hàng VN", "Mức lãi suất"],
        [18, "vn10y_yield", "% (mức)", "Lợi suất TPCP 10 năm", "Mức lợi suất"],
        [19, "vn5y_yield", "% (mức)", "Lợi suất TPCP 5 năm", "Mức lợi suất"],
        [20, "vn10y_yield_volatility", "Hệ số", "Biến động lợi suất TPCP 10Y", "Từ ADB/AsianBondsOnline"],
        [21, "vix", "Điểm (mức)", "Chỉ số biến động VIX", "Mức VIX"],
        [22, "stress_vnindex_q10", "Dummy (0/1)", "Stress tail Q10", "1 nếu r_vnindex ≤ quantile 10%"],
        [23, "stress_vnindex_q05", "Dummy (0/1)", "Stress tail Q05", "1 nếu r_vnindex ≤ quantile 5%"],
        [24, "stress_vnindex_q01", "Dummy (0/1)", "Stress tail Q01", "1 nếu r_vnindex ≤ quantile 1%"],
        [25, "r_vnfin … r_vnene", "% (log return ×100)", "Suất sinh lợi 10 sector HOSE", "Tương tự r_vnindex; mở rộng ngành"],
        [26, "y_1y / y_5y / y_10y", "% (mức)", "Lợi suất TPCP theo kỳ hạn", "Mức lợi suất 1/5/10 năm"],
        [27, "d_y_1y_bps / d_y_5y_bps / d_y_10y_bps", "Điểm cơ bản (bps)", "Thay đổi lợi suất", "100 × (yₜ − yₜ₋₁)"],
        [28, "bond_slope_10y_1y", "Điểm phần trăm", "Độ dốc đường cong lợi suất 10Y−1Y", "y_10y − y_1y"],
        [29, "bond_slope_10y_5y", "Điểm phần trăm", "Độ dốc đường cong lợi suất 10Y−5Y", "y_10y − y_5y"],
        [30, "bond_curvature_1_5_10", "Điểm phần trăm", "Độ cong đường cong lợi suất", "y_10y − 2×y_5y + y_1y"],
        [31, "bond_tri_1y / bond_tri_5y", "Chỉ số", "TRI mô phỏng TPCP 1Y/5Y", "Mô phỏng từ yield; không phải chỉ số chính thức"],
        [32, "r_bond_tri_1y / r_bond_tri_5y / r_bond_tri_10y", "% (log return ×100)", "Suất sinh lợi TRI theo kỳ hạn", "100 × ln(TRIₜ/TRIₜ₋₁)"],
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
# HELPER: Write a CSV DataFrame to a sheet
# ═══════════════════════════════════════════════════════════════════════════════
def write_csv_to_sheet(wb, sheet_name, csv_path, description=None):
    """Read a CSV and write it to a new sheet with professional formatting."""
    print(f"  Reading {csv_path.name}...")
    df = pd.read_csv(csv_path, low_memory=False)

    ws = wb.create_sheet(sheet_name)

    # Write header
    headers = list(df.columns)
    ws.append(headers)

    # Identify column types
    date_col_indices = set()
    int_col_indices = set()
    for i, col in enumerate(headers, 1):
        if col == "date":
            date_col_indices.add(i)
        elif col.startswith("stress_"):
            int_col_indices.add(i)

    # Write data rows
    for _, row in df.iterrows():
        row_values = []
        for col in headers:
            val = row[col]
            if pd.isna(val):
                row_values.append(None)
            elif col == "date":
                # Convert to date object for proper Excel formatting
                try:
                    from datetime import datetime
                    row_values.append(datetime.strptime(str(val), "%Y-%m-%d").date())
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

    # Style
    style_header(ws, ncols)
    style_body(ws, nrows, ncols,
               date_col_indices=date_col_indices,
               int_col_indices=int_col_indices)
    auto_column_width(ws, ncols, max_width=22, min_width=12)
    add_autofilter_and_freeze(ws, ncols)

    print(f"  Sheet '{sheet_name}': {nrows - 1} rows × {ncols} cols")
    return ws


# ═══════════════════════════════════════════════════════════════════════════════
# MAIN
# ═══════════════════════════════════════════════════════════════════════════════
def main():
    print("=" * 70)
    print("  COMPILE DATA SUMMARY WORKBOOK")
    print("  Output: data/data_summary_workbook.xlsx")
    print("=" * 70)

    wb = Workbook()

    # Sheet 1: Tổng quan nguồn dữ liệu
    print("\n[1/8] Building: Tổng quan nguồn dữ liệu...")
    build_sheet_data_sources(wb)

    # Sheet 2: Từ điển biến số
    print("\n[2/8] Building: Từ điển biến số...")
    build_sheet_variable_dict(wb)

    # Sheet 3: Core - Suất sinh lợi ngày
    print("\n[3/8] Building: Core - Suất sinh lợi ngày...")
    write_csv_to_sheet(
        wb, "3. Core Returns (Daily)",
        MODEL_INPUT / "core" / "final_daily_returns.csv"
    )

    # Sheet 4: Core - Suất sinh lợi tuần
    print("\n[4/8] Building: Core - Suất sinh lợi tuần...")
    write_csv_to_sheet(
        wb, "4. Core Returns (Weekly)",
        MODEL_INPUT / "core" / "final_weekly_returns.csv"
    )

    # Sheet 5: Core - Bảng giá ngày
    print("\n[5/8] Building: Core - Bảng giá ngày...")
    write_csv_to_sheet(
        wb, "5. Core Panel (Daily)",
        MODEL_INPUT / "core" / "final_daily_panel.csv"
    )

    # Sheet 6: Crypto Extension
    print("\n[6/8] Building: Crypto Extension...")
    write_csv_to_sheet(
        wb, "6. Crypto Extension",
        MODEL_INPUT / "crypto_extension" / "final_crypto_daily_returns.csv"
    )

    # Sheet 7: Sector Extension
    print("\n[7/8] Building: Sector Extension...")
    write_csv_to_sheet(
        wb, "7. Sector Extension",
        MODEL_INPUT / "sector_extension" / "final_sector_daily_returns.csv"
    )

    # Sheet 8: Bond Term Structure
    print("\n[8/8] Building: Bond Term Structure...")
    write_csv_to_sheet(
        wb, "8. Bond Term Structure",
        MODEL_INPUT / "bond_term_structure" / "final_bond_term_structure_daily.csv"
    )

    # Save workbook
    print(f"\nSaving to {OUTPUT_FILE}...")
    wb.save(str(OUTPUT_FILE))

    # Summary
    file_size_mb = OUTPUT_FILE.stat().st_size / (1024 * 1024)
    print(f"\n{'=' * 70}")
    print(f"  DONE! File saved: {OUTPUT_FILE}")
    print(f"  File size: {file_size_mb:.2f} MB")
    print(f"  Total sheets: {len(wb.sheetnames)}")
    for name in wb.sheetnames:
        print(f"    - {name}")
    print(f"{'=' * 70}")


if __name__ == "__main__":
    main()
