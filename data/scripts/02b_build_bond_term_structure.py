"""Build the Vietnam government bond term-structure model inputs.

This script creates a self-contained bond term-structure extension for the
research pipeline. It does not overwrite raw files or the approved core model
inputs.

Current design:
1. clean available 1Y/5Y/10Y government bond yield files;
2. align them to the VNIndex trading calendar;
3. construct yield changes, level, slope, curvature and 10Y TRI return;
4. construct simulated constant-maturity TRI series for 1Y and 5Y;
5. write daily/weekly term-structure panels and a Vietnamese QA report.

The script is deliberately tolerant of a missing 1Y file. When 1Y is absent,
it writes partial outputs with the available 5Y/10Y variables and records the
missing 1Y requirement in metadata.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[2]
DATA = ROOT / "data"
RAW = DATA / "00_raw"
PROCESSED = DATA / "01_processed"
MODEL_INPUT = DATA / "02_model_input"
METADATA = DATA / "metadata"

BOND_RAW = RAW / "bonds"
BOND_TERM_RAW = BOND_RAW / "term_structure"
BOND_TERM_PROCESSED = PROCESSED / "bonds" / "term_structure"
BOND_TERM_INPUT = MODEL_INPUT / "bond_term_structure"

VNINDEX_RAW = RAW / "equity" / "chi_so_vnindex_vietnam.xlsx"
CORE_PANEL = MODEL_INPUT / "core" / "final_daily_panel.csv"

DATE_COL = "\u004e\u0067\u00e0\u0079"  # Ngay with Vietnamese diacritics
YIELD_COL = "\u004c\u1ee3\u0069 \u0073\u0075\u1ea5\u0074 \u0028\u0025\u0029"
TRI_COL = (
    "\u0043\u0068\u1ec9 \u0073\u1ed1 \u0054\u1ed5\u006e\u0067 "
    "\u0074\u0068\u0075 \u006e\u0068\u1ead\u0070 "
    "\u0028\u0054\u006f\u0074\u0061\u006c \u0052\u0065\u0074\u0075\u0072\u006e "
    "\u0049\u006e\u0064\u0065\u0078\u0029"
)


@dataclass(frozen=True)
class YieldSpec:
    tenor: str
    canonical_name: str
    candidate_paths: tuple[Path, ...]
    source_label: str


YIELD_SPECS = [
    YieldSpec(
        tenor="1Y",
        canonical_name="vn_gbond_1y_yield",
        candidate_paths=(
            BOND_TERM_RAW / "vn_gbond_1y_yield_raw.xlsx",
            BOND_TERM_RAW / "vn_gbond_1y_yield_raw.csv",
            BOND_RAW / "trai_phieu_chinh_phu_viet_nam_1n.xlsx",
            BOND_RAW / "trai_phieu_chinh_phu_viet_nam_1y.xlsx",
            BOND_RAW / "vn_gbond_1y_yield_raw.xlsx",
        ),
        source_label="pending_user_import",
    ),
    YieldSpec(
        tenor="5Y",
        canonical_name="vn_gbond_5y_yield",
        candidate_paths=(
            BOND_TERM_RAW / "vn_gbond_5y_yield_raw.xlsx",
            BOND_RAW / "trai_phieu_chinh_phu_viet_nam_5n.xlsx",
        ),
        source_label="TradingView_VN5Y_user_import",
    ),
    YieldSpec(
        tenor="10Y",
        canonical_name="vn_gbond_10y_yield",
        candidate_paths=(
            BOND_TERM_RAW / "vn_gbond_10y_yield_raw.xlsx",
            BOND_RAW / "trai_phieu_chinh_phu_viet_nam_10n.xlsx",
        ),
        source_label="TradingView_TVC_VN10Y_user_import",
    ),
]

TRI_PATH = BOND_RAW / "chi_so_bond_total_return_vn.xlsx"

SIMULATED_TRI_SPECS = {
    "1y": {
        "maturity_years": 1,
        "tri_col": "bond_tri_1y",
        "return_col": "r_bond_tri_1y",
    },
    "5y": {
        "maturity_years": 5,
        "tri_col": "bond_tri_5y",
        "return_col": "r_bond_tri_5y",
    },
}


def ensure_dirs() -> None:
    for path in [BOND_TERM_RAW, BOND_TERM_PROCESSED, BOND_TERM_INPUT, METADATA]:
        path.mkdir(parents=True, exist_ok=True)


def parse_number(value) -> float:
    if pd.isna(value):
        return np.nan
    if isinstance(value, (int, float, np.number)):
        return float(value)
    text = str(value).strip().replace(",", "").replace("%", "")
    if text in {"", "-", "nan", "None"}:
        return np.nan
    return float(text)


def read_any_table(path: Path) -> pd.DataFrame:
    if path.suffix.lower() in {".xlsx", ".xls"}:
        return pd.read_excel(path)
    return pd.read_csv(path)


def first_existing(paths: tuple[Path, ...]) -> Path | None:
    for path in paths:
        if path.exists():
            return path
    return None


def detect_date_col(df: pd.DataFrame) -> str:
    if DATE_COL in df.columns:
        return DATE_COL
    for col in df.columns:
        if str(col).strip().lower() in {"date", "ngay", "ng\u00e0y"}:
            return col
    return df.columns[0]


def detect_yield_col(df: pd.DataFrame) -> str:
    if YIELD_COL in df.columns:
        return YIELD_COL
    for col in df.columns:
        text = str(col).lower()
        if "yield" in text or "l\u1ee3i" in text or "su\u1ea5t" in text:
            return col
    if len(df.columns) < 2:
        raise ValueError("Cannot identify a yield column.")
    return df.columns[1]


def clean_yield_series(spec: YieldSpec) -> tuple[pd.DataFrame | None, dict]:
    path = first_existing(spec.candidate_paths)
    if path is None:
        return None, {
            "tenor": spec.tenor,
            "status": "missing",
            "source_file": "",
            "rows": 0,
            "start": "",
            "end": "",
            "notes": "No raw file found for this tenor.",
        }

    raw = read_any_table(path)
    date_col = detect_date_col(raw)
    yield_col = detect_yield_col(raw)
    out = pd.DataFrame(
        {
            "date": pd.to_datetime(raw[date_col], errors="coerce").dt.normalize(),
            "yield_pct": raw[yield_col].map(parse_number),
        }
    )
    out = out.dropna(subset=["date", "yield_pct"])
    out = out.drop_duplicates("date", keep="last").sort_values("date").reset_index(drop=True)
    out["tenor"] = spec.tenor
    out["source"] = spec.source_label
    out["source_file"] = str(path.relative_to(ROOT)).replace("\\", "/")

    output_path = BOND_TERM_PROCESSED / f"{spec.canonical_name}_clean.csv"
    out.to_csv(output_path, index=False, encoding="utf-8-sig")

    return out, {
        "tenor": spec.tenor,
        "status": "ok",
        "source_file": str(path.relative_to(ROOT)).replace("\\", "/"),
        "rows": int(len(out)),
        "start": out["date"].min().date().isoformat() if len(out) else "",
        "end": out["date"].max().date().isoformat() if len(out) else "",
        "notes": f"Cleaned file written to {output_path.relative_to(ROOT).as_posix()}",
    }


def read_bond_tri() -> tuple[pd.DataFrame, dict]:
    if not TRI_PATH.exists():
        raise FileNotFoundError(TRI_PATH)
    raw = pd.read_excel(TRI_PATH)
    if TRI_COL not in raw.columns:
        candidates = [c for c in raw.columns if "Return Index" in str(c)]
        if not candidates:
            raise ValueError("Cannot identify Total Return Index column.")
        tri_col = candidates[0]
    else:
        tri_col = TRI_COL

    out = pd.DataFrame(
        {
            "date": pd.to_datetime(raw[DATE_COL], errors="coerce").dt.normalize(),
            "bond_tri_10y": raw[tri_col].map(parse_number),
        }
    )
    out = out.dropna(subset=["date", "bond_tri_10y"])
    out = out.drop_duplicates("date", keep="last").sort_values("date").reset_index(drop=True)
    out["r_bond_tri_10y"] = 100 * np.log(out["bond_tri_10y"] / out["bond_tri_10y"].shift(1))
    return out, {
        "tenor": "10Y_TRI",
        "status": "ok",
        "source_file": str(TRI_PATH.relative_to(ROOT)).replace("\\", "/"),
        "rows": int(len(out)),
        "start": out["date"].min().date().isoformat(),
        "end": out["date"].max().date().isoformat(),
        "notes": "Simulated 10Y total return index retained as the main investable bond proxy.",
    }


def read_vnindex_calendar() -> pd.DataFrame:
    if CORE_PANEL.exists():
        panel = pd.read_csv(CORE_PANEL, parse_dates=["date"])
        return panel[["date"]].drop_duplicates().sort_values("date").reset_index(drop=True)
    raw = pd.read_excel(VNINDEX_RAW)
    date_col = detect_date_col(raw)
    return (
        pd.DataFrame({"date": pd.to_datetime(raw[date_col], errors="coerce").dt.normalize()})
        .dropna()
        .drop_duplicates()
        .sort_values("date")
        .reset_index(drop=True)
    )


def align_to_calendar(calendar: pd.DataFrame, series: pd.DataFrame, value_col: str, fill_limit: int = 5) -> tuple[pd.Series, dict]:
    merged = calendar.merge(series[["date", value_col]], on="date", how="left")
    before = int(merged[value_col].isna().sum())
    observed = int(merged[value_col].notna().sum())
    filled = merged[value_col].ffill(limit=fill_limit)
    report = {
        "variable": value_col,
        "raw_observed_on_vnindex_calendar": observed,
        "missing_before_fill": before,
        "filled_by_rule": int((merged[value_col].isna() & filled.notna()).sum()),
        "remaining_missing": int(filled.isna().sum()),
        "fill_limit_vnindex_trading_days": fill_limit,
    }
    return filled, report


def price_constant_maturity_par_bond(
    coupon_rate: float,
    yield_rate: float,
    maturity_years: int,
    face_value: float = 100.0,
    coupon_frequency: int = 2,
) -> float:
    """Price a fixed-coupon bond with semiannual coupons."""
    if not np.isfinite(coupon_rate) or not np.isfinite(yield_rate):
        return np.nan
    periods = int(round(maturity_years * coupon_frequency))
    if periods <= 0:
        return np.nan
    periodic_coupon = face_value * coupon_rate / coupon_frequency
    periodic_yield = yield_rate / coupon_frequency
    discount = 1.0 + periodic_yield
    if discount <= 0:
        return np.nan
    cash_flows = np.full(periods, periodic_coupon, dtype=float)
    cash_flows[-1] += face_value
    period_index = np.arange(1, periods + 1, dtype=float)
    return float(np.sum(cash_flows / np.power(discount, period_index)))


def construct_simulated_tri(
    panel: pd.DataFrame,
    yield_col: str,
    maturity_years: int,
    tri_col: str,
    return_col: str,
    base_value: float = 100.0,
) -> pd.DataFrame:
    """Construct a transparent simulated total return index from yield levels.

    The return combines a full cash-flow repricing term and daily carry. Yields
    are read as percentage points, while output returns are in percent.
    """
    out = panel.copy()
    yields_pct = pd.to_numeric(out[yield_col], errors="coerce")
    tri = pd.Series(np.nan, index=out.index, dtype=float)

    valid_positions = np.flatnonzero(yields_pct.notna().to_numpy())
    if len(valid_positions) == 0:
        out[tri_col] = tri
        out[return_col] = np.nan
        return out

    first_pos = int(valid_positions[0])
    tri.iloc[first_pos] = base_value
    last_tri = base_value
    last_yield = yields_pct.iloc[first_pos]

    for pos in range(first_pos + 1, len(out)):
        current_yield = yields_pct.iloc[pos]
        if not np.isfinite(current_yield):
            continue

        previous_row_yield = yields_pct.iloc[pos - 1]
        if not np.isfinite(previous_row_yield) or not np.isfinite(last_tri) or not np.isfinite(last_yield):
            tri.iloc[pos] = last_tri
            last_yield = current_yield
            continue

        current_yield = yields_pct.iloc[pos]
        previous_yield_rate = last_yield / 100.0
        current_yield_rate = current_yield / 100.0
        current_price = price_constant_maturity_par_bond(
            coupon_rate=previous_yield_rate,
            yield_rate=current_yield_rate,
            maturity_years=maturity_years,
        )
        if not np.isfinite(current_price):
            continue

        capital_return = current_price / base_value - 1.0
        interest_return = previous_yield_rate / 252.0
        total_return = capital_return + interest_return
        tri.iloc[pos] = last_tri * (1.0 + total_return)
        last_tri = tri.iloc[pos]
        last_yield = current_yield

    out[tri_col] = tri
    out[return_col] = 100.0 * np.log(out[tri_col] / out[tri_col].shift(1))
    return out


def build_term_structure() -> None:
    ensure_dirs()
    calendar = read_vnindex_calendar()
    daily = calendar.copy()

    availability_rows = []
    fill_rows = []
    cleaned_by_tenor: dict[str, pd.DataFrame] = {}

    for spec in YIELD_SPECS:
        cleaned, availability = clean_yield_series(spec)
        availability_rows.append(availability)
        col = f"y_{spec.tenor.lower()}"
        if cleaned is None:
            daily[col] = np.nan
            continue
        cleaned_by_tenor[spec.tenor] = cleaned
        temp = cleaned.rename(columns={"yield_pct": col})
        daily[col], fill_report = align_to_calendar(calendar, temp, col, fill_limit=5)
        fill_rows.append(fill_report)

    tri, tri_availability = read_bond_tri()
    availability_rows.append(tri_availability)
    daily["bond_tri_10y"], tri_fill = align_to_calendar(calendar, tri, "bond_tri_10y", fill_limit=5)
    fill_rows.append(tri_fill)

    for tenor in ["1y", "5y", "10y"]:
        daily[f"d_y_{tenor}_bps"] = daily[f"y_{tenor}"].diff() * 100

    daily["bond_level_1_5_10"] = daily[["y_1y", "y_5y", "y_10y"]].mean(axis=1, skipna=False)
    daily["bond_level_5_10"] = daily[["y_5y", "y_10y"]].mean(axis=1, skipna=False)
    daily["bond_slope_10y_1y"] = daily["y_10y"] - daily["y_1y"]
    daily["bond_slope_10y_5y"] = daily["y_10y"] - daily["y_5y"]
    daily["bond_curvature_1_5_10"] = daily["y_10y"] - 2 * daily["y_5y"] + daily["y_1y"]
    daily["r_bond_tri_10y"] = 100 * np.log(daily["bond_tri_10y"] / daily["bond_tri_10y"].shift(1))

    for tenor, spec in SIMULATED_TRI_SPECS.items():
        daily = construct_simulated_tri(
            daily,
            yield_col=f"y_{tenor}",
            maturity_years=spec["maturity_years"],
            tri_col=spec["tri_col"],
            return_col=spec["return_col"],
        )

    daily_output_cols = [
        "date",
        "y_1y",
        "y_5y",
        "y_10y",
        "d_y_1y_bps",
        "d_y_5y_bps",
        "d_y_10y_bps",
        "bond_level_1_5_10",
        "bond_level_5_10",
        "bond_slope_10y_1y",
        "bond_slope_10y_5y",
        "bond_curvature_1_5_10",
        "bond_tri_1y",
        "r_bond_tri_1y",
        "bond_tri_5y",
        "r_bond_tri_5y",
        "bond_tri_10y",
        "r_bond_tri_10y",
    ]
    daily = daily[daily_output_cols]

    weekly_level_cols = ["y_1y", "y_5y", "y_10y", "bond_tri_1y", "bond_tri_5y", "bond_tri_10y"]
    weekly_levels = daily.set_index("date")[weekly_level_cols].resample("W-FRI").last().dropna(how="all")
    weekly = weekly_levels.reset_index()
    for tenor in ["1y", "5y", "10y"]:
        weekly[f"d_y_{tenor}_bps"] = weekly[f"y_{tenor}"].diff() * 100
    weekly["bond_level_1_5_10"] = weekly[["y_1y", "y_5y", "y_10y"]].mean(axis=1, skipna=False)
    weekly["bond_level_5_10"] = weekly[["y_5y", "y_10y"]].mean(axis=1, skipna=False)
    weekly["bond_slope_10y_1y"] = weekly["y_10y"] - weekly["y_1y"]
    weekly["bond_slope_10y_5y"] = weekly["y_10y"] - weekly["y_5y"]
    weekly["bond_curvature_1_5_10"] = weekly["y_10y"] - 2 * weekly["y_5y"] + weekly["y_1y"]
    weekly["r_bond_tri_1y"] = 100 * np.log(weekly["bond_tri_1y"] / weekly["bond_tri_1y"].shift(1))
    weekly["r_bond_tri_5y"] = 100 * np.log(weekly["bond_tri_5y"] / weekly["bond_tri_5y"].shift(1))
    weekly["r_bond_tri_10y"] = 100 * np.log(weekly["bond_tri_10y"] / weekly["bond_tri_10y"].shift(1))
    weekly = weekly[daily_output_cols]

    daily_path = BOND_TERM_INPUT / "final_bond_term_structure_daily.csv"
    weekly_path = BOND_TERM_INPUT / "final_bond_term_structure_weekly.csv"
    availability_path = METADATA / "bond_term_structure_availability.csv"
    fill_path = METADATA / "bond_term_structure_fill_report.csv"
    report_path = METADATA / "bond_term_structure_report.md"

    daily.to_csv(daily_path, index=False, encoding="utf-8-sig")
    weekly.to_csv(weekly_path, index=False, encoding="utf-8-sig")
    pd.DataFrame(availability_rows).to_csv(availability_path, index=False, encoding="utf-8-sig")
    pd.DataFrame(fill_rows).to_csv(fill_path, index=False, encoding="utf-8-sig")

    complete_5_10_tri = daily.dropna(subset=["y_5y", "y_10y", "r_bond_tri_5y", "r_bond_tri_10y"])
    complete_1_5_10_tri = daily.dropna(subset=["y_1y", "y_5y", "y_10y", "r_bond_tri_1y", "r_bond_tri_5y", "r_bond_tri_10y"])
    missing_1y = "1Y" not in cleaned_by_tenor

    status = "PARTIAL - thiếu yield 1Y" if missing_1y else "READY - đủ 1Y/5Y/10Y"
    report_lines = [
        "# Bond Term-Structure Data Report",
        "",
        f"**Trạng thái:** {status}.",
        "",
        "## Mục tiêu",
        "",
        "Module này xây dựng kênh cấu trúc kỳ hạn trái phiếu Chính phủ Việt Nam để phục vụ phân tích safe-haven/hedging.",
        "Bond TRI 10Y vẫn là tài sản phòng hộ chính; yield 1Y/5Y/10Y được dùng để đo short-, medium- và long-end yield channel.",
        "",
        "## Biến được tạo",
        "",
        "- `d_y_1y_bps`, `d_y_5y_bps`, `d_y_10y_bps`: thay đổi yield theo basis points.",
        "- `bond_level_1_5_10`: trung bình yield 1Y, 5Y, 10Y; chỉ đầy đủ khi có 1Y.",
        "- `bond_level_5_10`: trung bình yield 5Y và 10Y; dùng tạm trong khi chưa có 1Y.",
        "- `bond_slope_10y_1y`: slope dài hạn/ngắn hạn; chỉ đầy đủ khi có 1Y.",
        "- `bond_slope_10y_5y`: slope dài hạn/trung hạn.",
        "- `bond_curvature_1_5_10`: curvature của đường cong lợi suất; chỉ đầy đủ khi có 1Y.",
        "- `bond_tri_1y`, `bond_tri_5y`: simulated constant-maturity TRI tự dựng từ yield 1Y/5Y.",
        "- `r_bond_tri_1y`, `r_bond_tri_5y`: log return của simulated 1Y/5Y Bond TRI.",
        "- `r_bond_tri_10y`: log return của simulated 10Y Bond TRI do người dùng cung cấp.",
        "",
        "## Phương pháp dựng simulated TRI 1Y/5Y",
        "",
        "Với mỗi kỳ hạn, module giả định nhà đầu tư nắm trái phiếu coupon bán niên có kỳ hạn không đổi.",
        "Coupon ngày t-1 được đặt bằng yield ngày t-1 nên giá tại t-1 bằng mệnh giá 100.",
        "Giá ngày t được chiết khấu bằng yield ngày t, sau đó cộng carry hằng ngày `yield_{t-1}/252`.",
        "Các chuỗi 1Y/5Y này là simulated TRI phục vụ robustness và so sánh duration, không phải chỉ số chính thức.",
        "",
        "## Sample coverage",
        "",
        f"- Daily rows: `{len(daily)}`.",
        f"- Daily range: `{daily['date'].min().date()}` to `{daily['date'].max().date()}`.",
        f"- Complete 5Y/10Y/TRI observations: `{len(complete_5_10_tri)}`.",
        f"- Complete 1Y/5Y/10Y/TRI observations: `{len(complete_1_5_10_tri)}`.",
        "",
        "## Output files",
        "",
        f"- `{daily_path.relative_to(ROOT).as_posix()}`",
        f"- `{weekly_path.relative_to(ROOT).as_posix()}`",
        f"- `{availability_path.relative_to(ROOT).as_posix()}`",
        f"- `{fill_path.relative_to(ROOT).as_posix()}`",
        "",
        "## Yêu cầu bổ sung",
        "",
    ]
    if missing_1y:
        report_lines.extend(
            [
                "Cần import thêm file yield trái phiếu Chính phủ Việt Nam kỳ hạn 1 năm.",
                "Tên file khuyến nghị: `data/00_raw/bonds/term_structure/vn_gbond_1y_yield_raw.xlsx`.",
                "Schema tối thiểu: cột ngày và cột lợi suất đóng cửa/yield theo phần trăm.",
            ]
        )
    else:
        report_lines.append("Đã có đủ 1Y, 5Y và 10Y để chạy term-structure channel.")

    report_path.write_text("\n".join(report_lines), encoding="utf-8")

    print(f"Built daily bond term-structure input: {daily_path} ({len(daily)} rows)")
    print(f"Built weekly bond term-structure input: {weekly_path} ({len(weekly)} rows)")
    print("Status: partial_missing_1y" if missing_1y else "Status: ready_1y_5y_10y")
    print(f"Report: {report_path}")


def main() -> None:
    build_term_structure()


if __name__ == "__main__":
    main()
