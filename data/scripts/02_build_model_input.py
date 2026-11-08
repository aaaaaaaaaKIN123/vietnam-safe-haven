"""Build model-ready daily panels from approved raw data.

This script implements Phase 1 of the research pipeline:

1. use the VNIndex trading calendar as the master calendar;
2. left-join all other assets and controls onto that calendar;
3. forward-fill only variables whose economic meaning supports it;
4. construct Vietnam-specific gold variables and asset returns;
5. write reproducible model inputs and metadata.

Raw files are read-only. All generated files are written under
`data/02_model_input/` and `data/metadata/`.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Callable

import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[2]
RAW = ROOT / "data" / "00_raw"
MODEL_INPUT = ROOT / "data" / "02_model_input"
METADATA = ROOT / "data" / "metadata"

CORE_DIR = MODEL_INPUT / "core"
CRYPTO_DIR = MODEL_INPUT / "crypto_extension"
ROBUSTNESS_DIR = MODEL_INPUT / "robustness"

TAEL_TROY_OUNCE_FACTOR = 1 / 0.829426


@dataclass(frozen=True)
class SeriesSpec:
    name: str
    path: Path
    reader: Callable[[Path], pd.DataFrame]
    date_col: str
    value_col: str
    output_col: str
    fill_limit: int | None
    economic_role: str
    transformation: str


def parse_number(value):
    if pd.isna(value):
        return np.nan
    if isinstance(value, (int, float, np.number)):
        return float(value)
    text = str(value).strip().replace(",", "").replace("%", "")
    if text in {"", "-", "nan", "None"}:
        return np.nan
    multipliers = {"K": 1_000, "M": 1_000_000, "B": 1_000_000_000}
    suffix = text[-1:].upper()
    if suffix in multipliers:
        return float(text[:-1]) * multipliers[suffix]
    return float(text)


def clean_date(series: pd.Series, dayfirst: bool = False) -> pd.Series:
    return pd.to_datetime(series, errors="coerce", dayfirst=dayfirst).dt.normalize()


def read_excel(path: Path) -> pd.DataFrame:
    return pd.read_excel(path)


def read_csv_standard(path: Path) -> pd.DataFrame:
    return pd.read_csv(path)


def read_investing_csv(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path)
    return df


def read_yield_volatility(path: Path) -> pd.DataFrame:
    lines = path.read_text(encoding="utf-8").splitlines()
    header_idx = next(i for i, line in enumerate(lines) if line.startswith("Economy,Date"))
    return pd.read_csv(path, skiprows=header_idx)


def keep_series(spec: SeriesSpec, dayfirst: bool = False) -> pd.DataFrame:
    df = spec.reader(spec.path)
    if spec.date_col not in df.columns or spec.value_col not in df.columns:
        raise ValueError(f"{spec.name}: required columns not found in {spec.path.name}")

    out = pd.DataFrame(
        {
            "date": clean_date(df[spec.date_col], dayfirst=dayfirst),
            spec.output_col: df[spec.value_col].map(parse_number),
        }
    )
    out = out.dropna(subset=["date"]).drop_duplicates("date", keep="last")
    out = out.sort_values("date").reset_index(drop=True)
    return out


def apply_calendar(calendar: pd.DataFrame, series: pd.DataFrame, value_col: str, fill_limit: int | None):
    merged = calendar.merge(series, on="date", how="left")
    raw_missing = int(merged[value_col].isna().sum())
    raw_observed_on_calendar = int(merged[value_col].notna().sum())

    if fill_limit == 0:
        filled = merged[value_col]
    elif fill_limit is None:
        filled = merged[value_col].ffill()
    else:
        filled = merged[value_col].ffill(limit=fill_limit)

    filled_count = int((merged[value_col].isna() & filled.notna()).sum())
    remaining_missing = int(filled.isna().sum())
    merged[value_col] = filled
    report = {
        "variable": value_col,
        "raw_observed_on_vnindex_calendar": raw_observed_on_calendar,
        "missing_before_fill": raw_missing,
        "filled_by_rule": filled_count,
        "remaining_missing": remaining_missing,
        "fill_limit_vnindex_trading_days": "unlimited" if fill_limit is None else fill_limit,
    }
    return merged[["date", value_col]], report


def log_return(series: pd.Series) -> pd.Series:
    clean = pd.to_numeric(series, errors="coerce")
    clean = clean.where(clean > 0)
    return 100 * np.log(clean / clean.shift(1))


def diff_level(series: pd.Series) -> pd.Series:
    return pd.to_numeric(series, errors="coerce").diff()


def build_panels() -> None:
    CORE_DIR.mkdir(parents=True, exist_ok=True)
    CRYPTO_DIR.mkdir(parents=True, exist_ok=True)
    ROBUSTNESS_DIR.mkdir(parents=True, exist_ok=True)
    METADATA.mkdir(parents=True, exist_ok=True)

    vnindex = keep_series(
        SeriesSpec(
            name="VNIndex",
            path=RAW / "equity" / "chi_so_vnindex_vietnam.xlsx",
            reader=read_excel,
            date_col="Ngày",
            value_col="Đóng cửa",
            output_col="vnindex",
            fill_limit=0,
            economic_role="Vietnamese equity market proxy and master calendar.",
            transformation="Use close level. No forward-fill.",
        )
    )
    calendar = vnindex[["date"]].copy()
    panel = vnindex.copy()

    specs = [
        SeriesSpec("SJC gold sell", RAW / "gold" / "gia_vang_sjc_lich_su.xlsx", read_excel, "Ngày", "Vàng miếng SJC - Bán ra (Triệu đồng/lượng)", "sjc_gold_million_vnd_per_tael", 3, "Domestic gold safe-haven candidate.", "Use SJC gold bar sell price in million VND per tael; forward-fill up to 3 VNIndex trading days."),
        SeriesSpec("XAU/USD", RAW / "gold" / "xau_usd_historical_data.csv", read_investing_csv, "Date", "Price", "xau_usd", 3, "Global spot gold benchmark.", "Use spot gold USD price; forward-fill up to 3 VNIndex trading days."),
        SeriesSpec("USD/VND", RAW / "macro" / "ty_gia_usd_vnd_vn.xlsx", read_excel, "Ngày", "Tỷ giá (VND)", "usd_vnd", 5, "FX conversion rate.", "Use USD/VND level; forward-fill up to 5 VNIndex trading days."),
        SeriesSpec("Bond TRI", RAW / "bonds" / "chi_so_bond_total_return_vn.xlsx", read_excel, "Ngày", "Chỉ số Tổng thu nhập (Total Return Index)", "bond_tri_10y", 5, "Government bond asset-return index.", "Use simulated total return index; forward-fill up to 5 VNIndex trading days."),
        SeriesSpec("VN10Y yield", RAW / "bonds" / "trai_phieu_chinh_phu_viet_nam_10n.xlsx", read_excel, "Ngày", "Lợi suất (%)", "vn10y_yield", 5, "Long-term bond yield channel.", "Use yield level in percent; forward-fill up to 5 VNIndex trading days."),
        SeriesSpec("VN5Y yield", RAW / "bonds" / "trai_phieu_chinh_phu_viet_nam_5n.xlsx", read_excel, "Ngày", "Lợi suất (%)", "vn5y_yield", 5, "Bond robustness yield channel.", "Use yield level in percent; forward-fill up to 5 VNIndex trading days."),
        SeriesSpec("VN10Y yield volatility", RAW / "bonds" / "yield_volatility_10yr_lcy_bonds.csv", read_yield_volatility, "Date", "Yield Volatility", "vn10y_yield_volatility", 5, "Bond-market risk indicator.", "Use ADB/AsianBondsOnline 21-trading-day yield-change volatility; forward-fill up to 5 VNIndex trading days."),
        SeriesSpec("Interbank rate", RAW / "macro" / "lai_suat_lien_ngan_hang_vn.xlsx", read_excel, "Ngày", "Lãi suất (%)", "interbank_rate", 5, "Market monetary-tightening proxy.", "Use VND interbank rate level in percent; forward-fill up to 5 VNIndex trading days."),
        SeriesSpec("Policy rate", RAW / "macro" / "lai_suat_dieu_hanh_vn.xlsx", read_excel, "Ngày", "Lãi suất điều hành (%)", "policy_rate", None, "SBV event-driven policy stance.", "Forward-fill until next policy decision because the rate remains valid between events."),
        SeriesSpec("Brent", RAW / "commodities" / "brent_futures_bz_daily.csv", read_csv_standard, "date", "close", "brent", 3, "Oil-price shock baseline proxy.", "Use close price; forward-fill up to 3 VNIndex trading days."),
        SeriesSpec("WTI", RAW / "commodities" / "wti_futures_cl_daily.csv", read_csv_standard, "date", "close", "wti", 3, "Oil-price robustness proxy.", "Use close price; forward-fill up to 3 VNIndex trading days."),
        SeriesSpec("VIX", RAW / "risk" / "vix_daily.csv", read_csv_standard, "date", "close", "vix", 3, "Global risk control.", "Use close level; forward-fill up to 3 VNIndex trading days."),
        SeriesSpec("BTC", RAW / "crypto" / "bitcoin_usd_daily.csv", read_csv_standard, "date", "close", "btc", 1, "Crypto extension.", "Use close price only on VNIndex trading dates; forward-fill at most 1 VNIndex trading day."),
        SeriesSpec("ETH", RAW / "crypto" / "ethereum_usd_daily.csv", read_csv_standard, "date", "close", "eth", 1, "Crypto extension.", "Use close price only on VNIndex trading dates; forward-fill at most 1 VNIndex trading day."),
    ]

    fill_reports = []
    dictionary_rows = [
        {
            "variable": "date",
            "source_file": "data/00_raw/equity/chi_so_vnindex_vietnam.xlsx",
            "source_column": "Ngày",
            "economic_role": "VNIndex trading calendar.",
            "transformation": "Calendar includes only days where VNIndex has an observed close.",
        },
        {
            "variable": "vnindex",
            "source_file": "data/00_raw/equity/chi_so_vnindex_vietnam.xlsx",
            "source_column": "Đóng cửa",
            "economic_role": "Vietnamese equity market proxy.",
            "transformation": "Close level; no forward-fill.",
        },
    ]

    for spec in specs:
        raw_series = keep_series(spec, dayfirst=False)
        aligned, report = apply_calendar(calendar, raw_series, spec.output_col, spec.fill_limit)
        panel = panel.merge(aligned, on="date", how="left")
        fill_reports.append(report)
        dictionary_rows.append(
            {
                "variable": spec.output_col,
                "source_file": str(spec.path.relative_to(ROOT)).replace("\\", "/"),
                "source_column": spec.value_col,
                "economic_role": spec.economic_role,
                "transformation": spec.transformation,
            }
        )

    panel["sjc_gold"] = panel["sjc_gold_million_vnd_per_tael"] * 1_000_000
    panel["xau_vnd"] = panel["xau_usd"] * panel["usd_vnd"] * TAEL_TROY_OUNCE_FACTOR
    panel["gold_premium_log"] = np.log(panel["sjc_gold"] / panel["xau_vnd"])
    panel["gold_premium_pct"] = (panel["sjc_gold"] / panel["xau_vnd"] - 1) * 100

    derived_dictionary = [
        ("sjc_gold", "Derived", "sjc_gold_million_vnd_per_tael", "Domestic SJC gold in VND per tael.", "sjc_gold = SJC sell price in million VND/tael × 1,000,000."),
        ("xau_vnd", "Derived", "xau_usd, usd_vnd", "Global gold converted to VND per tael.", "xau_vnd = xau_usd × usd_vnd × (1 / 0.829426)."),
        ("gold_premium_log", "Derived", "sjc_gold, xau_vnd", "Vietnam domestic gold segmentation premium.", "gold_premium_log = ln(sjc_gold / xau_vnd)."),
        ("gold_premium_pct", "Derived", "sjc_gold, xau_vnd", "Vietnam domestic gold premium in percent.", "gold_premium_pct = (sjc_gold / xau_vnd - 1) × 100."),
    ]
    for variable, source_file, source_col, role, transformation in derived_dictionary:
        dictionary_rows.append(
            {
                "variable": variable,
                "source_file": source_file,
                "source_column": source_col,
                "economic_role": role,
                "transformation": transformation,
            }
        )

    panel = panel.sort_values("date").reset_index(drop=True)

    returns = pd.DataFrame({"date": panel["date"]})
    price_return_cols = {
        "r_vnindex": "vnindex",
        "r_sjc_gold": "sjc_gold",
        "r_xau_vnd": "xau_vnd",
        "r_gold_premium": "gold_premium_log",
        "r_bond_tri": "bond_tri_10y",
        "r_brent": "brent",
        "r_wti": "wti",
        "r_btc": "btc",
        "r_eth": "eth",
    }
    for out_col, source_col in price_return_cols.items():
        if source_col == "gold_premium_log":
            returns[out_col] = 100 * panel[source_col].diff()
        else:
            returns[out_col] = log_return(panel[source_col])

    for out_col, source_col in {
        "d_vn10y_yield": "vn10y_yield",
        "d_vn5y_yield": "vn5y_yield",
        "d_interbank_rate": "interbank_rate",
        "d_policy_rate": "policy_rate",
        "d_vix": "vix",
    }.items():
        returns[out_col] = diff_level(panel[source_col])

    returns["policy_rate"] = panel["policy_rate"]
    returns["interbank_rate"] = panel["interbank_rate"]
    returns["vn10y_yield"] = panel["vn10y_yield"]
    returns["vn5y_yield"] = panel["vn5y_yield"]
    returns["vn10y_yield_volatility"] = panel["vn10y_yield_volatility"]
    returns["vix"] = panel["vix"]

    for q in [0.10, 0.05, 0.01]:
        threshold = returns["r_vnindex"].quantile(q)
        label = str(int(q * 100)).zfill(2)
        returns[f"stress_vnindex_q{label}"] = (returns["r_vnindex"] <= threshold).astype("Int64")

    core_panel = panel[
        [
            "date",
            "vnindex",
            "sjc_gold",
            "xau_usd",
            "usd_vnd",
            "xau_vnd",
            "gold_premium_log",
            "gold_premium_pct",
            "bond_tri_10y",
            "vn10y_yield",
            "vn5y_yield",
            "vn10y_yield_volatility",
            "interbank_rate",
            "policy_rate",
            "brent",
            "wti",
            "vix",
            "btc",
            "eth",
        ]
    ].copy()

    core_returns = returns.dropna(subset=["r_vnindex"]).reset_index(drop=True)
    crypto_returns = returns[
        [
            "date",
            "r_vnindex",
            "r_btc",
            "r_eth",
            "r_sjc_gold",
            "r_xau_vnd",
            "r_bond_tri",
            "d_vix",
            "stress_vnindex_q10",
            "stress_vnindex_q05",
            "stress_vnindex_q01",
        ]
    ].dropna(subset=["r_vnindex", "r_btc", "r_eth"], how="any")

    weekly_panel = core_panel.set_index("date").resample("W-FRI").last().dropna(how="all").reset_index()
    weekly_returns = pd.DataFrame({"date": weekly_panel["date"]})
    for out_col, source_col in {
        "r_vnindex": "vnindex",
        "r_sjc_gold": "sjc_gold",
        "r_xau_vnd": "xau_vnd",
        "r_bond_tri": "bond_tri_10y",
        "r_brent": "brent",
        "r_wti": "wti",
    }.items():
        weekly_returns[out_col] = log_return(weekly_panel[source_col])
    weekly_returns["d_vn10y_yield"] = diff_level(weekly_panel["vn10y_yield"])
    weekly_returns["d_vn5y_yield"] = diff_level(weekly_panel["vn5y_yield"])
    weekly_returns["d_interbank_rate"] = diff_level(weekly_panel["interbank_rate"])
    weekly_returns["d_policy_rate"] = diff_level(weekly_panel["policy_rate"])
    weekly_returns["vix"] = weekly_panel["vix"]

    outputs = {
        CORE_DIR / "model_panel_daily.csv": core_panel,
        CORE_DIR / "final_daily_panel.csv": core_panel,
        CORE_DIR / "final_daily_returns.csv": core_returns,
        CORE_DIR / "final_weekly_panel.csv": weekly_panel,
        CORE_DIR / "final_weekly_returns.csv": weekly_returns,
        CRYPTO_DIR / "final_crypto_daily_returns.csv": crypto_returns,
        ROBUSTNESS_DIR / "final_robustness_weekly_returns.csv": weekly_returns,
    }
    for path, df in outputs.items():
        df.to_csv(path, index=False, encoding="utf-8-sig")

    fill_report_df = pd.DataFrame(fill_reports)
    fill_report_df.to_csv(METADATA / "model_input_calendar_fill_report.csv", index=False, encoding="utf-8-sig")

    dictionary = pd.DataFrame(dictionary_rows)
    dictionary.to_csv(METADATA / "model_input_data_dictionary.csv", index=False, encoding="utf-8-sig")

    missing_summary = []
    for col in core_panel.columns:
        missing_summary.append(
            {
                "variable": col,
                "observations": int(core_panel[col].notna().sum()),
                "missing": int(core_panel[col].isna().sum()),
                "missing_pct": float(core_panel[col].isna().mean() * 100),
                "first_valid_date": core_panel.loc[core_panel[col].notna(), "date"].min(),
                "last_valid_date": core_panel.loc[core_panel[col].notna(), "date"].max(),
            }
        )
    missing_df = pd.DataFrame(missing_summary)
    missing_df.to_csv(METADATA / "model_input_missing_summary.csv", index=False, encoding="utf-8-sig")

    sample_groups = {
        "core_assets_vnindex_sjc_xau_bond": ["vnindex", "sjc_gold", "xau_vnd", "bond_tri_10y"],
        "core_with_oil_fx_rates_vix": [
            "vnindex",
            "sjc_gold",
            "xau_vnd",
            "bond_tri_10y",
            "brent",
            "wti",
            "usd_vnd",
            "policy_rate",
            "interbank_rate",
            "vix",
        ],
        "bond_yield_robustness": ["vnindex", "bond_tri_10y", "vn10y_yield", "vn5y_yield", "vn10y_yield_volatility"],
        "crypto_extension_btc_eth": ["vnindex", "btc", "eth"],
        "full_all_variables": [c for c in core_panel.columns if c != "date"],
    }
    overlap_rows = []
    for group, cols in sample_groups.items():
        valid = core_panel[cols].notna().all(axis=1)
        valid_dates = core_panel.loc[valid, "date"]
        overlap_rows.append(
            {
                "sample_group": group,
                "required_variables": ", ".join(cols),
                "observations": int(valid.sum()),
                "first_complete_date": valid_dates.min() if len(valid_dates) else pd.NaT,
                "last_complete_date": valid_dates.max() if len(valid_dates) else pd.NaT,
            }
        )
    overlap_df = pd.DataFrame(overlap_rows)
    overlap_df.to_csv(METADATA / "model_input_overlap_summary.csv", index=False, encoding="utf-8-sig")

    report_path = METADATA / "model_input_phase1_build_report.md"
    report_path.write_text(
        "\n".join(
            [
                "# Phase 1 Model Input Build Report",
                "",
                "## Calendar Rule",
                "",
                "The master calendar is the VNIndex trading calendar. VNIndex is never forward-filled.",
                "All other variables are left-joined to this calendar and forward-filled only according to their economic role.",
                "",
                "## Main Calculations",
                "",
                f"- `sjc_gold = sjc_gold_million_vnd_per_tael * 1,000,000`.",
                f"- `xau_vnd = xau_usd * usd_vnd * (1 / 0.829426)`.",
                "- `gold_premium_log = ln(sjc_gold / xau_vnd)`.",
                "- `gold_premium_pct = (sjc_gold / xau_vnd - 1) * 100`.",
                "- Asset returns: `r_x = 100 * ln(price_t / price_{t-1})`.",
                "- Gold premium return: `r_gold_premium = 100 * diff(gold_premium_log)`.",
                "- Yield/rate changes: `d_x = x_t - x_{t-1}`.",
                "- VNIndex stress dummies: lower-tail thresholds at 10%, 5%, and 1% of `r_vnindex`.",
                "",
                "## Output Files",
                "",
                "- `data/02_model_input/core/model_panel_daily.csv`",
                "- `data/02_model_input/core/final_daily_panel.csv`",
                "- `data/02_model_input/core/final_daily_returns.csv`",
                "- `data/02_model_input/core/final_weekly_panel.csv`",
                "- `data/02_model_input/core/final_weekly_returns.csv`",
                "- `data/02_model_input/crypto_extension/final_crypto_daily_returns.csv`",
                "- `data/02_model_input/robustness/final_robustness_weekly_returns.csv`",
                "- `data/metadata/model_input_calendar_fill_report.csv`",
                "- `data/metadata/model_input_data_dictionary.csv`",
                "- `data/metadata/model_input_missing_summary.csv`",
                "- `data/metadata/model_input_overlap_summary.csv`",
                "",
                f"Daily panel rows: {len(core_panel)}",
                f"Daily returns rows: {len(core_returns)}",
                f"Weekly returns rows: {len(weekly_returns)}",
                "",
                "## Complete-Sample Warning",
                "",
                "The SJC raw file contains date rows through 2026-05-26, but SJC gold bar buy/sell values are missing after the last valid SJC observation.",
                "Therefore, any core model requiring `sjc_gold` and `gold_premium` must use the complete-overlap sample reported in `model_input_overlap_summary.csv` unless new SJC data are added.",
            ]
        ),
        encoding="utf-8",
    )

    print(f"Built daily panel: {CORE_DIR / 'model_panel_daily.csv'} ({len(core_panel)} rows)")
    print(f"Built daily returns: {CORE_DIR / 'final_daily_returns.csv'} ({len(core_returns)} rows)")
    print(f"Built metadata report: {report_path}")


def main() -> None:
    build_panels()


if __name__ == "__main__":
    main()
