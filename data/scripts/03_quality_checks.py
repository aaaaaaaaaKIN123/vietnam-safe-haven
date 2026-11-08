"""Run Phase 2 quality checks and preliminary diagnostics.

Inputs:
    data/02_model_input/core/model_panel_daily.csv
    data/02_model_input/core/final_daily_returns.csv
    data/02_model_input/crypto_extension/final_crypto_daily_returns.csv
    data/metadata/model_input_overlap_summary.csv

Outputs:
    data/metadata/model_input_quality_report.md
    results/tables/data_quality/*.csv
    results/diagnostics/descriptive_statistics/*.csv
    results/diagnostics/normality/*.csv
    results/diagnostics/unit_root/*.csv
    results/diagnostics/arch_effect/*.csv

This Phase 3.0 version uses scipy/statsmodels for formal diagnostic p-values.
Raw data and model-input CSV files are read-only.
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd
from scipy import stats as scipy_stats
from statsmodels.stats.diagnostic import het_arch
from statsmodels.tsa.stattools import adfuller


ROOT = Path(__file__).resolve().parents[2]
DATA = ROOT / "data"
MODEL_INPUT = DATA / "02_model_input"
METADATA = DATA / "metadata"
RESULTS = ROOT / "results"

CORE_PANEL = MODEL_INPUT / "core" / "model_panel_daily.csv"
CORE_RETURNS = MODEL_INPUT / "core" / "final_daily_returns.csv"
CRYPTO_RETURNS = MODEL_INPUT / "crypto_extension" / "final_crypto_daily_returns.csv"
OVERLAP_SUMMARY = METADATA / "model_input_overlap_summary.csv"

DATA_QUALITY = RESULTS / "tables" / "data_quality"
DESC_DIR = RESULTS / "diagnostics" / "descriptive_statistics"
NORMALITY_DIR = RESULTS / "diagnostics" / "normality"
UNIT_ROOT_DIR = RESULTS / "diagnostics" / "unit_root"
ARCH_DIR = RESULTS / "diagnostics" / "arch_effect"


CORE_RETURN_COLUMNS = [
    "r_vnindex",
    "r_sjc_gold",
    "r_xau_vnd",
    "r_gold_premium",
    "r_bond_tri",
    "r_brent",
    "r_wti",
]

CONTROL_COLUMNS = [
    "d_vn10y_yield",
    "d_vn5y_yield",
    "d_interbank_rate",
    "d_policy_rate",
    "d_vix",
]

CRYPTO_COLUMNS = [
    "r_btc",
    "r_eth",
]


def ensure_dirs() -> None:
    for path in [DATA_QUALITY, DESC_DIR, NORMALITY_DIR, UNIT_ROOT_DIR, ARCH_DIR]:
        path.mkdir(parents=True, exist_ok=True)


def read_csv_dates(path: Path) -> pd.DataFrame:
    if not path.exists():
        raise FileNotFoundError(path)
    return pd.read_csv(path, parse_dates=["date"])


def merge_crypto_returns(returns: pd.DataFrame, crypto_returns: pd.DataFrame) -> pd.DataFrame:
    missing_crypto_cols = [c for c in CRYPTO_COLUMNS if c not in returns.columns and c in crypto_returns.columns]
    if not missing_crypto_cols:
        return returns.copy()
    return returns.merge(crypto_returns[["date", *missing_crypto_cols]], on="date", how="left")


def descriptive_stats(df: pd.DataFrame, columns: list[str]) -> pd.DataFrame:
    rows = []
    for col in columns:
        if col not in df.columns:
            continue
        s = pd.to_numeric(df[col], errors="coerce").dropna()
        if s.empty:
            continue
        rows.append(
            {
                "variable": col,
                "observations": int(s.count()),
                "mean": float(s.mean()),
                "std": float(s.std(ddof=1)),
                "min": float(s.min()),
                "p01": float(s.quantile(0.01)),
                "p05": float(s.quantile(0.05)),
                "median": float(s.median()),
                "p95": float(s.quantile(0.95)),
                "p99": float(s.quantile(0.99)),
                "max": float(s.max()),
                "skewness": float(scipy_stats.skew(s, bias=False)),
                "kurtosis": float(scipy_stats.kurtosis(s, fisher=False, bias=False)),
            }
        )
    return pd.DataFrame(rows)


def jarque_bera_tests(df: pd.DataFrame, columns: list[str]) -> pd.DataFrame:
    rows = []
    for col in columns:
        if col not in df.columns:
            continue
        s = pd.to_numeric(df[col], errors="coerce").dropna()
        if len(s) < 8:
            continue
        jb = scipy_stats.jarque_bera(s)
        p_value = float(jb.pvalue)
        rows.append(
            {
                "variable": col,
                "observations": int(len(s)),
                "skewness": float(scipy_stats.skew(s, bias=False)),
                "kurtosis": float(scipy_stats.kurtosis(s, fisher=False, bias=False)),
                "jarque_bera": float(jb.statistic),
                "p_value": p_value,
                "interpretation": "reject_normality_at_5pct" if p_value < 0.05 else "do_not_reject_at_5pct",
            }
        )
    return pd.DataFrame(rows)


def adf_test(series: pd.Series, lags: int = 1) -> dict[str, float | int | str]:
    s = pd.to_numeric(series, errors="coerce").dropna()
    if len(s) <= lags + 10 or s.nunique() <= 1:
        return {
            "observations": int(len(s)),
            "lags": lags,
            "adf_t_stat": np.nan,
            "p_value_mackinnon": np.nan,
            "critical_1pct": np.nan,
            "critical_5pct": np.nan,
            "critical_10pct": np.nan,
            "reject_5pct": "insufficient_data",
        }
    try:
        stat, p_value, used_lag, nobs, critical_values, *_ = adfuller(
            s.to_numpy(dtype=float),
            maxlag=lags,
            regression="c",
            autolag=None,
        )
    except Exception as exc:
        return {
            "observations": int(len(s)),
            "lags": lags,
            "adf_t_stat": np.nan,
            "p_value_mackinnon": np.nan,
            "critical_1pct": np.nan,
            "critical_5pct": np.nan,
            "critical_10pct": np.nan,
            "reject_5pct": f"test_failed: {exc}",
        }
    return {
        "observations": int(nobs),
        "lags": int(used_lag),
        "adf_t_stat": float(stat),
        "p_value_mackinnon": float(p_value),
        "critical_1pct": float(critical_values["1%"]),
        "critical_5pct": float(critical_values["5%"]),
        "critical_10pct": float(critical_values["10%"]),
        "reject_5pct": "yes" if p_value < 0.05 else "no",
    }


def arch_lm_test(series: pd.Series, lags: int = 12) -> dict[str, float | int | str]:
    s = pd.to_numeric(series, errors="coerce").dropna()
    if len(s) <= lags + 10 or s.nunique() <= 1:
        return {
            "observations": int(len(s)),
            "lags": lags,
            "lm_stat": np.nan,
            "p_value_chi2": np.nan,
            "f_stat": np.nan,
            "f_p_value": np.nan,
            "interpretation": "insufficient_data",
        }
    residual = s.to_numpy(dtype=float) - float(s.mean())
    lm_stat, p_value, f_stat, f_p_value = het_arch(residual, nlags=lags, ddof=0)
    return {
        "observations": int(len(residual) - lags),
        "lags": int(lags),
        "lm_stat": float(lm_stat),
        "p_value_chi2": float(p_value),
        "f_stat": float(f_stat),
        "f_p_value": float(f_p_value),
        "interpretation": "arch_effect_at_5pct" if p_value < 0.05 else "no_arch_effect_at_5pct",
    }


def sample_coverage(panel: pd.DataFrame, returns_full: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for name, df in [("daily_panel", panel), ("daily_returns", returns_full)]:
        for col in df.columns:
            if col == "date":
                continue
            valid = df[col].notna()
            valid_dates = df.loc[valid, "date"]
            rows.append(
                {
                    "dataset": name,
                    "variable": col,
                    "observations": int(valid.sum()),
                    "missing": int((~valid).sum()),
                    "missing_pct": float((~valid).mean() * 100),
                    "first_valid_date": valid_dates.min().date() if len(valid_dates) else "",
                    "last_valid_date": valid_dates.max().date() if len(valid_dates) else "",
                }
            )
    return pd.DataFrame(rows)


def extreme_return_table(returns: pd.DataFrame, columns: list[str], top_n: int = 10) -> pd.DataFrame:
    rows = []
    for col in columns:
        if col not in returns.columns:
            continue
        sub = returns[["date", col]].dropna().copy()
        if sub.empty:
            continue
        sub["absolute_value"] = sub[col].abs()
        sub = sub.sort_values("absolute_value", ascending=False).head(top_n)
        for _, row in sub.iterrows():
            rows.append(
                {
                    "variable": col,
                    "date": row["date"].date(),
                    "value": float(row[col]),
                    "absolute_value": float(row["absolute_value"]),
                }
            )
    return pd.DataFrame(rows)


def dataframe_to_markdown(df: pd.DataFrame) -> str:
    if df.empty:
        return "No rows."
    cols = list(df.columns)
    lines = [
        "| " + " | ".join(cols) + " |",
        "| " + " | ".join(["---"] * len(cols)) + " |",
    ]
    for _, row in df.iterrows():
        lines.append("| " + " | ".join(str(row[col]) for col in cols) + " |")
    return "\n".join(lines)


def write_report(
    panel: pd.DataFrame,
    returns_full: pd.DataFrame,
    analysis_returns: pd.DataFrame,
    crypto_returns: pd.DataFrame,
    descriptive: pd.DataFrame,
    jb: pd.DataFrame,
    adf: pd.DataFrame,
    arch: pd.DataFrame,
    overlap: pd.DataFrame,
) -> None:
    core_complete = analysis_returns[["r_vnindex", "r_sjc_gold", "r_xau_vnd", "r_bond_tri"]].dropna()
    core_controls = analysis_returns[
        [
            "r_vnindex",
            "r_sjc_gold",
            "r_xau_vnd",
            "r_bond_tri",
            "r_brent",
            "r_wti",
            "d_interbank_rate",
            "d_policy_rate",
            "d_vix",
        ]
    ].dropna()

    report = [
        "# Model Input Quality Report",
        "",
        "## Scope",
        "",
        "This report documents Phase 2 data quality checks and preliminary diagnostics for the model-ready inputs.",
        "The master calendar remains the VNIndex trading calendar. Raw data and model-input CSV files were not modified.",
        "",
        "## Phase 3.0 Diagnostic Upgrade",
        "",
        "- Jarque-Bera normality tests are computed with `scipy.stats.jarque_bera`.",
        "- ADF stationarity tests are computed with `statsmodels.tsa.stattools.adfuller` using a constant and one lag.",
        "- ARCH-LM tests are computed with `statsmodels.stats.diagnostic.het_arch` using 12 lags.",
        "",
        "## Main Sample Recommendation",
        "",
        f"- Daily panel range: `{panel['date'].min().date()}` to `{panel['date'].max().date()}`.",
        f"- Daily returns range: `{returns_full['date'].min().date()}` to `{returns_full['date'].max().date()}`.",
        f"- Diagnostics main-sample range: `{analysis_returns['date'].min().date()}` to `{analysis_returns['date'].max().date()}`.",
        f"- Core return complete observations (`r_vnindex`, `r_sjc_gold`, `r_xau_vnd`, `r_bond_tri`): `{len(core_complete)}`.",
        f"- Core + controls complete observations: `{len(core_controls)}`.",
        f"- Crypto extension return observations with BTC and ETH: `{len(crypto_returns.dropna(subset=['r_btc', 'r_eth']))}`.",
        "",
        "## Diagnostics Summary",
        "",
        f"- Descriptive statistics generated for `{len(descriptive)}` variables.",
        f"- Jarque-Bera normality tests generated for `{len(jb)}` variables.",
        f"- ADF stationarity tests generated for `{len(adf)}` variables.",
        f"- ARCH-LM diagnostics generated for `{len(arch)}` variables.",
        "",
        "## Scientific Interpretation",
        "",
        "- Jarque-Bera rejection supports using Student-t or skewed Student-t errors in GARCH-family models.",
        "- ADF tests on returns should generally reject unit roots; price/level variables are not the preferred input for GARCH returns.",
        "- ARCH-LM evidence supports using ARCH/GARCH-family volatility models for variables with significant conditional heteroskedasticity.",
        "- `d_policy_rate` is an event-driven policy variable; if ARCH-LM is not significant, use it as a control/event variable rather than as a volatility process.",
        "- Correlation matrices are descriptive only and should not be used as final safe-haven evidence.",
        "",
        "## Key Output Files",
        "",
        "- `results/tables/data_quality/sample_coverage_table.csv`",
        "- `results/diagnostics/descriptive_statistics/daily_return_descriptive_stats.csv`",
        "- `results/diagnostics/normality/jarque_bera_tests.csv`",
        "- `results/diagnostics/unit_root/adf_return_tests.csv`",
        "- `results/diagnostics/arch_effect/arch_lm_tests.csv`",
        "- `results/diagnostics/descriptive_statistics/core_return_correlation_matrix_pearson.csv`",
        "- `results/diagnostics/descriptive_statistics/core_return_correlation_matrix_spearman.csv`",
        "- `results/diagnostics/descriptive_statistics/extreme_daily_returns_top10.csv`",
        "",
        "## Complete Overlap Groups",
        "",
        dataframe_to_markdown(overlap) if not overlap.empty else "No overlap summary available.",
        "",
    ]
    (METADATA / "model_input_quality_report.md").write_text("\n".join(report), encoding="utf-8")


def main() -> None:
    ensure_dirs()

    panel = read_csv_dates(CORE_PANEL)
    returns = read_csv_dates(CORE_RETURNS)
    crypto_returns = read_csv_dates(CRYPTO_RETURNS)
    returns_full = merge_crypto_returns(returns, crypto_returns)
    overlap = pd.read_csv(OVERLAP_SUMMARY) if OVERLAP_SUMMARY.exists() else pd.DataFrame()

    if not overlap.empty and "core_assets_vnindex_sjc_xau_bond" in set(overlap["sample_group"]):
        core_row = overlap.loc[overlap["sample_group"] == "core_assets_vnindex_sjc_xau_bond"].iloc[0]
        sample_start = pd.Timestamp(core_row["first_complete_date"])
        sample_end = pd.Timestamp(core_row["last_complete_date"])
    else:
        sample_start = pd.Timestamp("2010-01-04")
        sample_end = returns_full["date"].max()
    analysis_returns = returns_full[(returns_full["date"] > sample_start) & (returns_full["date"] <= sample_end)].copy()

    all_diag_cols = [c for c in CORE_RETURN_COLUMNS + CONTROL_COLUMNS + CRYPTO_COLUMNS if c in analysis_returns.columns]
    core_corr_cols = [c for c in CORE_RETURN_COLUMNS + CONTROL_COLUMNS if c in analysis_returns.columns]

    coverage = sample_coverage(panel, returns_full)
    coverage.to_csv(DATA_QUALITY / "sample_coverage_table.csv", index=False, encoding="utf-8-sig")
    if not overlap.empty:
        overlap.to_csv(DATA_QUALITY / "complete_overlap_groups.csv", index=False, encoding="utf-8-sig")

    descriptive = descriptive_stats(analysis_returns, all_diag_cols)
    descriptive.to_csv(DESC_DIR / "daily_return_descriptive_stats.csv", index=False, encoding="utf-8-sig")

    jb = jarque_bera_tests(analysis_returns, all_diag_cols)
    jb.to_csv(NORMALITY_DIR / "jarque_bera_tests.csv", index=False, encoding="utf-8-sig")

    adf = pd.DataFrame([{"variable": col, **adf_test(analysis_returns[col], lags=1)} for col in all_diag_cols])
    adf = adf[
        [
            "variable",
            "observations",
            "lags",
            "adf_t_stat",
            "p_value_mackinnon",
            "critical_1pct",
            "critical_5pct",
            "critical_10pct",
            "reject_5pct",
        ]
    ]
    adf.to_csv(UNIT_ROOT_DIR / "adf_return_tests.csv", index=False, encoding="utf-8-sig")

    arch = pd.DataFrame([{"variable": col, **arch_lm_test(analysis_returns[col], lags=12)} for col in all_diag_cols])
    arch = arch[
        [
            "variable",
            "observations",
            "lags",
            "lm_stat",
            "p_value_chi2",
            "f_stat",
            "f_p_value",
            "interpretation",
        ]
    ]
    arch.to_csv(ARCH_DIR / "arch_lm_tests.csv", index=False, encoding="utf-8-sig")

    pearson = analysis_returns[core_corr_cols].corr(method="pearson")
    spearman = analysis_returns[core_corr_cols].corr(method="spearman")
    pearson.to_csv(DESC_DIR / "core_return_correlation_matrix_pearson.csv", encoding="utf-8-sig")
    spearman.to_csv(DESC_DIR / "core_return_correlation_matrix_spearman.csv", encoding="utf-8-sig")

    extreme = extreme_return_table(analysis_returns, all_diag_cols)
    extreme.to_csv(DESC_DIR / "extreme_daily_returns_top10.csv", index=False, encoding="utf-8-sig")

    write_report(panel, returns_full, analysis_returns, crypto_returns, descriptive, jb, adf, arch, overlap)

    print("Phase 2 quality checks completed with scipy/statsmodels diagnostics.")
    print(f"Quality report: {METADATA / 'model_input_quality_report.md'}")
    print(f"Descriptive stats variables: {len(descriptive)}")
    print(f"ADF tests: {len(adf)}")
    print(f"ARCH-LM tests: {len(arch)}")


if __name__ == "__main__":
    main()
