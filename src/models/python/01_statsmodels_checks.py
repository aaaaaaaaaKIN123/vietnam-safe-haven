"""Optional Python checks for the research model pipeline.

This script is intentionally secondary. The main GARCH/DCC/ADCC estimation
belongs to the R pipeline because rugarch/rmgarch match the imported
methodology sources more directly.
"""

from __future__ import annotations

from pathlib import Path

import pandas as pd


ROOT = Path(__file__).resolve().parents[3]
MODEL_INPUT = ROOT / "data" / "02_model_input" / "core" / "model_panel_daily.csv"
OUTPUT = ROOT / "results" / "diagnostics" / "python_statsmodels_checks.csv"


def main() -> None:
    try:
        import statsmodels.tsa.stattools as ts_tools
    except ImportError as exc:
        raise SystemExit("statsmodels is required for this optional check.") from exc

    if not MODEL_INPUT.exists():
        raise SystemExit(f"Model input not found: {MODEL_INPUT}")

    df = pd.read_csv(MODEL_INPUT, parse_dates=["date"])
    numeric_cols = [c for c in df.columns if c != "date" and pd.api.types.is_numeric_dtype(df[c])]
    rows = []
    for col in numeric_cols:
        series = df[col].dropna()
        if len(series) < 30:
            continue
        adf = ts_tools.adfuller(series)
        rows.append({"variable": col, "test": "ADF", "statistic": adf[0], "p_value": adf[1], "observations": len(series)})

    OUTPUT.parent.mkdir(parents=True, exist_ok=True)
    pd.DataFrame(rows).to_csv(OUTPUT, index=False, encoding="utf-8-sig")


if __name__ == "__main__":
    main()
