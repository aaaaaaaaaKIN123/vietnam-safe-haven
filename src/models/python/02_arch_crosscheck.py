"""Optional univariate GARCH cross-check with Python arch.

Use this only as a robustness/cross-check layer. It is not the main estimation
path for the paper because DCC/ADCC is handled in R through rmgarch.
"""

from __future__ import annotations

from pathlib import Path

import pandas as pd
import numpy as np


ROOT = Path(__file__).resolve().parents[3]
MODEL_INPUT = ROOT / "data" / "02_model_input" / "core" / "model_panel_daily.csv"
OUTPUT = ROOT / "results" / "tables" / "model_selection" / "python_arch_crosscheck.csv"


def main() -> None:
    try:
        from arch import arch_model
    except ImportError as exc:
        raise SystemExit("arch is required for this optional cross-check.") from exc

    if not MODEL_INPUT.exists():
        raise SystemExit(f"Model input not found: {MODEL_INPUT}")

    df = pd.read_csv(MODEL_INPUT, parse_dates=["date"]).sort_values("date")
    numeric_cols = [c for c in df.columns if c != "date" and pd.api.types.is_numeric_dtype(df[c])]
    prices = df[numeric_cols].astype(float).where(lambda x: x > 0)
    returns = 100 * np.log(prices / prices.shift(1))
    returns = returns.dropna(how="all")

    rows = []
    for col in returns.columns:
        series = returns[col].dropna()
        if len(series) < 100:
            continue
        fit = arch_model(series, mean="AR", lags=1, vol="GARCH", p=1, q=1, dist="t").fit(disp="off")
        rows.append({"asset": col, "model": "Python arch AR(1)-GARCH(1,1)-t", "aic": fit.aic, "bic": fit.bic})

    OUTPUT.parent.mkdir(parents=True, exist_ok=True)
    pd.DataFrame(rows).to_csv(OUTPUT, index=False, encoding="utf-8-sig")


if __name__ == "__main__":
    main()
