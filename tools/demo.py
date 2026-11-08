"""Exercise the unchanged ML protocol using synthetic, non-market inputs."""
from __future__ import annotations

import argparse
import json
import shutil
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]


def synthetic_inputs(root: Path, rows: int = 3100) -> None:
    rng = np.random.default_rng(20261108)
    dates = pd.bdate_range("2010-01-04", periods=rows)
    market = rng.normal(0.02, 1.2, rows)
    # Rotate correlations so the validation set can observe each hedge label.
    regime = (np.arange(rows) // 168) % 3
    returns = pd.DataFrame({"date": dates, "r_vnindex": market})
    for i, name in enumerate(["r_sjc_gold", "r_xau_vnd", "r_bond_tri"]):
        returns[name] = np.where(regime == i, 0.8, 0.1) * market + rng.normal(0, 0.5, rows)
    returns["r_gold_premium"] = returns.r_sjc_gold - returns.r_xau_vnd
    returns["r_brent"] = rng.normal(0, 1.5, rows)
    returns["vix"] = 15 + np.abs(rng.normal(0, 5, rows))
    returns["d_vix"] = returns.vix.diff().fillna(0)
    returns["interbank_rate"] = 3 + rng.normal(0, 0.2, rows)
    returns["d_interbank_rate"] = returns.interbank_rate.diff().fillna(0)
    returns["policy_rate"] = 4.5
    returns["d_policy_rate"] = 0.0
    bonds = pd.DataFrame({"date": dates})
    for tenor in [1, 5, 10]:
        bonds[f"y_{tenor}y"] = 2 + tenor / 10 + rng.normal(0, 0.1, rows)
        bonds[f"d_y_{tenor}y_bps"] = bonds[f"y_{tenor}y"].diff().fillna(0) * 100
    bonds["bond_slope_10y_1y"] = bonds.y_10y - bonds.y_1y
    bonds["bond_slope_10y_5y"] = bonds.y_10y - bonds.y_5y
    bonds["bond_curvature_1_5_10"] = bonds.y_10y - 2 * bonds.y_5y + bonds.y_1y
    for relative, frame in [
        ("core/final_daily_returns.csv", returns),
        ("bond_term_structure/final_bond_term_structure_daily.csv", bonds),
    ]:
        path = root / "data/02_model_input" / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        frame.to_csv(path, index=False)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--smoke", action="store_true", help="Fit one outer decision and test output writers.")
    args = parser.parse_args()
    stamp = datetime.now(timezone.utc).strftime("synthetic_%Y%m%dT%H%M%S_%fZ")
    work = ROOT / "results/demo_runs" / stamp
    work.mkdir(parents=True, exist_ok=False)
    model_dir = work / "src/models/python"
    model_dir.mkdir(parents=True)
    for name in ["03_adaptive_hedge_ml.py", "04_adaptive_hedge_ml_upgrade.py", "05_adaptive_hedge_ml_final.py"]:
        shutil.copy2(ROOT / "src/models/python" / name, model_dir / name)
    shutil.copy2(ROOT / "src/models/model_control.yaml", model_dir.parent / "model_control.yaml")
    synthetic_inputs(work)
    (work / "SYNTHETIC_DATA_ONLY.json").write_text(json.dumps({
        "purpose": "Software validation only; generated values are not market observations.",
        "seed": 20261108, "rows": 3100,
    }, indent=2), encoding="utf-8")
    subprocess.run([
        sys.executable, str(model_dir / "05_adaptive_hedge_ml_final.py"),
        "--smoke" if args.smoke else "--preflight", "--run-id", "demo",
    ], cwd=work, check=True)
    print(f"SYNTHETIC ONLY: {work}")


if __name__ == "__main__":
    main()
