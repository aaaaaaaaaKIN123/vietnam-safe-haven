from __future__ import annotations

import math
import json
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[3]


def load_validation_control() -> dict:
    text = (ROOT / "src/models/model_control.yaml").read_text(encoding="utf-8")
    _, marker, section = text.partition("\nadaptive_ml_validation:\n")
    if not marker:
        raise ValueError("Missing adaptive_ml_validation in model_control.yaml")
    control, _ = json.JSONDecoder().raw_decode(section.lstrip())
    if control["label_cutoff"] != "strictly_before_decision_date":
        raise ValueError("ML validation requires strict label maturity")
    if control["dynamic_benchmarks"] != "excluded_until_point_in_time_vintages":
        raise ValueError("Full-sample dynamic estimates cannot enter predictive ML")
    if not 0 < control["stress_quantile"] < 1 or control["stress_min_history"] < 2:
        raise ValueError("Invalid predictive stress settings")
    if not set(control["fallbacks"]).issubset({"model", "static_best", "always_xau_vnd"}):
        raise ValueError("Unsupported point-in-time fallback")
    if not control["bootstrap_block_windows"] or any(
        not isinstance(n, int) or n < 1 for n in control["bootstrap_block_windows"]
    ):
        raise ValueError("Bootstrap block lengths must be positive integers")
    return control


VALIDATION_CONTROL = load_validation_control()
RETURNS_PATH = ROOT / "data" / "02_model_input" / "core" / "final_daily_returns.csv"
BOND_TS_PATH = ROOT / "data" / "02_model_input" / "bond_term_structure" / "final_bond_term_structure_daily.csv"
DYNAMIC_HEDGE_PATH = ROOT / "results" / "tables" / "hedge_ratios" / "dynamic_hedge_ratios.csv"
TABLE_DIR = ROOT / "results" / "tables" / "adaptive_ml"
FIGURE_DIR = ROOT / "results" / "figures" / "adaptive_ml"
REPORT_PATH = ROOT / "data" / "metadata" / "phase3_13_adaptive_ml_report.md"

TARGET = "r_vnindex"
HEDGES = ["r_sjc_gold", "r_xau_vnd", "r_bond_tri"]
HEDGE_LABELS = {
    "r_sjc_gold": "SJC Gold",
    "r_xau_vnd": "XAU/VND",
    "r_bond_tri": "Bond TRI 10Y",
}

HORIZON = 21
REBALANCE_STEP = 21
HEDGE_LOOKBACK = 252
MIN_HISTORY_ROWS = 756
MIN_TRAIN_SAMPLES = 80
RANDOM_SEED = 20260529


@dataclass
class WindowResult:
    date: pd.Timestamp
    window_end: pd.Timestamp
    actual_best_hedge: str
    static_best_hedge: str
    dcc_best_hedge: str | None
    adcc_best_hedge: str | None
    hedge_ratios: dict[str, float]
    future_returns: dict[str, np.ndarray]
    actual_he: dict[str, float]
    future_dates: np.ndarray


def mature_training_samples(samples: pd.DataFrame, decision_date) -> pd.DataFrame:
    """Labels must be fully observable before the decision's calendar date."""
    return samples.loc[pd.to_datetime(samples["window_end"]) < pd.Timestamp(decision_date)].copy()


def predictive_stress(series: pd.Series) -> pd.Series:
    threshold = series.expanding(
        min_periods=VALIDATION_CONTROL["stress_min_history"]
    ).quantile(VALIDATION_CONTROL["stress_quantile"]).shift(1)
    return (series <= threshold).astype(float).where(threshold.notna() & series.notna())


def window_return_dates(window: WindowResult, values: np.ndarray) -> np.ndarray:
    dates = pd.DatetimeIndex(window.future_dates)
    if len(dates) != len(values) or dates.has_duplicates or not dates.is_monotonic_increasing:
        raise ValueError("Return values must align with actual, ordered trading dates")
    if len(dates) and (dates[0] <= window.date or dates[-1] != window.window_end):
        raise ValueError("Return dates do not match the decision window")
    return dates.to_numpy()


def ensure_dirs() -> None:
    TABLE_DIR.mkdir(parents=True, exist_ok=True)
    FIGURE_DIR.mkdir(parents=True, exist_ok=True)
    REPORT_PATH.parent.mkdir(parents=True, exist_ok=True)


def safe_read_csv(path: Path) -> pd.DataFrame:
    if not path.exists():
        raise FileNotFoundError(f"Required input not found: {path}")
    frame = pd.read_csv(path)
    if "date" not in frame.columns:
        raise ValueError(f"Missing date column in {path.name}")
    dates = pd.to_datetime(frame["date"], errors="coerce")
    if dates.isna().any():
        raise ValueError(f"Missing or invalid dates in {path.name}")
    if dates.duplicated().any():
        raise ValueError(f"Duplicate dates in {path.name}")
    frame["date"] = dates
    return frame


def rolling_shifted(series: pd.Series, window: int, func: str) -> pd.Series:
    min_periods = min(window, max(5, window // 4))
    roll = series.rolling(window=window, min_periods=min_periods)
    if func == "std":
        out = roll.std()
    elif func == "mean":
        out = roll.mean()
    elif func == "sum":
        out = roll.sum()
    else:
        raise ValueError(f"Unsupported rolling function: {func}")
    return out.shift(1)


def prepare_panel() -> tuple[pd.DataFrame, list[str]]:
    returns = safe_read_csv(RETURNS_PATH)
    required = {TARGET, *HEDGES}
    missing = required.difference(returns.columns)
    if missing:
        raise ValueError(f"Missing core return columns: {', '.join(sorted(missing))}")
    for column in required:
        values = pd.to_numeric(returns[column], errors="raise")
        if np.isinf(values.to_numpy(dtype=float)).any():
            raise ValueError(f"Infinite values in core return column: {column}")
        returns[column] = values
    returns["date"] = pd.to_datetime(returns["date"])
    returns = returns.sort_values("date")

    bond_ts = safe_read_csv(BOND_TS_PATH)
    bond_ts["date"] = pd.to_datetime(bond_ts["date"])
    bond_cols = [
        "date",
        "y_1y",
        "y_5y",
        "y_10y",
        "d_y_1y_bps",
        "d_y_5y_bps",
        "d_y_10y_bps",
        "bond_slope_10y_1y",
        "bond_slope_10y_5y",
        "bond_curvature_1_5_10",
    ]
    bond_ts = bond_ts[[c for c in bond_cols if c in bond_ts.columns]]
    panel = returns.merge(bond_ts, on="date", how="left", validate="one_to_one")
    panel = panel[panel["date"] >= pd.Timestamp("2010-01-04")].copy()
    if panel["date"].duplicated().any():
        raise ValueError("Duplicate dates in ML input panel")
    # The saved stress dummy is descriptive and uses a full-sample quantile.
    # Rebuild only the predictive ML copy; leave econometric inputs unchanged.
    panel["stress_vnindex_q05"] = predictive_stress(panel[TARGET])

    panel["wedge_ret_sjc_minus_xau"] = panel["r_sjc_gold"] - panel["r_xau_vnd"]
    panel["abs_brent_return"] = panel["r_brent"].abs() if "r_brent" in panel.columns else np.nan
    panel["abs_vnindex_return"] = panel[TARGET].abs()

    feature_specs = {
        "vnindex_vol_21": (TARGET, 21, "std"),
        "vnindex_vol_63": (TARGET, 63, "std"),
        "vnindex_mom_21": (TARGET, 21, "sum"),
        "vnindex_tail_21": ("stress_vnindex_q05", 21, "mean"),
        "sjc_vol_21": ("r_sjc_gold", 21, "std"),
        "xau_vnd_vol_21": ("r_xau_vnd", 21, "std"),
        "bond_tri_vol_21": ("r_bond_tri", 21, "std"),
        "gold_wedge_mean_21": ("wedge_ret_sjc_minus_xau", 21, "mean"),
        "gold_wedge_vol_21": ("wedge_ret_sjc_minus_xau", 21, "std"),
        "vix_level_lag": ("vix", 1, "mean"),
        "vix_change_21": ("d_vix", 21, "sum"),
        "interbank_level_lag": ("interbank_rate", 1, "mean"),
        "interbank_change_21": ("d_interbank_rate", 21, "sum"),
        "policy_level_lag": ("policy_rate", 1, "mean"),
        "policy_change_63": ("d_policy_rate", 63, "sum"),
        "oil_abs_return_21": ("abs_brent_return", 21, "mean"),
        "bond_slope_10y_1y_lag": ("bond_slope_10y_1y", 1, "mean"),
        "bond_slope_10y_5y_lag": ("bond_slope_10y_5y", 1, "mean"),
        "bond_curvature_lag": ("bond_curvature_1_5_10", 1, "mean"),
        "d_y_1y_21": ("d_y_1y_bps", 21, "sum"),
        "d_y_5y_21": ("d_y_5y_bps", 21, "sum"),
        "d_y_10y_21": ("d_y_10y_bps", 21, "sum"),
    }
    feature_cols: list[str] = []
    for out_col, (src_col, window, func) in feature_specs.items():
        if src_col in panel.columns:
            panel[out_col] = rolling_shifted(panel[src_col], window, func)
            feature_cols.append(out_col)

    lag_cols = [TARGET] + HEDGES + ["r_gold_premium", "r_brent"]
    for col in lag_cols:
        if col in panel.columns:
            lag_name = f"{col}_lag1"
            panel[lag_name] = panel[col].shift(1)
            feature_cols.append(lag_name)

    return panel.reset_index(drop=True), feature_cols


def hedge_beta(train_target: np.ndarray, train_hedge: np.ndarray) -> float:
    mask = np.isfinite(train_target) & np.isfinite(train_hedge)
    if mask.sum() < 30:
        return 0.0
    x = train_hedge[mask]
    y = train_target[mask]
    var_x = np.var(x, ddof=1)
    if not np.isfinite(var_x) or var_x <= 1e-10:
        return 0.0
    return float(np.cov(y, x, ddof=1)[0, 1] / var_x)


def variance_reduction(target: np.ndarray, hedged: np.ndarray) -> float:
    mask = np.isfinite(target) & np.isfinite(hedged)
    if mask.sum() < 5:
        return np.nan
    unhedged_var = np.var(target[mask], ddof=1)
    hedged_var = np.var(hedged[mask], ddof=1)
    if unhedged_var <= 1e-10:
        return np.nan
    return float(1.0 - hedged_var / unhedged_var)


def build_dynamic_lookup() -> pd.DataFrame | None:
    if not DYNAMIC_HEDGE_PATH.exists():
        return None
    dyn = pd.read_csv(DYNAMIC_HEDGE_PATH)
    needed = {"date", "hedge", "model", "hedged_return"}
    if not needed.issubset(set(dyn.columns)):
        return None
    dyn["date"] = pd.to_datetime(dyn["date"])
    dyn = dyn[dyn["hedge"].isin(HEDGES) & dyn["model"].isin(["DCC", "aDCC"])].copy()
    return dyn


def best_dynamic_hedge(
    dyn: pd.DataFrame | None,
    model: str,
    train_dates: pd.Series,
    target_train: pd.Series,
) -> str | None:
    if dyn is None:
        return None
    train = pd.DataFrame({"date": train_dates.to_numpy(), "target": target_train.to_numpy()})
    scores = {}
    for hedge in HEDGES:
        sub = dyn[(dyn["model"] == model) & (dyn["hedge"] == hedge)][["date", "hedged_return"]]
        merged = train.merge(sub, on="date", how="inner")
        if len(merged) < 30:
            continue
        scores[hedge] = variance_reduction(merged["target"].to_numpy(), merged["hedged_return"].to_numpy())
    if not scores:
        return None
    return max(scores, key=lambda h: -np.inf if np.isnan(scores[h]) else scores[h])


def dynamic_future_returns(
    dyn: pd.DataFrame | None,
    model: str,
    hedge: str | None,
    future_dates: pd.Series,
    fallback: np.ndarray,
) -> np.ndarray:
    if dyn is None or hedge is None:
        return fallback
    sub = dyn[(dyn["model"] == model) & (dyn["hedge"] == hedge)][["date", "hedged_return"]]
    merged = pd.DataFrame({"date": future_dates.to_numpy()}).merge(sub, on="date", how="left")
    out = merged["hedged_return"].to_numpy()
    if np.isfinite(out).sum() < max(5, len(out) // 2):
        return fallback
    out = np.where(np.isfinite(out), out, fallback)
    return out.astype(float)


def build_windows(panel: pd.DataFrame) -> list[WindowResult]:
    # DCC/aDCC artifacts were fitted on the full sample. Do not use them
    # for either predictive fallback selection or an alleged OOS benchmark.
    dyn = None
    required = [TARGET] + HEDGES
    complete = panel.dropna(subset=required).reset_index(drop=True)
    windows: list[WindowResult] = []

    for idx in range(MIN_HISTORY_ROWS, len(complete) - HORIZON - 1, REBALANCE_STEP):
        train = complete.iloc[idx - HEDGE_LOOKBACK : idx]
        future = complete.iloc[idx + 1 : idx + 1 + HORIZON]
        if len(train) < HEDGE_LOOKBACK or len(future) < HORIZON:
            continue

        target_train = train[TARGET].to_numpy(dtype=float)
        target_future = future[TARGET].to_numpy(dtype=float)
        betas: dict[str, float] = {}
        future_returns: dict[str, np.ndarray] = {}
        actual_he: dict[str, float] = {}
        trailing_he: dict[str, float] = {}

        for hedge in HEDGES:
            beta = hedge_beta(target_train, train[hedge].to_numpy(dtype=float))
            betas[hedge] = beta
            hedged_future = target_future - beta * future[hedge].to_numpy(dtype=float)
            hedged_train = target_train - beta * train[hedge].to_numpy(dtype=float)
            future_returns[hedge] = hedged_future
            actual_he[hedge] = variance_reduction(target_future, hedged_future)
            trailing_he[hedge] = variance_reduction(target_train, hedged_train)

        actual_best = max(actual_he, key=lambda h: -np.inf if np.isnan(actual_he[h]) else actual_he[h])
        static_best = max(trailing_he, key=lambda h: -np.inf if np.isnan(trailing_he[h]) else trailing_he[h])
        dcc_best = best_dynamic_hedge(dyn, "DCC", train["date"], train[TARGET])
        adcc_best = best_dynamic_hedge(dyn, "aDCC", train["date"], train[TARGET])

        fallback = future_returns[static_best]
        future_returns["unhedged"] = target_future
        future_returns["equal_basket"] = np.vstack([future_returns[h] for h in HEDGES]).mean(axis=0)
        future_returns["static_best_trailing"] = future_returns[static_best]

        windows.append(
            WindowResult(
                date=complete.loc[idx, "date"],
                window_end=future["date"].iloc[-1],
                actual_best_hedge=actual_best,
                static_best_hedge=static_best,
                dcc_best_hedge=dcc_best,
                adcc_best_hedge=adcc_best,
                hedge_ratios=betas,
                future_returns=future_returns,
                actual_he=actual_he,
                future_dates=future["date"].to_numpy(),
            )
        )

    return windows


def sample_matrix(panel: pd.DataFrame, feature_cols: list[str], windows: list[WindowResult]) -> pd.DataFrame:
    lookup = panel.set_index("date")
    rows = []
    for window in windows:
        if window.date not in lookup.index:
            continue
        row = {
            "decision_date": window.date,
            "window_end": window.window_end,
            "actual_best_hedge": window.actual_best_hedge,
            "static_best_hedge": window.static_best_hedge,
            "dcc_best_hedge": window.dcc_best_hedge,
            "adcc_best_hedge": window.adcc_best_hedge,
        }
        for hedge in HEDGES:
            row[f"actual_he_{hedge}"] = window.actual_he[hedge]
            row[f"beta_{hedge}"] = window.hedge_ratios[hedge]
        for col in feature_cols:
            row[col] = lookup.loc[window.date, col]
        rows.append(row)
    samples = pd.DataFrame(rows)
    return samples.dropna(subset=feature_cols + ["actual_best_hedge"]).reset_index(drop=True)


def fit_softmax(
    x_train: np.ndarray,
    y_train: np.ndarray,
    n_classes: int,
    epochs: int = 500,
    lr: float = 0.08,
    l2: float = 0.002,
) -> np.ndarray:
    rng = np.random.default_rng(RANDOM_SEED)
    n, p = x_train.shape
    xb = np.column_stack([np.ones(n), x_train])
    w = rng.normal(0.0, 0.01, size=(p + 1, n_classes))
    y = np.zeros((n, n_classes))
    y[np.arange(n), y_train] = 1.0

    for _ in range(epochs):
        logits = xb @ w
        logits -= logits.max(axis=1, keepdims=True)
        probs = np.exp(logits)
        probs /= probs.sum(axis=1, keepdims=True)
        grad = xb.T @ (probs - y) / n
        grad[1:, :] += l2 * w[1:, :]
        w -= lr * grad
    return w


def predict_softmax(x: np.ndarray, w: np.ndarray) -> np.ndarray:
    xb = np.concatenate([[1.0], x])
    logits = xb @ w
    logits -= np.max(logits)
    probs = np.exp(logits)
    return probs / probs.sum()


def run_walk_forward(panel: pd.DataFrame, feature_cols: list[str], windows: list[WindowResult]) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    samples = sample_matrix(panel, feature_cols, windows)
    class_to_idx = {h: i for i, h in enumerate(HEDGES)}
    idx_to_class = {i: h for h, i in class_to_idx.items()}
    samples["label_id"] = samples["actual_best_hedge"].map(class_to_idx)
    usable = samples.dropna(subset=["label_id"]).reset_index(drop=True)

    decision_rows = []
    return_rows = []
    importance_accumulator = []
    window_by_date = {w.date: w for w in windows}

    for test_idx in range(MIN_TRAIN_SAMPLES, len(usable)):
        test = usable.iloc[test_idx].copy()
        train = mature_training_samples(usable.iloc[:test_idx], test["decision_date"])
        if len(train) < MIN_TRAIN_SAMPLES:
            continue
        x_train_raw = train[feature_cols].to_numpy(dtype=float)
        y_train = train["label_id"].to_numpy(dtype=int)
        means = np.nanmean(x_train_raw, axis=0)
        stds = np.nanstd(x_train_raw, axis=0)
        stds = np.where(stds < 1e-8, 1.0, stds)
        x_train = (np.where(np.isfinite(x_train_raw), x_train_raw, means) - means) / stds
        x_test_raw = test[feature_cols].to_numpy(dtype=float)
        x_test = (np.where(np.isfinite(x_test_raw), x_test_raw, means) - means) / stds

        weights = fit_softmax(x_train, y_train, len(HEDGES))
        probs = predict_softmax(x_test, weights)
        selected_id = int(np.argmax(probs))
        selected_hedge = idx_to_class[selected_id]
        window = window_by_date[pd.Timestamp(test["decision_date"])]

        coefficient_importance = np.mean(np.abs(weights[1:, :]), axis=1)
        importance_accumulator.append(coefficient_importance)

        selected_he = window.actual_he[selected_hedge]
        actual_best_he = window.actual_he[window.actual_best_hedge]
        decision_row = {
            "decision_date": test["decision_date"],
            "window_end": test["window_end"],
            "selected_hedge": selected_hedge,
            "actual_best_hedge": window.actual_best_hedge,
            "static_best_hedge": window.static_best_hedge,
            "dcc_best_hedge": window.dcc_best_hedge,
            "adcc_best_hedge": window.adcc_best_hedge,
            "selected_is_actual_best": selected_hedge == window.actual_best_hedge,
            "selected_realized_he": selected_he,
            "actual_best_realized_he": actual_best_he,
            "realized_he_gap": actual_best_he - selected_he,
        }
        for hedge, prob in zip(HEDGES, probs):
            decision_row[f"prob_{hedge}"] = float(prob)
            decision_row[f"actual_he_{hedge}"] = window.actual_he[hedge]
        decision_rows.append(decision_row)

        strategies = {
            "unhedged_vnindex": "unhedged",
            "ml_softmax_adaptive": selected_hedge,
            "static_best_trailing": "static_best_trailing",
            "equal_safe_haven_basket": "equal_basket",
        }
        horizon_dates = window_return_dates(window, window.future_returns["unhedged"])

        for strategy, key in strategies.items():
            values = window.future_returns[key]
            strategy_hedge = selected_hedge if strategy == "ml_softmax_adaptive" else key
            if strategy == "static_best_trailing":
                strategy_hedge = window.static_best_hedge
            if strategy == "dcc_trailing_best":
                strategy_hedge = window.dcc_best_hedge or window.static_best_hedge
            if strategy == "adcc_trailing_best":
                strategy_hedge = window.adcc_best_hedge or window.static_best_hedge
            if strategy == "unhedged_vnindex":
                strategy_hedge = "none"
            if strategy == "equal_safe_haven_basket":
                strategy_hedge = "equal_basket"
            for day, ret in zip(horizon_dates, values):
                return_rows.append(
                    {
                        "date": pd.Timestamp(day),
                        "decision_date": window.date,
                        "window_end": window.window_end,
                        "strategy": strategy,
                        "selected_hedge": strategy_hedge,
                        "return_pct": float(ret),
                    }
                )

    decision_df = pd.DataFrame(decision_rows)
    returns_df = pd.DataFrame(return_rows)
    if importance_accumulator:
        imp = np.vstack(importance_accumulator).mean(axis=0)
    else:
        imp = np.zeros(len(feature_cols))
    importance_df = pd.DataFrame(
        {
            "feature": feature_cols,
            "importance_abs_softmax_coefficient": imp,
        }
    ).sort_values("importance_abs_softmax_coefficient", ascending=False)
    total = importance_df["importance_abs_softmax_coefficient"].sum()
    if total > 0:
        importance_df["importance_share"] = importance_df["importance_abs_softmax_coefficient"] / total
    else:
        importance_df["importance_share"] = 0.0
    return decision_df, returns_df, importance_df


def max_drawdown(return_pct: pd.Series) -> float:
    wealth = (1.0 + return_pct.fillna(0.0).to_numpy() / 100.0).cumprod()
    peak = np.maximum.accumulate(wealth)
    dd = wealth / peak - 1.0
    return float(np.min(dd) * 100.0)


def expected_shortfall(return_pct: pd.Series, alpha: float = 0.05) -> float:
    x = return_pct.dropna().to_numpy()
    if len(x) == 0:
        return np.nan
    cutoff = np.nanquantile(x, alpha)
    tail = x[x <= cutoff]
    return float(np.mean(tail)) if len(tail) else np.nan


def summarize_strategies(returns_df: pd.DataFrame) -> pd.DataFrame:
    unhedged = returns_df[returns_df["strategy"] == "unhedged_vnindex"]["return_pct"]
    unhedged_var = unhedged.var(ddof=1)
    unhedged_es = expected_shortfall(unhedged)
    rows = []
    for strategy, group in returns_df.groupby("strategy"):
        r = group.sort_values("date")["return_pct"]
        var = r.var(ddof=1)
        es = expected_shortfall(r)
        total_return = ((1.0 + r / 100.0).prod() - 1.0) * 100.0
        ann_vol = r.std(ddof=1) * math.sqrt(252)
        mean_daily = r.mean()
        sharpe_like = (mean_daily / r.std(ddof=1) * math.sqrt(252)) if r.std(ddof=1) > 1e-10 else np.nan
        rows.append(
            {
                "strategy": strategy,
                "observations": len(r),
                "mean_daily_return_pct": mean_daily,
                "annualized_vol_pct": ann_vol,
                "total_return_pct": total_return,
                "var_5pct": float(r.quantile(0.05)),
                "es_5pct": es,
                "max_drawdown_pct": max_drawdown(r),
                "variance_reduction_vs_unhedged": 1.0 - var / unhedged_var if unhedged_var > 1e-10 else np.nan,
                "es_reduction_vs_unhedged": 1.0 - abs(es) / abs(unhedged_es) if unhedged_es and np.isfinite(unhedged_es) else np.nan,
                "sharpe_like": sharpe_like,
            }
        )
    out = pd.DataFrame(rows)
    strategy_order = [
        "unhedged_vnindex",
        "static_best_trailing",
        "equal_safe_haven_basket",
        "dcc_trailing_best",
        "adcc_trailing_best",
        "ml_softmax_adaptive",
    ]
    out["strategy_order"] = out["strategy"].map({s: i for i, s in enumerate(strategy_order)})
    return out.sort_values("strategy_order").drop(columns=["strategy_order"])


def transaction_cost_sensitivity(returns_df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for cost_bps in [0, 5, 10, 25]:
        cost_pct = cost_bps / 100.0
        for strategy, group in returns_df.groupby("strategy"):
            group = group.sort_values(["decision_date", "date"]).copy()
            first_rows = group.groupby("decision_date").head(1).index
            switch = pd.Series(False, index=group.index)
            if strategy not in ["unhedged_vnindex", "equal_safe_haven_basket"]:
                decision_hedge = group.groupby("decision_date")["selected_hedge"].first()
                switch_decision = decision_hedge.ne(decision_hedge.shift(1)).fillna(False)
                switch_dates = set(switch_decision[switch_decision].index)
                switch = group["decision_date"].isin(switch_dates) & group.index.isin(first_rows)
            adjusted = group["return_pct"].copy()
            adjusted.loc[switch] = adjusted.loc[switch] - cost_pct
            rows.append(
                {
                    "strategy": strategy,
                    "transaction_cost_bps_per_switch": cost_bps,
                    "switch_count": int(switch.sum()),
                    "mean_daily_return_pct_net": adjusted.mean(),
                    "total_return_pct_net": ((1.0 + adjusted / 100.0).prod() - 1.0) * 100.0,
                    "max_drawdown_pct_net": max_drawdown(adjusted),
                    "es_5pct_net": expected_shortfall(adjusted),
                }
            )
    return pd.DataFrame(rows)


def write_outputs(
    panel: pd.DataFrame,
    feature_cols: list[str],
    windows: list[WindowResult],
    decisions: pd.DataFrame,
    returns_df: pd.DataFrame,
    importance: pd.DataFrame,
    benchmark: pd.DataFrame,
    tc: pd.DataFrame,
) -> None:
    decisions.to_csv(TABLE_DIR / "ml_walk_forward_decisions.csv", index=False, encoding="utf-8-sig")
    returns_df.to_csv(TABLE_DIR / "ml_walk_forward_performance.csv", index=False, encoding="utf-8-sig")
    benchmark.to_csv(TABLE_DIR / "ml_benchmark_comparison.csv", index=False, encoding="utf-8-sig")
    importance.to_csv(TABLE_DIR / "ml_feature_importance.csv", index=False, encoding="utf-8-sig")
    tc.to_csv(TABLE_DIR / "ml_transaction_cost_sensitivity.csv", index=False, encoding="utf-8-sig")

    if len(decisions) > 0:
        accuracy = decisions["selected_is_actual_best"].mean()
        mean_gap = decisions["realized_he_gap"].mean()
        test_start = decisions["decision_date"].min()
        test_end = decisions["window_end"].max()
    else:
        accuracy = np.nan
        mean_gap = np.nan
        test_start = test_end = None

    top_strategy = benchmark.sort_values("variance_reduction_vs_unhedged", ascending=False).iloc[0]["strategy"] if len(benchmark) else "NA"
    ml_row = benchmark[benchmark["strategy"] == "ml_softmax_adaptive"]
    ml_var_reduction = float(ml_row["variance_reduction_vs_unhedged"].iloc[0]) if len(ml_row) else np.nan

    report = f"""# Phase 3.13 Adaptive Hedge Selection ML Report

## Scope

- Task: test whether an explainable adaptive rule can select the next-period hedge better than static benchmarks. Full-sample DCC/aDCC is excluded from the predictive path.
- Core investable safe-haven candidates: {', '.join(HEDGE_LABELS[h] for h in HEDGES)}.
- Brent is not used as a hedge candidate. It is used only as an oil-price shock/risk-channel feature when available.
- Crypto is not used in the main adaptive hedge selection.

## Method

- Validation design: walk-forward time-series validation; no random train-test split.
- Rebalancing horizon: {HORIZON} trading days.
- Hedge-ratio estimation window: {HEDGE_LOOKBACK} trading days.
- Minimum pre-sample history: {MIN_HISTORY_ROWS} trading rows.
- ML model: multinomial softmax classifier implemented with numpy because xgboost/scikit-learn/shap are not installed in the current local runtime.
- Feature interpretation: coefficient-based feature importance. This is a transparent fallback, not SHAP.
- Target: next-window realized best hedge by variance-reduction/hedging-effectiveness.

## Sample

- Available input rows after 2010 start: {len(panel)}
- Decision windows built: {len(windows)}
- Out-of-sample ML decision windows: {len(decisions)}
- Out-of-sample test period: {test_start} to {test_end}
- Number of features: {len(feature_cols)}

## Main Diagnostics

- ML exact-hit rate against next-window best hedge: {accuracy:.4f}
- Average realized HE gap versus ex-post best hedge: {mean_gap:.6f}
- Best strategy by variance reduction versus unhedged VNIndex: {top_strategy}
- ML variance reduction versus unhedged VNIndex: {ml_var_reduction:.6f}

## Required Outputs

- `results/tables/adaptive_ml/ml_walk_forward_decisions.csv`
- `results/tables/adaptive_ml/ml_walk_forward_performance.csv`
- `results/tables/adaptive_ml/ml_benchmark_comparison.csv`
- `results/tables/adaptive_ml/ml_feature_importance.csv`
- `results/tables/adaptive_ml/ml_transaction_cost_sensitivity.csv`
- `results/figures/adaptive_ml/ml_cumulative_hedged_return.png`
- `results/figures/adaptive_ml/ml_shap_summary.png`

## Scientific Cautions

- This phase is an application layer, not a causal claim.
- Accuracy is not the main result; out-of-sample hedging performance, ES reduction and drawdown behavior are more important.
- If ML does not dominate benchmarks after transaction costs, that is still a valid scientific finding.
- `ml_shap_summary` is named to keep the planned output contract, but the current figure should be described as coefficient-based feature importance unless SHAP is installed and rerun.
"""
    REPORT_PATH.write_text(report, encoding="utf-8")


def main() -> None:
    ensure_dirs()
    panel, feature_cols = prepare_panel()
    windows = build_windows(panel)
    decisions, returns_df, importance = run_walk_forward(panel, feature_cols, windows)
    benchmark = summarize_strategies(returns_df)
    tc = transaction_cost_sensitivity(returns_df)
    write_outputs(panel, feature_cols, windows, decisions, returns_df, importance, benchmark, tc)
    print("Phase 3.13 adaptive ML tables completed.")
    print(f"Decision windows: {len(decisions)}")
    print(f"Performance rows: {len(returns_df)}")
    print(f"Feature rows: {len(importance)}")
    print(f"Report: {REPORT_PATH}")


if __name__ == "__main__":
    main()
