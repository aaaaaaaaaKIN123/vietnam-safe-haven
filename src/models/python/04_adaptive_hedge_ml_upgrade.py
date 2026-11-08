from __future__ import annotations

import importlib.util
import math
import os
import sys
import warnings
from pathlib import Path

import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[3]
OKABE_ITO = {
    "charcoal": "#1F2937",
    "grey": "#999999",
    "light_grey": "#D9D9D9",
    "orange": "#E69F00",
    "blue": "#0072B2",
    "vermillion": "#D55E00",
}
BASE_SCRIPT = ROOT / "src" / "models" / "python" / "03_adaptive_hedge_ml.py"
TABLE_DIR = ROOT / "results" / "tables" / "adaptive_ml_upgrade"
REPORT_PATH = ROOT / "data" / "metadata" / "phase3_18_ml_upgrade_report.md"
os.environ.setdefault("MPLCONFIGDIR", str(ROOT / ".cache" / "matplotlib"))

RANDOM_SEED = 20260622
BOOTSTRAP_ROUNDS = 500
PERMUTATION_ROUNDS = 20
warnings.filterwarnings("ignore", category=FutureWarning)


def load_base_module():
    spec = importlib.util.spec_from_file_location("adaptive_ml_base", BASE_SCRIPT)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


base = load_base_module()


def ensure_dirs() -> None:
    TABLE_DIR.mkdir(parents=True, exist_ok=True)
    REPORT_PATH.parent.mkdir(parents=True, exist_ok=True)


def optional_imports():
    out = {}
    try:
        from sklearn.ensemble import RandomForestClassifier
        from sklearn.impute import SimpleImputer
        from sklearn.linear_model import LogisticRegression
        from sklearn.multiclass import OneVsRestClassifier
        from sklearn.metrics import accuracy_score, f1_score
        from sklearn.pipeline import Pipeline
        from sklearn.preprocessing import StandardScaler
        from sklearn.utils.class_weight import compute_class_weight

        out.update(
            {
                "RandomForestClassifier": RandomForestClassifier,
                "SimpleImputer": SimpleImputer,
                "LogisticRegression": LogisticRegression,
                "OneVsRestClassifier": OneVsRestClassifier,
                "accuracy_score": accuracy_score,
                "f1_score": f1_score,
                "Pipeline": Pipeline,
                "StandardScaler": StandardScaler,
                "compute_class_weight": compute_class_weight,
            }
        )
    except Exception as exc:  # pragma: no cover - runtime dependency gate
        out["sklearn_error"] = repr(exc)

    try:
        from xgboost import XGBClassifier

        out["XGBClassifier"] = XGBClassifier
    except Exception as exc:  # pragma: no cover - runtime dependency gate
        out["xgboost_error"] = repr(exc)

    try:
        import shap

        out["shap"] = shap
    except Exception as exc:  # pragma: no cover - runtime dependency gate
        out["shap_error"] = repr(exc)

    return out


def class_maps():
    class_to_idx = {h: i for i, h in enumerate(base.HEDGES)}
    idx_to_class = {i: h for h, i in class_to_idx.items()}
    return class_to_idx, idx_to_class


def build_samples():
    panel, feature_cols = base.prepare_panel()
    windows = base.build_windows(panel)
    samples = base.sample_matrix(panel, feature_cols, windows)
    class_to_idx, _ = class_maps()
    samples["label_id"] = samples["actual_best_hedge"].map(class_to_idx)
    samples = samples.dropna(subset=["label_id"]).reset_index(drop=True)
    samples["label_id"] = samples["label_id"].astype(int)
    window_by_date = {w.date: w for w in windows}
    return panel, feature_cols, windows, samples, window_by_date


def class_distribution(samples: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for label_col in ["actual_best_hedge", "static_best_hedge", "dcc_best_hedge", "adcc_best_hedge"]:
        counts = samples[label_col].value_counts(dropna=False)
        total = int(counts.sum())
        for label, count in counts.items():
            rows.append(
                {
                    "label_source": label_col,
                    "hedge": "missing" if pd.isna(label) else label,
                    "count": int(count),
                    "share": float(count / total) if total else np.nan,
                }
            )
    return pd.DataFrame(rows)


def make_models(deps: dict):
    if "sklearn_error" in deps:
        raise RuntimeError(f"scikit-learn is required for Phase 3.18: {deps['sklearn_error']}")

    Pipeline = deps["Pipeline"]
    SimpleImputer = deps["SimpleImputer"]
    StandardScaler = deps["StandardScaler"]
    LogisticRegression = deps["LogisticRegression"]
    RandomForestClassifier = deps["RandomForestClassifier"]

    models = {
        "logistic_l2_balanced": Pipeline(
            [
                ("imputer", SimpleImputer(strategy="median")),
                ("scaler", StandardScaler()),
                (
                    "model",
                    LogisticRegression(
                        C=0.5,
                        class_weight="balanced",
                        max_iter=2000,
                        random_state=RANDOM_SEED,
                    ),
                ),
            ]
        ),
        "random_forest_balanced": Pipeline(
            [
                ("imputer", SimpleImputer(strategy="median")),
                (
                    "model",
                    RandomForestClassifier(
                        n_estimators=150,
                        max_depth=3,
                        min_samples_leaf=8,
                        class_weight="balanced_subsample",
                        random_state=RANDOM_SEED,
                    ),
                ),
            ]
        ),
    }

    if "XGBClassifier" in deps:
        XGBClassifier = deps["XGBClassifier"]
        models["xgboost_multiclass"] = Pipeline(
            [
                ("imputer", SimpleImputer(strategy="median")),
                (
                    "model",
                    XGBClassifier(
                        objective="multi:softprob",
                        num_class=len(base.HEDGES),
                        n_estimators=70,
                        max_depth=2,
                        learning_rate=0.04,
                        subsample=0.85,
                        colsample_bytree=0.85,
                        reg_lambda=3.0,
                        reg_alpha=0.1,
                        eval_metric="mlogloss",
                        random_state=RANDOM_SEED,
                    ),
                ),
            ]
        )
    return models


def class_weight_sample_weight(y_train: np.ndarray, deps: dict) -> np.ndarray:
    compute_class_weight = deps["compute_class_weight"]
    classes = np.array(sorted(np.unique(y_train)))
    weights = compute_class_weight(class_weight="balanced", classes=classes, y=y_train)
    weight_map = dict(zip(classes, weights))
    return np.array([weight_map[y] for y in y_train], dtype=float)


def fit_model(name: str, model, x_train: pd.DataFrame, y_train: np.ndarray, deps: dict):
    if name == "xgboost_multiclass":
        sample_weight = class_weight_sample_weight(y_train, deps)
        model.fit(x_train, y_train, model__sample_weight=sample_weight)
    else:
        model.fit(x_train, y_train)
    return model


def run_walk_forward_upgrade(samples: pd.DataFrame, feature_cols: list[str], window_by_date: dict, deps: dict):
    _, idx_to_class = class_maps()
    models = make_models(deps)
    decision_rows = []
    return_rows = []
    importance_rows = []
    y_true_all = []
    y_pred_by_model = {name: [] for name in models}

    for test_idx in range(base.MIN_TRAIN_SAMPLES, len(samples)):
        test = samples.iloc[test_idx].copy()
        train = base.mature_training_samples(samples.iloc[:test_idx], test["decision_date"])
        if len(train) < base.MIN_TRAIN_SAMPLES:
            continue
        x_train = train[feature_cols]
        y_train = train["label_id"].to_numpy(dtype=int)
        x_test = pd.DataFrame([test[feature_cols].to_dict()])
        y_true = int(test["label_id"])
        window = window_by_date[pd.Timestamp(test["decision_date"])]
        y_true_all.append(y_true)

        base_strategy_keys = {
            "unhedged_vnindex": "unhedged",
            "static_best_trailing": "static_best_trailing",
            "equal_safe_haven_basket": "equal_basket",
            "always_sjc_gold": "r_sjc_gold",
            "always_xau_vnd": "r_xau_vnd",
            "always_bond_tri": "r_bond_tri",
        }

        for strategy, key in base_strategy_keys.items():
            selected_hedge = {
                "unhedged_vnindex": "none",
                "equal_safe_haven_basket": "equal_basket",
                "static_best_trailing": window.static_best_hedge,
                "dcc_trailing_best": window.dcc_best_hedge or window.static_best_hedge,
                "adcc_trailing_best": window.adcc_best_hedge or window.static_best_hedge,
                "always_sjc_gold": "r_sjc_gold",
                "always_xau_vnd": "r_xau_vnd",
                "always_bond_tri": "r_bond_tri",
            }[strategy]
            append_future_returns(return_rows, window, strategy, selected_hedge, window.future_returns[key])

        for model_name, model in models.items():
            fitted = fit_model(model_name, model, x_train, y_train, deps)
            pred_id = int(fitted.predict(x_test)[0])
            pred_hedge = idx_to_class[pred_id]
            y_pred_by_model[model_name].append(pred_id)
            probs = fitted.predict_proba(x_test)[0] if hasattr(fitted, "predict_proba") else np.full(len(base.HEDGES), np.nan)

            decision_row = {
                "model": model_name,
                "decision_date": test["decision_date"],
                "window_end": test["window_end"],
                "selected_hedge": pred_hedge,
                "actual_best_hedge": window.actual_best_hedge,
                "selected_is_actual_best": pred_hedge == window.actual_best_hedge,
                "selected_realized_he": window.actual_he[pred_hedge],
                "actual_best_realized_he": window.actual_he[window.actual_best_hedge],
                "realized_he_gap": window.actual_he[window.actual_best_hedge] - window.actual_he[pred_hedge],
            }
            for hedge, prob in zip(base.HEDGES, probs):
                decision_row[f"prob_{hedge}"] = float(prob)
                decision_row[f"actual_he_{hedge}"] = window.actual_he[hedge]
            decision_rows.append(decision_row)
            append_future_returns(return_rows, window, model_name, pred_hedge, window.future_returns[pred_hedge])

            importance_rows.extend(extract_importance(model_name, fitted, feature_cols, test_idx))

    decisions = pd.DataFrame(decision_rows)
    returns = pd.DataFrame(return_rows)
    importance = pd.DataFrame(importance_rows)
    confusion = confusion_table(y_true_all, y_pred_by_model)
    return decisions, returns, importance, confusion


def append_future_returns(rows: list[dict], window, strategy: str, selected_hedge: str, values: np.ndarray) -> None:
    dates = base.window_return_dates(window, values)
    for day, ret in zip(dates, values):
        rows.append(
            {
                "date": pd.Timestamp(day),
                "decision_date": window.date,
                "window_end": window.window_end,
                "strategy": strategy,
                "selected_hedge": selected_hedge,
                "return_pct": float(ret),
            }
        )


def extract_importance(model_name: str, fitted, feature_cols: list[str], fold_index: int) -> list[dict]:
    model = fitted.named_steps.get("model") if hasattr(fitted, "named_steps") else fitted
    values = None
    source = ""
    if hasattr(model, "coef_"):
        values = np.mean(np.abs(model.coef_), axis=0)
        source = "absolute_coefficient"
    elif hasattr(model, "feature_importances_"):
        values = np.asarray(model.feature_importances_, dtype=float)
        source = "tree_feature_importance"
    if values is None:
        return []
    total = float(np.sum(values))
    return [
        {
            "model": model_name,
            "fold_index": fold_index,
            "feature": feature,
            "importance": float(value),
            "importance_share": float(value / total) if total > 0 else np.nan,
            "source": source,
        }
        for feature, value in zip(feature_cols, values)
    ]


def confusion_table(y_true: list[int], y_pred_by_model: dict[str, list[int]]) -> pd.DataFrame:
    _, idx_to_class = class_maps()
    rows = []
    for model_name, preds in y_pred_by_model.items():
        for actual_id in sorted(idx_to_class):
            for pred_id in sorted(idx_to_class):
                rows.append(
                    {
                        "model": model_name,
                        "actual_hedge": idx_to_class[actual_id],
                        "predicted_hedge": idx_to_class[pred_id],
                        "count": int(sum((a == actual_id and p == pred_id) for a, p in zip(y_true, preds))),
                    }
                )
    return pd.DataFrame(rows)


def summarize_decisions(decisions: pd.DataFrame, deps: dict) -> pd.DataFrame:
    accuracy_score = deps["accuracy_score"]
    f1_score = deps["f1_score"]
    rows = []
    for model_name, sub in decisions.groupby("model"):
        y_true = sub["actual_best_hedge"]
        y_pred = sub["selected_hedge"]
        rows.append(
            {
                "model": model_name,
                "decision_windows": len(sub),
                "accuracy": float(accuracy_score(y_true, y_pred)),
                "weighted_f1": float(f1_score(y_true, y_pred, average="weighted")),
                "mean_realized_he_gap": float(sub["realized_he_gap"].mean()),
            }
        )
    return pd.DataFrame(rows)


def summarize_strategies(returns_df: pd.DataFrame) -> pd.DataFrame:
    unhedged = returns_df[returns_df["strategy"] == "unhedged_vnindex"]["return_pct"]
    unhedged_var = unhedged.var(ddof=1)
    unhedged_es = base.expected_shortfall(unhedged)
    rows = []
    for strategy, group in returns_df.groupby("strategy"):
        group = group.sort_values(["date", "decision_date"])
        r = group["return_pct"]
        var = r.var(ddof=1)
        es = base.expected_shortfall(r)
        total_return = ((1.0 + r / 100.0).prod() - 1.0) * 100.0
        std = r.std(ddof=1)
        rows.append(
            {
                "strategy": strategy,
                "observations": len(r),
                "mean_daily_return_pct": float(r.mean()),
                "annualized_vol_pct": float(std * math.sqrt(252)),
                "total_return_pct": float(total_return),
                "var_5pct": float(r.quantile(0.05)),
                "es_5pct": float(es),
                "max_drawdown_pct": base.max_drawdown(r),
                "variance_reduction_vs_unhedged": float(1.0 - var / unhedged_var) if unhedged_var > 1e-10 else np.nan,
                "es_reduction_vs_unhedged": float(1.0 - abs(es) / abs(unhedged_es)) if unhedged_es and np.isfinite(unhedged_es) else np.nan,
                "sharpe_like": float(r.mean() / std * math.sqrt(252)) if std > 1e-10 else np.nan,
            }
        )
    return pd.DataFrame(rows).sort_values("strategy").reset_index(drop=True)


def transaction_cost_sensitivity(returns_df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for cost_bps in [0, 5, 10, 25]:
        cost_pct = cost_bps / 100.0
        for strategy, group in returns_df.groupby("strategy"):
            group = group.sort_values(["decision_date", "date"]).copy()
            first_rows = group.groupby("decision_date").head(1).index
            switch = pd.Series(False, index=group.index)
            if strategy not in ["unhedged_vnindex", "equal_safe_haven_basket"] and not strategy.startswith("always_"):
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
                    "mean_daily_return_pct_net": float(adjusted.mean()),
                    "total_return_pct_net": float(((1.0 + adjusted / 100.0).prod() - 1.0) * 100.0),
                    "max_drawdown_pct_net": base.max_drawdown(adjusted),
                    "es_5pct_net": base.expected_shortfall(adjusted),
                }
            )
    return pd.DataFrame(rows)


def bootstrap_ci(returns_df: pd.DataFrame) -> pd.DataFrame:
    rng = np.random.default_rng(RANDOM_SEED)
    wide = returns_df.pivot_table(index=["decision_date", "date"], columns="strategy", values="return_pct", aggfunc="first")
    strategies = list(wide.columns)
    rows = []
    for strategy in strategies:
        x = wide[strategy].dropna().to_numpy()
        if len(x) == 0:
            continue
        stats = []
        for _ in range(BOOTSTRAP_ROUNDS):
            sample = rng.choice(x, size=len(x), replace=True)
            stats.append(((1.0 + sample / 100.0).prod() - 1.0) * 100.0)
        rows.append(
            {
                "strategy": strategy,
                "metric": "total_return_pct",
                "estimate": float(((1.0 + x / 100.0).prod() - 1.0) * 100.0),
                "ci_low_2p5": float(np.quantile(stats, 0.025)),
                "ci_high_97p5": float(np.quantile(stats, 0.975)),
                "bootstrap_rounds": BOOTSTRAP_ROUNDS,
            }
        )
    return pd.DataFrame(rows)


def permutation_label_test(samples: pd.DataFrame, feature_cols: list[str], deps: dict) -> pd.DataFrame:
    rng = np.random.default_rng(RANDOM_SEED)
    if "sklearn_error" in deps:
        return pd.DataFrame()

    Pipeline = deps["Pipeline"]
    SimpleImputer = deps["SimpleImputer"]
    StandardScaler = deps["StandardScaler"]
    LogisticRegression = deps["LogisticRegression"]
    accuracy_score = deps["accuracy_score"]

    observed_acc = []
    for test_idx in range(base.MIN_TRAIN_SAMPLES, len(samples)):
        test = samples.iloc[test_idx]
        train = base.mature_training_samples(samples.iloc[:test_idx], test["decision_date"])
        if len(train) < base.MIN_TRAIN_SAMPLES:
            continue
        model = Pipeline(
            [
                ("imputer", SimpleImputer(strategy="median")),
                ("scaler", StandardScaler()),
                (
                    "model",
                        LogisticRegression(C=0.5, class_weight="balanced", max_iter=2000, random_state=RANDOM_SEED),
                ),
            ]
        )
        model.fit(train[feature_cols], train["label_id"].to_numpy(dtype=int))
        observed_acc.append(int(model.predict(pd.DataFrame([test[feature_cols].to_dict()]))[0] == int(test["label_id"])))
    observed = float(np.mean(observed_acc))

    perm_acc = []
    y = samples["label_id"].to_numpy(dtype=int)
    for _ in range(PERMUTATION_ROUNDS):
        y_perm = rng.permutation(y)
        hits = []
        for test_idx in range(base.MIN_TRAIN_SAMPLES, len(samples)):
            test = samples.iloc[test_idx]
            train = base.mature_training_samples(samples.iloc[:test_idx], test["decision_date"])
            if len(train) < base.MIN_TRAIN_SAMPLES:
                continue
            model = Pipeline(
                [
                    ("imputer", SimpleImputer(strategy="median")),
                    ("scaler", StandardScaler()),
                    (
                        "model",
                        LogisticRegression(C=0.5, class_weight="balanced", max_iter=1000, random_state=RANDOM_SEED),
                    ),
                ]
            )
            model.fit(train[feature_cols], y_perm[train.index.to_numpy()])
            hits.append(int(model.predict(pd.DataFrame([test[feature_cols].to_dict()]))[0] == y_perm[test_idx]))
        perm_acc.append(float(np.mean(hits)))
    p_value = float((np.sum(np.asarray(perm_acc) >= observed) + 1) / (len(perm_acc) + 1))
    return pd.DataFrame(
        [
            {
                "test": "label_permutation_accuracy_logistic",
                "observed_accuracy": observed,
                "permutation_mean_accuracy": float(np.mean(perm_acc)),
                "permutation_p_value_upper_tail": p_value,
                "permutation_rounds": PERMUTATION_ROUNDS,
            }
        ]
    )


def write_report(
    deps: dict,
    samples: pd.DataFrame,
    decisions: pd.DataFrame,
    decision_summary: pd.DataFrame,
    benchmark: pd.DataFrame,
    permutation: pd.DataFrame,
    shap_summary: pd.DataFrame,
) -> None:
    best_total = benchmark.sort_values("total_return_pct", ascending=False).iloc[0]
    best_variance = benchmark.sort_values("variance_reduction_vs_unhedged", ascending=False).iloc[0]
    decision_markdown = dataframe_to_markdown(decision_summary)
    permutation_markdown = dataframe_to_markdown(permutation) if len(permutation) else "Permutation test was not available."
    lines = [
        "# Phase 3.18 ML Upgrade and Validation Report",
        "",
        "## Scope",
        "",
        "Phase 3.18 upgrades the adaptive hedge-selection layer beyond the Phase 3.13 NumPy softmax baseline.",
        "It adds always-asset baselines, class diagnostics, Logistic/RF/XGBoost model comparison, transaction-cost sensitivity, bootstrap CIs and a permutation-label sanity test.",
        "",
        "## Runtime Dependencies",
        "",
        f"- scikit-learn available: `{ 'sklearn_error' not in deps }`",
        f"- XGBoost available: `{ 'XGBClassifier' in deps }`",
        f"- SHAP available: `{ 'shap' in deps }`",
        "",
        "## Sample",
        "",
        f"- Decision samples: `{len(samples)}`.",
        f"- Out-of-sample decision windows: `{len(decisions) // max(1, decisions['model'].nunique()) if len(decisions) else 0}`.",
        f"- Features: `{len([c for c in samples.columns if c not in ['decision_date','window_end','actual_best_hedge','static_best_hedge','dcc_best_hedge','adcc_best_hedge','label_id'] and not c.startswith('actual_he_') and not c.startswith('beta_')])}`.",
        "",
        "## Main Results",
        "",
        f"- Best strategy by total return: `{best_total['strategy']}`, total return `{best_total['total_return_pct']:.4f}`.",
        f"- Best strategy by variance reduction: `{best_variance['strategy']}`, variance reduction `{best_variance['variance_reduction_vs_unhedged']:.6f}`.",
        "",
        "## Model Decision Metrics",
        "",
        decision_markdown,
        "",
        "## Permutation Sanity Test",
        "",
        permutation_markdown,
        "",
        "## SHAP Summary",
        "",
        dataframe_to_markdown(shap_summary.head(12)) if len(shap_summary) else "SHAP summary was not generated.",
        "",
        "## Interpretation Guardrail",
        "",
        "If upgraded ML improves return but does not improve variance/ES/drawdown, describe it as an adaptive performance layer, not as strong safe-haven proof.",
        "If always-XAU/SJC/Bond baselines match or beat ML, do not claim that ML learns a robust hedge-selection signal.",
    ]
    REPORT_PATH.write_text("\n".join(lines), encoding="utf-8")


def dataframe_to_markdown(df: pd.DataFrame) -> str:
    if df.empty:
        return ""
    cols = list(df.columns)
    lines = [
        "| " + " | ".join(cols) + " |",
        "| " + " | ".join(["---"] * len(cols)) + " |",
    ]
    for _, row in df.iterrows():
        values = []
        for col in cols:
            value = row[col]
            if isinstance(value, float):
                values.append(f"{value:.6f}")
            else:
                values.append(str(value))
        lines.append("| " + " | ".join(values) + " |")
    return "\n".join(lines)


def compute_shap_summary(samples: pd.DataFrame, feature_cols: list[str], deps: dict) -> pd.DataFrame:
    if "shap" not in deps or "XGBClassifier" not in deps or "sklearn_error" in deps:
        return pd.DataFrame()

    SimpleImputer = deps["SimpleImputer"]
    XGBClassifier = deps["XGBClassifier"]
    compute_class_weight = deps["compute_class_weight"]
    shap = deps["shap"]

    x = samples[feature_cols]
    y = samples["label_id"].to_numpy(dtype=int)
    imputer = SimpleImputer(strategy="median")
    x_imp = imputer.fit_transform(x)

    classes = np.array(sorted(np.unique(y)))
    class_weights = compute_class_weight(class_weight="balanced", classes=classes, y=y)
    weight_map = dict(zip(classes, class_weights))
    sample_weight = np.array([weight_map[i] for i in y], dtype=float)

    model = XGBClassifier(
        objective="multi:softprob",
        num_class=len(base.HEDGES),
        n_estimators=70,
        max_depth=2,
        learning_rate=0.04,
        subsample=0.85,
        colsample_bytree=0.85,
        reg_lambda=3.0,
        reg_alpha=0.1,
        eval_metric="mlogloss",
        random_state=RANDOM_SEED,
    )
    model.fit(x_imp, y, sample_weight=sample_weight)
    explainer = shap.TreeExplainer(model)
    values = explainer.shap_values(x_imp)
    if isinstance(values, list):
        arr = np.stack(values, axis=-1)
    else:
        arr = np.asarray(values)
    if arr.ndim == 3:
        mean_abs = np.mean(np.abs(arr), axis=(0, 2))
    else:
        mean_abs = np.mean(np.abs(arr), axis=0)
    out = pd.DataFrame(
        {
            "model": "xgboost_multiclass",
            "feature": feature_cols,
            "mean_abs_shap": mean_abs,
            "source": "TreeExplainer on final full-sample XGBoost model",
        }
    ).sort_values("mean_abs_shap", ascending=False)
    total = out["mean_abs_shap"].sum()
    out["importance_share"] = out["mean_abs_shap"] / total if total > 0 else np.nan
    write_shap_figure(out)
    return out


def write_shap_figure(shap_summary: pd.DataFrame) -> None:
    try:
        import matplotlib

        matplotlib.use("Agg")
        import matplotlib.pyplot as plt
    except Exception:
        return

    fig_dir = ROOT / "results" / "figures" / "adaptive_ml_upgrade"
    fig_dir.mkdir(parents=True, exist_ok=True)
    top = shap_summary.head(15).iloc[::-1]
    fig, ax = plt.subplots(figsize=(8, 5.2))
    ax.barh(top["feature"], top["mean_abs_shap"], color=OKABE_ITO["blue"], edgecolor="white", linewidth=0.8)
    ax.set_xlabel("Mean absolute SHAP value")
    ax.set_title("XGBoost SHAP feature importance")
    ax.grid(axis="x", color=OKABE_ITO["light_grey"], linewidth=0.8)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    fig.tight_layout()
    fig.savefig(fig_dir / "ml_upgrade_xgboost_shap_importance.png", dpi=180)
    fig.savefig(fig_dir / "ml_upgrade_xgboost_shap_importance.svg")
    plt.close(fig)


def write_paper_table(decision_summary: pd.DataFrame, benchmark: pd.DataFrame, permutation: pd.DataFrame) -> None:
    paper_dir = ROOT / "paper" / "tables"
    paper_dir.mkdir(parents=True, exist_ok=True)
    model_rows = decision_summary.copy()
    model_rows["panel"] = "model_decision_metrics"
    strategy_rows = benchmark[
        benchmark["strategy"].isin(
            [
                "unhedged_vnindex",
                "always_sjc_gold",
                "always_xau_vnd",
                "always_bond_tri",
                "logistic_l2_balanced",
                "random_forest_balanced",
                "xgboost_multiclass",
            ]
        )
    ].copy()
    strategy_rows["panel"] = "strategy_performance"
    strategy_rows = strategy_rows[
        [
            "panel",
            "strategy",
            "total_return_pct",
            "variance_reduction_vs_unhedged",
            "es_reduction_vs_unhedged",
            "max_drawdown_pct",
            "sharpe_like",
        ]
    ]
    model_rows = model_rows.rename(columns={"model": "strategy"})
    model_rows = model_rows[
        ["panel", "strategy", "decision_windows", "accuracy", "weighted_f1", "mean_realized_he_gap"]
    ]
    perm = permutation.copy()
    perm["panel"] = "permutation_test"
    paper = pd.concat([model_rows, strategy_rows, perm], ignore_index=True, sort=False)
    paper_path = paper_dir / "table_17_ml_upgrade_validation.csv"
    paper.to_csv(paper_path, index=False, encoding="utf-8-sig")

    manifest_path = paper_dir / "table_manifest.csv"
    if manifest_path.exists():
        manifest = pd.read_csv(manifest_path)
    else:
        manifest = pd.DataFrame(columns=["table", "source", "destination", "rows"])
    manifest = manifest[manifest["table"] != "table_17_ml_upgrade_validation"]
    manifest = pd.concat(
        [
            manifest,
            pd.DataFrame(
                [
                    {
                        "table": "table_17_ml_upgrade_validation",
                        "source": "results/tables/adaptive_ml_upgrade/ml_upgrade_decision_metrics.csv;results/tables/adaptive_ml_upgrade/ml_upgrade_benchmark_comparison.csv;results/tables/adaptive_ml_upgrade/ml_upgrade_permutation_test.csv",
                        "destination": "paper/tables/table_17_ml_upgrade_validation.csv",
                        "rows": len(paper),
                    }
                ]
            ),
        ],
        ignore_index=True,
    )
    manifest.to_csv(manifest_path, index=False, encoding="utf-8")


def main() -> None:
    ensure_dirs()
    deps = optional_imports()
    panel, feature_cols, windows, samples, window_by_date = build_samples()
    decisions, returns, importance, confusion = run_walk_forward_upgrade(samples, feature_cols, window_by_date, deps)
    decision_summary = summarize_decisions(decisions, deps)
    benchmark = summarize_strategies(returns)
    tc = transaction_cost_sensitivity(returns)
    class_dist = class_distribution(samples)
    boot = bootstrap_ci(returns)
    permutation = permutation_label_test(samples, feature_cols, deps)
    shap_summary = compute_shap_summary(samples, feature_cols, deps)

    decisions.to_csv(TABLE_DIR / "ml_upgrade_decisions.csv", index=False, encoding="utf-8-sig")
    returns.to_csv(TABLE_DIR / "ml_upgrade_strategy_returns.csv", index=False, encoding="utf-8-sig")
    decision_summary.to_csv(TABLE_DIR / "ml_upgrade_decision_metrics.csv", index=False, encoding="utf-8-sig")
    benchmark.to_csv(TABLE_DIR / "ml_upgrade_benchmark_comparison.csv", index=False, encoding="utf-8-sig")
    tc.to_csv(TABLE_DIR / "ml_upgrade_transaction_cost_sensitivity.csv", index=False, encoding="utf-8-sig")
    class_dist.to_csv(TABLE_DIR / "ml_upgrade_class_distribution.csv", index=False, encoding="utf-8-sig")
    confusion.to_csv(TABLE_DIR / "ml_upgrade_confusion_matrix.csv", index=False, encoding="utf-8-sig")
    importance.to_csv(TABLE_DIR / "ml_upgrade_feature_importance_by_fold.csv", index=False, encoding="utf-8-sig")
    if len(importance):
        (
            importance.groupby(["model", "feature", "source"], as_index=False)["importance"]
            .mean()
            .sort_values(["model", "importance"], ascending=[True, False])
            .to_csv(TABLE_DIR / "ml_upgrade_feature_importance_summary.csv", index=False, encoding="utf-8-sig")
        )
    boot.to_csv(TABLE_DIR / "ml_upgrade_bootstrap_ci.csv", index=False, encoding="utf-8-sig")
    permutation.to_csv(TABLE_DIR / "ml_upgrade_permutation_test.csv", index=False, encoding="utf-8-sig")
    shap_summary.to_csv(TABLE_DIR / "ml_upgrade_xgboost_shap_summary.csv", index=False, encoding="utf-8-sig")
    write_paper_table(decision_summary, benchmark, permutation)
    write_report(deps, samples, decisions, decision_summary, benchmark, permutation, shap_summary)

    print("Phase 3.18 ML upgrade completed.")
    print(f"Models: {', '.join(sorted(decisions['model'].unique())) if len(decisions) else 'none'}")
    print(f"Decision rows: {len(decisions)}")
    print(f"Return rows: {len(returns)}")
    print(f"Report: {REPORT_PATH}")


if __name__ == "__main__":
    main()
