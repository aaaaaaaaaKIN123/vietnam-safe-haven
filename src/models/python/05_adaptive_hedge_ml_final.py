from __future__ import annotations

import argparse
import hashlib
import importlib.util
import json
import math
import os
import sys
import warnings
from datetime import datetime, timezone
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[3]
BASE_SCRIPT = ROOT / "src" / "models" / "python" / "04_adaptive_hedge_ml_upgrade.py"
TABLE_DIR = ROOT / "results" / "tables" / "adaptive_ml_final"
FIGURE_DIR = ROOT / "results" / "figures" / "adaptive_ml_final"
REPORT_PATH = ROOT / "data" / "metadata" / "phase3_20_ml_final_report.md"
PAPER_TABLE = ROOT / "paper" / "tables" / "table_19_ml_final_validation.csv"

RANDOM_SEED = 20260622
INNER_VALIDATION_WINDOWS = 18
MAX_INNER_VALIDATION_WINDOWS = 30
MIN_INNER_CLASS_COUNT = 2
CONFIDENCE_THRESHOLDS = [0.0, 0.50]
BOOTSTRAP_ROUNDS = 500
HAC_LAGS = 5
RF_N_ESTIMATORS = 60
LOGISTIC_MAX_ITER = 1000
LOGISTIC_TOL = 1e-3
THEORY_CORE_FEATURES = [
    "r_vnindex_lag1",
    "vnindex_vol_63",
    "vix_level_lag",
    "gold_wedge_mean_21",
    "r_xau_vnd_lag1",
    "r_sjc_gold_lag1",
    "r_bond_tri_lag1",
    "bond_slope_10y_1y_lag",
    "d_y_10y_21",
    "r_gold_premium_lag1",
]
MACRO_DISCRETIZATION_ALLOWLIST = {
    "bond_slope_10y_1y_lag",
    "bond_slope_10y_5y_lag",
    "bond_curvature_lag",
    "vix_level_lag",
    "interbank_level_lag",
    "policy_level_lag",
}
warnings.filterwarnings("ignore", category=FutureWarning)
warnings.filterwarnings("ignore", message="Inconsistent values: penalty=l1.*", category=UserWarning)
os.environ.setdefault("MPLCONFIGDIR", str(ROOT / ".cache" / "matplotlib"))


def load_upgrade_module():
    spec = importlib.util.spec_from_file_location("adaptive_ml_upgrade", BASE_SCRIPT)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


upgrade = load_upgrade_module()
base = upgrade.base
FALLBACKS = list(base.VALIDATION_CONTROL["fallbacks"])
RUN_DIR: Path | None = None


@dataclass(frozen=True)
class Candidate:
    name: str
    family: str
    params: dict
    feature_policy: str = "l1_sparse"


@dataclass(frozen=True)
class Policy:
    threshold: float
    fallback: str


def ensure_dirs() -> None:
    TABLE_DIR.mkdir(parents=True, exist_ok=True)
    FIGURE_DIR.mkdir(parents=True, exist_ok=True)
    REPORT_PATH.parent.mkdir(parents=True, exist_ok=True)
    PAPER_TABLE.parent.mkdir(parents=True, exist_ok=True)


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Final nested adaptive hedge-selection ML validation.")
    parser.add_argument("--main", action="store_true", help="Run the main nested walk-forward final ML assessment.")
    parser.add_argument("--preflight", action="store_true", help="Validate inputs/timing and write an isolated manifest, without fitting candidates.")
    parser.add_argument("--smoke", action="store_true", help="Exercise one eligible outer decision and all output writers; not a research result.")
    parser.add_argument("--run-id", help="New isolated run folder name; an existing run is never overwritten.")
    parser.add_argument("--include-xgboost", action="store_true", help="Include shallow XGBoost candidates when available.")
    parser.add_argument("--xgboost-audit", action="store_true", help="Write XGBoost availability/inclusion audit and exit unless --main is also set.")
    parser.add_argument("--update-paper-table", action="store_true", help="Export a review-only table_19 candidate within this new run (requires --main).")
    parser.add_argument("--target-robustness-oos", action="store_true", help="Write target-robustness diagnostics for outer OOS decision windows only.")
    parser.add_argument("--penalty-sensitivity", action="store_true", help="Run nested scoring-penalty sensitivity and write a separate summary table.")
    parser.add_argument(
        "--penalty-sensitivity-mode",
        choices=["full", "one_factor"],
        default="one_factor",
        help="Penalty sensitivity grid size. one_factor is the runtime-safe default.",
    )
    return parser.parse_args(argv)


def configure_run_paths(run_id: str | None) -> Path:
    global RUN_DIR, TABLE_DIR, FIGURE_DIR, REPORT_PATH, PAPER_TABLE
    name = run_id or datetime.now(timezone.utc).strftime("ml_v2_%Y%m%dT%H%M%S_%fZ")
    if not name or any(c not in "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-" for c in name):
        raise ValueError("run-id must contain only letters, numbers, underscores and hyphens")
    parent = (ROOT / base.VALIDATION_CONTROL["run_output_root"]).resolve()
    if ROOT.resolve() not in parent.parents:
        raise ValueError("ML output root must be inside the project")
    new_run = parent / name
    new_run.mkdir(parents=True, exist_ok=False)
    RUN_DIR = new_run
    TABLE_DIR = RUN_DIR / "tables"
    FIGURE_DIR = RUN_DIR / "figures"
    REPORT_PATH = RUN_DIR / "report.md"
    # Manuscript candidates are reviewable exports, never automatic publication.
    PAPER_TABLE = RUN_DIR / "table_19_candidate.csv"
    return RUN_DIR


def optional_imports() -> dict:
    deps = upgrade.optional_imports()
    try:
        from sklearn.base import clone
        from sklearn.calibration import CalibratedClassifierCV
        from sklearn.metrics import brier_score_loss, f1_score, precision_score, recall_score

        deps["clone"] = clone
        deps["CalibratedClassifierCV"] = CalibratedClassifierCV
        deps["brier_score_loss"] = brier_score_loss
        deps["f1_score"] = f1_score
        deps["precision_score"] = precision_score
        deps["recall_score"] = recall_score
    except Exception as exc:
        deps["sklearn_extra_error"] = repr(exc)
    return deps


def xgboost_availability_audit(deps: dict, include_xgboost: bool) -> pd.DataFrame:
    available = "XGBClassifier" in deps
    candidates = candidate_grid(deps, include_xgboost=include_xgboost) if available else candidate_grid(deps, include_xgboost=False)
    xgb_count = sum(candidate.family == "xgboost" for candidate in candidates)
    if available and include_xgboost:
        reason = "available and requested"
    elif available:
        reason = "available but not requested"
    else:
        reason = deps.get("xgboost_error", "XGBClassifier unavailable")
    return pd.DataFrame(
        [
            {
                "xgboost_available": bool(available),
                "include_xgboost_requested": bool(include_xgboost),
                "included_in_final_grid": bool(available and include_xgboost and xgb_count > 0),
                "xgboost_candidate_count": int(xgb_count),
                "reason": reason,
            }
        ]
    )


def write_xgboost_availability_audit(deps: dict, include_xgboost: bool) -> pd.DataFrame:
    audit = xgboost_availability_audit(deps, include_xgboost=include_xgboost)
    audit.to_csv(TABLE_DIR / "ml_final_xgboost_availability_audit.csv", index=False, encoding="utf-8-sig")
    return audit


class SoftmaxBaselineClassifier:
    """Small sklearn-like wrapper around the Phase 3.13 NumPy softmax baseline."""

    def __init__(self, n_classes: int, epochs: int = 500, lr: float = 0.08, l2: float = 0.002):
        self.n_classes = n_classes
        self.epochs = epochs
        self.lr = lr
        self.l2 = l2
        self.means_: np.ndarray | None = None
        self.stds_: np.ndarray | None = None
        self.weights_: np.ndarray | None = None

    def fit(self, x_train: pd.DataFrame, y_train: np.ndarray):
        self.classes_ = np.arange(self.n_classes)
        x_raw = x_train.to_numpy(dtype=float)
        self.means_ = np.nanmean(x_raw, axis=0)
        self.stds_ = np.nanstd(x_raw, axis=0)
        self.stds_ = np.where(self.stds_ < 1e-8, 1.0, self.stds_)
        x = (np.where(np.isfinite(x_raw), x_raw, self.means_) - self.means_) / self.stds_
        self.weights_ = base.fit_softmax(
            x,
            y_train.astype(int),
            self.n_classes,
            epochs=self.epochs,
            lr=self.lr,
            l2=self.l2,
        )
        return self

    def _transform(self, x: pd.DataFrame) -> np.ndarray:
        if self.means_ is None or self.stds_ is None:
            raise RuntimeError("SoftmaxBaselineClassifier must be fitted before prediction.")
        x_raw = x.to_numpy(dtype=float)
        return (np.where(np.isfinite(x_raw), x_raw, self.means_) - self.means_) / self.stds_

    def predict_proba(self, x: pd.DataFrame) -> np.ndarray:
        if self.weights_ is None:
            raise RuntimeError("SoftmaxBaselineClassifier must be fitted before prediction.")
        transformed = self._transform(x)
        return np.vstack([base.predict_softmax(row, self.weights_) for row in transformed])

    def predict(self, x: pd.DataFrame) -> np.ndarray:
        return np.argmax(self.predict_proba(x), axis=1)


def macro_discretization_features(feature_cols: list[str]) -> list[str]:
    return [col for col in feature_cols if col in MACRO_DISCRETIZATION_ALLOWLIST]


def bound_feature_selection(
    ranked_features: list[str],
    theory_core_features: list[str],
    available_features: list[str],
    min_features: int = 3,
    max_features: int = 10,
    selector_type: str = "l1_logistic",
) -> tuple[list[str], dict]:
    available = list(dict.fromkeys(available_features))
    available_set = set(available)
    ranked = [feature for feature in ranked_features if feature in available_set]
    fallback = False

    if len(ranked) < min_features:
        fallback = True
        selected = [feature for feature in theory_core_features if feature in available_set]
        if len(selected) < min_features:
            selected = available[: max(min_features, min(max_features, len(available)))]
    else:
        selected = ranked[:max_features]

    audit = {
        "selector_type": selector_type,
        "fallback_triggered": fallback,
        "selected_feature_count": len(selected),
        "selected_features": ";".join(selected),
    }
    return selected, audit


def rank_features_with_l1(train_part: pd.DataFrame, feature_cols: list[str], deps: dict) -> list[str]:
    if "sklearn_error" in deps or train_part["label_id"].nunique() < 2:
        return []
    try:
        Pipeline = deps["Pipeline"]
        SimpleImputer = deps["SimpleImputer"]
        StandardScaler = deps["StandardScaler"]
        LogisticRegression = deps["LogisticRegression"]
        model = Pipeline(
            [
                ("imputer", SimpleImputer(strategy="median")),
                ("scaler", StandardScaler()),
                (
                    "model",
                    deps["OneVsRestClassifier"](LogisticRegression(
                        C=0.25,
                        penalty="l1",
                        solver="liblinear",
                        class_weight="balanced",
                        max_iter=LOGISTIC_MAX_ITER,
                        tol=LOGISTIC_TOL,
                        random_state=RANDOM_SEED,
                    )),
                ),
            ]
        )
        model.fit(train_part[feature_cols], train_part["label_id"].to_numpy(dtype=int))
        coef = np.vstack([est.coef_ for est in model.named_steps["model"].estimators_])
        importance = np.mean(np.abs(coef), axis=0)
        ranked = [
            feature
            for feature, value in sorted(
                zip(feature_cols, importance),
                key=lambda item: item[1],
                reverse=True,
            )
            if value > 1e-10
        ]
        return ranked
    except Exception as exc:
        raise RuntimeError("L1 feature selector failed; refusing silent theory fallback") from exc


def select_candidate_features(
    candidate: Candidate,
    train_part: pd.DataFrame,
    feature_cols: list[str],
    deps: dict,
) -> tuple[list[str], dict]:
    if candidate.feature_policy == "full":
        selected = list(feature_cols)
        return selected, {
            "selector_type": "full",
            "fallback_triggered": False,
            "selected_feature_count": len(selected),
            "selected_features": ";".join(selected),
        }
    if candidate.feature_policy == "theory_core":
        return bound_feature_selection(
            ranked_features=THEORY_CORE_FEATURES,
            theory_core_features=THEORY_CORE_FEATURES,
            available_features=feature_cols,
            min_features=3,
            max_features=10,
            selector_type="theory_core",
        )
    ranked = rank_features_with_l1(train_part, feature_cols, deps)
    return bound_feature_selection(
        ranked_features=ranked,
        theory_core_features=THEORY_CORE_FEATURES,
        available_features=feature_cols,
        min_features=3,
        max_features=10,
        selector_type="l1_logistic_sparse",
    )


def time_respecting_validation_split(
    outer_train: pd.DataFrame,
    label_col: str,
    labels: list[str],
    initial_windows: int = INNER_VALIDATION_WINDOWS,
    max_windows: int = MAX_INNER_VALIDATION_WINDOWS,
    min_class_count: int = MIN_INNER_CLASS_COUNT,
) -> tuple[pd.DataFrame, pd.DataFrame, dict]:
    max_allowed = max(1, len(outer_train) - 2)
    validation_size = min(initial_windows, max_allowed)
    max_validation_size = min(max_windows, max_allowed)

    while validation_size < max_validation_size:
        validation_part = outer_train.iloc[-validation_size:]
        counts = validation_part[label_col].value_counts().to_dict()
        if all(counts.get(label, 0) >= min_class_count for label in labels):
            break
        validation_size += 1

    validation_part = outer_train.iloc[-validation_size:].copy()
    train_part = outer_train.iloc[:-validation_size].copy()
    if not {"decision_date", "window_end"}.issubset(outer_train.columns):
        raise ValueError("Validation requires decision_date and window_end for label maturity")
    before_maturity = len(train_part)
    train_part = base.mature_training_samples(train_part, validation_part["decision_date"].min())
    counts = validation_part[label_col].value_counts().to_dict()
    min_count = min(counts.get(label, 0) for label in labels)
    coverage_status = "ok" if min_count >= min_class_count else "warning_sparse_class"
    audit = {
        "validation_size": int(validation_size),
        "min_class_count": int(min_count),
        "coverage_status": coverage_status,
        "excluded_unmatured_train_labels": before_maturity - len(train_part),
    }
    for label in labels:
        audit[f"count_{label}"] = int(counts.get(label, 0))
    return train_part, validation_part, audit


def candidate_grid(deps: dict, include_xgboost: bool = False) -> list[Candidate]:
    candidates = [
        Candidate("softmax_l2_0002_theory", "softmax", {"l2": 0.002}, "theory_core"),
        Candidate("softmax_l2_001_theory", "softmax", {"l2": 0.01}, "theory_core"),
    ]
    for c_value, label in [(0.1, "C0p1"), (0.5, "C0p5"), (1.0, "C1p0")]:
        candidates.append(
            Candidate(
                f"logistic_l2_{label}_theory",
                "logistic",
                {"C": c_value, "penalty": "l2"},
                "theory_core",
            )
        )
    for c_value, label in [(0.1, "C0p1"), (0.5, "C0p5")]:
        candidates.append(
            Candidate(
                f"logistic_l1_{label}_l1fs",
                "logistic",
                {"C": c_value, "penalty": "l1"},
                "l1_sparse",
            )
        )
    for max_depth in [1, 2, 3]:
        for min_samples_leaf in [8, 12]:
            candidates.append(
                Candidate(
                    f"rf_depth{max_depth}_leaf{min_samples_leaf}_theory",
                    "random_forest",
                    {"max_depth": max_depth, "min_samples_leaf": min_samples_leaf},
                    "theory_core",
                )
            )
    if include_xgboost and "XGBClassifier" in deps:
        for max_depth in [1, 2]:
            for learning_rate, lr_label in [(0.03, "lr003"), (0.06, "lr006")]:
                for n_estimators in [40, 60]:
                    for reg_lambda, lambda_label in [(3.0, "l3"), (6.0, "l6")]:
                        candidates.append(
                            Candidate(
                                f"xgb_d{max_depth}_{lr_label}_n{n_estimators}_{lambda_label}_theory",
                                "xgboost",
                                {
                                    "max_depth": max_depth,
                                    "learning_rate": learning_rate,
                                    "n_estimators": n_estimators,
                                    "reg_lambda": reg_lambda,
                                },
                                "theory_core",
                            )
                        )
    return candidates


def make_estimator(candidate: Candidate, deps: dict):
    if candidate.family == "softmax":
        return SoftmaxBaselineClassifier(
            n_classes=len(base.HEDGES),
            l2=candidate.params.get("l2", 0.002),
        )

    if "sklearn_error" in deps:
        raise RuntimeError(f"scikit-learn is required: {deps['sklearn_error']}")

    Pipeline = deps["Pipeline"]
    SimpleImputer = deps["SimpleImputer"]
    StandardScaler = deps["StandardScaler"]
    LogisticRegression = deps["LogisticRegression"]
    RandomForestClassifier = deps["RandomForestClassifier"]

    if candidate.family == "logistic":
        penalty = candidate.params.get("penalty", "l2")
        solver = "liblinear" if penalty == "l1" else "saga" if penalty == "elasticnet" else "lbfgs"
        logistic_kwargs = {
            "C": candidate.params["C"],
            "penalty": penalty,
            "solver": solver,
            "class_weight": "balanced",
            "max_iter": LOGISTIC_MAX_ITER,
            "tol": LOGISTIC_TOL,
            "random_state": RANDOM_SEED,
        }
        if penalty == "elasticnet":
            logistic_kwargs["l1_ratio"] = candidate.params.get("l1_ratio", 0.5)
        classifier = LogisticRegression(**logistic_kwargs)
        if penalty == "l1":
            classifier = deps["OneVsRestClassifier"](classifier)
        return Pipeline(
            [
                ("imputer", SimpleImputer(strategy="median")),
                ("scaler", StandardScaler()),
                ("model", classifier),
            ]
        )
    if candidate.family == "random_forest":
        return Pipeline(
            [
                ("imputer", SimpleImputer(strategy="median")),
                (
                    "model",
                    RandomForestClassifier(
                        n_estimators=RF_N_ESTIMATORS,
                        max_depth=candidate.params["max_depth"],
                        min_samples_leaf=candidate.params["min_samples_leaf"],
                        class_weight="balanced_subsample",
                        random_state=RANDOM_SEED,
                        n_jobs=-1,
                    ),
                ),
            ]
        )
    if candidate.family == "xgboost":
        XGBClassifier = deps["XGBClassifier"]
        return Pipeline(
            [
                ("imputer", SimpleImputer(strategy="median")),
                (
                    "model",
                    XGBClassifier(
                        objective="multi:softprob",
                        num_class=len(base.HEDGES),
                        n_estimators=candidate.params["n_estimators"],
                        max_depth=candidate.params["max_depth"],
                        learning_rate=candidate.params["learning_rate"],
                        subsample=0.85,
                        colsample_bytree=0.85,
                        reg_lambda=candidate.params.get("reg_lambda", 4.0),
                        reg_alpha=0.1,
                        eval_metric="mlogloss",
                        random_state=RANDOM_SEED,
                        n_jobs=1,
                        verbosity=0,
                    ),
                ),
            ]
        )
    raise ValueError(f"Unsupported candidate family: {candidate.family}")


def fit_estimator(candidate: Candidate, estimator, x_train: pd.DataFrame, y_train: np.ndarray, deps: dict):
    if candidate.family == "softmax":
        estimator.fit(x_train, y_train)
    elif candidate.family == "xgboost":
        sample_weight = upgrade.class_weight_sample_weight(y_train, deps)
        estimator.fit(x_train, y_train, model__sample_weight=sample_weight)
    else:
        estimator.fit(x_train, y_train)
    return estimator


def classifier_prediction(estimator, x_row: pd.DataFrame, idx_to_class: dict[int, str]) -> tuple[str, np.ndarray]:
    raw = np.asarray(estimator.predict_proba(x_row)[0], dtype=float)
    classes = np.asarray(estimator.classes_, dtype=int)
    if len(raw) != len(classes) or not set(classes).issubset(idx_to_class):
        raise ValueError("Probability columns do not match class IDs")
    probs = np.zeros(len(idx_to_class), dtype=float)
    probs[classes] = raw
    if not np.isfinite(probs).all() or (probs < 0).any() or not np.isclose(probs.sum(), 1.0):
        raise ValueError("Invalid classifier probabilities")
    return idx_to_class[int(np.argmax(probs))], probs


def predict_with_policy(estimator, x_row: pd.DataFrame, sample_row: pd.Series, policy: Policy, idx_to_class: dict[int, str]) -> tuple[str, float, str]:
    if policy.fallback not in FALLBACKS:
        raise ValueError("Fallback is not allowed by the point-in-time protocol")
    pred_hedge, probs = classifier_prediction(estimator, x_row, idx_to_class)
    max_prob = float(np.nanmax(probs))
    fallback_used = "none"

    if policy.fallback != "model" and max_prob < policy.threshold:
        fallback_used = policy.fallback
        if policy.fallback == "static_best":
            pred_hedge = sample_row.get("static_best_hedge", pred_hedge)
        elif policy.fallback == "always_xau_vnd":
            pred_hedge = "r_xau_vnd"
    return str(pred_hedge), max_prob, fallback_used


def selected_he(sample_row: pd.Series, hedge: str) -> float:
    col = f"actual_he_{hedge}"
    value = sample_row.get(col, np.nan)
    return float(value) if pd.notna(value) else np.nan


def policy_score(
    validation_rows: list[dict],
    switch_penalty: float = 0.0025,
    gap_penalty: float = 0.05,
) -> dict:
    df = pd.DataFrame(validation_rows)
    if df.empty:
        return {
            "validation_score": -np.inf,
            "validation_accuracy": np.nan,
            "validation_mean_he": np.nan,
            "validation_mean_gap": np.nan,
            "validation_switch_rate": np.nan,
        }
    switch_rate = float(df["selected_hedge"].ne(df["selected_hedge"].shift(1)).mean())
    mean_he = float(df["selected_realized_he"].mean())
    mean_gap = float(df["realized_he_gap"].mean())
    accuracy = float(df["selected_is_actual_best"].mean())
    # Main selection criterion: high realized HE, small complexity/turnover penalty.
    score = mean_he - switch_penalty * switch_rate - gap_penalty * max(mean_gap, 0.0)
    return {
        "validation_score": float(score),
        "validation_accuracy": accuracy,
        "validation_mean_he": mean_he,
        "validation_mean_gap": mean_gap,
        "validation_switch_rate": switch_rate,
    }


def evaluate_candidate_policy(
    candidate: Candidate,
    policy: Policy,
    train_part: pd.DataFrame,
    validation_part: pd.DataFrame,
    feature_cols: list[str],
    deps: dict,
    idx_to_class: dict[int, str],
    switch_penalty: float = 0.0025,
    gap_penalty: float = 0.05,
) -> tuple[dict, object | None]:
    if train_part["label_id"].nunique() < 2:
        return {"validation_score": -np.inf}, None
    try:
        estimator = make_estimator(candidate, deps)
        estimator = fit_estimator(candidate, estimator, train_part[feature_cols], train_part["label_id"].to_numpy(dtype=int), deps)
    except Exception as exc:
        return {"validation_score": -np.inf, "error": repr(exc)}, None

    rows = []
    for _, row in validation_part.iterrows():
        x_row = pd.DataFrame([row[feature_cols].to_dict()])
        hedge, max_prob, fallback_used = predict_with_policy(estimator, x_row, row, policy, idx_to_class)
        actual_best = row["actual_best_hedge"]
        actual_best_he = selected_he(row, actual_best)
        pred_he = selected_he(row, hedge)
        rows.append(
            {
                "selected_hedge": hedge,
                "actual_best_hedge": actual_best,
                "selected_is_actual_best": hedge == actual_best,
                "selected_realized_he": pred_he,
                "actual_best_realized_he": actual_best_he,
                "realized_he_gap": actual_best_he - pred_he,
                "max_probability": max_prob,
                "fallback_used": fallback_used,
            }
        )
    out = policy_score(rows, switch_penalty=switch_penalty, gap_penalty=gap_penalty)
    return out, estimator


def select_best_candidate(
    outer_train: pd.DataFrame,
    feature_cols: list[str],
    deps: dict,
    idx_to_class: dict[int, str],
    include_xgboost: bool = False,
    switch_penalty: float = 0.0025,
    gap_penalty: float = 0.05,
) -> tuple[Candidate, Policy, dict]:
    train_part, validation_part, validation_audit = time_respecting_validation_split(
        outer_train,
        label_col="actual_best_hedge",
        labels=base.HEDGES,
        initial_windows=INNER_VALIDATION_WINDOWS,
        max_windows=MAX_INNER_VALIDATION_WINDOWS,
        min_class_count=MIN_INNER_CLASS_COUNT,
    )
    candidates = candidate_grid(deps, include_xgboost=include_xgboost)
    policies = [Policy(threshold=t, fallback=f) for t in CONFIDENCE_THRESHOLDS for f in FALLBACKS if not (t == 0.0 and f != "model")]

    rows = []
    best_tuple = None
    for candidate in candidates:
        selected_features, feature_audit = select_candidate_features(candidate, train_part, feature_cols, deps)
        fitted = None
        fit_error = None
        if train_part["label_id"].nunique() < 2:
            fit_error = "inner training contains fewer than two classes"
        else:
            try:
                estimator = make_estimator(candidate, deps)
                fitted = fit_estimator(
                    candidate,
                    estimator,
                    train_part[selected_features],
                    train_part["label_id"].to_numpy(dtype=int),
                    deps,
                )
            except Exception as exc:
                fit_error = repr(exc)
        if fit_error is not None:
            raise RuntimeError(f"Candidate {candidate.name} failed in inner validation: {fit_error}")
        for policy in policies:
            if fitted is None:
                metrics = {"validation_score": -np.inf, "error": fit_error}
            else:
                validation_rows = []
                for _, row_val in validation_part.iterrows():
                    x_row = pd.DataFrame([row_val[selected_features].to_dict()])
                    hedge, max_prob, fallback_used = predict_with_policy(fitted, x_row, row_val, policy, idx_to_class)
                    actual_best = row_val["actual_best_hedge"]
                    actual_best_he = selected_he(row_val, actual_best)
                    pred_he = selected_he(row_val, hedge)
                    validation_rows.append(
                        {
                            "selected_hedge": hedge,
                            "actual_best_hedge": actual_best,
                            "selected_is_actual_best": hedge == actual_best,
                            "selected_realized_he": pred_he,
                            "actual_best_realized_he": actual_best_he,
                            "realized_he_gap": actual_best_he - pred_he,
                            "max_probability": max_prob,
                            "fallback_used": fallback_used,
                        }
                    )
                metrics = policy_score(
                    validation_rows,
                    switch_penalty=switch_penalty,
                    gap_penalty=gap_penalty,
                )
            row = {
                "candidate": candidate.name,
                "family": candidate.family,
                "params": repr(candidate.params),
                "feature_policy": candidate.feature_policy,
                **feature_audit,
                "confidence_threshold": policy.threshold,
                "fallback": policy.fallback,
                **validation_audit,
                **metrics,
            }
            rows.append(row)
            if best_tuple is None or row.get("validation_score", -np.inf) > best_tuple[2].get("validation_score", -np.inf):
                best_tuple = (candidate, policy, row, selected_features)
    if best_tuple is None or not np.isfinite(best_tuple[2].get("validation_score", -np.inf)):
        raise RuntimeError("No valid ML candidate found in nested validation.")
    return best_tuple[0], best_tuple[1], {
        "grid": rows,
        "best": best_tuple[2],
        "selected_features": best_tuple[3],
        "validation_audit": validation_audit,
    }


def run_nested_walk_forward(
    samples: pd.DataFrame,
    feature_cols: list[str],
    window_by_date: dict,
    deps: dict,
    include_xgboost: bool = False,
    switch_penalty: float = 0.0025,
    gap_penalty: float = 0.05,
):
    _, idx_to_class = upgrade.class_maps()
    decision_rows = []
    return_rows = []
    grid_rows = []
    class_audit_rows = []
    feature_audit_rows = []
    score_component_rows = []

    base_strategies = {
        "unhedged_vnindex": "unhedged",
        "always_sjc_gold": "r_sjc_gold",
        "always_xau_vnd": "r_xau_vnd",
        "always_bond_tri": "r_bond_tri",
        "static_best_trailing": "static_best_trailing",
        "equal_safe_haven_basket": "equal_basket",
    }

    total_outer = max(0, len(samples) - base.MIN_TRAIN_SAMPLES)
    for outer_number, test_idx in enumerate(range(base.MIN_TRAIN_SAMPLES, len(samples)), start=1):
        if outer_number == 1 or outer_number % 10 == 0 or outer_number == total_outer:
            print(f"Nested ML outer window {outer_number}/{total_outer}", flush=True)
        test = samples.iloc[test_idx].copy()
        outer_train = base.mature_training_samples(samples.iloc[:test_idx], test["decision_date"])
        if len(outer_train) < base.MIN_TRAIN_SAMPLES:
            continue
        candidate, policy, selection = select_best_candidate(
            outer_train,
            feature_cols,
            deps,
            idx_to_class,
            include_xgboost=include_xgboost,
            switch_penalty=switch_penalty,
            gap_penalty=gap_penalty,
        )
        for row in selection["grid"]:
            row["outer_test_index"] = test_idx
            row["decision_date"] = test["decision_date"]
            row["selected_by_nested_validation"] = (
                row["candidate"] == selection["best"]["candidate"]
                and row["feature_policy"] == selection["best"]["feature_policy"]
                and row["confidence_threshold"] == selection["best"]["confidence_threshold"]
                and row["fallback"] == selection["best"]["fallback"]
            )
            grid_rows.append(row)
            feature_audit_rows.append(
                {
                    "outer_test_index": test_idx,
                    "decision_date": test["decision_date"],
                    "candidate": row["candidate"],
                    "family": row["family"],
                    "feature_policy": row["feature_policy"],
                    "selector_type": row["selector_type"],
                    "selected_feature_count": row["selected_feature_count"],
                    "selected_features": row["selected_features"],
                    "fallback_triggered": row["fallback_triggered"],
                    "selected_by_nested_validation": row["selected_by_nested_validation"],
                }
            )
            score_component_rows.append(
                {
                    "outer_test_index": test_idx,
                    "decision_date": test["decision_date"],
                    "candidate": row["candidate"],
                    "feature_policy": row["feature_policy"],
                    "confidence_threshold": row["confidence_threshold"],
                    "fallback": row["fallback"],
                    "validation_mean_he": row.get("validation_mean_he", np.nan),
                    "validation_switch_rate": row.get("validation_switch_rate", np.nan),
                    "validation_mean_gap": row.get("validation_mean_gap", np.nan),
                    "validation_score": row.get("validation_score", np.nan),
                    "selected_by_inner_validation": row["selected_by_nested_validation"],
                }
            )

        class_audit = dict(selection["validation_audit"])
        class_audit["outer_test_index"] = test_idx
        class_audit["decision_date"] = test["decision_date"]
        class_audit_rows.append(class_audit)

        final_features, final_feature_audit = select_candidate_features(candidate, outer_train, feature_cols, deps)

        estimator = make_estimator(candidate, deps)
        estimator = fit_estimator(candidate, estimator, outer_train[final_features], outer_train["label_id"].to_numpy(dtype=int), deps)
        x_test = pd.DataFrame([test[final_features].to_dict()])
        raw_hedge, raw_probs = classifier_prediction(estimator, x_test, idx_to_class)
        pred_hedge, max_prob, fallback_used = predict_with_policy(estimator, x_test, test, policy, idx_to_class)
        window = window_by_date[pd.Timestamp(test["decision_date"])]
        actual_best = window.actual_best_hedge
        pred_he = window.actual_he[pred_hedge]
        actual_best_he = window.actual_he[actual_best]

        decision_rows.append(
            {
                "strategy": "ml_final_nested",
                "decision_date": test["decision_date"],
                "window_end": test["window_end"],
                "selected_hedge": pred_hedge,
                "actual_best_hedge": actual_best,
                "selected_is_actual_best": pred_hedge == actual_best,
                "raw_model_hedge": raw_hedge,
                "raw_model_is_actual_best": raw_hedge == actual_best,
                "raw_model_realized_he": window.actual_he[raw_hedge],
                "raw_model_he_gap": actual_best_he - window.actual_he[raw_hedge],
                **{f"raw_prob_{idx_to_class[i]}": float(prob) for i, prob in enumerate(raw_probs)},
                **{f"prior_prob_{idx_to_class[i]}": float((outer_train['label_id'] == i).mean()) for i in idx_to_class},
                "outer_train_samples": len(outer_train),
                "last_train_label_end": outer_train["window_end"].max(),
                "selected_realized_he": pred_he,
                "actual_best_realized_he": actual_best_he,
                "realized_he_gap": actual_best_he - pred_he,
                "max_probability": max_prob,
                "fallback_used": fallback_used,
                "chosen_candidate": candidate.name,
                "chosen_family": candidate.family,
                "chosen_params": repr(candidate.params),
                "feature_policy": candidate.feature_policy,
                "selected_feature_count": final_feature_audit["selected_feature_count"],
                "selected_features": final_feature_audit["selected_features"],
                "feature_fallback_triggered": final_feature_audit["fallback_triggered"],
                "confidence_threshold": policy.threshold,
                "fallback_policy": policy.fallback,
                "inner_validation_score": selection["best"].get("validation_score", np.nan),
                "inner_validation_accuracy": selection["best"].get("validation_accuracy", np.nan),
                "inner_validation_mean_he": selection["best"].get("validation_mean_he", np.nan),
                "inner_validation_mean_gap": selection["best"].get("validation_mean_gap", np.nan),
                "inner_validation_switch_rate": selection["best"].get("validation_switch_rate", np.nan),
                "inner_validation_size": selection["best"].get("validation_size", np.nan),
                "inner_validation_coverage_status": selection["best"].get("coverage_status", ""),
            }
        )

        for strategy, key in base_strategies.items():
            selected_hedge = {
                "unhedged_vnindex": "none",
                "equal_safe_haven_basket": "equal_basket",
                "static_best_trailing": window.static_best_hedge,
                "always_sjc_gold": "r_sjc_gold",
                "always_xau_vnd": "r_xau_vnd",
                "always_bond_tri": "r_bond_tri",
            }[strategy]
            append_future_returns(return_rows, window, strategy, selected_hedge, window.future_returns[key])
        append_future_returns(return_rows, window, "ml_final_nested", pred_hedge, window.future_returns[pred_hedge])
        append_future_returns(return_rows, window, "ml_raw_selected", raw_hedge, window.future_returns[raw_hedge])

    return (
        pd.DataFrame(decision_rows),
        pd.DataFrame(return_rows),
        pd.DataFrame(grid_rows),
        pd.DataFrame(class_audit_rows),
        pd.DataFrame(feature_audit_rows),
        pd.DataFrame(score_component_rows),
    )


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


def decision_metrics(decisions: pd.DataFrame) -> pd.DataFrame:
    if decisions.empty:
        return pd.DataFrame()
    rows = []
    for strategy, sub in decisions.groupby("strategy"):
        scores = classification_scores(sub["actual_best_hedge"], sub["selected_hedge"])
        rows.append(
            {
                "strategy": strategy,
                "decision_windows": int(len(sub)),
                "accuracy": float(sub["selected_is_actual_best"].mean()),
                "weighted_f1": scores["weighted_f1"],
                "macro_precision": scores["macro_precision"],
                "macro_recall": scores["macro_recall"],
                "mean_realized_he": float(sub["selected_realized_he"].mean()),
                "mean_realized_he_gap": float(sub["realized_he_gap"].mean()),
                "mean_max_probability": float(sub["max_probability"].mean()),
                "fallback_share": float((sub["fallback_used"] != "none").mean()),
                "switch_rate": float(sub["selected_hedge"].ne(sub["selected_hedge"].shift(1)).mean()),
            }
        )
    return pd.DataFrame(rows)


def classification_scores(y_true: pd.Series, y_pred: pd.Series) -> dict:
    labels = sorted(set(base.HEDGES) | set(y_true.dropna()) | set(y_pred.dropna()))
    total = len(y_true)
    weighted_f1 = 0.0
    precision_values = []
    recall_values = []
    for label in labels:
        true_is_label = y_true == label
        pred_is_label = y_pred == label
        tp = int((true_is_label & pred_is_label).sum())
        fp = int((~true_is_label & pred_is_label).sum())
        fn = int((true_is_label & ~pred_is_label).sum())
        support = int(true_is_label.sum())
        precision = tp / (tp + fp) if (tp + fp) else 0.0
        recall = tp / (tp + fn) if (tp + fn) else 0.0
        f1 = 2.0 * precision * recall / (precision + recall) if (precision + recall) else 0.0
        weighted_f1 += f1 * support
        precision_values.append(precision)
        recall_values.append(recall)
    return {
        "weighted_f1": float(weighted_f1 / total) if total else np.nan,
        "macro_precision": float(np.mean(precision_values)) if precision_values else np.nan,
        "macro_recall": float(np.mean(recall_values)) if recall_values else np.nan,
    }


def decision_confusion_matrix(decisions: pd.DataFrame) -> pd.DataFrame:
    if decisions.empty:
        return pd.DataFrame()
    labels = list(base.HEDGES)
    rows = []
    for strategy, sub in decisions.groupby("strategy"):
        for actual in labels:
            for predicted in labels:
                rows.append(
                    {
                        "strategy": strategy,
                        "actual_hedge": actual,
                        "predicted_hedge": predicted,
                        "count": int(((sub["actual_best_hedge"] == actual) & (sub["selected_hedge"] == predicted)).sum()),
                    }
                )
    return pd.DataFrame(rows)


def decision_prediction_distribution(decisions: pd.DataFrame) -> pd.DataFrame:
    if decisions.empty:
        return pd.DataFrame()
    rows = []
    for strategy, sub in decisions.groupby("strategy"):
        total = len(sub)
        for hedge in base.HEDGES:
            pred_count = int((sub["selected_hedge"] == hedge).sum())
            actual_count = int((sub["actual_best_hedge"] == hedge).sum())
            rows.append(
                {
                    "strategy": strategy,
                    "hedge": hedge,
                    "predicted_count": pred_count,
                    "predicted_share": float(pred_count / total) if total else np.nan,
                    "actual_count": actual_count,
                    "actual_share": float(actual_count / total) if total else np.nan,
                }
            )
    return pd.DataFrame(rows)


def confidence_calibration(decisions: pd.DataFrame) -> pd.DataFrame:
    if decisions.empty:
        return pd.DataFrame()
    bins = [0.0, 0.40, 0.50, 0.60, 0.75, 1.0]
    labels = ["<=0.40", "0.40-0.50", "0.50-0.60", "0.60-0.75", ">0.75"]
    df = decisions.copy()
    if "raw_model_is_actual_best" not in df:
        raise ValueError("Classifier calibration requires raw correctness before fallback")
    df["confidence_bin"] = pd.cut(df["max_probability"], bins=bins, labels=labels, include_lowest=True)
    return (
        df.groupby("confidence_bin", observed=False)
        .agg(
            windows=("decision_date", "count"),
            hit_rate=("raw_model_is_actual_best", "mean"),
            policy_hit_rate=("selected_is_actual_best", "mean"),
            mean_probability=("max_probability", "mean"),
        )
        .reset_index()
    )


def probability_scores(decisions: pd.DataFrame) -> pd.DataFrame:
    if decisions.empty:
        return pd.DataFrame()
    class_ids, _ = upgrade.class_maps()
    y = decisions["actual_best_hedge"].map(class_ids).to_numpy(dtype=int)
    onehot = np.eye(len(base.HEDGES))[y]
    rows = []
    for name, prefix in [("raw_selected_classifier", "raw_prob_"), ("past_class_prior", "prior_prob_")]:
        probs = decisions[[prefix + h for h in base.HEDGES]].to_numpy(dtype=float)
        rows.append({
            "predictor": name, "decision_windows": len(y),
            "log_loss": float(-np.log(np.clip(probs[np.arange(len(y)), y], 1e-15, 1.0)).mean()),
            "multiclass_brier_sum": float(np.square(probs - onehot).sum(axis=1).mean()),
            "accuracy": float((probs.argmax(axis=1) == y).mean()),
        })
    return pd.DataFrame(rows)


def hac_t_stat(x: np.ndarray, lags: int = HAC_LAGS) -> tuple[float, float, float]:
    x = np.asarray(x, dtype=float)
    x = x[np.isfinite(x)]
    n = len(x)
    if n < 10:
        return np.nan, np.nan, np.nan
    mean = float(np.mean(x))
    centered = x - mean
    gamma0 = float(np.dot(centered, centered) / n)
    var = gamma0
    max_lag = min(lags, n - 2)
    for lag in range(1, max_lag + 1):
        gamma = float(np.dot(centered[lag:], centered[:-lag]) / n)
        weight = 1.0 - lag / (max_lag + 1.0)
        var += 2.0 * weight * gamma
    se = math.sqrt(max(var, 1e-14) / n)
    t_stat = mean / se if se > 0 else np.nan
    p_one_sided_positive = 0.5 * math.erfc(t_stat / math.sqrt(2.0)) if np.isfinite(t_stat) else np.nan
    return mean, t_stat, p_one_sided_positive


def paired_hac_tests(returns_df: pd.DataFrame, strategy: str = "ml_final_nested") -> pd.DataFrame:
    wide = returns_df.pivot_table(index=["decision_date", "date"], columns="strategy", values="return_pct", aggfunc="first")
    if strategy not in wide.columns:
        return pd.DataFrame()
    rows = []
    for benchmark in wide.columns:
        if benchmark == strategy:
            continue
        pair = wide[[strategy, benchmark]].dropna()
        if pair.empty:
            continue
        return_diff = pair[strategy].to_numpy() - pair[benchmark].to_numpy()
        risk_loss_diff = pair[benchmark].to_numpy() ** 2 - pair[strategy].to_numpy() ** 2
        mean_ret, t_ret, p_ret = hac_t_stat(return_diff)
        mean_risk, t_risk, p_risk = hac_t_stat(risk_loss_diff)
        rows.append(
            {
                "strategy": strategy,
                "benchmark": benchmark,
                "observations": len(pair),
                "mean_return_diff_pct": mean_ret,
                "hac_t_return_diff": t_ret,
                "p_value_return_superiority": p_ret,
                "mean_squared_loss_reduction": mean_risk,
                "hac_t_squared_loss_reduction": t_risk,
                "p_value_squared_loss_reduction": p_risk,
            }
        )
    return pd.DataFrame(rows)


def block_bootstrap_tests(returns_df: pd.DataFrame, strategy: str = "ml_final_nested") -> pd.DataFrame:
    rng = np.random.default_rng(RANDOM_SEED)
    window_returns = (
        returns_df.groupby(["strategy", "decision_date"], as_index=False)["return_pct"]
        .sum()
        .pivot(index="decision_date", columns="strategy", values="return_pct")
    )
    if strategy not in window_returns.columns:
        return pd.DataFrame()
    rows = []
    for benchmark in window_returns.columns:
        if benchmark == strategy:
            continue
        pair = window_returns[[strategy, benchmark]].dropna()
        if pair.empty:
            continue
        diff = pair[strategy].to_numpy() - pair[benchmark].to_numpy()
        observed = float(np.mean(diff))
        # Circular moving blocks of consecutive decision windows. L=1 is
        # explicitly an iid-window sensitivity, not a dependence correction.
        for block_length in base.VALIDATION_CONTROL["bootstrap_block_windows"]:
            if block_length > len(diff):
                continue
            boot = np.asarray([
                diff[circular_block_indices(len(diff), block_length, rng)].mean()
                for _ in range(BOOTSTRAP_ROUNDS)
            ])
            rows.append({
                "strategy": strategy,
                "benchmark": benchmark,
                "decision_windows": len(diff),
                "mean_window_return_diff_pct": observed,
                "statistic": "mean_difference_of_summed_daily_pct_returns",
                "resampling_method": "iid_windows" if block_length == 1 else "circular_moving_block_windows",
                "block_length_windows": block_length,
                "ci_low_2p5": float(np.quantile(boot, 0.025)),
                "ci_high_97p5": float(np.quantile(boot, 0.975)),
                "p_value_centered_mean_test": float((np.sum(boot - observed >= observed) + 1) / (len(boot) + 1)),
                "bootstrap_rounds": BOOTSTRAP_ROUNDS,
                "inference_scope": "single_pair_conditional_dependence_sensitivity_not_RC_or_SPA",
            })
    return pd.DataFrame(rows)


def circular_block_indices(n: int, block_length: int, rng) -> np.ndarray:
    if not 1 <= block_length <= n:
        raise ValueError("Block length must lie between 1 and sample size")
    starts = rng.integers(0, n, size=math.ceil(n / block_length))
    return ((starts[:, None] + np.arange(block_length)) % n).ravel()[:n]


def penalty_sensitivity_grid(mode: str = "one_factor") -> list[tuple[float, float, str]]:
    switch_values = [0.001, 0.0025, 0.005]
    gap_values = [0.025, 0.05, 0.10]
    if mode == "full":
        return [(switch, gap, "full_grid") for switch in switch_values for gap in gap_values]
    combos = []
    for switch in switch_values:
        combos.append((switch, 0.05, "one_factor_switch"))
    for gap in gap_values:
        if gap != 0.05:
            combos.append((0.0025, gap, "one_factor_gap"))
    return combos


def run_penalty_sensitivity(mode: str, include_xgboost: bool, deps: dict) -> pd.DataFrame:
    _, feature_cols, _, samples, window_by_date = upgrade.build_samples()
    rows = []
    for switch_penalty, gap_penalty, note in penalty_sensitivity_grid(mode):
        print(
            f"Penalty sensitivity run: switch_penalty={switch_penalty}, gap_penalty={gap_penalty}, include_xgboost={include_xgboost}",
            flush=True,
        )
        decisions, returns_df, _, _, _, _ = run_nested_walk_forward(
            samples,
            feature_cols,
            window_by_date,
            deps,
            include_xgboost=include_xgboost,
            switch_penalty=switch_penalty,
            gap_penalty=gap_penalty,
        )
        decision_summary = decision_metrics(decisions)
        benchmark = upgrade.summarize_strategies(returns_df)
        ml_decision = decision_summary[decision_summary["strategy"] == "ml_final_nested"].iloc[0]
        ml_benchmark = benchmark[benchmark["strategy"] == "ml_final_nested"].iloc[0]
        family_distribution = decisions["chosen_family"].value_counts().sort_index().to_dict()
        rows.append(
            {
                "switch_penalty": switch_penalty,
                "gap_penalty": gap_penalty,
                "outer_windows": int(ml_decision["decision_windows"]),
                "accuracy": float(ml_decision["accuracy"]),
                "weighted_f1": float(ml_decision["weighted_f1"]),
                "total_return_pct": float(ml_benchmark["total_return_pct"]),
                "mean_realized_HE": float(ml_decision["mean_realized_he"]),
                "mean_HE_gap": float(ml_decision["mean_realized_he_gap"]),
                "ES_reduction_vs_unhedged": float(ml_benchmark["es_reduction_vs_unhedged"]),
                "max_drawdown_pct": float(ml_benchmark["max_drawdown_pct"]),
                "fallback_share": float(ml_decision["fallback_share"]),
                "switch_rate": float(ml_decision["switch_rate"]),
                "chosen_family_distribution": ";".join(f"{key}:{value}" for key, value in family_distribution.items()),
                "note": note,
            }
        )
    out = pd.DataFrame(rows)
    out.to_csv(TABLE_DIR / "ml_final_penalty_sensitivity.csv", index=False, encoding="utf-8-sig")
    return out


def target_robustness(windows: list) -> pd.DataFrame:
    rows = []
    for window in windows:
        he_best = window.actual_best_hedge
        es_scores = {}
        dd_scores = {}
        for hedge in base.HEDGES:
            r = pd.Series(window.future_returns[hedge])
            es_scores[hedge] = upgrade.base.expected_shortfall(r)
            dd_scores[hedge] = upgrade.base.max_drawdown(r)
        es_best = max(es_scores, key=lambda h: -np.inf if pd.isna(es_scores[h]) else es_scores[h])
        dd_best = max(dd_scores, key=lambda h: -np.inf if pd.isna(dd_scores[h]) else dd_scores[h])
        rows.append(
            {
                "decision_date": window.date,
                "he_best_hedge": he_best,
                "es_best_hedge": es_best,
                "drawdown_best_hedge": dd_best,
                "he_equals_es_best": he_best == es_best,
                "he_equals_drawdown_best": he_best == dd_best,
            }
        )
    detail = pd.DataFrame(rows)
    if detail.empty:
        return pd.DataFrame()
    summary = pd.DataFrame(
        [
            {
                "metric": "he_vs_es_target_agreement",
                "windows": len(detail),
                "agreement_share": float(detail["he_equals_es_best"].mean()),
            },
            {
                "metric": "he_vs_drawdown_target_agreement",
                "windows": len(detail),
                "agreement_share": float(detail["he_equals_drawdown_best"].mean()),
            },
        ]
    )
    detail.to_csv(TABLE_DIR / "ml_final_target_robustness_detail.csv", index=False, encoding="utf-8-sig")
    return summary


def target_robustness_oos_only(decisions: pd.DataFrame, window_by_date: dict) -> tuple[pd.DataFrame, pd.DataFrame]:
    rows = []
    for _, decision in decisions.iterrows():
        decision_date = pd.Timestamp(decision["decision_date"])
        window = window_by_date[decision_date]
        he_best = window.actual_best_hedge
        es_scores = {}
        drawdown_scores = {}
        for hedge in base.HEDGES:
            returns = pd.Series(window.future_returns[hedge])
            es_scores[hedge] = upgrade.base.expected_shortfall(returns)
            drawdown_scores[hedge] = upgrade.base.max_drawdown(returns)
        es_best = max(es_scores, key=lambda hedge: -np.inf if pd.isna(es_scores[hedge]) else es_scores[hedge])
        drawdown_best = max(
            drawdown_scores,
            key=lambda hedge: -np.inf if pd.isna(drawdown_scores[hedge]) else drawdown_scores[hedge],
        )
        selected = decision["selected_hedge"]
        rows.append(
            {
                "decision_date": decision_date,
                "selected_hedge": selected,
                "he_best_hedge": he_best,
                "es_best_hedge": es_best,
                "drawdown_best_hedge": drawdown_best,
                "match_he_best": selected == he_best,
                "match_es_best": selected == es_best,
                "match_drawdown_best": selected == drawdown_best,
                "he_equals_es_best": he_best == es_best,
                "he_equals_drawdown_best": he_best == drawdown_best,
                "es_equals_drawdown_best": es_best == drawdown_best,
            }
        )
    detail = pd.DataFrame(rows)
    if detail.empty:
        return detail, pd.DataFrame()
    summary = pd.DataFrame(
        [
            {
                "oos_windows": len(detail),
                "he_vs_es_label_agreement": float(detail["he_equals_es_best"].mean()),
                "he_vs_drawdown_label_agreement": float(detail["he_equals_drawdown_best"].mean()),
                "es_vs_drawdown_label_agreement": float(detail["es_equals_drawdown_best"].mean()),
                "selected_match_he_best": float(detail["match_he_best"].mean()),
                "selected_match_es_best": float(detail["match_es_best"].mean()),
                "selected_match_drawdown_best": float(detail["match_drawdown_best"].mean()),
            }
        ]
    )
    return detail, summary


def write_target_robustness_oos_only() -> tuple[pd.DataFrame, pd.DataFrame]:
    _, _, _, _, window_by_date = upgrade.build_samples()
    decisions = pd.read_csv(TABLE_DIR / "ml_final_decisions.csv")
    detail, summary = target_robustness_oos_only(decisions, window_by_date)
    detail.to_csv(TABLE_DIR / "ml_final_target_robustness_oos_only.csv", index=False, encoding="utf-8-sig")
    return detail, summary


def imputation_audit(samples: pd.DataFrame, feature_cols: list[str]) -> pd.DataFrame:
    rows = []
    for feature in feature_cols:
        rows.append(
            {
                "feature": feature,
                "missing_in_final_samples": int(samples[feature].isna().sum()),
                "imputation_method": "none_in_final_sample; model pipelines keep train-fold median as safeguard",
                "time_respecting_guardrail": "no full-sample imputer is fitted for assessment; robustness variants must use forward-fill or trailing median",
            }
        )
    return pd.DataFrame(rows)


def discretization_guardrail(feature_cols: list[str]) -> pd.DataFrame:
    macro_features = set(macro_discretization_features(feature_cols))
    rows = []
    for feature in feature_cols:
        rows.append(
            {
                "feature": feature,
                "macro_binning_allowed": feature in macro_features,
                "reason": (
                    "slow-moving macro/state variable"
                    if feature in macro_features
                    else "continuous return/volatility/tail-risk signal retained to avoid smoothing extremes"
                ),
            }
        )
    return pd.DataFrame(rows)


def write_summary_figure(benchmark: pd.DataFrame) -> None:
    try:
        import matplotlib

        matplotlib.use("Agg")
        import matplotlib.pyplot as plt
        from matplotlib.patches import Patch
    except Exception:
        return
    top = benchmark.sort_values("total_return_pct", ascending=True)
    colors = ["#999999" if s != "ml_final_nested" else "#0072B2" for s in top["strategy"]]
    fig, ax = plt.subplots(figsize=(8.2, 5.2))
    ax.barh(top["strategy"], top["total_return_pct"], color=colors, edgecolor="white", linewidth=0.8)
    ax.axvline(0, color="#1F2937", linestyle="--", linewidth=0.8)
    ax.set_xlabel("Out-of-sample total return (%)")
    ax.set_title("Final adaptive ML strategy comparison")
    ax.grid(axis="x", color="#D9D9D9", linewidth=0.8)
    ax.legend(
        handles=[
            Patch(facecolor="#0072B2", label="Final adaptive ML"),
            Patch(facecolor="#999999", label="Benchmark strategy"),
        ],
        frameon=False,
        loc="lower right",
    )
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    fig.tight_layout()
    fig.savefig(FIGURE_DIR / "ml_final_strategy_total_return.png", dpi=180)
    fig.savefig(FIGURE_DIR / "ml_final_strategy_total_return.svg")
    plt.close(fig)


def update_paper_table(decision_summary: pd.DataFrame, benchmark: pd.DataFrame, hac: pd.DataFrame, bootstrap: pd.DataFrame) -> None:
    model_rows = decision_summary.copy()
    model_rows["panel"] = "decision_metrics"
    strategy_rows = benchmark.copy()
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
    hac_rows = hac[hac["benchmark"].isin(["unhedged_vnindex", "always_xau_vnd", "adcc_trailing_best"])].copy()
    hac_rows["panel"] = "hac_superiority_tests"
    boot_rows = bootstrap[bootstrap["benchmark"].isin(["unhedged_vnindex", "always_xau_vnd", "adcc_trailing_best"])].copy()
    boot_rows["panel"] = "block_bootstrap_tests"
    paper = pd.concat([model_rows, strategy_rows, hac_rows, boot_rows], ignore_index=True, sort=False)
    paper.to_csv(PAPER_TABLE, index=False, encoding="utf-8-sig")
    update_manifest(len(paper))


def update_paper_table_from_existing_outputs() -> pd.DataFrame:
    decision_summary = pd.read_csv(TABLE_DIR / "ml_final_decision_metrics.csv")
    benchmark = pd.read_csv(TABLE_DIR / "ml_final_benchmark_comparison.csv")
    hac = pd.read_csv(TABLE_DIR / "ml_final_hac_tests.csv")
    bootstrap = pd.read_csv(TABLE_DIR / "ml_final_block_bootstrap_tests.csv")
    decisions = pd.read_csv(TABLE_DIR / "ml_final_decisions.csv")
    xgb_audit_path = TABLE_DIR / "ml_final_xgboost_availability_audit.csv"
    xgb_audit = pd.read_csv(xgb_audit_path) if xgb_audit_path.exists() else pd.DataFrame()

    model_rows = decision_summary.copy()
    model_rows["panel"] = "decision_metrics"

    selected_model_rows = (
        decisions.groupby("chosen_family", as_index=False)
        .size()
        .rename(columns={"chosen_family": "model_family", "size": "selected_windows"})
    )
    selected_model_rows["selected_share"] = selected_model_rows["selected_windows"] / len(decisions) if len(decisions) else np.nan
    selected_model_rows["panel"] = "selected_model_distribution"

    strategy_rows = benchmark.copy()
    strategy_rows["panel"] = "benchmark_performance"
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

    hac_rows = hac.copy()
    hac_rows["panel"] = "hac_return_tests"
    hac_rows = hac_rows[
        [
            "panel",
            "strategy",
            "benchmark",
            "observations",
            "mean_return_diff_pct",
            "hac_t_return_diff",
            "p_value_return_superiority",
        ]
    ]

    boot_rows = bootstrap.copy()
    boot_rows["panel"] = "block_bootstrap_return_tests"
    boot_rows = boot_rows[
        [
            "panel",
            "strategy",
            "benchmark",
            "decision_windows",
            "mean_window_return_diff_pct",
            "ci_low_2p5",
            "ci_high_97p5",
            "block_length_windows",
            "p_value_centered_mean_test",
        ]
    ]

    xgb_rows = xgb_audit.copy()
    if len(xgb_rows):
        xgb_rows["panel"] = "xgboost_audit_summary"

    paper = pd.concat([model_rows, selected_model_rows, strategy_rows, hac_rows, boot_rows, xgb_rows], ignore_index=True, sort=False)
    paper.to_csv(PAPER_TABLE, index=False, encoding="utf-8-sig")
    update_manifest(len(paper))
    return paper


def update_manifest(rows: int) -> None:
    if RUN_DIR is None:
        raise RuntimeError("An isolated ML run is required for exports")
    manifest_path = RUN_DIR / "table_manifest.csv"
    if manifest_path.exists():
        manifest = pd.read_csv(manifest_path)
    else:
        manifest = pd.DataFrame(columns=["table", "source", "destination", "rows"])
    manifest = manifest[manifest["table"] != "table_19_ml_final_validation"]
    manifest = pd.concat(
        [
            manifest,
            pd.DataFrame(
                [
                    {
                        "table": "table_19_ml_final_validation",
                        "source": str(TABLE_DIR.relative_to(ROOT)),
                        "destination": str(PAPER_TABLE.relative_to(ROOT)),
                        "rows": rows,
                    }
                ]
            ),
        ],
        ignore_index=True,
    )
    table_order = [
        "table_01_descriptive_statistics",
        "table_02_garch_selection",
        "table_03_dcc_adcc_summary",
        "table_04_hedging_effectiveness",
        "table_05_safe_haven_regression",
        "table_06_robustness_summary",
        "table_07_markov_switching_robustness",
        "table_08_connectedness",
        "table_09_tail_risk",
        "table_10_tras_ranking",
        "table_10a_tras_component_rank_guardrail",
        "table_11_sector_application",
        "table_12_adaptive_ml",
        "table_13_interpretation_guardrails",
        "table_14_bond_term_structure_channel",
        "table_15_bond_tenor_proxy_hedging",
        "table_16_bond_tri_tenor_hedging",
        "table_17_ml_upgrade_validation",
        "table_18_phase3_19_evidence_audit",
        "table_19_ml_final_validation",
    ]
    manifest["_order"] = manifest["table"].map({name: i for i, name in enumerate(table_order)})
    missing_order = manifest["_order"].isna()
    manifest.loc[missing_order, "_order"] = len(table_order) + np.arange(missing_order.sum())
    manifest = manifest.sort_values("_order").drop(columns=["_order"])
    manifest.to_csv(manifest_path, index=False, encoding="utf-8")


def write_report(
    deps: dict,
    samples: pd.DataFrame,
    decisions: pd.DataFrame,
    decision_summary: pd.DataFrame,
    benchmark: pd.DataFrame,
    hac: pd.DataFrame,
    bootstrap: pd.DataFrame,
    target_summary: pd.DataFrame,
    class_audit: pd.DataFrame,
    feature_audit: pd.DataFrame,
    include_xgboost: bool,
) -> None:
    best_return = benchmark.sort_values("total_return_pct", ascending=False).iloc[0]
    best_es = benchmark.sort_values("es_reduction_vs_unhedged", ascending=False).iloc[0]
    ml = benchmark[benchmark["strategy"] == "ml_final_nested"].iloc[0]
    xgb_included = bool(include_xgboost and "XGBClassifier" in deps)
    lines = [
        "# Phase 3.20 Final Adaptive ML Validation Report",
        "",
        "## Scope",
        "",
        "Phase 3.20 is the final ML validation layer for the research paper. It does not replace the core econometric evidence. It tests whether an adaptive ML allocation rule can survive stricter validation than Phase 3.18.",
        "",
        "## What Is New Versus Phase 3.18",
        "",
        "- Outer expanding walk-forward with an inner chronological holdout; inner candidates fit once per holdout, not once per inner decision.",
        "- Strict calendar cutoff: train label end must precede the prediction date in both loops. Intraday releases and historical source revisions are not verified.",
        "- L1 selection and preprocessing fit inside the training portion. Predictive stress uses an expanding past-only threshold; full-sample DCC/aDCC is excluded.",
        "- Softmax baseline included in the same nested model-selection grid as Logistic and shallow Random Forest candidates.",
        "- XGBoost remains available for Phase 3.18 SHAP interpretability; final nested-grid inclusion is controlled by `--include-xgboost`.",
        "- Time-respecting class-coverage audit: inner validation expands backward when class counts are too sparse, never forward into test data.",
        "- Confidence fallback policy: low-confidence ML predictions can fall back to static/XAU rules selected by inner validation.",
        "- HAC paired tests on return and squared-loss differences.",
        "- Circular moving-block bootstrap sensitivity at configured lengths in decision windows, with a centered one-sided mean test; not RC/SPA or search-adjusted inference.",
        "- Calibration uses raw classifier correctness; final policy hit rate is a separate column. Raw probabilities and the past class-prior baseline are exported.",
        "- Returns use actual source trading dates. Raw model and combined policy return paths are separate.",
        "- Transaction-cost tables remain a label-switch fee sensitivity, not a complete execution/turnover model.",
        "- Target robustness audit comparing HE-best labels with ES-best and drawdown-best labels.",
        "- Imputation and macro-only discretization guardrail audits for preprocessing robustness; these are not treated as guaranteed performance improvements.",
        "",
        "## Runtime Dependencies",
        "",
        f"- scikit-learn available: `{ 'sklearn_error' not in deps }`",
        f"- XGBoost available: `{ 'XGBClassifier' in deps }`",
        f"- XGBoost included in final nested grid: `{xgb_included}`",
        f"- SHAP available: `{ 'shap' in deps }`",
        "- LightGBM available: `False` (not installed in current runtime; not required for this phase).",
        "",
        "## Sample",
        "",
        f"- Decision samples: `{len(samples)}`.",
        f"- Out-of-sample decision windows: `{len(decisions)}`.",
        f"- Test period: `{decisions['decision_date'].min()}` to `{decisions['window_end'].max()}`.",
        f"- Inner-validation class warnings: `{int((class_audit['coverage_status'] != 'ok').sum()) if len(class_audit) else 0}`.",
        f"- Selected-feature fallback share: `{float(decisions['feature_fallback_triggered'].mean()) if len(decisions) and 'feature_fallback_triggered' in decisions else np.nan:.6f}`.",
        "",
        "## Main Results",
        "",
        f"- Best total-return strategy: `{best_return['strategy']}` ({best_return['total_return_pct']:.4f}%).",
        f"- Best ES-reduction strategy: `{best_es['strategy']}` ({best_es['es_reduction_vs_unhedged']:.6f}).",
        f"- Final ML total return: `{ml['total_return_pct']:.4f}%`.",
        f"- Final ML variance reduction versus unhedged: `{ml['variance_reduction_vs_unhedged']:.6f}`.",
        f"- Final ML ES reduction versus unhedged: `{ml['es_reduction_vs_unhedged']:.6f}`.",
        "",
        "## Decision Metrics",
        "",
        dataframe_to_markdown(decision_summary),
        "",
        "## HAC Tests",
        "",
        dataframe_to_markdown(hac),
        "",
        "## Conditional Block Bootstrap Sensitivity (Not Search-Adjusted)",
        "",
        dataframe_to_markdown(bootstrap),
        "",
        "## Target Robustness",
        "",
        dataframe_to_markdown(target_summary),
        "",
        "## Methodological Guardrails",
        "",
        "- Inner validation is used for model selection: feature set, model family, hyperparameters and fallback policy are selected using only past observations.",
        "- Outer walk-forward windows are reserved for model assessment on future observations not used in selection.",
        "- L1 feature selection is used for its sparse-solution property, shrinking noisy coefficients to zero; if it selects fewer than three features, a theory-core feature set is used as a safety fallback.",
        "- The inner score acts as strategy regularization: it rewards realized hedging effectiveness and penalizes excessive hedge switching and distance from the ex-post best hedge.",
        "- Imputation and discretization variants are robustness checks under a No-Free-Lunch view; their value is empirical and domain-dependent.",
        "",
        "## Scientific Interpretation",
        "",
        "If Phase 3.20 ML does not dominate on risk metrics or superiority tests, the correct conclusion is not failure. It means the adaptive layer is useful as an application experiment but should not be advertised as the core safe-haven proof. The core contribution remains the econometric framework: SJC premium, bond term structure, tail/frequency connectedness and TRAS.",
    ]
    REPORT_PATH.write_text("\n".join(lines), encoding="utf-8")


def dataframe_to_markdown(df: pd.DataFrame) -> str:
    if df.empty:
        return ""
    cols = list(df.columns)
    lines = ["| " + " | ".join(cols) + " |", "| " + " | ".join(["---"] * len(cols)) + " |"]
    for _, row in df.iterrows():
        vals = []
        for col in cols:
            val = row[col]
            if isinstance(val, float):
                vals.append(f"{val:.6f}")
            else:
                vals.append(str(val))
        lines.append("| " + " | ".join(vals) + " |")
    return "\n".join(lines)


def preflight_checks(panel: pd.DataFrame, feature_cols: list[str], windows: list, samples: pd.DataFrame) -> dict:
    if samples.empty or samples["decision_date"].duplicated().any():
        raise ValueError("Empty or duplicate decision sample dates")
    if not np.isfinite(samples[feature_cols].to_numpy(dtype=float)).all():
        raise ValueError("Nonfinite ML feature values in usable samples")
    # Prefix invariance checks the predictive tail transform itself, without
    # making unsupported claims about macro publication/vintage availability.
    cut = len(panel) // 2
    prefix_tail = base.predictive_stress(panel[base.TARGET].iloc[:cut])
    pd.testing.assert_series_equal(prefix_tail, panel["stress_vnindex_q05"].iloc[:cut], check_names=False)
    excluded = 0
    evaluated = 0
    timing = []
    for i in range(base.MIN_TRAIN_SAMPLES, len(samples)):
        test = samples.iloc[i]
        past = samples.iloc[:i]
        train = base.mature_training_samples(past, test["decision_date"])
        excluded += len(past) - len(train)
        if len(train) < base.MIN_TRAIN_SAMPLES:
            continue
        inner, validation, audit = time_respecting_validation_split(train, "actual_best_hedge", base.HEDGES)
        if inner.empty or validation.empty or not (inner["window_end"] < validation["decision_date"].min()).all():
            raise ValueError("Inner label maturity check failed")
        if not (train["window_end"] < test["decision_date"]).all():
            raise ValueError("Outer label maturity check failed")
        timing.append({
            "decision_date": test["decision_date"], "outer_train_samples": len(train),
            "outer_max_label_end": train["window_end"].max(),
            "inner_max_label_end": inner["window_end"].max(),
            "first_validation_date": validation["decision_date"].min(), **audit,
        })
        evaluated += 1
    if evaluated == 0:
        raise ValueError("Insufficient mature training history for any OOS decision")
    for window in windows:
        if window.dcc_best_hedge is not None or window.adcc_best_hedge is not None:
            raise ValueError("Full-sample dynamic artifact entered the predictive path")
        for values in window.future_returns.values():
            base.window_return_dates(window, values)
    pd.DataFrame(timing).to_csv(TABLE_DIR / "ml_preflight_label_timing.csv", index=False)
    return {
        "status": "passed_implemented_checks_with_declared_data_vintage_limits",
        "panel_rows": len(panel), "features": len(feature_cols), "windows": len(windows),
        "samples": len(samples), "eligible_outer_decisions": evaluated,
        "excluded_unmatured_labels_across_outer_splits": excluded,
        "predictive_tail_prefix_invariance": True, "actual_return_dates_validated": True,
        "full_sample_dcc_excluded": True,
        "unverified": ["intraday_execution_and_release_timestamps", "source_revision_vintages", "full_turnover_cost_model", "search_adjusted_inference"],
    }


def write_run_manifest(args, deps: dict, status: str, preflight: dict | None = None) -> None:
    if RUN_DIR is None:
        raise RuntimeError("An isolated run must be configured")
    from importlib.metadata import PackageNotFoundError, version
    versions = {}
    for name in ["numpy", "pandas", "scikit-learn", "xgboost", "shap"]:
        try:
            versions[name] = version(name)
        except PackageNotFoundError:
            versions[name] = None
    files = [Path(__file__), BASE_SCRIPT, upgrade.BASE_SCRIPT, ROOT / "src/models/model_control.yaml", base.RETURNS_PATH, base.BOND_TS_PATH]
    if status == "started":
        for source in files[:4]:
            destination = RUN_DIR / "source_snapshot" / source.relative_to(ROOT)
            destination.parent.mkdir(parents=True, exist_ok=True)
            destination.write_bytes(source.read_bytes())
    manifest = {
        "status": status, "updated_at_utc": datetime.now(timezone.utc).isoformat(),
        "protocol": base.VALIDATION_CONTROL, "arguments": vars(args),
        "python": sys.version, "packages": versions,
        "runtime_errors": {k: str(v) for k, v in deps.items() if k.endswith("error")},
        "sha256": {str(path.relative_to(ROOT)): hashlib.sha256(path.read_bytes()).hexdigest() for path in files},
        "preflight": preflight,
        "data_availability_scope": "calendar_date_cutoff_on_observed_input_values_not_verified_historical_vintages",
        "manuscript_updated": False,
    }
    (RUN_DIR / "run_manifest.json").write_text(json.dumps(manifest, ensure_ascii=False, indent=2), encoding="utf-8")


def main(argv: list[str] | None = None) -> None:
    args = parse_args(argv)
    # Postprocessing is supported within the new run after --main, never by
    # silently opening legacy artifacts or reusing an existing run directory.
    if (args.update_paper_table or args.target_robustness_oos) and not args.main:
        raise ValueError("Postprocessing requires --main in a new isolated run")
    if args.preflight and any([args.main, args.smoke, args.penalty_sensitivity, args.update_paper_table, args.target_robustness_oos]):
        raise ValueError("--preflight cannot be combined with fitting/postprocessing actions")
    if args.smoke and args.penalty_sensitivity:
        raise ValueError("--smoke cannot trigger a full penalty sensitivity run")
    configure_run_paths(args.run_id)
    ensure_dirs()
    requested_actions = [
        args.main,
        args.xgboost_audit,
        args.update_paper_table,
        args.target_robustness_oos,
        args.penalty_sensitivity,
        args.preflight,
        args.smoke,
    ]
    run_main = args.main or args.smoke or not any(requested_actions)
    deps = optional_imports()
    if "sklearn_error" in deps:
        raise RuntimeError(f"scikit-learn unavailable: {deps['sklearn_error']}")
    if args.include_xgboost and "XGBClassifier" not in deps:
        raise RuntimeError(f"Requested XGBoost unavailable: {deps.get('xgboost_error')}")
    write_run_manifest(args, deps, "started")
    panel, feature_cols, windows, samples, window_by_date = upgrade.build_samples()
    if args.smoke:
        for i in range(base.MIN_TRAIN_SAMPLES, len(samples)):
            if len(base.mature_training_samples(samples.iloc[:i], samples.iloc[i]["decision_date"])) >= base.MIN_TRAIN_SAMPLES:
                samples = samples.iloc[:i + 1].copy()
                break
    checks = preflight_checks(panel, feature_cols, windows, samples)
    write_run_manifest(args, deps, "preflight_passed" if args.preflight else "inputs_validated", checks)
    if args.preflight:
        print(json.dumps(checks, indent=2))
        print(f"Manifest: {RUN_DIR / 'run_manifest.json'}")
        return
    if run_main or args.xgboost_audit or args.include_xgboost:
        xgb_audit = write_xgboost_availability_audit(deps, include_xgboost=args.include_xgboost)
        print("XGBoost availability audit completed.")
        print(xgb_audit.to_string(index=False))
    if args.penalty_sensitivity:
        sensitivity = run_penalty_sensitivity(
            mode=args.penalty_sensitivity_mode,
            include_xgboost=args.include_xgboost,
            deps=deps,
        )
        print(f"Created {TABLE_DIR / 'ml_final_penalty_sensitivity.csv'}")
        print(sensitivity.to_string(index=False))
    if not run_main:
        return

    decisions, returns_df, grid, class_audit, feature_audit, score_components = run_nested_walk_forward(
        samples,
        feature_cols,
        window_by_date,
        deps,
        include_xgboost=args.include_xgboost,
    )
    decision_summary = decision_metrics(decisions)
    confusion = decision_confusion_matrix(decisions)
    prediction_distribution = decision_prediction_distribution(decisions)
    benchmark = upgrade.summarize_strategies(returns_df)
    tc = upgrade.transaction_cost_sensitivity(returns_df)
    calibration = confidence_calibration(decisions)
    hac = paired_hac_tests(returns_df)
    bootstrap = block_bootstrap_tests(returns_df)
    target_summary = target_robustness(windows)
    imputation = imputation_audit(samples, feature_cols)
    discretization = discretization_guardrail(feature_cols)

    decisions.to_csv(TABLE_DIR / "ml_final_decisions.csv", index=False, encoding="utf-8-sig")
    returns_df.to_csv(TABLE_DIR / "ml_final_strategy_returns.csv", index=False, encoding="utf-8-sig")
    grid.to_csv(TABLE_DIR / "ml_final_nested_validation_grid.csv", index=False, encoding="utf-8-sig")
    class_audit.to_csv(TABLE_DIR / "ml_final_inner_validation_class_audit.csv", index=False, encoding="utf-8-sig")
    feature_audit.to_csv(TABLE_DIR / "ml_final_feature_selection_audit.csv", index=False, encoding="utf-8-sig")
    score_components.to_csv(TABLE_DIR / "ml_final_inner_score_components.csv", index=False, encoding="utf-8-sig")
    decision_summary.to_csv(TABLE_DIR / "ml_final_decision_metrics.csv", index=False, encoding="utf-8-sig")
    confusion.to_csv(TABLE_DIR / "ml_final_confusion_matrix.csv", index=False, encoding="utf-8-sig")
    prediction_distribution.to_csv(TABLE_DIR / "ml_final_prediction_distribution.csv", index=False, encoding="utf-8-sig")
    benchmark.to_csv(TABLE_DIR / "ml_final_benchmark_comparison.csv", index=False, encoding="utf-8-sig")
    tc.to_csv(TABLE_DIR / "ml_final_transaction_cost_sensitivity.csv", index=False, encoding="utf-8-sig")
    calibration.to_csv(TABLE_DIR / "ml_final_confidence_calibration.csv", index=False, encoding="utf-8-sig")
    probability_scores(decisions).to_csv(TABLE_DIR / "ml_final_probability_scores.csv", index=False)
    hac.to_csv(TABLE_DIR / "ml_final_hac_tests.csv", index=False, encoding="utf-8-sig")
    bootstrap.to_csv(TABLE_DIR / "ml_final_block_bootstrap_tests.csv", index=False, encoding="utf-8-sig")
    target_summary.to_csv(TABLE_DIR / "ml_final_target_robustness_summary.csv", index=False, encoding="utf-8-sig")
    imputation.to_csv(TABLE_DIR / "ml_final_imputation_audit.csv", index=False, encoding="utf-8-sig")
    discretization.to_csv(TABLE_DIR / "ml_final_discretization_guardrail.csv", index=False, encoding="utf-8-sig")

    write_summary_figure(benchmark)
    if args.update_paper_table:
        update_paper_table_from_existing_outputs()
    if args.target_robustness_oos:
        write_target_robustness_oos_only()
    write_report(
        deps,
        samples,
        decisions,
        decision_summary,
        benchmark,
        hac,
        bootstrap,
        target_summary,
        class_audit,
        feature_audit,
        args.include_xgboost,
    )
    if args.smoke:
        REPORT_PATH.write_text("# SMOKE TEST ONLY — one decision, not research evidence\n\n" + REPORT_PATH.read_text(encoding="utf-8"), encoding="utf-8")
    write_run_manifest(args, deps, "completed_smoke_not_research_result" if args.smoke else "completed", checks)

    print("Phase 3.20 final adaptive ML validation completed.")
    print(f"Decision windows: {len(decisions)}")
    print(f"Benchmark rows: {len(benchmark)}")
    print(f"Report: {REPORT_PATH}")


if __name__ == "__main__":
    try:
        main()
    except Exception as exc:
        if RUN_DIR is not None and (RUN_DIR / "run_manifest.json").exists():
            manifest_path = RUN_DIR / "run_manifest.json"
            manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
            manifest.update(status="failed", error=repr(exc))
            manifest_path.write_text(json.dumps(manifest, ensure_ascii=False, indent=2), encoding="utf-8")
        raise
