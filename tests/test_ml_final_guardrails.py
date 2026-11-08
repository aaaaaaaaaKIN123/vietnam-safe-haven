import importlib.util
import sys
import unittest
from pathlib import Path

import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
MODULE_PATH = ROOT / "src" / "models" / "python" / "05_adaptive_hedge_ml_final.py"


def load_module():
    spec = importlib.util.spec_from_file_location("ml_final_guardrails", MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class MlFinalGuardrailTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.ml = load_module()

    def test_validation_window_expands_backward_until_class_coverage_is_met(self):
        labels = (
            ["r_sjc_gold"] * 2
            + ["r_bond_tri"] * 2
            + ["r_xau_vnd"] * 6
            + ["r_sjc_gold"] * 2
            + ["r_bond_tri"] * 2
            + ["r_xau_vnd"] * 10
        )
        dates = pd.date_range("2020-01-01", periods=len(labels), freq="30D")
        outer_train = pd.DataFrame({
            "actual_best_hedge": labels, "decision_date": dates,
            "window_end": dates + pd.Timedelta(days=21),
        })

        train_part, validation_part, audit = self.ml.time_respecting_validation_split(
            outer_train,
            label_col="actual_best_hedge",
            labels=["r_sjc_gold", "r_xau_vnd", "r_bond_tri"],
            initial_windows=8,
            max_windows=18,
            min_class_count=2,
        )

        self.assertEqual(len(validation_part), 14)
        self.assertEqual(len(train_part), len(outer_train) - 14)
        self.assertEqual(audit["coverage_status"], "ok")
        self.assertEqual(audit["validation_size"], 14)
        self.assertEqual(audit["count_r_sjc_gold"], 2)
        self.assertEqual(audit["count_r_bond_tri"], 2)
        self.assertEqual(audit["count_r_xau_vnd"], 10)

    def test_feature_selection_falls_back_to_theory_core_when_l1_is_too_sparse(self):
        selected, audit = self.ml.bound_feature_selection(
            ranked_features=["vix_level_lag"],
            theory_core_features=[
                "r_vnindex_lag1",
                "vnindex_vol_63",
                "vix_level_lag",
                "gold_wedge_mean_21",
            ],
            available_features=[
                "r_vnindex_lag1",
                "vnindex_vol_63",
                "vix_level_lag",
                "gold_wedge_mean_21",
            ],
            min_features=3,
            max_features=10,
            selector_type="l1_logistic",
        )

        self.assertEqual(
            selected,
            ["r_vnindex_lag1", "vnindex_vol_63", "vix_level_lag", "gold_wedge_mean_21"],
        )
        self.assertTrue(audit["fallback_triggered"])
        self.assertEqual(audit["selected_feature_count"], 4)

    def test_feature_selection_caps_large_ranked_sets_without_using_future_shap(self):
        selected, audit = self.ml.bound_feature_selection(
            ranked_features=["f1", "f2", "f3", "f4", "f5"],
            theory_core_features=["f1", "f2", "f3"],
            available_features=["f1", "f2", "f3", "f4", "f5"],
            min_features=3,
            max_features=3,
            selector_type="inner_l1",
        )

        self.assertEqual(selected, ["f1", "f2", "f3"])
        self.assertFalse(audit["fallback_triggered"])
        self.assertEqual(audit["selector_type"], "inner_l1")

    def test_macro_discretization_excludes_returns_volatility_and_tail_features(self):
        features = [
            "bond_slope_10y_1y_lag",
            "vix_level_lag",
            "r_vnindex_lag1",
            "vnindex_vol_63",
            "vnindex_tail_21",
            "oil_abs_return_21",
        ]

        allowed = self.ml.macro_discretization_features(features)

        self.assertEqual(allowed, ["bond_slope_10y_1y_lag", "vix_level_lag"])

    def test_final_grid_contains_small_fair_regularized_candidates(self):
        deps = self.ml.optional_imports()

        names = {candidate.name for candidate in self.ml.candidate_grid(deps)}

        self.assertIn("softmax_l2_0002_theory", names)
        self.assertIn("softmax_l2_001_theory", names)
        self.assertIn("logistic_l2_C0p1_theory", names)
        self.assertIn("logistic_l2_C0p5_theory", names)
        self.assertIn("logistic_l2_C1p0_theory", names)
        self.assertIn("logistic_l1_C0p1_l1fs", names)
        self.assertIn("logistic_l1_C0p5_l1fs", names)
        self.assertIn("rf_depth1_leaf8_theory", names)
        self.assertIn("rf_depth3_leaf12_theory", names)
        self.assertFalse(any("xgb" in name for name in names))

    def test_include_xgboost_adds_shallow_xgb_candidates_when_available(self):
        deps = {"XGBClassifier": object}

        candidates = self.ml.candidate_grid(deps, include_xgboost=True)
        xgb_candidates = [candidate for candidate in candidates if candidate.family == "xgboost"]

        self.assertEqual(len(xgb_candidates), 16)
        self.assertIn("xgb_d1_lr003_n40_l3_theory", {candidate.name for candidate in xgb_candidates})
        self.assertIn("xgb_d2_lr006_n60_l6_theory", {candidate.name for candidate in xgb_candidates})

    def test_include_xgboost_flag_is_parsed_without_changing_default(self):
        default_args = self.ml.parse_args([])
        include_args = self.ml.parse_args(["--main", "--include-xgboost"])

        self.assertFalse(default_args.include_xgboost)
        self.assertTrue(include_args.main)
        self.assertTrue(include_args.include_xgboost)

    def test_remaining_work_flags_are_parsed_without_enabling_main_by_default(self):
        args = self.ml.parse_args(
            [
                "--update-paper-table",
                "--target-robustness-oos",
                "--penalty-sensitivity",
                "--penalty-sensitivity-mode",
                "one_factor",
            ]
        )

        self.assertFalse(args.main)
        self.assertTrue(args.update_paper_table)
        self.assertTrue(args.target_robustness_oos)
        self.assertTrue(args.penalty_sensitivity)
        self.assertEqual(args.penalty_sensitivity_mode, "one_factor")

    def test_logistic_l1_candidate_uses_true_l1_penalty(self):
        deps = self.ml.optional_imports()
        if "sklearn_error" in deps:
            self.skipTest("scikit-learn is not available")
        candidate = self.ml.Candidate(
            "logistic_l1_C0p1_l1fs",
            "logistic",
            {"C": 0.1, "penalty": "l1"},
            "l1_sparse",
        )

        estimator = self.ml.make_estimator(candidate, deps)
        model = estimator.named_steps["model"]

        self.assertEqual(model.estimator.penalty, "l1")
        self.assertEqual(model.estimator.solver, "liblinear")

    def test_final_decision_diagnostics_include_classification_metrics_and_confusion(self):
        decisions = pd.DataFrame(
            [
                {
                    "strategy": "ml_final_nested",
                    "decision_date": "2020-01-01",
                    "selected_hedge": "r_xau_vnd",
                    "actual_best_hedge": "r_xau_vnd",
                    "selected_is_actual_best": True,
                    "selected_realized_he": 0.10,
                    "realized_he_gap": 0.00,
                    "max_probability": 0.70,
                    "fallback_used": "none",
                },
                {
                    "strategy": "ml_final_nested",
                    "decision_date": "2020-02-01",
                    "selected_hedge": "r_sjc_gold",
                    "actual_best_hedge": "r_bond_tri",
                    "selected_is_actual_best": False,
                    "selected_realized_he": 0.02,
                    "realized_he_gap": 0.05,
                    "max_probability": 0.45,
                    "fallback_used": "static_best",
                },
                {
                    "strategy": "ml_final_nested",
                    "decision_date": "2020-03-01",
                    "selected_hedge": "r_bond_tri",
                    "actual_best_hedge": "r_bond_tri",
                    "selected_is_actual_best": True,
                    "selected_realized_he": 0.08,
                    "realized_he_gap": 0.00,
                    "max_probability": 0.65,
                    "fallback_used": "none",
                },
            ]
        )

        metrics = self.ml.decision_metrics(decisions)
        confusion = self.ml.decision_confusion_matrix(decisions)

        for column in ["weighted_f1", "macro_precision", "macro_recall"]:
            self.assertIn(column, metrics.columns)
        self.assertEqual(int(confusion["count"].sum()), len(decisions))
        self.assertIn("actual_hedge", confusion.columns)
        self.assertIn("predicted_hedge", confusion.columns)


if __name__ == "__main__":
    unittest.main()
