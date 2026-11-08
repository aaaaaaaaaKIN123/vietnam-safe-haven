import importlib.util
import sys
import tempfile
import unittest
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import patch

import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("ml_point_in_time_tests", ROOT / "src/models/python/05_adaptive_hedge_ml_final.py")
ml = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = ml
spec.loader.exec_module(ml)


class PointInTimeTests(unittest.TestCase):
    def test_l1_candidates_and_selector_fit_three_classes_without_silent_fallback(self):
        deps = ml.optional_imports()
        rng = np.random.default_rng(42)
        x = pd.DataFrame(rng.normal(size=(90, 4)), columns=["a", "b", "c", "d"])
        y = np.tile(np.arange(3), 30)
        x["a"] += y * 4
        sample = x.assign(label_id=y)
        ranked = ml.rank_features_with_l1(sample, list(x), deps)
        self.assertIn("a", ranked)
        candidate = ml.Candidate("test_l1", "logistic", {"C": .5, "penalty": "l1"})
        estimator = ml.make_estimator(candidate, deps)
        ml.fit_estimator(candidate, estimator, x, y, deps)
        probs = estimator.predict_proba(x.iloc[:2])
        self.assertEqual(probs.shape, (2, 3))
        np.testing.assert_allclose(probs.sum(axis=1), 1.)

    def test_predictive_tail_is_invariant_to_future_append_and_changes(self):
        rng = np.random.default_rng(10)
        past = pd.Series(rng.normal(size=350))
        extended = pd.concat([past, pd.Series([-100.0] * 150)], ignore_index=True)
        pd.testing.assert_series_equal(ml.base.predictive_stress(past), ml.base.predictive_stress(extended).iloc[:len(past)])
        self.assertTrue(ml.base.predictive_stress(past).iloc[:252].isna().all())

    def test_strict_label_maturity_excludes_equal_and_future_end_dates(self):
        samples = pd.DataFrame({"window_end": pd.to_datetime(["2020-01-01", "2020-01-02", "2020-01-03"])})
        self.assertEqual(list(ml.base.mature_training_samples(samples, "2020-01-02").index), [0])

    def test_inner_boundary_purges_equal_date_label(self):
        dates = pd.date_range("2020-01-01", periods=30, freq="21D")
        data = pd.DataFrame({"decision_date": dates, "window_end": dates + pd.Timedelta(days=21), "actual_best_hedge": ml.base.HEDGES * 10})
        train, validation, audit = ml.time_respecting_validation_split(data, "actual_best_hedge", ml.base.HEDGES)
        self.assertTrue((train.window_end < validation.decision_date.min()).all())
        self.assertEqual(audit["excluded_unmatured_train_labels"], 1)

    def test_exporters_preserve_real_dates_across_weekend_and_holiday(self):
        dates = pd.to_datetime(["2026-04-29", "2026-05-04", "2026-05-05"]).to_numpy()
        window = SimpleNamespace(date=pd.Timestamp("2026-04-28"), window_end=pd.Timestamp("2026-05-05"), future_dates=dates)
        for exporter in [ml.append_future_returns, ml.upgrade.append_future_returns]:
            rows = []
            exporter(rows, window, "test", "r_sjc_gold", np.array([1., 2., 3.]))
            self.assertEqual([r["date"] for r in rows], list(pd.to_datetime(dates)))
        with self.assertRaises(ValueError):
            ml.base.window_return_dates(window, np.array([1.]))

    def test_probability_mapping_respects_estimator_classes(self):
        model = SimpleNamespace(classes_=np.array([0, 2]), predict_proba=lambda x: np.array([[.2, .8]]))
        hedge, probs = ml.classifier_prediction(model, pd.DataFrame(), dict(enumerate(ml.base.HEDGES)))
        self.assertEqual(hedge, "r_bond_tri")
        np.testing.assert_array_equal(probs, [.2, 0., .8])

    def test_calibration_uses_raw_classifier_correctness(self):
        d = pd.DataFrame({"decision_date": ["2020-01-01"], "max_probability": [.45], "selected_is_actual_best": [True], "raw_model_is_actual_best": [False]})
        row = ml.confidence_calibration(d).query("windows == 1").iloc[0]
        self.assertEqual(row.hit_rate, 0.)
        self.assertEqual(row.policy_hit_rate, 1.)
        with self.assertRaises(ValueError):
            ml.confidence_calibration(d.drop(columns="raw_model_is_actual_best"))

    def test_dynamic_file_is_never_loaded_into_new_windows(self):
        dates = pd.bdate_range("2020-01-01", periods=36)
        panel = pd.DataFrame({"date": dates, **{name: np.sin(np.arange(36) + i) for i, name in enumerate([ml.base.TARGET] + ml.base.HEDGES)}})
        with patch.object(ml.base, "MIN_HISTORY_ROWS", 10), patch.object(ml.base, "HEDGE_LOOKBACK", 8), patch.object(ml.base, "HORIZON", 3), patch.object(ml.base, "REBALANCE_STEP", 3), patch.object(ml.base, "build_dynamic_lookup", side_effect=AssertionError("must not read full-sample DCC")):
            windows = ml.base.build_windows(panel)
        self.assertGreater(len(windows), 0)
        for window in windows:
            self.assertIsNone(window.adcc_best_hedge)
            self.assertNotIn("adcc_trailing_best", window.future_returns)
            self.assertEqual(pd.Timestamp(window.future_dates[-1]), window.window_end)
        self.assertNotIn("adcc_best", ml.FALLBACKS)

    def test_circular_bootstrap_preserves_consecutive_windows_inside_blocks(self):
        indices = ml.circular_block_indices(12, 3, np.random.default_rng(5))
        for block in indices.reshape(-1, 3):
            np.testing.assert_array_equal(np.diff(block) % 12, [1, 1])

    def test_bootstrap_reports_block_sensitivity_and_centered_test(self):
        data = pd.DataFrame([
            {"decision_date": date, "strategy": strategy, "return_pct": value}
            for date in pd.date_range("2020-01-01", periods=12, freq="30D")
            for strategy, value in [("ml_final_nested", 1.), ("unhedged_vnindex", 0.)]
        ])
        result = ml.block_bootstrap_tests(data)
        self.assertEqual(result.block_length_windows.tolist(), [1, 3, 6])
        self.assertTrue((result.p_value_centered_mean_test <= 1 / 501 + 1e-10).all())
        self.assertNotIn("p_value_bootstrap_not_superior", result)

    def test_run_output_is_isolated_and_existing_run_is_refused(self):
        old = {key: getattr(ml, key) for key in ["RUN_DIR", "TABLE_DIR", "FIGURE_DIR", "REPORT_PATH", "PAPER_TABLE"]}
        try:
            cache = ROOT / ".cache"
            cache.mkdir(parents=True, exist_ok=True)
            with tempfile.TemporaryDirectory(dir=cache) as temp:
                control = dict(ml.base.VALIDATION_CONTROL, run_output_root=str(Path(temp).relative_to(ROOT)))
                with patch.dict(ml.base.VALIDATION_CONTROL, control):
                    run = ml.configure_run_paths("test_run")
                    self.assertEqual(ml.TABLE_DIR, run / "tables")
                    self.assertEqual(ml.PAPER_TABLE, run / "table_19_candidate.csv")
                    with self.assertRaises(FileExistsError):
                        ml.configure_run_paths("test_run")
        finally:
            for key, value in old.items():
                setattr(ml, key, value)


if __name__ == "__main__":
    unittest.main()
