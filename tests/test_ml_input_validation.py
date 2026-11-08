import importlib.util
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("input_validation_base", ROOT / "src/models/python/03_adaptive_hedge_ml.py")
base = importlib.util.module_from_spec(spec)
import sys
sys.modules[spec.name] = base
spec.loader.exec_module(base)


class InputValidationTests(unittest.TestCase):
    def read_frame(self, frame):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "input.csv"
            frame.to_csv(path, index=False)
            return base.safe_read_csv(path)

    def test_rejects_missing_invalid_and_duplicate_dates(self):
        for frame, message in [
            (pd.DataFrame({"price": [1]}), "Missing date column"),
            (pd.DataFrame({"date": [None]}), "Missing or invalid dates"),
            (pd.DataFrame({"date": ["invalid"]}), "Missing or invalid dates"),
            (pd.DataFrame({"date": ["2020-01-01", "2020-01-01"]}), "Duplicate dates"),
        ]:
            with self.subTest(message=message), self.assertRaisesRegex(ValueError, message):
                self.read_frame(frame)

    def test_rejects_missing_core_column_before_feature_engineering(self):
        with patch.object(base, "safe_read_csv", return_value=pd.DataFrame({"date": [pd.Timestamp("2020-01-01")]})):
            with self.assertRaisesRegex(ValueError, "Missing core return columns"):
                base.prepare_panel()

    def test_rejects_infinite_core_returns(self):
        frame = pd.DataFrame({"date": [pd.Timestamp("2020-01-01")], **{name: [np.inf] for name in [base.TARGET] + base.HEDGES}})
        with patch.object(base, "safe_read_csv", return_value=frame):
            with self.assertRaisesRegex(ValueError, "Infinite values"):
                base.prepare_panel()


if __name__ == "__main__":
    unittest.main()
