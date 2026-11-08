# Release validation — 0.1.0

Validated locally on Windows with Python 3.12 and R 4.5.2 on 2026-11-08.

| Check | Result |
|---|---|
| Original Python tests before changes | 21 passed |
| Python tests after input-validation changes | 24 passed |
| Source ZIP extracted into a separate directory, fresh minimal Python environment | 24 passed |
| Dependency consistency in the fresh environment | `pip check` passed |
| Synthetic preflight | 3,100 rows, 28 features, 111 windows, 30 eligible outer decisions; timing checks passed |
| Synthetic smoke in the fresh environment and extracted source | Completed one eligible outer decision; 8 strategy summaries and output writers completed |
| Python syntax | 27 source files parsed |
| R syntax | 39 source files parsed |
| Source archive | Member hashes, UTF-8 decoding, ZIP integrity and excluded-data boundary checked |
| Credential-pattern scan on selected release files | No matches for the checked token/private-key/credential patterns |

The first extracted-source test run exposed a test assumption that `.cache/`
already existed. The test now creates that directory before using it; the
subsequent fresh extraction passed all 24 tests.

The minimal environment was installed from `requirements.txt` without XGBoost,
SHAP, statsmodels or the data/manuscript extras. Optional imports were correctly
reported as unavailable; the default ML smoke completed without them.

R startup emitted Windows locale warnings; parsing still succeeded. No full R
estimation rerun or complete empirical replication was performed. The optional
Python research stack and all R package installations were not revalidated here.
CI results on hosted Linux runners are reported separately by GitHub Actions.

The credential-pattern check is a limited automated check, not a guarantee that
every possible confidential value or third-party licensing issue was detected.
