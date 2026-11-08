# Reproducing the workflow

## Environment and scope

The minimal Python stack is pinned in `requirements.txt`; the validated local
interpreter is Python 3.12. Optional research tools use bounded versions in
`requirements-research.txt` and are not a fully locked environment. R syntax is
checked separately; R package installation uses CRAN and is not version-locked.
Record `sessionInfo()` with any new R estimation run.

The source ZIP preserves the repository directory structure. Extract it before
running commands. It is a source distribution, not an installable Python wheel.
No network connection is needed for the demo after dependencies are installed.

## Synthetic verification

```bash
python -m unittest discover -s tests -v
python tools/demo.py
python tools/demo.py --smoke
Rscript tools/check_r_syntax.R
python tools/package_release.py
```

The demo copies only three Python model scripts and their configuration into a
new workspace under `results/demo_runs/`. It generates 3,100 artificial business-day
observations with seed 20261108. No market CSVs are distributed or reused.
Each `--smoke` invocation fits one eligible decision. A passed smoke check tests
software integration; it does not validate financial conclusions or predictive skill.

## Market-data workflow

1. Supply legally obtained local inputs at the paths in `data/scripts/config.py`.
   Read `data/AGENT_DATA_GUIDE.md` for asset definitions and units.
2. Run `python data/scripts/00_audit_raw.py` and inspect its missing-file report.
3. Run `python data/scripts/01_build_processed.py`, followed by
   `python data/scripts/02_build_model_input.py` and
   `python data/scripts/03_quality_checks.py`.
4. The ML extension additionally requires
   `python data/scripts/02b_build_bond_term_structure.py` and the associated
   raw 1Y/5Y/10Y yield inputs. Sector extensions have their own runners.
5. Review `src/models/model_control.yaml`, then execute the R pipeline or the
   final Python preflight, smoke and main commands shown in the README.

Required ML inputs:

| Relative path | Meaning |
|---|---|
| `data/02_model_input/core/final_daily_returns.csv` | Dated equity, gold and simulated bond returns plus controls |
| `data/02_model_input/bond_term_structure/final_bond_term_structure_daily.csv` | Dated yields, yield changes, slopes and curvature |

The final Python workflow creates a fresh run beneath the configured
`adaptive_ml_validation.run_output_root` (normally `results/ml_runs`). A reused
`--run-id` is refused. Its manifest stores arguments, selected package versions,
input/source hashes, timing checks and declared limitations. Manuscript table
exports are candidates for review inside the run directory.

## What this release does not reproduce

The public repository does not include the market-data snapshot or the literature
PDF library. Earlier empirical tables and the full R estimation pipeline have not
been recomputed as part of release preparation. The original R research guide and
method map retain references to local literature/metadata paths; consult the named
papers independently and rebuild metadata locally. Exact empirical reproduction
requires the same licensed data, transformations, R package versions and sample.
