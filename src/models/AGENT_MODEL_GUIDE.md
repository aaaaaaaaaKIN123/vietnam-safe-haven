# Agent Model Guide

This folder contains the model pipeline for the Vietnamese safe-haven and hedging research project.

## First Rule

Do not hard-code model choices inside scripts. Change model parameters in:

```text
src/models/model_control.yaml
```

Use this file for sample dates, asset lists, GARCH specs, DCC/ADCC choices, stress quantiles, output switches and robustness toggles.

## Method Integrity Rule

Before adding or changing a model, check:

```text
src/models/model_method_map.yaml
```

Each model must map to:

- at least one academic source from `01_literature/citation_matrix/`;
- the package or custom module used;
- its role in the paper: main model, benchmark, robustness or extension.

## Main Model Logic

The core estimation path is R-based:

1. `rugarch` for univariate GARCH, GJR-GARCH, EGARCH and Student-t errors.
2. `rmgarch` for CCC, DCC and ADCC/aDCC.
3. Custom R modules for Kroner-Sultan hedge ratios and Baur-Lucey safe-haven tests.
4. `MSGARCH` only as robustness, not as the main model.
5. Python scripts are optional checks only.

## Run Order

Prefer phase-specific runners while Phase 3 is being built and validated. The
integrated pipeline is available for later end-to-end reruns after Phase 3.3+
has been validated.

Run the integrated pipeline from the project root only after checking the
phase-specific outputs:

```r
source("src/models/run_model_pipeline.R")
run_model_pipeline()
```

The current Phase 3 model path uses prepared returns from:

```text
src/models/model_control.yaml -> data.returns_input
```

Current value: `data/02_model_input/core/final_daily_returns.csv`.

It writes outputs to `results/`.

On the current Windows workspace, `Rscript` may not be available in PATH. Use
the absolute runtime path when running from PowerShell:

```powershell
& 'C:\Program Files\R\R-4.5.2\bin\Rscript.exe' 'src\models\validate_model_control.R'
& 'C:\Program Files\R\R-4.5.2\bin\Rscript.exe' 'src\models\run_model_pipeline.R'
```

## Phase-Specific Runners

Use the phase-specific runner when you need to execute only one stage:

```powershell
& 'C:\Program Files\R\R-4.5.2\bin\Rscript.exe' 'src\models\run_phase3_2_univariate_garch.R'
& 'C:\Program Files\R\R-4.5.2\bin\Rscript.exe' 'src\models\run_phase3_3_dcc_adcc.R'
```

Phase 3.2 writes:

```text
results/tables/model_selection/univariate_garch_selection.csv
results/tables/model_selection/univariate_garch_best_by_bic.csv
results/tables/volatility_estimates/
data/metadata/phase3_2_univariate_garch_report.md
```

Phase 3.3 should consume the Phase 3.2 best-spec table when building DCC/ADCC
univariate specifications:

```text
results/tables/model_selection/univariate_garch_best_by_bic.csv
```

Expected Phase 3.3 output locations:

```text
results/tables/dynamic_correlations/dcc_adcc_dynamic_correlations.csv
results/tables/dynamic_correlations/dcc_adcc_summary.csv
results/tables/dynamic_correlations/dcc_adcc_parameter_estimates.csv
results/tables/dynamic_correlations/dcc_adcc_model_fit_statistics.csv
results/diagnostics/residual/dcc_adcc_residual_diagnostics.csv
results/tables/model_selection/univariate_garch_parameter_estimates.csv
results/tables/model_selection/univariate_garch_fit_statistics.csv
results/figures/dynamic_correlations/
results/model_logs/convergence_warnings/dcc_adcc_warnings.csv
data/metadata/phase3_3_dcc_adcc_report.md
```

The constant-correlation benchmark is labelled `CCC` in outputs. It is not
estimated via `rmgarch::dccspec(model = "CCC")`, because the local rmgarch
interface supports `DCC`, `aDCC` and `FDCC`, not `CCC`. The project builds
CCC-GARCH directly from univariate GARCH conditional variances plus the
constant correlation of standardized residuals, consistent with the
Bollerslev-style CCC benchmark.

For CCC/DCC/aDCC results, always inspect both the dynamic-correlation table and
the fit-statistics table. The fit-statistics table stores `dcc_alpha`,
`dcc_beta`, optional `dcc_gamma`, and `dcc_persistence`; these are the primary
dynamic-correlation parameters needed for scientific reporting. DCC/aDCC fits
should use `dccfit(eval.se = TRUE)` so standard errors, t-values and p-values
are available when the optimizer can compute them.

Phase 3.4 consumes the Phase 3.3 dynamic-correlation output without refitting
DCC/aDCC models:

```text
src/models/run_phase3_4_hedging_effectiveness.R
results/tables/hedge_ratios/dynamic_hedge_ratios.csv
results/tables/hedge_ratios/hedge_ratio_summary.csv
results/tables/hedge_ratios/dynamic_min_variance_weights.csv
results/tables/hedge_ratios/min_variance_weight_summary.csv
results/tables/hedging_effectiveness/hedging_effectiveness_summary.csv
results/figures/hedge_ratios/
data/metadata/phase3_4_hedging_report.md
```

Use `d_policy_rate`, `d_interbank_rate`, and yield changes as controls or
state variables in later safe-haven/robustness phases, not as hedge assets for
Kroner-Sultan hedge-ratio calculations.

Phase 3.4b is the robustness layer for Phase 3.4:

```text
src/models/run_phase3_4b_hedging_robustness.R
results/tables/hedging_effectiveness/hedging_effectiveness_common_sample.csv
results/tables/hedging_effectiveness/hedging_effectiveness_core_sample.csv
results/tables/hedging_effectiveness/hedging_effectiveness_capped_sensitivity.csv
results/tables/hedging_effectiveness/hedging_effectiveness_by_state.csv
data/metadata/phase3_4b_hedging_robustness_report.md
```

Use the common-sample tables when ranking hedge assets with different data
availability. Use the state-dependent table only as descriptive robustness;
Phase 3.5 remains the formal safe-haven test.

Phase 3.5 runs Baur-Lucey style safe-haven regressions with HAC/Newey-West
standard errors and a gold-premium channel test:

```text
src/models/run_phase3_5_safe_haven.R
results/tables/safe_haven_tests/baur_lucey_tail_regressions.csv
results/tables/safe_haven_tests/baur_lucey_model_summary.csv
results/tables/safe_haven_tests/safe_haven_interpretation_matrix.csv
results/tables/safe_haven_tests/gold_premium_channel_tests.csv
results/tables/safe_haven_tests/gold_premium_interpretation.csv
data/metadata/phase3_5_safe_haven_report.md
```

Interpret `strong_safe_haven` only when the stress-state slope
`beta_market + gamma_interaction` is negative and statistically significant.
Gold premium is a domestic-market-friction channel, not a hedge asset.

Phase 3.6 is the robustness/sensitivity synthesis layer. It does not refit
DCC/aDCC models; it summarizes weekly safe-haven checks, core asset contrast,
oil proxy robustness, crypto extension robustness, and stress-regime HE:

```text
src/models/run_phase3_6_robustness.R
results/tables/robustness/weekly_return_summary.csv
results/tables/robustness/weekly_safe_haven_interpretation.csv
results/tables/robustness/core_asset_contrast_matrix.csv
results/tables/robustness/oil_proxy_robustness.csv
results/tables/robustness/crypto_extension_robustness.csv
results/tables/robustness/stress_regime_summary.csv
results/tables/robustness/robustness_synthesis_matrix.csv
data/metadata/phase3_6_robustness_report.md
```

Do not promote crypto or oil proxy findings to the main conclusion without
separating their sample and economic role. Treat weekly `r_gold_premium`
signals as a domestic premium channel, not as a tradeable hedge asset.

Phase 3.7 exports the manuscript-facing table and figure set. It does not
replace the detailed model outputs in `results/`; it selects and standardizes
the clearest artefacts for paper drafting:

```text
src/models/run_phase3_7_paper_ready.R
paper/tables/table_01_descriptive_statistics.csv
paper/tables/table_02_garch_selection.csv
paper/tables/table_03_dcc_adcc_summary.csv
paper/tables/table_04_hedging_effectiveness.csv
paper/tables/table_05_safe_haven_regression.csv
paper/tables/table_06_robustness_summary.csv
paper/tables/table_07_markov_switching_robustness.csv
paper/tables/table_manifest.csv
paper/figures/figure_manifest.csv
data/metadata/phase3_7_paper_ready_report.md
```

Use `paper/tables/` and `paper/figures/` when drafting the manuscript. Use the
source paths recorded in each manifest to trace every table or figure back to
the underlying model output.

Phase 3.8 adds Markov-switching robustness with MSGARCH. It estimates a
two-regime model for `r_vnindex`, labels low/high-volatility states, and
summarizes dynamic hedge performance by regime:

```text
src/models/run_phase3_8_markov_switching.R
results/tables/markov_switching/msgarch_fit_summary.csv
results/tables/markov_switching/msgarch_state_probabilities.csv
results/tables/markov_switching/msgarch_regime_summary.csv
results/tables/markov_switching/msgarch_hedging_by_regime.csv
results/tables/markov_switching/markov_switching_synthesis.csv
results/figures/markov_switching/figure_09_msgarch_high_volatility_probability.png
data/metadata/phase3_8_markov_switching_report.md
```

Use Phase 3.8 as state-dependent robustness only. Do not use MSGARCH regime
probabilities as a substitute for Baur-Lucey safe-haven classification.

For the latest model-architecture synchronization note, read:

```text
data/metadata/phase3_sync_alignment_report.md
```

## Do Not

- Do not overwrite raw data.
- Do not manually edit model-input CSV files.
- Do not call the simulated bond TRI an official index.
- Do not call Brent/WTI controls "structural oil shocks" unless the project adds a structural shock decomposition.
- Do not make BTC/ETH the core model unless the research design is explicitly changed.
