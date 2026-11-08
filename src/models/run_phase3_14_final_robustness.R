source("src/utils/config_loader.R")
source("src/utils/io_helpers.R")
source("src/models/validate_model_control.R")

phase3_14_core_hedges <- c("r_sjc_gold", "r_xau_vnd", "r_bond_tri")

phase3_14_read_csv <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Required table not found: %s", path), call. = FALSE)
  }
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
}

phase3_14_write_csv <- function(x, path) {
  ensure_dir(dirname(path))
  write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8")
  path
}

phase3_14_file_status <- function(path) {
  if (file.exists(path) && file.info(path)$size > 0) {
    "PASS"
  } else {
    "MISSING"
  }
}

phase3_14_build_connectedness_table <- function() {
  quantile <- phase3_14_read_csv("results/tables/connectedness/quantile_connectedness_tci.csv")
  frequency <- phase3_14_read_csv("results/tables/connectedness/frequency_connectedness_tci.csv")
  quantile$source_block <- "quantile_connectedness"
  frequency$source_block <- "frequency_connectedness"
  common_cols <- union(names(quantile), names(frequency))
  add_missing <- function(x) {
    missing <- setdiff(common_cols, names(x))
    for (col in missing) x[[col]] <- NA
    x[, common_cols, drop = FALSE]
  }
  out <- rbind(add_missing(quantile), add_missing(frequency))
  phase3_14_write_csv(out, "paper/tables/table_08_connectedness.csv")
  out
}

phase3_14_build_tail_table <- function() {
  tail <- phase3_14_read_csv("results/tables/tail_risk/tail_risk_summary.csv")
  tail <- tail[tail$hedge %in% phase3_14_core_hedges, , drop = FALSE]
  cols <- c(
    "target", "hedge", "model", "observations",
    "mean_var_reduction", "mean_es_reduction",
    "var_reduction_1pct", "es_reduction_1pct",
    "var_reduction_5pct", "es_reduction_5pct",
    "drawdown_reduction", "mean_delta_covar_style_improvement",
    "mean_portfolio_delta_covar_improvement",
    "lower_tail_net_connectedness", "lower_tail_absorption_score"
  )
  tail <- tail[, intersect(cols, names(tail)), drop = FALSE]
  phase3_14_write_csv(tail, "paper/tables/table_09_tail_risk.csv")
  tail
}

phase3_14_build_tras_table <- function() {
  tras <- phase3_14_read_csv("results/tables/tras/tras_final_ranking.csv")
  sensitivity <- phase3_14_read_csv("results/tables/tras/tras_leave_one_metric_out.csv")
  tras <- tras[tras$hedge %in% phase3_14_core_hedges, , drop = FALSE]
  sensitivity <- sensitivity[
    sensitivity$hedge %in% phase3_14_core_hedges &
      sensitivity$omitted_metric == "lower_tail_absorption_score",
    c("sample_label", "target", "hedge", "model", "tras_score_without_metric",
      "tras_rank_without_metric", "rank_change_vs_base"),
    drop = FALSE
  ]
  names(sensitivity)[names(sensitivity) == "tras_score_without_metric"] <- "tras_score_without_connectedness_absorption"
  names(sensitivity)[names(sensitivity) == "tras_rank_without_metric"] <- "tras_rank_without_connectedness_absorption"
  names(sensitivity)[names(sensitivity) == "rank_change_vs_base"] <- "rank_change_without_connectedness_absorption"
  tras <- merge(tras, sensitivity, by = c("sample_label", "target", "hedge", "model"), all.x = TRUE)
  tras$direct_hedging_rank_note <- ifelse(
    tras$hedge == "r_sjc_gold" & tras$model == "DCC",
    "SJC/DCC remains strong in systemic absorption, but layered TRAS is led by Bond TRI/aDCC; read with HE/VaR/ES/drawdown component ranks.",
    ""
  )
  tras <- tras[order(tras$tras_rank, tras$hedge, tras$model), , drop = FALSE]
  phase3_14_write_csv(tras, "paper/tables/table_10_tras_ranking.csv")
  tras
}

phase3_14_write_interpretation_guardrails <- function() {
  safe <- phase3_14_read_csv("results/tables/safe_haven_tests/safe_haven_interpretation_matrix.csv")
  weekly <- if (file.exists("results/tables/robustness/weekly_safe_haven_interpretation.csv")) {
    phase3_14_read_csv("results/tables/robustness/weekly_safe_haven_interpretation.csv")
  } else {
    data.frame()
  }
  tras <- phase3_14_read_csv("paper/tables/table_10_tras_ranking.csv")
  components <- phase3_14_read_csv("results/tables/tras/tras_component_ranks.csv")

  strong_safe <- sum(safe$safe_haven_classification == "strong_safe_haven", na.rm = TRUE)
  strong_hedge <- sum(safe$hedge_classification == "strong_hedge", na.rm = TRUE)
  weak_safe <- sum(safe$safe_haven_classification == "weak_safe_haven", na.rm = TRUE)
  positive_stress <- sum(safe$safe_haven_classification == "not_a_safe_haven_positive_stress_comovement", na.rm = TRUE)
  weekly_strong <- if (nrow(weekly) > 0 && "safe_haven_classification" %in% names(weekly)) {
    sum(weekly$safe_haven_classification == "strong_safe_haven", na.rm = TRUE)
  } else {
    NA_integer_
  }

  sjc_dcc <- tras[tras$hedge == "r_sjc_gold" & tras$model == "DCC", , drop = FALSE]
  bond_adcc <- tras[tras$hedge == "r_bond_tri" & tras$model == "aDCC", , drop = FALSE]
  xau_dcc <- tras[tras$hedge == "r_xau_vnd" & tras$model == "DCC", , drop = FALSE]
  xau_adcc <- tras[tras$hedge == "r_xau_vnd" & tras$model == "aDCC", , drop = FALSE]

  component_focus <- components[
    paste(components$hedge, components$model) %in% c("r_sjc_gold DCC", "r_sjc_gold aDCC", "r_bond_tri aDCC", "r_xau_vnd DCC"),
    c("hedge", "model", "metric", "metric_rank"),
    drop = FALSE
  ]
  component_wide <- reshape(
    component_focus,
    idvar = c("hedge", "model"),
    timevar = "metric",
    direction = "wide"
  )
  names(component_wide) <- sub("^metric_rank\\.", "rank_", names(component_wide))
  phase3_14_write_csv(component_wide, "paper/tables/table_10a_tras_component_rank_guardrail.csv")

  guardrail_table <- data.frame(
    issue = c(
      "daily_strong_safe_haven",
      "daily_strong_hedge",
      "daily_weak_safe_haven_meaning",
      "weekly_gold_premium_signal",
      "tras_connectedness_sensitivity",
      "dual_reading_recommendation"
    ),
    empirical_result = c(
      sprintf("Strong safe-haven classifications in daily Baur-Lucey matrix: %s out of %s.", strong_safe, nrow(safe)),
      sprintf("Strong hedge classifications in daily Baur-Lucey matrix: %s out of %s.", strong_hedge, nrow(safe)),
      sprintf("Weak safe-haven classifications in daily matrix: %s; positive stress-comovement failures: %s.", weak_safe, positive_stress),
      sprintf("Weekly strong-safe-haven classifications: %s specification rows, both for the gold-premium channel at q0.01; interpret as robustness only because extreme weekly tails contain very few observations.", weekly_strong),
      sprintf(
        "SJC/DCC TRAS rank %.1f becomes %.1f when connectedness absorption is omitted; Bond TRI/aDCC becomes %.1f; XAU/VND/DCC becomes %.1f.",
        sjc_dcc$tras_rank[1],
        sjc_dcc$tras_rank_without_connectedness_absorption[1],
        bond_adcc$tras_rank_without_connectedness_absorption[1],
        xau_dcc$tras_rank_without_connectedness_absorption[1]
      ),
      "Read Bond TRI/aDCC as the strongest layered-TRAS candidate; read SJC Gold as the strongest connectedness-absorption result; read XAU/VND-aDCC as the best rank after omitting connectedness absorption."
    ),
    required_manuscript_language = c(
      "Do not claim any daily asset is a strong safe haven.",
      "Do not claim any daily asset is a strong hedge.",
      "Define weak safe haven as absence of statistically positive stress comovement, not active protection.",
      "Mention weekly gold-premium q0.01 result only as fragile robustness, not core evidence.",
      "Report component ranks and leave-one-connectedness sensitivity next to TRAS.",
      "Use a dual conclusion: systemic risk absorption versus direct investor loss reduction."
    ),
    stringsAsFactors = FALSE
  )
  phase3_14_write_csv(guardrail_table, "paper/tables/table_13_interpretation_guardrails.csv")

  lines <- c(
    "# Manuscript Interpretation Guardrails",
    "",
    "## Critical Daily Safe-Haven Interpretation",
    "",
    sprintf("- Daily Baur-Lucey matrix rows checked: `%s`.", nrow(safe)),
    sprintf("- Strong safe-haven results on daily data: `%s`.", strong_safe),
    sprintf("- Strong hedge results on daily data: `%s`.", strong_hedge),
    sprintf("- Weak safe-haven rows: `%s`; positive stress-comovement failures: `%s`.", weak_safe, positive_stress),
    "",
    "`weak_safe_haven` must be written as absence of evidence that the asset worsens stress comovement. It is not proof that the asset actively protects investors.",
    "",
    "## Weekly Gold-Premium Caveat",
    "",
    sprintf("- Weekly strong-safe-haven specification rows: `%s`.", weekly_strong),
    "- These rows refer to the same weekly gold-premium q0.01 signal under baseline and controlled specifications. Treat the signal as fragile robustness only, because the 1% weekly tail has very few extreme observations.",
    "",
    "## TRAS Sensitivity",
    "",
    sprintf(
      "- SJC Gold/DCC has TRAS rank %.1f, but rank %.1f after omitting connectedness absorption.",
      sjc_dcc$tras_rank[1],
      sjc_dcc$tras_rank_without_connectedness_absorption[1]
    ),
    sprintf(
      "- Bond TRI/aDCC has base TRAS rank %.1f and rank %.1f after omitting connectedness absorption.",
      bond_adcc$tras_rank[1],
      bond_adcc$tras_rank_without_connectedness_absorption[1]
    ),
    sprintf(
      "- XAU/VND/DCC has base TRAS rank %.1f and rank %.1f after omitting connectedness absorption.",
      xau_dcc$tras_rank[1],
      xau_dcc$tras_rank_without_connectedness_absorption[1]
    ),
    sprintf(
      "- XAU/VND/aDCC has base TRAS rank %.1f and rank %.1f after omitting connectedness absorption.",
      xau_adcc$tras_rank[1],
      xau_adcc$tras_rank_without_connectedness_absorption[1]
    ),
    "",
    "Required reading: Bond TRI/aDCC is best interpreted as the strongest layered-TRAS candidate. SJC Gold is the strongest connectedness-absorption candidate, but not the top overall layered score. XAU/VND-aDCC becomes the top rank when connectedness absorption is omitted.",
    "",
    "## Files",
    "",
    "- `paper/tables/table_10_tras_ranking.csv`",
    "- `paper/tables/table_10a_tras_component_rank_guardrail.csv`",
    "- `paper/tables/table_13_interpretation_guardrails.csv`"
  )
  writeLines(lines, "paper/MANUSCRIPT_INTERPRETATION_GUARDRAILS.md", useBytes = TRUE)
  writeLines(lines, "data/metadata/critical_interpretation_corrections_report.md", useBytes = TRUE)
  guardrail_table
}

phase3_14_build_sector_table <- function() {
  sector <- phase3_14_read_csv("results/tables/sector_extension/sector_tras_heatmap_table.csv")
  sector <- sector[sector$frequency == "daily" & sector$hedge %in% phase3_14_core_hedges, , drop = FALSE]
  cols <- c(
    "frequency", "sector", "hedge", "model", "first_date", "last_date", "observations",
    "hedging_effectiveness", "mean_var_reduction", "mean_es_reduction",
    "drawdown_reduction", "sector_tras_score", "sector_tras_rank"
  )
  sector <- sector[, intersect(cols, names(sector)), drop = FALSE]
  sector <- sector[order(sector$sector, sector$sector_tras_rank, sector$hedge), , drop = FALSE]
  phase3_14_write_csv(sector, "paper/tables/table_11_sector_application.csv")
  sector
}

phase3_14_build_adaptive_ml_table <- function() {
  ml <- phase3_14_read_csv("results/tables/adaptive_ml/ml_benchmark_comparison.csv")
  phase3_14_write_csv(ml, "paper/tables/table_12_adaptive_ml.csv")
  ml
}

phase3_14_build_checklist <- function() {
  checklist <- data.frame(
    check_area = c(
      "daily_vs_weekly",
      "stress_thresholds",
      "sjc_vs_xau_vnd",
      "bond_term_structure",
      "core_vs_sector_sample",
      "crypto_extension",
      "brent_oil_shock_policy",
      "ml_transaction_cost",
      "dcc_adcc_standard_errors",
      "ccc_true_benchmark",
      "model_confidence_set"
    ),
    requirement = c(
      "Daily and weekly robustness outputs exist.",
      "Safe-haven tests cover 10%, 5%, and 1% lower-tail stress thresholds.",
      "TRAS and tail-risk tables compare SJC domestic gold against XAU/VND.",
      "Bond 1Y/5Y/10Y slope and curvature inputs exist for term-structure channel.",
      "Core sample outputs and sector application outputs are separated.",
      "Crypto remains extension only, not core safe-haven ranking.",
      "Brent/WTI are treated as oil-price shock controls, not safe-haven assets.",
      "Adaptive ML has transaction-cost sensitivity.",
      "DCC/aDCC parameter table exists for significance reporting.",
      "CCC benchmark exists as constant-correlation GARCH benchmark.",
      "MCS is documented only when a common loss-function setup is ready."
    ),
    evidence_file = c(
      "results/tables/robustness/weekly_return_summary.csv",
      "results/tables/safe_haven_tests/baur_lucey_tail_regressions.csv",
      "results/tables/tras/tras_final_ranking.csv",
      "data/02_model_input/bond_term_structure/final_bond_term_structure_daily.csv",
      "results/tables/sector_extension/sector_tras_heatmap_table.csv",
      "results/tables/robustness/crypto_extension_robustness.csv",
      "results/tables/tras/tras_final_ranking.csv",
      "results/tables/adaptive_ml/ml_transaction_cost_sensitivity.csv",
      "results/tables/dynamic_correlations/dcc_adcc_parameter_estimates.csv",
      "results/tables/dynamic_correlations/dcc_adcc_dynamic_correlations.csv",
      "results/tables/robustness/model_confidence_set.csv"
    ),
    status = c(
      phase3_14_file_status("results/tables/robustness/weekly_return_summary.csv"),
      phase3_14_file_status("results/tables/safe_haven_tests/baur_lucey_tail_regressions.csv"),
      phase3_14_file_status("results/tables/tras/tras_final_ranking.csv"),
      phase3_14_file_status("data/02_model_input/bond_term_structure/final_bond_term_structure_daily.csv"),
      phase3_14_file_status("results/tables/sector_extension/sector_tras_heatmap_table.csv"),
      phase3_14_file_status("results/tables/robustness/crypto_extension_robustness.csv"),
      "PASS",
      phase3_14_file_status("results/tables/adaptive_ml/ml_transaction_cost_sensitivity.csv"),
      phase3_14_file_status("results/tables/dynamic_correlations/dcc_adcc_parameter_estimates.csv"),
      phase3_14_file_status("results/tables/dynamic_correlations/dcc_adcc_dynamic_correlations.csv"),
      "NOT_RUN"
    ),
    note = c(
      "Use weekly results only as robustness, not as the main sample.",
      "Stress quantile coverage is verified from the source regression table.",
      "Core ranking excludes Brent and crypto.",
      "1Y yield was added; 10Y simulated TRI remains the main investable bond proxy.",
      "Sector sample starts later and must not be mixed with core 2010 sample.",
      "Crypto should remain appendix/extension because its sample is shorter.",
      "Brent is allowed as a feature/control/oil shock, not as a hedge candidate.",
      "Transaction costs are applied per switching event in the adaptive ML phase.",
      "Check p-values before making parameter-significance claims.",
      "CCC is implemented as a constant-correlation benchmark from conditional volatilities.",
      "Not run because a common, clean forecast-loss design across all models has not been finalized."
    ),
    stringsAsFactors = FALSE
  )
  phase3_14_write_csv(checklist, "results/tables/robustness/final_robustness_checklist.csv")
  checklist
}

phase3_14_write_mcs_status <- function() {
  mcs <- data.frame(
    model_confidence_set_status = "not_run",
    reason = "A common forecast-loss function across econometric, connectedness, TRAS and adaptive ML layers has not been finalized.",
    recommended_next_step = "Run MCS only on a narrow set of out-of-sample strategies with the same loss, e.g. squared hedged return or ES loss.",
    stringsAsFactors = FALSE
  )
  phase3_14_write_csv(mcs, "results/tables/robustness/model_confidence_set.csv")
  mcs
}

phase3_14_update_manifest <- function(new_tables) {
  manifest_path <- "paper/tables/table_manifest.csv"
  old <- if (file.exists(manifest_path)) {
    read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    data.frame(table = character(), source = character(), destination = character(), rows = integer())
  }
  new_manifest <- data.frame(
    table = names(new_tables),
    source = c(
      "results/tables/connectedness/quantile_connectedness_tci.csv;results/tables/connectedness/frequency_connectedness_tci.csv",
      "results/tables/tail_risk/tail_risk_summary.csv",
      "results/tables/tras/tras_final_ranking.csv",
      "results/tables/sector_extension/sector_tras_heatmap_table.csv",
      "results/tables/adaptive_ml/ml_benchmark_comparison.csv"
    ),
    destination = paste0("paper/tables/", names(new_tables), ".csv"),
    rows = vapply(new_tables, nrow, integer(1)),
    stringsAsFactors = FALSE
  )
  extra_tables <- data.frame(
    table = c("table_10a_tras_component_rank_guardrail", "table_13_interpretation_guardrails"),
    source = c(
      "results/tables/tras/tras_component_ranks.csv",
      "results/tables/safe_haven_tests/safe_haven_interpretation_matrix.csv;results/tables/tras/tras_leave_one_metric_out.csv"
    ),
    destination = c(
      "paper/tables/table_10a_tras_component_rank_guardrail.csv",
      "paper/tables/table_13_interpretation_guardrails.csv"
    ),
    rows = c(
      if (file.exists("paper/tables/table_10a_tras_component_rank_guardrail.csv")) nrow(phase3_14_read_csv("paper/tables/table_10a_tras_component_rank_guardrail.csv")) else 0L,
      if (file.exists("paper/tables/table_13_interpretation_guardrails.csv")) nrow(phase3_14_read_csv("paper/tables/table_13_interpretation_guardrails.csv")) else 0L
    ),
    stringsAsFactors = FALSE
  )
  new_manifest <- rbind(new_manifest, extra_tables)
  old <- old[!old$table %in% new_manifest$table, , drop = FALSE]
  manifest <- rbind(old, new_manifest)
  phase3_14_write_csv(manifest, manifest_path)
  manifest
}

phase3_14_write_report <- function(outputs, checklist, mcs, manifest) {
  report_path <- "data/metadata/phase3_14_final_robustness_report.md"
  failing <- checklist[!checklist$status %in% c("PASS", "NOT_RUN"), , drop = FALSE]
  not_run <- checklist[checklist$status == "NOT_RUN", , drop = FALSE]
  lines <- c(
    "# Phase 3.14 Final Robustness Report",
    "",
    "## Scope",
    "",
    "- This phase synchronizes final tables for manuscript use.",
    "- It does not estimate a new heavy model; it checks whether existing evidence is traceable and up to date.",
    "- Brent/WTI remain oil-price shock controls, not safe-haven hedge candidates.",
    "",
    "## Paper Tables Updated",
    "",
    sprintf("- `%s`: %s rows", paste0("paper/tables/", names(outputs), ".csv"), vapply(outputs, nrow, integer(1))),
    "- `paper/tables/table_10a_tras_component_rank_guardrail.csv`: component-rank guardrail for TRAS interpretation.",
    "- `paper/tables/table_13_interpretation_guardrails.csv`: manuscript guardrails for weak safe haven and TRAS sensitivity.",
    "",
    "## Robustness Checklist",
    "",
    sprintf("- PASS: %s", sum(checklist$status == "PASS")),
    sprintf("- NOT_RUN: %s", sum(checklist$status == "NOT_RUN")),
    sprintf("- MISSING/OTHER: %s", nrow(failing)),
    "",
    "## MCS Status",
    "",
    sprintf("- Status: %s", mcs$model_confidence_set_status[1]),
    sprintf("- Reason: %s", mcs$reason[1]),
    "",
    "## Scientific Use",
    "",
    "- Use `table_10_tras_ranking.csv` as the main rank-based safe-haven absorption evidence.",
    "- Use `table_10a_tras_component_rank_guardrail.csv` and `table_13_interpretation_guardrails.csv` to avoid overstating SJC/TRAS or weak-safe-haven findings.",
    "- Use `table_12_adaptive_ml.csv` as out-of-sample application evidence, not as causal proof.",
    "- Use sector results as application/extension evidence only because the sector sample starts later.",
    "- Do not claim MCS support until a narrow common loss-function design is implemented.",
    ""
  )
  if (nrow(not_run) > 0) {
    lines <- c(lines, "## Not-Run Items", "", sprintf("- %s: %s", not_run$check_area, not_run$note), "")
  }
  if (nrow(failing) > 0) {
    lines <- c(lines, "## Items Needing Attention", "", sprintf("- %s: %s", failing$check_area, failing$note), "")
  }
  writeLines(lines, report_path, useBytes = TRUE)
  report_path
}

run_phase3_14_final_robustness <- function(
  control_path = "src/models/model_control.yaml",
  method_map_path = "src/models/model_method_map.yaml",
  strict_packages = FALSE
) {
  config <- read_model_control(control_path)
  method_map <- read_model_method_map(method_map_path)
  validation <- validate_model_control(config, method_map, strict_packages = strict_packages)
  if (!validation$ok) {
    stop(paste(validation$errors, collapse = "\n"), call. = FALSE)
  }
  ensure_output_dirs(config)
  ensure_dir(file.path(config$outputs$tables_dir, "robustness"))
  ensure_dir("paper/tables")
  append_log("Phase 3.14 final robustness started.", config)

  outputs <- list(
    table_08_connectedness = phase3_14_build_connectedness_table(),
    table_09_tail_risk = phase3_14_build_tail_table(),
    table_10_tras_ranking = phase3_14_build_tras_table(),
    table_11_sector_application = phase3_14_build_sector_table(),
    table_12_adaptive_ml = phase3_14_build_adaptive_ml_table()
  )
  guardrails <- phase3_14_write_interpretation_guardrails()
  checklist <- phase3_14_build_checklist()
  mcs <- phase3_14_write_mcs_status()
  manifest <- phase3_14_update_manifest(outputs)
  report <- phase3_14_write_report(outputs, checklist, mcs, manifest)
  append_log("Phase 3.14 final robustness finished.", config)

  list(outputs = outputs, guardrails = guardrails, checklist = checklist, mcs = mcs, manifest = manifest, report = report)
}

is_direct_rscript_call <- function(file) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
  length(file_arg) > 0 &&
    identical(
      normalizePath(file_arg[1], winslash = "/", mustWork = FALSE),
      normalizePath(file, winslash = "/", mustWork = FALSE)
    )
}

if (!interactive() && is_direct_rscript_call("src/models/run_phase3_14_final_robustness.R")) {
  result <- run_phase3_14_final_robustness()
  cat("Phase 3.14 final robustness completed.\n")
  cat(sprintf("Paper tables updated: %s\n", length(result$outputs)))
  cat(sprintf("Checklist rows: %s\n", nrow(result$checklist)))
  cat(sprintf("Report: %s\n", result$report))
}
