source("src/utils/config_loader.R")
source("src/utils/io_helpers.R")
source("src/models/validate_model_control.R")

paper_read_csv <- function(path) {
  full_path <- project_path(path)
  if (!file.exists(full_path)) {
    stop(sprintf("Required paper-ready source file not found: %s", full_path), call. = FALSE)
  }
  utils::read.csv(full_path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
}

paper_write_csv <- function(x, path) {
  write_csv_output(x, path, overwrite = TRUE)
}

paper_select_existing <- function(x, columns) {
  x[, intersect(columns, names(x)), drop = FALSE]
}

paper_copy_figure <- function(source, destination) {
  source_path <- project_path(source)
  destination_path <- project_path(destination)
  if (!file.exists(source_path)) {
    return(data.frame(
      source = source,
      destination = destination,
      copied = FALSE,
      reason = "source_not_found",
      stringsAsFactors = FALSE
    ))
  }
  ensure_dir(dirname(destination_path))
  ok <- file.copy(source_path, destination_path, overwrite = TRUE)
  data.frame(
    source = source,
    destination = destination,
    copied = isTRUE(ok),
    reason = if (isTRUE(ok)) "ok" else "copy_failed",
    stringsAsFactors = FALSE
  )
}

paper_upsert_manifest <- function(new_rows, path = "paper/tables/table_manifest.csv") {
  full_path <- project_path(path)
  if (file.exists(full_path)) {
    existing <- utils::read.csv(full_path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
    names(existing) <- sub("^\ufeff", "", names(existing))
    existing <- existing[!existing$table %in% new_rows$table, , drop = FALSE]
    combined <- rbind(existing, new_rows)
  } else {
    combined <- new_rows
  }

  table_order <- c(
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
    "table_19_ml_final_validation"
  )
  combined$.order <- match(combined$table, table_order)
  combined$.order[is.na(combined$.order)] <- length(table_order) + seq_len(sum(is.na(combined$.order)))
  combined <- combined[order(combined$.order), setdiff(names(combined), ".order"), drop = FALSE]
  paper_write_csv(combined, path)
  combined
}

create_table_01_descriptive <- function(config) {
  desc_src <- "results/diagnostics/descriptive_statistics/daily_return_descriptive_stats.csv"
  jb_src <- "results/diagnostics/normality/jarque_bera_tests.csv"
  adf_src <- "results/diagnostics/unit_root/adf_return_tests.csv"
  arch_src <- "results/diagnostics/arch_effect/arch_lm_tests.csv"
  desc <- paper_read_csv(desc_src)
  jb <- paper_read_csv(jb_src)
  adf <- paper_read_csv(adf_src)
  arch <- paper_read_csv(arch_src)

  jb <- paper_select_existing(jb, c("variable", "jarque_bera", "p_value"))
  names(jb)[names(jb) == "jarque_bera"] <- "jb_stat"
  names(jb)[names(jb) == "p_value"] <- "jb_p_value"

  adf <- paper_select_existing(adf, c("variable", "adf_t_stat", "p_value_mackinnon", "reject_5pct"))
  names(adf)[names(adf) == "adf_t_stat"] <- "adf_stat"
  names(adf)[names(adf) == "p_value_mackinnon"] <- "adf_p_value"
  names(adf)[names(adf) == "reject_5pct"] <- "adf_reject_5pct"

  arch <- paper_select_existing(arch, c("variable", "lm_stat", "p_value_chi2", "lags", "interpretation"))
  names(arch)[names(arch) == "lm_stat"] <- "arch_lm_stat"
  names(arch)[names(arch) == "p_value_chi2"] <- "arch_lm_p_value"
  names(arch)[names(arch) == "lags"] <- "arch_lags"
  names(arch)[names(arch) == "interpretation"] <- "arch_interpretation"

  df <- merge(desc, jb, by = "variable", all.x = TRUE)
  df <- merge(df, adf, by = "variable", all.x = TRUE)
  df <- merge(df, arch, by = "variable", all.x = TRUE)
  keep <- c(
    "r_vnindex", "r_sjc_gold", "r_xau_vnd", "r_bond_tri", "r_gold_premium",
    "r_brent", "r_wti", "r_btc", "r_eth", "d_vix", "d_interbank_rate",
    "d_policy_rate", "d_vn10y_yield", "d_vn5y_yield"
  )
  df <- df[df$variable %in% keep, , drop = FALSE]
  df <- paper_select_existing(df, c(
    "variable", "observations", "mean", "std", "min", "max",
    "skewness", "kurtosis", "jb_stat", "jb_p_value",
    "adf_stat", "adf_p_value", "adf_reject_5pct",
    "arch_lm_stat", "arch_lm_p_value", "arch_lags", "arch_interpretation"
  ))
  df <- df[match(intersect(keep, df$variable), df$variable), , drop = FALSE]
  dst <- "paper/tables/table_01_descriptive_statistics.csv"
  paper_write_csv(df, dst)
  src <- paste(desc_src, jb_src, adf_src, arch_src, sep = ";")
  data.frame(table = "table_01_descriptive_statistics", source = src, destination = dst, rows = nrow(df), stringsAsFactors = FALSE)
}

create_table_02_garch <- function(config) {
  src <- "results/tables/model_selection/univariate_garch_best_by_bic.csv"
  df <- paper_read_csv(src)
  df <- paper_select_existing(df, c(
    "asset", "model", "distribution", "observations", "first_date", "last_date",
    "convergence", "AIC", "BIC", "selected_criterion", "criterion_value"
  ))
  dst <- "paper/tables/table_02_garch_selection.csv"
  paper_write_csv(df, dst)
  data.frame(table = "table_02_garch_selection", source = src, destination = dst, rows = nrow(df), stringsAsFactors = FALSE)
}

create_table_03_dcc <- function(config) {
  src <- "results/tables/dynamic_correlations/dcc_adcc_summary.csv"
  df <- paper_read_csv(src)
  df <- paper_select_existing(df, c(
    "target", "hedge", "model", "observations", "first_date", "last_date",
    "mean_correlation", "sd_correlation", "min_correlation", "max_correlation",
    "share_negative_correlation", "mean_conditional_covariance", "convergence"
  ))
  dst <- "paper/tables/table_03_dcc_adcc_summary.csv"
  paper_write_csv(df, dst)
  data.frame(table = "table_03_dcc_adcc_summary", source = src, destination = dst, rows = nrow(df), stringsAsFactors = FALSE)
}

create_table_04_hedging <- function(config) {
  core_src <- "results/tables/hedging_effectiveness/hedging_effectiveness_core_sample.csv"
  common_src <- "results/tables/hedging_effectiveness/hedging_effectiveness_common_sample.csv"
  core <- paper_read_csv(core_src)
  common <- paper_read_csv(common_src)
  core$paper_sample <- "core_common_sample"
  common$paper_sample <- "all_asset_common_sample"
  df <- rbind(core, common)
  df <- paper_select_existing(df, c(
    "paper_sample", "target", "hedge", "model", "observations", "first_date", "last_date",
    "hedging_effectiveness", "volatility_reduction", "mean_hedge_ratio",
    "min_hedge_ratio", "max_hedge_ratio"
  ))
  dst <- "paper/tables/table_04_hedging_effectiveness.csv"
  paper_write_csv(df, dst)
  data.frame(table = "table_04_hedging_effectiveness", source = paste(core_src, common_src, sep = ";"), destination = dst, rows = nrow(df), stringsAsFactors = FALSE)
}

create_table_05_safe_haven <- function(config) {
  main_src <- "results/tables/safe_haven_tests/safe_haven_interpretation_matrix.csv"
  premium_src <- "results/tables/safe_haven_tests/gold_premium_interpretation.csv"
  main <- paper_read_csv(main_src)
  premium <- paper_read_csv(premium_src)
  main <- main[main$model_spec == "controlled", , drop = FALSE]
  premium <- premium[premium$model_spec == "controlled", , drop = FALSE]
  df <- rbind(main, premium)
  df <- paper_select_existing(df, c(
    "asset_role", "hedge", "model_spec", "stress_quantile", "observations",
    "first_date", "last_date", "beta_market", "beta_market_p_value",
    "stress_slope_beta_plus_gamma", "stress_slope_p_value",
    "hedge_classification", "safe_haven_classification", "final_interpretation",
    "controls_used"
  ))
  dst <- "paper/tables/table_05_safe_haven_regression.csv"
  paper_write_csv(df, dst)
  data.frame(table = "table_05_safe_haven_regression", source = paste(main_src, premium_src, sep = ";"), destination = dst, rows = nrow(df), stringsAsFactors = FALSE)
}

create_table_06_robustness <- function(config) {
  src <- "results/tables/robustness/robustness_synthesis_matrix.csv"
  df <- paper_read_csv(src)
  dst <- "paper/tables/table_06_robustness_summary.csv"
  paper_write_csv(df, dst)
  data.frame(table = "table_06_robustness_summary", source = src, destination = dst, rows = nrow(df), stringsAsFactors = FALSE)
}

create_table_07_markov_switching <- function(config) {
  src <- "results/tables/markov_switching/markov_switching_synthesis.csv"
  df <- paper_read_csv(src)
  dst <- "paper/tables/table_07_markov_switching_robustness.csv"
  paper_write_csv(df, dst)
  data.frame(table = "table_07_markov_switching_robustness", source = src, destination = dst, rows = nrow(df), stringsAsFactors = FALSE)
}

copy_paper_figures <- function(config) {
  figures <- data.frame(
    source = c(
      "results/figures/data_quality_bw/correlation/figure_08_core_correlation_heatmap.png",
      "results/figures/data_quality_bw/correlation/figure_08_core_correlation_heatmap.svg",
      "results/figures/data_quality_bw/stress_timeline/figure_07_vnindex_stress_timeline.png",
      "results/figures/data_quality_bw/stress_timeline/figure_07_vnindex_stress_timeline.svg",
      "results/figures/dynamic_correlations/r_sjc_gold_dynamic_correlation.png",
      "results/figures/dynamic_correlations/r_sjc_gold_dynamic_correlation.svg",
      "results/figures/dynamic_correlations/r_xau_vnd_dynamic_correlation.png",
      "results/figures/dynamic_correlations/r_xau_vnd_dynamic_correlation.svg",
      "results/figures/dynamic_correlations/r_bond_tri_dynamic_correlation.png",
      "results/figures/dynamic_correlations/r_bond_tri_dynamic_correlation.svg",
      "results/figures/hedge_ratios/r_sjc_gold_hedge_ratio.png",
      "results/figures/hedge_ratios/r_sjc_gold_hedge_ratio.svg",
      "results/figures/hedge_ratios/r_xau_vnd_hedge_ratio.png",
      "results/figures/hedge_ratios/r_xau_vnd_hedge_ratio.svg",
      "results/figures/hedge_ratios/r_bond_tri_hedge_ratio.png",
      "results/figures/hedge_ratios/r_bond_tri_hedge_ratio.svg",
      "results/figures/markov_switching/figure_09_msgarch_high_volatility_probability.png",
      "results/figures/markov_switching/figure_09_msgarch_high_volatility_probability.svg"
    ),
    destination = c(
      "paper/figures/figure_01_core_correlation_heatmap.png",
      "paper/figures/figure_01_core_correlation_heatmap.svg",
      "paper/figures/figure_02_vnindex_stress_timeline.png",
      "paper/figures/figure_02_vnindex_stress_timeline.svg",
      "paper/figures/figure_03a_sjc_dynamic_correlation.png",
      "paper/figures/figure_03a_sjc_dynamic_correlation.svg",
      "paper/figures/figure_03b_xau_vnd_dynamic_correlation.png",
      "paper/figures/figure_03b_xau_vnd_dynamic_correlation.svg",
      "paper/figures/figure_03c_bond_tri_dynamic_correlation.png",
      "paper/figures/figure_03c_bond_tri_dynamic_correlation.svg",
      "paper/figures/figure_04a_sjc_hedge_ratio.png",
      "paper/figures/figure_04a_sjc_hedge_ratio.svg",
      "paper/figures/figure_04b_xau_vnd_hedge_ratio.png",
      "paper/figures/figure_04b_xau_vnd_hedge_ratio.svg",
      "paper/figures/figure_04c_bond_tri_hedge_ratio.png",
      "paper/figures/figure_04c_bond_tri_hedge_ratio.svg",
      "paper/figures/figure_05_msgarch_high_volatility_probability.png",
      "paper/figures/figure_05_msgarch_high_volatility_probability.svg"
    ),
    stringsAsFactors = FALSE
  )
  extra_figures <- data.frame(
    source = c(
      "results/figures/connectedness/figure_phase3_9_rolling_quantile_tci.png",
      "results/figures/connectedness/figure_phase3_9_rolling_quantile_tci.svg",
      "results/figures/connectedness/figure_phase3_9_lower_tail_net.png",
      "results/figures/connectedness/figure_phase3_9_lower_tail_net.svg",
      "results/figures/connectedness/figure_phase3_9_lower_tail_network.png",
      "results/figures/connectedness/figure_phase3_9_lower_tail_network.svg",
      "results/figures/connectedness/figure_phase3_9_lower_tail_network_package_native.png",
      "results/figures/connectedness/figure_phase3_9_lower_tail_network_package_native.svg",
      "results/figures/connectedness/figure_phase3_9_frequency_tci.png",
      "results/figures/connectedness/figure_phase3_9_frequency_tci.svg",
      "results/figures/tail_risk/figure_phase3_10_tail_risk_summary.png",
      "results/figures/tail_risk/figure_phase3_10_tail_risk_summary.svg",
      "results/figures/tail_risk/figure_phase3_10_es5_reduction.png",
      "results/figures/tail_risk/figure_phase3_10_es5_reduction.svg",
      "results/figures/tras/tras_ranking_bar.png",
      "results/figures/tras/tras_ranking_bar.svg",
      "results/figures/tras/tras_heatmap.png",
      "results/figures/tras/tras_heatmap.svg",
      "results/figures/sector_extension/sector_tras_heatmap.png",
      "results/figures/sector_extension/sector_tras_heatmap.svg",
      "results/figures/sector_extension/sector_best_hedge_by_sector.png",
      "results/figures/sector_extension/sector_best_hedge_by_sector.svg",
      "results/figures/adaptive_ml_upgrade/ml_upgrade_xgboost_shap_importance.png",
      "results/figures/adaptive_ml_upgrade/ml_upgrade_xgboost_shap_importance.svg",
      "results/figures/adaptive_ml_final/ml_final_strategy_total_return.png",
      "results/figures/adaptive_ml_final/ml_final_strategy_total_return.svg"
    ),
    destination = c(
      "paper/figures/figure_06_rolling_quantile_connectedness_tci.png",
      "paper/figures/figure_06_rolling_quantile_connectedness_tci.svg",
      "paper/figures/figure_07_lower_tail_net_connectedness.png",
      "paper/figures/figure_07_lower_tail_net_connectedness.svg",
      "paper/figures/figure_07b_lower_tail_connectedness_network.png",
      "paper/figures/figure_07b_lower_tail_connectedness_network.svg",
      "paper/figures/figure_07c_lower_tail_connectedness_network_package_native.png",
      "paper/figures/figure_07c_lower_tail_connectedness_network_package_native.svg",
      "paper/figures/figure_08_frequency_connectedness_tci.png",
      "paper/figures/figure_08_frequency_connectedness_tci.svg",
      "paper/figures/figure_09_tail_risk_summary.png",
      "paper/figures/figure_09_tail_risk_summary.svg",
      "paper/figures/figure_10_es5_reduction.png",
      "paper/figures/figure_10_es5_reduction.svg",
      "paper/figures/figure_11_tras_ranking_bar.png",
      "paper/figures/figure_11_tras_ranking_bar.svg",
      "paper/figures/figure_12_tras_heatmap.png",
      "paper/figures/figure_12_tras_heatmap.svg",
      "paper/figures/figure_13_sector_tras_heatmap.png",
      "paper/figures/figure_13_sector_tras_heatmap.svg",
      "paper/figures/figure_14_sector_best_hedge.png",
      "paper/figures/figure_14_sector_best_hedge.svg",
      "paper/figures/figure_15_ml_xgboost_shap_importance.png",
      "paper/figures/figure_15_ml_xgboost_shap_importance.svg",
      "paper/figures/figure_16_ml_final_strategy_total_return.png",
      "paper/figures/figure_16_ml_final_strategy_total_return.svg"
    ),
    stringsAsFactors = FALSE
  )
  figures <- rbind(figures, extra_figures)
  results <- lapply(seq_len(nrow(figures)), function(i) {
    paper_copy_figure(figures$source[i], figures$destination[i])
  })
  out <- do.call(rbind, results)
  paper_write_csv(out, "paper/figures/figure_manifest.csv")
  out
}

write_phase3_7_report <- function(table_manifest, figure_manifest) {
  report_path <- project_path("data/metadata/phase3_7_paper_ready_report.md")
  lines <- c(
    "# Phase 3.7 Paper-Ready Tables And Figures Report",
    "",
    "## Mục tiêu",
    "",
    "Phase 3.7 chọn và chuẩn hóa các bảng/figure quan trọng nhất từ pipeline định lượng để dùng trong bản thảo. Các bảng trong `paper/tables` là bản paper-ready; output thô vẫn nằm trong `results`.",
    "",
    "## Table Outputs",
    "",
    sprintf("- Paper-ready table files: `%s`", nrow(table_manifest)),
    sprintf("- Total table rows: `%s`", sum(table_manifest$rows)),
    "",
    "## Figure Outputs",
    "",
    sprintf("- Figure files copied: `%s`", sum(figure_manifest$copied)),
    sprintf("- Figure copy failures: `%s`", sum(!figure_manifest$copied)),
    "",
    "## Notes",
    "",
    "- Table 01 now merges descriptive statistics, Jarque-Bera normality, ADF stationarity and ARCH-LM volatility-clustering diagnostics. It must not be replaced by a pointer row.",
    "- Table 04 includes both core-common and all-asset-common hedging effectiveness; use the core block for SJC/XAU-VND/Bond TRI conclusions.",
    "- Table 05 includes controlled Baur-Lucey results plus the gold-premium channel.",
    "- Table 07 includes Markov-switching robustness; use it for state-dependent hedging discussion, not as a replacement for DCC/aDCC.",
    "- Figure captions and manuscript notes should be written outside the image files.",
    "",
    "## Output Folders",
    "",
    "- `paper/tables/`",
    "- `paper/figures/`"
  )
  con <- file(report_path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw("\ufeff"), con)
  writeLines(enc2utf8(lines), con, useBytes = TRUE)
  invisible(report_path)
}

run_phase3_7_paper_ready <- function(
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

  ensure_dir("paper/tables")
  ensure_dir("paper/figures")
  append_log("Phase 3.7 paper-ready export started.", config)

  table_manifest <- do.call(rbind, list(
    create_table_01_descriptive(config),
    create_table_02_garch(config),
    create_table_03_dcc(config),
    create_table_04_hedging(config),
    create_table_05_safe_haven(config),
    create_table_06_robustness(config),
    create_table_07_markov_switching(config)
  ))
  table_manifest <- paper_upsert_manifest(table_manifest, "paper/tables/table_manifest.csv")
  figure_manifest <- copy_paper_figures(config)
  report <- write_phase3_7_report(table_manifest, figure_manifest)

  append_log("Phase 3.7 paper-ready export finished.", config)
  list(tables = table_manifest, figures = figure_manifest, report = report)
}

is_direct_rscript_call <- function(file) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
  length(file_arg) > 0 && identical(
    normalizePath(file_arg[1], winslash = "/", mustWork = FALSE),
    normalizePath(file, winslash = "/", mustWork = FALSE)
  )
}

if (!interactive() && is_direct_rscript_call("src/models/run_phase3_7_paper_ready.R")) {
  result <- run_phase3_7_paper_ready()
  cat("Phase 3.7 paper-ready export completed.\n")
  cat(sprintf("Paper-ready tables: %s\n", nrow(result$tables)))
  cat(sprintf("Copied figure files: %s\n", sum(result$figures$copied)))
  cat(sprintf("Report: %s\n", result$report))
}
