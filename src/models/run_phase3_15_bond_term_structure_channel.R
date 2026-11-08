source("src/utils/config_loader.R")
source("src/utils/io_helpers.R")
source("src/models/validate_model_control.R")

phase3_15_tenor_map <- data.frame(
  tenor = c("1Y", "5Y", "10Y"),
  yield_col = c("y_1y", "y_5y", "y_10y"),
  dy_col = c("d_y_1y_bps", "d_y_5y_bps", "d_y_10y_bps"),
  proxy_return_col = c("r_bond_proxy_1y", "r_bond_proxy_5y", "r_bond_proxy_10y"),
  tri_return_col = c("r_bond_tri_1y", "r_bond_tri_5y", "r_bond_tri_10y"),
  duration_assumption = c(0.95, 4.50, 8.00),
  channel_interpretation = c(
    "short_end_policy_liquidity_channel",
    "medium_end_policy_growth_inflation_channel",
    "long_end_duration_safe_asset_channel"
  ),
  stringsAsFactors = FALSE
)

phase3_15_read_csv <- function(path) {
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
}

phase3_15_write_csv <- function(x, path) {
  ensure_dir(dirname(path))
  write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8")
  path
}

phase3_15_numeric_summary <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(c(observations = 0, mean = NA, std = NA, min = NA, p05 = NA, median = NA, p95 = NA, max = NA))
  }
  c(
    observations = length(x),
    mean = mean(x),
    std = stats::sd(x),
    min = min(x),
    p05 = unname(stats::quantile(x, 0.05, names = FALSE)),
    median = stats::median(x),
    p95 = unname(stats::quantile(x, 0.95, names = FALSE)),
    max = max(x)
  )
}

phase3_15_max_drawdown <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  wealth <- cumprod(1 + x / 100)
  drawdown <- wealth / cummax(wealth) - 1
  min(drawdown) * 100
}

phase3_15_expected_shortfall <- function(x, alpha = 0.05) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  cutoff <- unname(stats::quantile(x, alpha, names = FALSE))
  mean(x[x <= cutoff])
}

phase3_15_var_reduction <- function(target, hedged) {
  ok <- is.finite(target) & is.finite(hedged)
  if (sum(ok) < 30) return(NA_real_)
  unhedged_var <- stats::var(target[ok])
  if (!is.finite(unhedged_var) || unhedged_var <= 1e-12) return(NA_real_)
  1 - stats::var(hedged[ok]) / unhedged_var
}

phase3_15_build_panel <- function(
  bond_path = "data/02_model_input/bond_term_structure/final_bond_term_structure_daily.csv",
  returns_path = "data/02_model_input/core/final_daily_returns.csv"
) {
  bond <- phase3_15_read_csv(bond_path)
  returns <- phase3_15_read_csv(returns_path)
  bond$date <- as.Date(bond$date)
  returns$date <- as.Date(returns$date)
  panel <- merge(
    returns[, c("date", "r_vnindex", "stress_vnindex_q10", "stress_vnindex_q05", "stress_vnindex_q01"), drop = FALSE],
    bond,
    by = "date",
    all.x = FALSE,
    all.y = FALSE
  )
  panel <- panel[panel$date >= as.Date("2010-01-04"), , drop = FALSE]
  panel <- panel[order(panel$date), , drop = FALSE]

  for (i in seq_len(nrow(phase3_15_tenor_map))) {
    yield_col <- phase3_15_tenor_map$yield_col[i]
    dy_col <- phase3_15_tenor_map$dy_col[i]
    proxy_col <- phase3_15_tenor_map$proxy_return_col[i]
    duration <- phase3_15_tenor_map$duration_assumption[i]
    carry <- c(NA_real_, head(panel[[yield_col]], -1)) / 252
    price_component <- -duration * panel[[dy_col]] / 100
    panel[[proxy_col]] <- carry + price_component
  }
  panel
}

phase3_15_term_summary <- function(panel) {
  rows <- list()
  variables <- c(
    phase3_15_tenor_map$yield_col,
    phase3_15_tenor_map$dy_col,
    "bond_slope_10y_1y",
    "bond_slope_10y_5y",
    "bond_curvature_1_5_10",
    phase3_15_tenor_map$proxy_return_col,
    phase3_15_tenor_map$tri_return_col
  )
  for (variable in variables) {
    if (!variable %in% names(panel)) next
    summary <- phase3_15_numeric_summary(panel[[variable]])
    rows[[length(rows) + 1]] <- data.frame(
      variable = variable,
      t(summary),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

phase3_15_stress_response <- function(panel) {
  rows <- list()
  stress_cols <- c(q10 = "stress_vnindex_q10", q05 = "stress_vnindex_q05", q01 = "stress_vnindex_q01")
  variables <- c(phase3_15_tenor_map$dy_col, "bond_slope_10y_1y", "bond_slope_10y_5y", "bond_curvature_1_5_10")
  for (stress_name in names(stress_cols)) {
    stress_col <- stress_cols[[stress_name]]
    for (variable in variables) {
      if (!variable %in% names(panel)) next
      ok <- is.finite(panel[[variable]]) & !is.na(panel[[stress_col]])
      stress_values <- panel[[variable]][ok & panel[[stress_col]] == 1]
      normal_values <- panel[[variable]][ok & panel[[stress_col]] == 0]
      test <- tryCatch(stats::t.test(stress_values, normal_values), error = function(e) NULL)
      rows[[length(rows) + 1]] <- data.frame(
        stress_threshold = stress_name,
        variable = variable,
        stress_observations = length(stress_values),
        normal_observations = length(normal_values),
        stress_mean = if (length(stress_values) > 0) mean(stress_values) else NA_real_,
        normal_mean = if (length(normal_values) > 0) mean(normal_values) else NA_real_,
        stress_minus_normal = if (length(stress_values) > 0 && length(normal_values) > 0) mean(stress_values) - mean(normal_values) else NA_real_,
        t_statistic = if (is.null(test)) NA_real_ else unname(test$statistic),
        p_value = if (is.null(test)) NA_real_ else test$p.value,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

phase3_15_proxy_hedging <- function(panel) {
  rows <- list()
  complete <- panel[complete.cases(panel[, c("r_vnindex", phase3_15_tenor_map$proxy_return_col), drop = FALSE]), , drop = FALSE]
  for (i in seq_len(nrow(phase3_15_tenor_map))) {
    proxy_col <- phase3_15_tenor_map$proxy_return_col[i]
    target <- complete$r_vnindex
    hedge <- complete[[proxy_col]]
    hedge_var <- stats::var(hedge)
    hedge_ratio <- if (is.finite(hedge_var) && hedge_var > 1e-12) stats::cov(target, hedge) / hedge_var else 0
    hedged <- target - hedge_ratio * hedge
    unhedged_es <- phase3_15_expected_shortfall(target)
    hedged_es <- phase3_15_expected_shortfall(hedged)
    rows[[length(rows) + 1]] <- data.frame(
      tenor = phase3_15_tenor_map$tenor[i],
      proxy_return = proxy_col,
      duration_assumption = phase3_15_tenor_map$duration_assumption[i],
      channel_interpretation = phase3_15_tenor_map$channel_interpretation[i],
      observations = length(target),
      first_date = min(complete$date),
      last_date = max(complete$date),
      hedge_ratio = hedge_ratio,
      unhedged_variance = stats::var(target),
      hedged_variance = stats::var(hedged),
      hedging_effectiveness = phase3_15_var_reduction(target, hedged),
      unhedged_es_5pct = unhedged_es,
      hedged_es_5pct = hedged_es,
      es_reduction_5pct = if (is.finite(unhedged_es) && abs(unhedged_es) > 1e-12) 1 - abs(hedged_es) / abs(unhedged_es) else NA_real_,
      unhedged_drawdown = phase3_15_max_drawdown(target),
      hedged_drawdown = phase3_15_max_drawdown(hedged),
      drawdown_reduction = if (abs(phase3_15_max_drawdown(target)) > 1e-12) 1 - abs(phase3_15_max_drawdown(hedged)) / abs(phase3_15_max_drawdown(target)) else NA_real_,
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  out$he_rank <- rank(-out$hedging_effectiveness, ties.method = "average")
  out$es_rank <- rank(-out$es_reduction_5pct, ties.method = "average")
  out$drawdown_rank <- rank(-out$drawdown_reduction, ties.method = "average")
  out$average_direct_rank <- rowMeans(out[, c("he_rank", "es_rank", "drawdown_rank")], na.rm = TRUE)
  out <- out[order(out$average_direct_rank), , drop = FALSE]
  rownames(out) <- NULL
  out
}

phase3_15_tri_hedging <- function(panel) {
  rows <- list()
  available_return_cols <- phase3_15_tenor_map$tri_return_col[phase3_15_tenor_map$tri_return_col %in% names(panel)]
  complete <- panel[complete.cases(panel[, c("r_vnindex", available_return_cols), drop = FALSE]), , drop = FALSE]
  for (i in seq_len(nrow(phase3_15_tenor_map))) {
    tri_col <- phase3_15_tenor_map$tri_return_col[i]
    if (!tri_col %in% names(complete)) next
    target <- complete$r_vnindex
    hedge <- complete[[tri_col]]
    hedge_var <- stats::var(hedge)
    hedge_ratio <- if (is.finite(hedge_var) && hedge_var > 1e-12) stats::cov(target, hedge) / hedge_var else 0
    hedged <- target - hedge_ratio * hedge
    unhedged_es <- phase3_15_expected_shortfall(target)
    hedged_es <- phase3_15_expected_shortfall(hedged)
    rows[[length(rows) + 1]] <- data.frame(
      tenor = phase3_15_tenor_map$tenor[i],
      tri_return = tri_col,
      duration_assumption = phase3_15_tenor_map$duration_assumption[i],
      channel_interpretation = phase3_15_tenor_map$channel_interpretation[i],
      observations = length(target),
      first_date = min(complete$date),
      last_date = max(complete$date),
      hedge_ratio = hedge_ratio,
      unhedged_variance = stats::var(target),
      hedged_variance = stats::var(hedged),
      hedging_effectiveness = phase3_15_var_reduction(target, hedged),
      unhedged_es_5pct = unhedged_es,
      hedged_es_5pct = hedged_es,
      es_reduction_5pct = if (is.finite(unhedged_es) && abs(unhedged_es) > 1e-12) 1 - abs(hedged_es) / abs(unhedged_es) else NA_real_,
      unhedged_drawdown = phase3_15_max_drawdown(target),
      hedged_drawdown = phase3_15_max_drawdown(hedged),
      drawdown_reduction = if (abs(phase3_15_max_drawdown(target)) > 1e-12) 1 - abs(phase3_15_max_drawdown(hedged)) / abs(phase3_15_max_drawdown(target)) else NA_real_,
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  out$he_rank <- rank(-out$hedging_effectiveness, ties.method = "average")
  out$es_rank <- rank(-out$es_reduction_5pct, ties.method = "average")
  out$drawdown_rank <- rank(-out$drawdown_reduction, ties.method = "average")
  out$average_direct_rank <- rowMeans(out[, c("he_rank", "es_rank", "drawdown_rank")], na.rm = TRUE)
  out <- out[order(out$average_direct_rank), , drop = FALSE]
  rownames(out) <- NULL
  out
}

phase3_15_correlation_table <- function(panel) {
  variables <- c(
    "r_vnindex",
    phase3_15_tenor_map$tri_return_col,
    phase3_15_tenor_map$proxy_return_col,
    phase3_15_tenor_map$dy_col,
    "bond_slope_10y_1y",
    "bond_slope_10y_5y",
    "bond_curvature_1_5_10"
  )
  variables <- variables[variables %in% names(panel)]
  rows <- list()
  for (variable in setdiff(variables, "r_vnindex")) {
    ok <- is.finite(panel$r_vnindex) & is.finite(panel[[variable]])
    rows[[length(rows) + 1]] <- data.frame(
      variable = variable,
      observations = sum(ok),
      pearson_with_vnindex = if (sum(ok) >= 30) stats::cor(panel$r_vnindex[ok], panel[[variable]][ok], method = "pearson") else NA_real_,
      spearman_with_vnindex = if (sum(ok) >= 30) stats::cor(panel$r_vnindex[ok], panel[[variable]][ok], method = "spearman") else NA_real_,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

phase3_15_write_report <- function(panel, term_summary, stress_response, proxy_hedging, tri_hedging, correlation_table) {
  report_path <- "data/metadata/phase3_15_bond_term_structure_channel_report.md"
  best_proxy <- proxy_hedging[which.min(proxy_hedging$average_direct_rank), , drop = FALSE]
  best_tri <- tri_hedging[which.min(tri_hedging$average_direct_rank), , drop = FALSE]
  lines <- c(
    "# Phase 3.15 Bond Term-Structure Channel Report",
    "",
    "## Scope",
    "",
    "Phase 3.15 triển khai kênh cấu trúc kỳ hạn trái phiếu theo ba mức 1Y, 5Y và 10Y.",
    "Bond TRI 10Y vẫn là tài sản hedge chính. Các proxy 1Y/5Y/10Y dưới đây là duration-adjusted robustness, không phải TRI chính thức.",
    "",
    "## Three-Level Bond Channel",
    "",
    "- 1Y: short-end policy/liquidity channel.",
    "- 5Y: medium-end policy-growth-inflation channel.",
    "- 10Y: long-end duration/safe-asset demand channel.",
    "",
    "## Sample",
    "",
    sprintf("- Panel rows after 2010 filter: `%s`.", nrow(panel)),
    sprintf("- First date: `%s`.", min(panel$date, na.rm = TRUE)),
    sprintf("- Last date: `%s`.", max(panel$date, na.rm = TRUE)),
    "",
    "## Main Proxy-Hedging Diagnostic",
    "",
    sprintf(
      "- Best average direct-risk proxy tenor: `%s` (%s).",
      best_proxy$tenor[1],
      best_proxy$channel_interpretation[1]
    ),
    sprintf("- Best proxy HE: `%.6f`.", best_proxy$hedging_effectiveness[1]),
    sprintf("- Best proxy ES reduction: `%.6f`.", best_proxy$es_reduction_5pct[1]),
    "",
    "## Main Simulated-TRI Hedging Diagnostic",
    "",
    sprintf(
      "- Best average direct-risk simulated TRI tenor: `%s` (%s).",
      best_tri$tenor[1],
      best_tri$channel_interpretation[1]
    ),
    sprintf("- Best simulated-TRI HE: `%.6f`.", best_tri$hedging_effectiveness[1]),
    sprintf("- Best simulated-TRI ES reduction: `%.6f`.", best_tri$es_reduction_5pct[1]),
    "",
    "## Output Tables",
    "",
    "- `results/tables/bond_term_structure/bond_term_structure_summary.csv`",
    "- `results/tables/bond_term_structure/bond_term_structure_stress_response.csv`",
    "- `results/tables/bond_term_structure/bond_tenor_proxy_hedging.csv`",
    "- `results/tables/bond_term_structure/bond_tenor_tri_hedging.csv`",
    "- `results/tables/bond_term_structure/bond_term_structure_correlations.csv`",
    "- `paper/tables/table_14_bond_term_structure_channel.csv`",
    "- `paper/tables/table_15_bond_tenor_proxy_hedging.csv`",
    "- `paper/tables/table_16_bond_tri_tenor_hedging.csv`",
    "",
    "## Scientific Cautions",
    "",
    "- Không gọi proxy 1Y/5Y là official total return index.",
    "- Không thay thế Bond TRI 10Y trong mô hình chính nếu chưa dựng TRI 1Y/5Y bằng cùng phương pháp định giá dòng tiền.",
    "- Dùng kết quả này như mechanism/robustness layer cho Bond Term-Structure Channel."
  )
  writeLines(lines, report_path, useBytes = TRUE)
  report_path
}

phase3_15_write_report <- function(panel, term_summary, stress_response, proxy_hedging, tri_hedging, correlation_table) {
  report_path <- "data/metadata/phase3_15_bond_term_structure_channel_report.md"
  best_proxy <- proxy_hedging[which.min(proxy_hedging$average_direct_rank), , drop = FALSE]
  best_tri <- tri_hedging[which.min(tri_hedging$average_direct_rank), , drop = FALSE]
  lines <- c(
    "# Phase 3.15 Bond Term-Structure Channel Report",
    "",
    "## Scope",
    "",
    "Phase 3.15 implements the Vietnam government bond term-structure channel at 1Y, 5Y and 10Y.",
    "Bond TRI 10Y remains the core bond hedge in the main model. This phase adds simulated constant-maturity TRI series for 1Y and 5Y to compare duration channels.",
    "The 1Y/5Y TRI series are simulated research series, not official market indices.",
    "",
    "## Three-Level Bond Channel",
    "",
    "- 1Y: short-end policy/liquidity channel.",
    "- 5Y: medium-end policy-growth-inflation channel.",
    "- 10Y: long-end duration/safe-asset demand channel.",
    "",
    "## Sample",
    "",
    sprintf("- Panel rows after 2010 filter: `%s`.", nrow(panel)),
    sprintf("- First date: `%s`.", min(panel$date, na.rm = TRUE)),
    sprintf("- Last date: `%s`.", max(panel$date, na.rm = TRUE)),
    "",
    "## Main Proxy-Hedging Diagnostic",
    "",
    sprintf("- Best average direct-risk proxy tenor: `%s` (%s).", best_proxy$tenor[1], best_proxy$channel_interpretation[1]),
    sprintf("- Best proxy HE: `%.6f`.", best_proxy$hedging_effectiveness[1]),
    sprintf("- Best proxy ES reduction: `%.6f`.", best_proxy$es_reduction_5pct[1]),
    "",
    "## Main Simulated-TRI Hedging Diagnostic",
    "",
    sprintf("- Best average direct-risk simulated TRI tenor: `%s` (%s).", best_tri$tenor[1], best_tri$channel_interpretation[1]),
    sprintf("- Best simulated-TRI HE: `%.6f`.", best_tri$hedging_effectiveness[1]),
    sprintf("- Best simulated-TRI ES reduction: `%.6f`.", best_tri$es_reduction_5pct[1]),
    "",
    "## Output Tables",
    "",
    "- `results/tables/bond_term_structure/bond_term_structure_summary.csv`",
    "- `results/tables/bond_term_structure/bond_term_structure_stress_response.csv`",
    "- `results/tables/bond_term_structure/bond_tenor_proxy_hedging.csv`",
    "- `results/tables/bond_term_structure/bond_tenor_tri_hedging.csv`",
    "- `results/tables/bond_term_structure/bond_term_structure_correlations.csv`",
    "- `paper/tables/table_14_bond_term_structure_channel.csv`",
    "- `paper/tables/table_15_bond_tenor_proxy_hedging.csv`",
    "- `paper/tables/table_16_bond_tri_tenor_hedging.csv`",
    "",
    "## Scientific Cautions",
    "",
    "- Do not call simulated 1Y/5Y TRI official total return indices.",
    "- Do not replace Bond TRI 10Y in the main model until the user approves the robustness interpretation.",
    "- Use these results as a mechanism/robustness layer for the Bond Term-Structure Channel."
  )
  writeLines(lines, report_path, useBytes = TRUE)
  report_path
}

phase3_15_update_manifest <- function(term_summary, proxy_hedging, tri_hedging) {
  manifest_path <- "paper/tables/table_manifest.csv"
  manifest <- if (file.exists(manifest_path)) {
    phase3_15_read_csv(manifest_path)
  } else {
    data.frame(table = character(), source = character(), destination = character(), rows = integer())
  }
  new_rows <- data.frame(
    table = c("table_14_bond_term_structure_channel", "table_15_bond_tenor_proxy_hedging", "table_16_bond_tri_tenor_hedging"),
    source = c(
      "results/tables/bond_term_structure/bond_term_structure_summary.csv;results/tables/bond_term_structure/bond_term_structure_stress_response.csv",
      "results/tables/bond_term_structure/bond_tenor_proxy_hedging.csv",
      "results/tables/bond_term_structure/bond_tenor_tri_hedging.csv"
    ),
    destination = c(
      "paper/tables/table_14_bond_term_structure_channel.csv",
      "paper/tables/table_15_bond_tenor_proxy_hedging.csv",
      "paper/tables/table_16_bond_tri_tenor_hedging.csv"
    ),
    rows = c(nrow(term_summary), nrow(proxy_hedging), nrow(tri_hedging)),
    stringsAsFactors = FALSE
  )
  manifest <- manifest[!manifest$table %in% new_rows$table, , drop = FALSE]
  manifest <- rbind(manifest, new_rows)
  phase3_15_write_csv(manifest, manifest_path)
  manifest
}

run_phase3_15_bond_term_structure_channel <- function(
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
  ensure_dir(file.path(config$outputs$tables_dir, "bond_term_structure"))
  ensure_dir("paper/tables")
  append_log("Phase 3.15 bond term-structure channel started.", config)

  panel <- phase3_15_build_panel()
  term_summary <- phase3_15_term_summary(panel)
  stress_response <- phase3_15_stress_response(panel)
  proxy_hedging <- phase3_15_proxy_hedging(panel)
  tri_hedging <- phase3_15_tri_hedging(panel)
  correlation_table <- phase3_15_correlation_table(panel)

  phase3_15_write_csv(term_summary, file.path(config$outputs$tables_dir, "bond_term_structure", "bond_term_structure_summary.csv"))
  phase3_15_write_csv(stress_response, file.path(config$outputs$tables_dir, "bond_term_structure", "bond_term_structure_stress_response.csv"))
  phase3_15_write_csv(proxy_hedging, file.path(config$outputs$tables_dir, "bond_term_structure", "bond_tenor_proxy_hedging.csv"))
  phase3_15_write_csv(tri_hedging, file.path(config$outputs$tables_dir, "bond_term_structure", "bond_tenor_tri_hedging.csv"))
  phase3_15_write_csv(correlation_table, file.path(config$outputs$tables_dir, "bond_term_structure", "bond_term_structure_correlations.csv"))

  paper_term <- term_summary
  paper_term$paper_note <- "Bond term-structure mechanism table; yield levels/changes/slope/curvature are channels, not investable TRI assets."
  phase3_15_write_csv(paper_term, "paper/tables/table_14_bond_term_structure_channel.csv")

  paper_proxy <- proxy_hedging
  paper_proxy$paper_note <- "Duration-adjusted tenor proxy; robustness/mechanism only, not official TRI."
  phase3_15_write_csv(paper_proxy, "paper/tables/table_15_bond_tenor_proxy_hedging.csv")

  paper_tri <- tri_hedging
  paper_tri$paper_note <- "Simulated constant-maturity TRI tenor comparison; 1Y/5Y are research-built series, not official indices."
  phase3_15_write_csv(paper_tri, "paper/tables/table_16_bond_tri_tenor_hedging.csv")

  manifest <- phase3_15_update_manifest(paper_term, paper_proxy, paper_tri)
  report <- phase3_15_write_report(panel, term_summary, stress_response, proxy_hedging, tri_hedging, correlation_table)
  append_log("Phase 3.15 bond term-structure channel finished.", config)

  list(
    panel = panel,
    term_summary = term_summary,
    stress_response = stress_response,
    proxy_hedging = proxy_hedging,
    tri_hedging = tri_hedging,
    correlation_table = correlation_table,
    manifest = manifest,
    report = report
  )
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

if (!interactive() && is_direct_rscript_call("src/models/run_phase3_15_bond_term_structure_channel.R")) {
  result <- run_phase3_15_bond_term_structure_channel()
  cat("Phase 3.15 bond term-structure channel completed.\n")
  cat(sprintf("Panel rows: %s\n", nrow(result$panel)))
  cat(sprintf("Term summary rows: %s\n", nrow(result$term_summary)))
  cat(sprintf("Proxy hedging rows: %s\n", nrow(result$proxy_hedging)))
  cat(sprintf("TRI hedging rows: %s\n", nrow(result$tri_hedging)))
  cat(sprintf("Report: %s\n", result$report))
}
