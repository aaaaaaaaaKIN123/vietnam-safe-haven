source("src/utils/config_loader.R")
source("src/utils/io_helpers.R")
source("src/utils/date_asset_helpers.R")
source("src/models/validate_model_control.R")

as_numeric_column <- function(x) {
  suppressWarnings(as.numeric(x))
}

read_phase3_4_hedge_ratios <- function(config) {
  input_path <- project_path(file.path(
    config$outputs$tables_dir,
    "hedge_ratios",
    "dynamic_hedge_ratios.csv"
  ))
  if (!file.exists(input_path)) {
    stop(sprintf("Phase 3.4 dynamic hedge-ratio file not found: %s", input_path), call. = FALSE)
  }
  df <- utils::read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
  df <- strip_bom_from_names(df)
  df$date <- as.Date(df$date)
  numeric_cols <- c(
    "dynamic_correlation", "conditional_covariance", "target_variance",
    "hedge_variance", "hedge_ratio", "target_return", "hedge_return",
    "hedged_return"
  )
  for (col in intersect(numeric_cols, names(df))) {
    df[[col]] <- as_numeric_column(df[[col]])
  }
  df
}

read_phase3_4b_state_data <- function(config) {
  input_path <- project_path(config$data$returns_input)
  if (!file.exists(input_path)) {
    stop(sprintf("Prepared returns file not found: %s", input_path), call. = FALSE)
  }
  returns <- utils::read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
  returns <- strip_bom_from_names(returns)
  date_col <- config$data$date_column
  if (!date_col %in% names(returns)) {
    stop(sprintf("Date column '%s' not found in prepared returns.", date_col), call. = FALSE)
  }
  returns[[date_col]] <- as.Date(returns[[date_col]])
  returns <- returns[
    returns[[date_col]] > as.Date(config$data$sample_start) &
      returns[[date_col]] <= as.Date(config$data$sample_end),
    ,
    drop = FALSE
  ]
  returns
}

common_dates_for_hedges <- function(hedge_ratios, hedges) {
  date_sets <- lapply(hedges, function(hedge) {
    unique(as.character(hedge_ratios$date[hedge_ratios$hedge == hedge]))
  })
  if (length(date_sets) == 0) {
    return(as.Date(character()))
  }
  as.Date(Reduce(intersect, date_sets))
}

cap_hedge_ratio <- function(x, cap_value) {
  if (is.na(cap_value) || !is.finite(cap_value)) {
    return(x)
  }
  pmin(pmax(x, -cap_value), cap_value)
}

compute_effectiveness_from_hedge_ratios <- function(
  hedge_ratios,
  sample_label,
  cap_label = "uncapped",
  cap_value = NA_real_,
  state_label = "all",
  min_observations = 20
) {
  if (nrow(hedge_ratios) == 0) {
    return(data.frame())
  }

  groups <- unique(hedge_ratios[, c("target", "hedge", "model"), drop = FALSE])
  rows <- list()
  for (i in seq_len(nrow(groups))) {
    sub <- hedge_ratios[
      hedge_ratios$target == groups$target[i] &
        hedge_ratios$hedge == groups$hedge[i] &
        hedge_ratios$model == groups$model[i],
      ,
      drop = FALSE
    ]
    hedge_ratio_used <- cap_hedge_ratio(sub$hedge_ratio, cap_value)
    hedged_return <- sub$target_return - hedge_ratio_used * sub$hedge_return
    valid <- is.finite(sub$target_return) &
      is.finite(sub$hedge_return) &
      is.finite(hedge_ratio_used) &
      is.finite(hedged_return)
    eval_df <- sub[valid, , drop = FALSE]
    if (nrow(eval_df) < min_observations) next

    target_return <- eval_df$target_return
    hedge_return <- eval_df$hedge_return
    hedge_ratio_eval <- hedge_ratio_used[valid]
    hedged_return_eval <- hedged_return[valid]
    unhedged_var <- stats::var(target_return, na.rm = TRUE)
    hedged_var <- stats::var(hedged_return_eval, na.rm = TRUE)
    unhedged_vol <- sqrt(unhedged_var)
    hedged_vol <- sqrt(hedged_var)

    rows[[length(rows) + 1]] <- data.frame(
      sample_label = sample_label,
      state_label = state_label,
      cap_label = cap_label,
      cap_value = cap_value,
      target = groups$target[i],
      hedge = groups$hedge[i],
      model = groups$model[i],
      observations = nrow(eval_df),
      first_date = min(eval_df$date),
      last_date = max(eval_df$date),
      unhedged_variance = unhedged_var,
      hedged_variance = hedged_var,
      hedging_effectiveness = 1 - hedged_var / unhedged_var,
      unhedged_volatility = unhedged_vol,
      hedged_volatility = hedged_vol,
      volatility_reduction = 1 - hedged_vol / unhedged_vol,
      mean_hedge_ratio = mean(hedge_ratio_eval, na.rm = TRUE),
      min_hedge_ratio = min(hedge_ratio_eval, na.rm = TRUE),
      max_hedge_ratio = max(hedge_ratio_eval, na.rm = TRUE),
      mean_unhedged_return = mean(target_return, na.rm = TRUE),
      mean_hedged_return = mean(hedged_return_eval, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0) data.frame() else do.call(rbind, rows)
}

build_state_definitions <- function(returns) {
  returns$date <- as.Date(returns$date)
  states <- list()
  if ("stress_vnindex_q10" %in% names(returns)) {
    states[["vnindex_lower_tail_10pct"]] <- returns$date[as_numeric_column(returns$stress_vnindex_q10) == 1]
  }
  if ("stress_vnindex_q05" %in% names(returns)) {
    states[["vnindex_lower_tail_5pct"]] <- returns$date[as_numeric_column(returns$stress_vnindex_q05) == 1]
  }
  if ("stress_vnindex_q01" %in% names(returns)) {
    states[["vnindex_lower_tail_1pct"]] <- returns$date[as_numeric_column(returns$stress_vnindex_q01) == 1]
  }
  if ("d_policy_rate" %in% names(returns)) {
    d_policy <- as_numeric_column(returns$d_policy_rate)
    states[["policy_rate_change_event"]] <- returns$date[is.finite(d_policy) & d_policy != 0]
    states[["monetary_tightening_policy_rate_up"]] <- returns$date[is.finite(d_policy) & d_policy > 0]
  }
  if ("d_interbank_rate" %in% names(returns)) {
    d_interbank <- as_numeric_column(returns$d_interbank_rate)
    threshold <- stats::quantile(d_interbank[is.finite(d_interbank)], probs = 0.90, na.rm = TRUE, names = FALSE)
    states[["interbank_rate_top_10pct_change"]] <- returns$date[is.finite(d_interbank) & d_interbank >= threshold]
  }
  if ("d_vn10y_yield" %in% names(returns)) {
    d_vn10y <- as_numeric_column(returns$d_vn10y_yield)
    threshold <- stats::quantile(d_vn10y[is.finite(d_vn10y)], probs = 0.90, na.rm = TRUE, names = FALSE)
    states[["vn10y_yield_top_10pct_change"]] <- returns$date[is.finite(d_vn10y) & d_vn10y >= threshold]
  }
  if ("d_vix" %in% names(returns)) {
    d_vix <- as_numeric_column(returns$d_vix)
    threshold <- stats::quantile(d_vix[is.finite(d_vix)], probs = 0.90, na.rm = TRUE, names = FALSE)
    states[["vix_top_10pct_change"]] <- returns$date[is.finite(d_vix) & d_vix >= threshold]
  }
  states
}

write_phase3_4b_report <- function(outputs, config) {
  report_path <- project_path("data/metadata/phase3_4b_hedging_robustness_report.md")
  common_all <- outputs$common_all
  core <- outputs$core
  capped <- outputs$capped
  states <- outputs$states

  top_common <- if (nrow(common_all) > 0) common_all[order(-common_all$hedging_effectiveness), , drop = FALSE] else common_all
  top_core <- if (nrow(core) > 0) core[order(-core$hedging_effectiveness), , drop = FALSE] else core

  lines <- c(
    "# Phase 3.4b Hedging Robustness And State Comparison Report",
    "",
    "## Mục tiêu",
    "",
    "Phase 3.4b kiểm tra độ bền của kết quả hedging effectiveness bằng ba lớp kiểm định: so sánh trên mẫu thời gian chung, độ nhạy khi cap hedge ratio, và hiệu quả hedge theo trạng thái căng thẳng thị trường/tiền tệ.",
    "",
    "## Output Summary",
    "",
    sprintf("- Common all-asset sample rows: `%s`", nrow(common_all)),
    sprintf("- Core-asset common sample rows: `%s`", nrow(core)),
    sprintf("- Capped sensitivity rows: `%s`", nrow(capped)),
    sprintf("- State-dependent rows: `%s`", nrow(states)),
    "",
    "## Common All-Asset Sample Ranking",
    ""
  )

  if (nrow(top_common) == 0) {
    lines <- c(lines, "No common all-asset result was generated.")
  } else {
    lines <- c(
      lines,
      "| Rank | Hedge | Model | Observations | HE | Volatility reduction | Mean hedge ratio |",
      "| ---: | --- | --- | ---: | ---: | ---: | ---: |"
    )
    for (i in seq_len(nrow(top_common))) {
      lines <- c(lines, sprintf(
        "| %s | %s | %s | %s | %.6f | %.6f | %.6f |",
        i,
        top_common$hedge[i],
        top_common$model[i],
        top_common$observations[i],
        top_common$hedging_effectiveness[i],
        top_common$volatility_reduction[i],
        top_common$mean_hedge_ratio[i]
      ))
    }
  }

  lines <- c(lines, "", "## Core-Asset Common Sample Ranking", "")
  if (nrow(top_core) == 0) {
    lines <- c(lines, "No core-sample result was generated.")
  } else {
    lines <- c(
      lines,
      "| Rank | Hedge | Model | Observations | HE | Volatility reduction | Mean hedge ratio |",
      "| ---: | --- | --- | ---: | ---: | ---: | ---: |"
    )
    for (i in seq_len(nrow(top_core))) {
      lines <- c(lines, sprintf(
        "| %s | %s | %s | %s | %.6f | %.6f | %.6f |",
        i,
        top_core$hedge[i],
        top_core$model[i],
        top_core$observations[i],
        top_core$hedging_effectiveness[i],
        top_core$volatility_reduction[i],
        top_core$mean_hedge_ratio[i]
      ))
    }
  }

  lines <- c(
    lines,
    "",
    "## Scientific Notes",
    "",
    "- Common-sample results should be used when comparing assets with different data availability, especially crypto versus traditional assets.",
    "- Capped-sensitivity results check whether a few extreme hedge ratios drive the conclusion.",
    "- State-dependent results are descriptive robustness checks; they do not replace the formal Baur-Lucey safe-haven regressions in Phase 3.5.",
    "- `d_policy_rate`, `d_interbank_rate`, `d_vn10y_yield`, and `d_vix` are used as state variables, not hedge assets.",
    "",
    "## Output Files",
    "",
    sprintf("- `%s`", project_path(file.path(config$outputs$tables_dir, "hedging_effectiveness", "hedging_effectiveness_common_sample.csv"))),
    sprintf("- `%s`", project_path(file.path(config$outputs$tables_dir, "hedging_effectiveness", "hedging_effectiveness_core_sample.csv"))),
    sprintf("- `%s`", project_path(file.path(config$outputs$tables_dir, "hedging_effectiveness", "hedging_effectiveness_capped_sensitivity.csv"))),
    sprintf("- `%s`", project_path(file.path(config$outputs$tables_dir, "hedging_effectiveness", "hedging_effectiveness_by_state.csv")))
  )

  con <- file(report_path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw("\ufeff"), con)
  writeLines(enc2utf8(lines), con, useBytes = TRUE)
  invisible(report_path)
}

run_phase3_4b_hedging_robustness <- function(
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
  append_log("Phase 3.4b hedging robustness started.", config)

  hedge_ratios <- read_phase3_4_hedge_ratios(config)
  returns <- read_phase3_4b_state_data(config)

  core_hedges <- vapply(config$assets$core_hedges, asset_return_name, character(1))
  core_hedges <- core_hedges[core_hedges %in% unique(hedge_ratios$hedge)]
  all_hedges <- sort(unique(hedge_ratios$hedge))
  core_common_dates <- common_dates_for_hedges(hedge_ratios, core_hedges)
  all_common_dates <- common_dates_for_hedges(hedge_ratios, all_hedges)

  core_df <- hedge_ratios[hedge_ratios$hedge %in% core_hedges & hedge_ratios$date %in% core_common_dates, , drop = FALSE]
  all_common_df <- hedge_ratios[hedge_ratios$date %in% all_common_dates, , drop = FALSE]

  common_all <- compute_effectiveness_from_hedge_ratios(
    all_common_df,
    sample_label = "common_all_assets",
    cap_label = "uncapped"
  )
  core <- compute_effectiveness_from_hedge_ratios(
    core_df,
    sample_label = "common_core_assets",
    cap_label = "uncapped"
  )

  capped <- do.call(rbind, list(
    compute_effectiveness_from_hedge_ratios(hedge_ratios, "pairwise_full", "uncapped", NA_real_),
    compute_effectiveness_from_hedge_ratios(hedge_ratios, "pairwise_full", "cap_abs_1", 1),
    compute_effectiveness_from_hedge_ratios(hedge_ratios, "pairwise_full", "cap_abs_2", 2)
  ))

  states <- build_state_definitions(returns)
  state_rows <- list()
  for (state_name in names(states)) {
    state_dates <- unique(states[[state_name]])
    state_df <- hedge_ratios[hedge_ratios$date %in% state_dates, , drop = FALSE]
    state_min_observations <- if (grepl("policy_rate", state_name)) 5 else 10
    state_rows[[length(state_rows) + 1]] <- compute_effectiveness_from_hedge_ratios(
      state_df,
      sample_label = "pairwise_state",
      cap_label = "uncapped",
      state_label = state_name,
      min_observations = state_min_observations
    )
  }
  state_effectiveness <- if (length(state_rows) == 0) data.frame() else do.call(rbind, state_rows)

  write_csv_output(
    common_all,
    file.path(config$outputs$tables_dir, "hedging_effectiveness", "hedging_effectiveness_common_sample.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  write_csv_output(
    core,
    file.path(config$outputs$tables_dir, "hedging_effectiveness", "hedging_effectiveness_core_sample.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  write_csv_output(
    capped,
    file.path(config$outputs$tables_dir, "hedging_effectiveness", "hedging_effectiveness_capped_sensitivity.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  write_csv_output(
    state_effectiveness,
    file.path(config$outputs$tables_dir, "hedging_effectiveness", "hedging_effectiveness_by_state.csv"),
    overwrite = config$outputs$overwrite_existing
  )

  outputs <- list(
    common_all = common_all,
    core = core,
    capped = capped,
    states = state_effectiveness
  )
  report <- write_phase3_4b_report(outputs, config)

  append_log("Phase 3.4b hedging robustness finished.", config)
  list(outputs = outputs, report = report)
}

is_direct_rscript_call <- function(file) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
  length(file_arg) > 0 && identical(
    normalizePath(file_arg[1], winslash = "/", mustWork = FALSE),
    normalizePath(file, winslash = "/", mustWork = FALSE)
  )
}

if (!interactive() && is_direct_rscript_call("src/models/run_phase3_4b_hedging_robustness.R")) {
  result <- run_phase3_4b_hedging_robustness()
  cat("Phase 3.4b hedging robustness completed.\n")
  cat(sprintf("Common all-asset rows: %s\n", nrow(result$outputs$common_all)))
  cat(sprintf("Core common-sample rows: %s\n", nrow(result$outputs$core)))
  cat(sprintf("Capped-sensitivity rows: %s\n", nrow(result$outputs$capped)))
  cat(sprintf("State-dependent rows: %s\n", nrow(result$outputs$states)))
  cat(sprintf("Report: %s\n", result$report))
}
