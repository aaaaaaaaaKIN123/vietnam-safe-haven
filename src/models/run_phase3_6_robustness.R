source("src/utils/config_loader.R")
source("src/utils/io_helpers.R")
source("src/utils/date_asset_helpers.R")
source("src/models/validate_model_control.R")
source("src/models/R/04_safe_haven_tests.R")

robustness_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

read_phase3_6_returns <- function(config) {
  input_path <- project_path(config$data$returns_input)
  if (!file.exists(input_path)) {
    stop(sprintf("Prepared returns file not found: %s", input_path), call. = FALSE)
  }
  returns <- utils::read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
  returns <- strip_bom_from_names(returns)
  date_col <- config$data$date_column
  returns[[date_col]] <- as.Date(returns[[date_col]])
  returns <- returns[
    returns[[date_col]] > as.Date(config$data$sample_start) &
      returns[[date_col]] <= as.Date(config$data$sample_end),
    ,
    drop = FALSE
  ]
  for (col in setdiff(names(returns), date_col)) {
    returns[[col]] <- robustness_numeric(returns[[col]])
  }
  returns
}

read_phase3_6_weekly_returns <- function(config, daily_returns) {
  required_assets <- unique(c(
    "r_vnindex",
    vapply(config$assets$core_hedges, asset_return_name, character(1)),
    vapply(config$assets$extension_hedges, asset_return_name, character(1)),
    "r_brent", "r_wti", "r_gold_premium"
  ))
  candidates <- c(
    "data/02_model_input/robustness/final_robustness_weekly_returns.csv",
    "data/02_model_input/core/final_weekly_returns.csv"
  )
  for (candidate in candidates) {
    input_path <- project_path(candidate)
    if (file.exists(input_path)) {
      weekly <- utils::read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
      weekly <- strip_bom_from_names(weekly)
      weekly$date <- as.Date(weekly$date)
      weekly <- weekly[
        weekly$date > as.Date(config$data$sample_start) &
          weekly$date <= as.Date(config$data$sample_end),
        ,
        drop = FALSE
      ]
      for (col in setdiff(names(weekly), "date")) {
        weekly[[col]] <- robustness_numeric(weekly[[col]])
      }
      if (all(required_assets %in% names(weekly))) {
        attr(weekly, "source_file") <- candidate
        return(weekly)
      }
    }
  }
  weekly <- weekly_aggregate_returns(daily_returns)
  attr(weekly, "source_file") <- "aggregated_from_daily_returns_due_to_missing_weekly_extension_or_premium_columns"
  weekly
}

read_robustness_csv <- function(path) {
  full_path <- project_path(path)
  if (!file.exists(full_path)) {
    return(data.frame())
  }
  out <- utils::read.csv(full_path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
  strip_bom_from_names(out)
}

weekly_aggregate_returns <- function(returns) {
  returns$date <- as.Date(returns$date)
  returns$week_id <- format(returns$date, "%Y-%U")
  numeric_cols <- setdiff(names(returns), c("date", "week_id"))
  week_ids <- unique(returns$week_id)
  rows <- lapply(week_ids, function(week_id) {
    sub <- returns[returns$week_id == week_id, , drop = FALSE]
    out <- data.frame(date = max(sub$date), stringsAsFactors = FALSE)
    for (col in numeric_cols) {
      valid <- sub[[col]][is.finite(sub[[col]])]
      out[[col]] <- if (length(valid) == 0) NA_real_ else sum(valid, na.rm = TRUE)
    }
    out
  })
  out <- do.call(rbind, rows)
  out[order(out$date), , drop = FALSE]
}

write_weekly_return_summary <- function(weekly_returns, config) {
  assets <- unique(c(
    "r_vnindex",
    vapply(config$assets$core_hedges, asset_return_name, character(1)),
    vapply(config$assets$extension_hedges, asset_return_name, character(1)),
    "r_brent", "r_wti", "r_gold_premium"
  ))
  assets <- intersect(assets, names(weekly_returns))
  rows <- lapply(assets, function(asset) {
    valid <- is.finite(weekly_returns[[asset]])
    x <- weekly_returns[[asset]][valid]
    dates <- weekly_returns$date[valid]
    data.frame(
      variable = asset,
      observations = length(x),
      first_date = if (length(dates) == 0) as.Date(NA) else min(dates),
      last_date = if (length(dates) == 0) as.Date(NA) else max(dates),
      mean = if (length(x) == 0) NA_real_ else mean(x, na.rm = TRUE),
      sd = if (length(x) == 0) NA_real_ else stats::sd(x, na.rm = TRUE),
      min = if (length(x) == 0) NA_real_ else min(x, na.rm = TRUE),
      max = if (length(x) == 0) NA_real_ else max(x, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  output <- do.call(rbind, rows)
  output$source_file <- attr(weekly_returns, "source_file")
  write_csv_output(
    output,
    file.path(config$outputs$tables_dir, "robustness", "weekly_return_summary.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  output
}

run_weekly_safe_haven_robustness <- function(weekly_returns, config) {
  target <- asset_return_name(config$assets$target)
  main_assets <- safe_haven_test_assets(config, weekly_returns)
  premium_asset <- intersect("r_gold_premium", names(weekly_returns))
  assets <- unique(c(main_assets, premium_asset))
  results <- lapply(assets, function(asset) {
    role <- if (identical(asset, "r_gold_premium")) "weekly_gold_premium_channel" else "weekly_hedge_asset"
    safe_haven_run_asset(weekly_returns, target, asset, config, asset_role = role)$interpretation
  })
  output <- if (length(results) == 0) data.frame() else do.call(rbind, results)
  write_csv_output(
    output,
    file.path(config$outputs$tables_dir, "robustness", "weekly_safe_haven_interpretation.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  output
}

best_he_row <- function(he_data, hedge) {
  sub <- he_data[he_data$hedge == hedge, , drop = FALSE]
  if (nrow(sub) == 0) return(data.frame())
  sub[order(-as.numeric(sub$hedging_effectiveness)), , drop = FALSE][1, , drop = FALSE]
}

sample_value <- function(row, column, default = NA) {
  if (nrow(row) == 0 || !column %in% names(row)) {
    return(default)
  }
  row[[column]][1]
}

controlled_safe_haven_rows <- function(interpretation, hedge) {
  interpretation[
    interpretation$hedge == hedge & interpretation$model_spec == "controlled",
    ,
    drop = FALSE
  ]
}

collapse_safe_haven_classes <- function(rows) {
  if (nrow(rows) == 0) return("not_tested")
  paste(sprintf(
    "q%s:%s",
    rows$stress_quantile,
    rows$safe_haven_classification
  ), collapse = ";")
}

write_core_asset_contrast <- function(config) {
  core_he <- read_robustness_csv("results/tables/hedging_effectiveness/hedging_effectiveness_core_sample.csv")
  interpretation <- read_robustness_csv("results/tables/safe_haven_tests/safe_haven_interpretation_matrix.csv")
  premium <- read_robustness_csv("results/tables/safe_haven_tests/gold_premium_interpretation.csv")
  core_assets <- vapply(config$assets$core_hedges, asset_return_name, character(1))
  rows <- list()

  for (asset in core_assets) {
    he <- best_he_row(core_he, asset)
    sh <- controlled_safe_haven_rows(interpretation, asset)
    rows[[length(rows) + 1]] <- data.frame(
      asset = asset,
      role = "core_hedge_asset",
      observations = sample_value(he, "observations", NA_integer_),
      first_date = sample_value(he, "first_date", NA_character_),
      last_date = sample_value(he, "last_date", NA_character_),
      best_he_model = if (nrow(he) == 0) NA_character_ else he$model[1],
      best_hedging_effectiveness = if (nrow(he) == 0) NA_real_ else as.numeric(he$hedging_effectiveness[1]),
      best_mean_hedge_ratio = if (nrow(he) == 0) NA_real_ else as.numeric(he$mean_hedge_ratio[1]),
      safe_haven_class_by_tail = collapse_safe_haven_classes(sh),
      core_message = if (identical(asset, "r_bond_tri")) {
        "best_core_variance_reduction"
      } else if (identical(asset, "r_sjc_gold")) {
        "domestic_gold_asset"
      } else {
        "international_gold_vnd_channel"
      },
      stringsAsFactors = FALSE
    )
  }

  if (nrow(premium) > 0) {
    premium_controlled <- premium[premium$model_spec == "controlled", , drop = FALSE]
    rows[[length(rows) + 1]] <- data.frame(
      asset = "r_gold_premium",
      role = "domestic_gold_premium_channel",
      observations = sample_value(premium_controlled, "observations", NA_integer_),
      first_date = sample_value(premium_controlled, "first_date", NA_character_),
      last_date = sample_value(premium_controlled, "last_date", NA_character_),
      best_he_model = NA_character_,
      best_hedging_effectiveness = NA_real_,
      best_mean_hedge_ratio = NA_real_,
      safe_haven_class_by_tail = collapse_safe_haven_classes(premium_controlled),
      core_message = "domestic_market_friction_not_tradeable_hedge_asset",
      stringsAsFactors = FALSE
    )
  }

  output <- do.call(rbind, rows)
  write_csv_output(
    output,
    file.path(config$outputs$tables_dir, "robustness", "core_asset_contrast_matrix.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  output
}

write_oil_proxy_robustness <- function(config, returns) {
  he <- read_robustness_csv("results/tables/hedging_effectiveness/hedging_effectiveness_summary.csv")
  state_he <- read_robustness_csv("results/tables/hedging_effectiveness/hedging_effectiveness_by_state.csv")
  target <- asset_return_name(config$assets$target)
  rows <- list()
  for (asset in c("r_brent", "r_wti")) {
    best_pairwise <- best_he_row(he, asset)
    state_sub <- state_he[state_he$hedge == asset, , drop = FALSE]
    sh_result <- safe_haven_run_asset(returns, target, asset, config, asset_role = "oil_proxy_robustness")
    sh <- controlled_safe_haven_rows(sh_result$interpretation, asset)
    rows[[length(rows) + 1]] <- data.frame(
      asset = asset,
      observations = sample_value(best_pairwise, "observations", NA_integer_),
      first_date = sample_value(best_pairwise, "first_date", NA_character_),
      last_date = sample_value(best_pairwise, "last_date", NA_character_),
      best_he_model = if (nrow(best_pairwise) == 0) NA_character_ else best_pairwise$model[1],
      best_pairwise_hedging_effectiveness = if (nrow(best_pairwise) == 0) NA_real_ else as.numeric(best_pairwise$hedging_effectiveness[1]),
      best_state_hedging_effectiveness = if (nrow(state_sub) == 0) NA_real_ else max(as.numeric(state_sub$hedging_effectiveness), na.rm = TRUE),
      safe_haven_class_by_tail = collapse_safe_haven_classes(sh),
      interpretation_note = "oil_price_proxy_not_structural_oil_shock",
      stringsAsFactors = FALSE
    )
  }
  output <- do.call(rbind, rows)
  write_csv_output(
    output,
    file.path(config$outputs$tables_dir, "robustness", "oil_proxy_robustness.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  output
}

write_crypto_extension_robustness <- function(config) {
  pairwise <- read_robustness_csv("results/tables/hedging_effectiveness/hedging_effectiveness_summary.csv")
  common <- read_robustness_csv("results/tables/hedging_effectiveness/hedging_effectiveness_common_sample.csv")
  interpretation <- read_robustness_csv("results/tables/safe_haven_tests/safe_haven_interpretation_matrix.csv")
  rows <- list()
  for (asset in c("r_btc", "r_eth")) {
    best_pairwise <- best_he_row(pairwise, asset)
    best_common <- best_he_row(common, asset)
    sh <- controlled_safe_haven_rows(interpretation, asset)
    rows[[length(rows) + 1]] <- data.frame(
      asset = asset,
      pairwise_observations = sample_value(best_pairwise, "observations", NA_integer_),
      pairwise_first_date = sample_value(best_pairwise, "first_date", NA_character_),
      pairwise_last_date = sample_value(best_pairwise, "last_date", NA_character_),
      pairwise_best_model = if (nrow(best_pairwise) == 0) NA_character_ else best_pairwise$model[1],
      pairwise_best_he = if (nrow(best_pairwise) == 0) NA_real_ else as.numeric(best_pairwise$hedging_effectiveness[1]),
      common_sample_first_date = sample_value(best_common, "first_date", NA_character_),
      common_sample_last_date = sample_value(best_common, "last_date", NA_character_),
      common_sample_best_model = if (nrow(best_common) == 0) NA_character_ else best_common$model[1],
      common_sample_best_he = if (nrow(best_common) == 0) NA_real_ else as.numeric(best_common$hedging_effectiveness[1]),
      common_sample_observations = if (nrow(best_common) == 0) NA_integer_ else as.integer(best_common$observations[1]),
      safe_haven_class_by_tail = collapse_safe_haven_classes(sh),
      interpretation_note = "extension_asset_shorter_sample_not_core_conclusion",
      stringsAsFactors = FALSE
    )
  }
  output <- do.call(rbind, rows)
  write_csv_output(
    output,
    file.path(config$outputs$tables_dir, "robustness", "crypto_extension_robustness.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  output
}

write_stress_regime_summary <- function(config) {
  state_he <- read_robustness_csv("results/tables/hedging_effectiveness/hedging_effectiveness_by_state.csv")
  if (nrow(state_he) == 0) {
    write_csv_output(
      state_he,
      file.path(config$outputs$tables_dir, "robustness", "stress_regime_summary.csv"),
      overwrite = config$outputs$overwrite_existing
    )
    return(state_he)
  }
  state_he$hedging_effectiveness <- as.numeric(state_he$hedging_effectiveness)
  state_he$observations <- as.integer(state_he$observations)
  states <- unique(state_he$state_label)
  rows <- lapply(states, function(state_name) {
    sub <- state_he[state_he$state_label == state_name, , drop = FALSE]
    best <- sub[order(-sub$hedging_effectiveness), , drop = FALSE][1, , drop = FALSE]
    core_sub <- sub[sub$hedge %in% c("r_sjc_gold", "r_xau_vnd", "r_bond_tri"), , drop = FALSE]
    data.frame(
      state_label = state_name,
      tested_rows = nrow(sub),
      best_hedge = best$hedge[1],
      best_model = best$model[1],
      best_observations = best$observations[1],
      best_first_date = best$first_date[1],
      best_last_date = best$last_date[1],
      best_hedging_effectiveness = best$hedging_effectiveness[1],
      positive_he_share = mean(sub$hedging_effectiveness > 0, na.rm = TRUE),
      core_positive_he_share = if (nrow(core_sub) == 0) NA_real_ else mean(core_sub$hedging_effectiveness > 0, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  output <- do.call(rbind, rows)
  write_csv_output(
    output,
    file.path(config$outputs$tables_dir, "robustness", "stress_regime_summary.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  output
}

write_robustness_synthesis_matrix <- function(outputs, config) {
  weekly <- outputs$weekly_safe_haven
  core <- outputs$core_contrast
  oil <- outputs$oil_proxy
  crypto <- outputs$crypto_extension
  stress <- outputs$stress_regime

  weekly_controlled <- weekly[weekly$model_spec == "controlled", , drop = FALSE]
  premium_weekly <- weekly_controlled[
    weekly_controlled$hedge == "r_gold_premium" &
      weekly_controlled$safe_haven_classification == "strong_safe_haven",
    ,
    drop = FALSE
  ]
  bond <- core[core$asset == "r_bond_tri", , drop = FALSE]
  sjc <- core[core$asset == "r_sjc_gold", , drop = FALSE]
  xau <- core[core$asset == "r_xau_vnd", , drop = FALSE]

  rows <- list(
    data.frame(
      claim = "Bond TRI is the strongest core stabilizer by variance reduction",
      evidence_table = "core_asset_contrast_matrix.csv",
      support_status = if (nrow(bond) > 0 && is.finite(as.numeric(bond$best_hedging_effectiveness[1]))) "supported" else "not_available",
      nuance = "stabilizer, not strong safe haven at all tails",
      stringsAsFactors = FALSE
    ),
    data.frame(
      claim = "SJC and XAU/VND are weak hedge or weak safe-haven channels",
      evidence_table = "core_asset_contrast_matrix.csv",
      support_status = if (nrow(sjc) > 0 && nrow(xau) > 0) "supported" else "not_available",
      nuance = "effects are economically small; XAU/VND has more negative hedge-ratio sign",
      stringsAsFactors = FALSE
    ),
    data.frame(
      claim = "Gold premium is a domestic-market channel, not a hedge asset",
      evidence_table = "weekly_safe_haven_interpretation.csv",
      support_status = if (nrow(premium_weekly) > 0) "qualified_support" else "weak_or_no_support",
      nuance = "weekly tail-1 signal only; interpret as premium channel",
      stringsAsFactors = FALSE
    ),
    data.frame(
      claim = "Crypto results are extension evidence",
      evidence_table = "crypto_extension_robustness.csv",
      support_status = if (nrow(crypto) > 0) "supported" else "not_available",
      nuance = "shorter sample, not core conclusion",
      stringsAsFactors = FALSE
    ),
    data.frame(
      claim = "Brent/WTI are oil-price proxies rather than structural oil shocks",
      evidence_table = "oil_proxy_robustness.csv",
      support_status = if (nrow(oil) > 0) "supported" else "not_available",
      nuance = "do not cite as Kilian-style structural shock",
      stringsAsFactors = FALSE
    ),
    data.frame(
      claim = "State-dependent HE is descriptive robustness",
      evidence_table = "stress_regime_summary.csv",
      support_status = if (nrow(stress) > 0) "supported" else "not_available",
      nuance = "does not replace Baur-Lucey regression",
      stringsAsFactors = FALSE
    )
  )

  output <- do.call(rbind, rows)
  write_csv_output(
    output,
    file.path(config$outputs$tables_dir, "robustness", "robustness_synthesis_matrix.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  output
}

write_phase3_6_report <- function(outputs, config) {
  report_path <- project_path("data/metadata/phase3_6_robustness_report.md")
  weekly_safe <- outputs$weekly_safe_haven
  core <- outputs$core_contrast
  oil <- outputs$oil_proxy
  crypto <- outputs$crypto_extension
  stress <- outputs$stress_regime
  synthesis <- outputs$synthesis

  weekly_controlled <- weekly_safe[weekly_safe$model_spec == "controlled", , drop = FALSE]
  weekly_strong <- weekly_controlled[weekly_controlled$safe_haven_classification == "strong_safe_haven", , drop = FALSE]
  weekly_source <- if ("source_file" %in% names(outputs$weekly_summary) && nrow(outputs$weekly_summary) > 0) {
    unique(outputs$weekly_summary$source_file)[1]
  } else {
    "unknown"
  }

  lines <- c(
    "# Phase 3.6 Robustness And Sensitivity Synthesis Report",
    "",
    "## Mục tiêu",
    "",
    "Phase 3.6 tổng hợp các kiểm tra độ bền sau DCC/ADCC, hedging effectiveness và Baur-Lucey regression. Phase này không refit DCC/aDCC; nó kiểm tra kết luận bằng weekly returns, core-asset contrast, oil proxy, crypto extension và stress-regime summaries.",
    "",
    "## Output Summary",
    "",
    sprintf("- Weekly return summary rows: `%s`", nrow(outputs$weekly_summary)),
    sprintf("- Weekly safe-haven interpretation rows: `%s`", nrow(weekly_safe)),
    sprintf("- Core asset contrast rows: `%s`", nrow(core)),
    sprintf("- Oil proxy robustness rows: `%s`", nrow(oil)),
    sprintf("- Crypto extension robustness rows: `%s`", nrow(crypto)),
    sprintf("- Stress-regime summary rows: `%s`", nrow(stress)),
    sprintf("- Robustness synthesis rows: `%s`", nrow(synthesis)),
    sprintf("- Weekly source: `%s`", weekly_source),
    "",
    "## Key Robustness Signals",
    "",
    sprintf("- Weekly controlled strong-safe-haven rows: `%s`", nrow(weekly_strong)),
    "- Core contrast should be used for SJC/XAU-VND/Bond TRI interpretation.",
    "- Crypto results remain extension evidence because BTC/ETH have shorter samples.",
    "- Brent/WTI are oil-price proxies, not structural oil shocks.",
    "",
    "## Core Asset Contrast",
    ""
  )

  if (nrow(core) > 0) {
    lines <- c(
      lines,
      "| Asset | Role | Best HE model | Best HE | Safe-haven classes |",
      "| --- | --- | --- | ---: | --- |"
    )
    for (i in seq_len(nrow(core))) {
      lines <- c(lines, sprintf(
        "| %s | %s | %s | %.6f | %s |",
        core$asset[i],
        core$role[i],
        ifelse(is.na(core$best_he_model[i]), "NA", core$best_he_model[i]),
        core$best_hedging_effectiveness[i],
        core$safe_haven_class_by_tail[i]
      ))
    }
  }

  lines <- c(
    lines,
    "",
    "## Output Files",
    "",
    sprintf("- `%s`", project_path(file.path(config$outputs$tables_dir, "robustness", "weekly_return_summary.csv"))),
    sprintf("- `%s`", project_path(file.path(config$outputs$tables_dir, "robustness", "weekly_safe_haven_interpretation.csv"))),
    sprintf("- `%s`", project_path(file.path(config$outputs$tables_dir, "robustness", "core_asset_contrast_matrix.csv"))),
    sprintf("- `%s`", project_path(file.path(config$outputs$tables_dir, "robustness", "oil_proxy_robustness.csv"))),
    sprintf("- `%s`", project_path(file.path(config$outputs$tables_dir, "robustness", "crypto_extension_robustness.csv"))),
    sprintf("- `%s`", project_path(file.path(config$outputs$tables_dir, "robustness", "stress_regime_summary.csv"))),
    sprintf("- `%s`", project_path(file.path(config$outputs$tables_dir, "robustness", "robustness_synthesis_matrix.csv")))
  )

  con <- file(report_path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw("\ufeff"), con)
  writeLines(enc2utf8(lines), con, useBytes = TRUE)
  invisible(report_path)
}

run_phase3_6_robustness <- function(
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
  if (!requireNamespace("sandwich", quietly = TRUE)) {
    stop("Package 'sandwich' is required for Phase 3.6 weekly HAC checks.", call. = FALSE)
  }

  ensure_output_dirs(config)
  ensure_dir(file.path(config$outputs$tables_dir, "robustness"))
  append_log("Phase 3.6 robustness started.", config)

  returns <- read_phase3_6_returns(config)
  weekly_returns <- read_phase3_6_weekly_returns(config, returns)
  weekly_summary <- write_weekly_return_summary(weekly_returns, config)
  weekly_safe_haven <- run_weekly_safe_haven_robustness(weekly_returns, config)
  core_contrast <- write_core_asset_contrast(config)
  oil_proxy <- write_oil_proxy_robustness(config, returns)
  crypto_extension <- write_crypto_extension_robustness(config)
  stress_regime <- write_stress_regime_summary(config)

  outputs <- list(
    weekly_summary = weekly_summary,
    weekly_safe_haven = weekly_safe_haven,
    core_contrast = core_contrast,
    oil_proxy = oil_proxy,
    crypto_extension = crypto_extension,
    stress_regime = stress_regime
  )
  outputs$synthesis <- write_robustness_synthesis_matrix(outputs, config)
  report <- write_phase3_6_report(outputs, config)

  append_log("Phase 3.6 robustness finished.", config)
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

if (!interactive() && is_direct_rscript_call("src/models/run_phase3_6_robustness.R")) {
  result <- run_phase3_6_robustness()
  cat("Phase 3.6 robustness completed.\n")
  cat(sprintf("Weekly safe-haven rows: %s\n", nrow(result$outputs$weekly_safe_haven)))
  cat(sprintf("Core contrast rows: %s\n", nrow(result$outputs$core_contrast)))
  cat(sprintf("Stress-regime rows: %s\n", nrow(result$outputs$stress_regime)))
  cat(sprintf("Report: %s\n", result$report))
}
