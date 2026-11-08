source("src/utils/config_loader.R")
source("src/utils/io_helpers.R")
source("src/utils/date_asset_helpers.R")
source("src/models/validate_model_control.R")
source("src/models/R/model_helpers.R")
source("src/visualization/scientific_theme.R")

phase3_8_read_returns <- function(config) {
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
    returns[[col]] <- suppressWarnings(as.numeric(returns[[col]]))
  }
  returns
}

phase3_8_read_csv <- function(path) {
  full_path <- project_path(path)
  if (!file.exists(full_path)) {
    return(data.frame())
  }
  out <- utils::read.csv(full_path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
  strip_bom_from_names(out)
}

state_prob_matrix <- function(state_object) {
  prob_array <- if ("SmoothProb" %in% names(state_object)) {
    state_object$SmoothProb
  } else {
    state_object$FiltProb
  }
  if (length(dim(prob_array)) == 3) {
    prob_matrix <- prob_array[, 1, , drop = FALSE]
    prob_matrix <- matrix(
      prob_matrix,
      nrow = dim(prob_array)[1],
      ncol = dim(prob_array)[3]
    )
  } else {
    prob_matrix <- as.matrix(prob_array)
  }
  colnames(prob_matrix) <- paste0("regime_", seq_len(ncol(prob_matrix)), "_prob")
  prob_matrix
}

regime_labels_from_returns <- function(target_return, dominant_regime, k) {
  regime_sd <- vapply(seq_len(k), function(regime) {
    x <- target_return[dominant_regime == regime]
    if (length(stats::na.omit(x)) < 20) return(NA_real_)
    stats::sd(x, na.rm = TRUE)
  }, numeric(1))
  if (all(is.na(regime_sd)) || length(unique(stats::na.omit(regime_sd))) < k) {
    ordered <- seq_len(k)
  } else {
    ordered <- order(regime_sd)
  }
  labels <- rep(NA_character_, k)
  labels[ordered[1]] <- "low_volatility_regime"
  if (k >= 2) {
    labels[ordered[k]] <- "high_volatility_regime"
  }
  if (k > 2) {
    labels[ordered[2:(k - 1)]] <- "medium_volatility_regime"
  }
  labels
}

create_msgarch_spec <- function(k, distribution) {
  MSGARCH::CreateSpec(
    variance.spec = list(model = rep("sGARCH", k)),
    distribution.spec = list(distribution = rep(distribution, k))
  )
}

write_msgarch_fit_outputs <- function(fit, config, target, dates, target_return, conditional_volatility) {
  k <- as.integer(config$robustness$msgarch$regimes)
  state <- MSGARCH::State(fit)
  prob_matrix <- state_prob_matrix(state)
  if (nrow(prob_matrix) != length(target_return)) {
    prob_matrix <- prob_matrix[
      (nrow(prob_matrix) - length(target_return) + 1):nrow(prob_matrix),
      ,
      drop = FALSE
    ]
  }
  dominant_regime <- max.col(prob_matrix, ties.method = "first")
  labels <- regime_labels_from_returns(target_return, dominant_regime, k)
  regime_label <- labels[dominant_regime]
  high_regime <- which(labels == "high_volatility_regime")[1]
  high_volatility_probability <- if (length(high_regime) == 0 || is.na(high_regime)) {
    NA_real_
  } else {
    prob_matrix[, high_regime]
  }

  state_probabilities <- data.frame(
    date = dates,
    target = target,
    target_return = target_return,
    prob_matrix,
    dominant_regime = dominant_regime,
    regime_label = regime_label,
    high_volatility_probability = high_volatility_probability,
    conditional_volatility = conditional_volatility,
    stringsAsFactors = FALSE
  )
  write_csv_output(
    state_probabilities,
    file.path(config$outputs$tables_dir, "markov_switching", "msgarch_state_probabilities.csv"),
    overwrite = config$outputs$overwrite_existing
  )

  par <- fit$par
  n_par <- length(par)
  n_obs <- length(target_return)
  fit_summary <- data.frame(
    target = target,
    model = "MSGARCH_sGARCH",
    regimes = k,
    distribution = config$robustness$msgarch$distribution,
    observations = n_obs,
    first_date = min(dates),
    last_date = max(dates),
    parameters = n_par,
    log_likelihood = as.numeric(fit$loglik),
    aic = -2 * as.numeric(fit$loglik) + 2 * n_par,
    bic = -2 * as.numeric(fit$loglik) + log(n_obs) * n_par,
    mean_conditional_volatility = mean(conditional_volatility, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  write_csv_output(
    fit_summary,
    file.path(config$outputs$tables_dir, "markov_switching", "msgarch_fit_summary.csv"),
    overwrite = config$outputs$overwrite_existing
  )

  parameter_estimates <- data.frame(
    parameter = names(par),
    estimate = as.numeric(par),
    stringsAsFactors = FALSE
  )
  write_csv_output(
    parameter_estimates,
    file.path(config$outputs$tables_dir, "markov_switching", "msgarch_parameter_estimates.csv"),
    overwrite = config$outputs$overwrite_existing
  )

  transition_matrix <- as.data.frame(MSGARCH::TransMat(fit), stringsAsFactors = FALSE)
  transition_matrix$from_regime <- rownames(transition_matrix)
  transition_matrix <- transition_matrix[, c("from_regime", setdiff(names(transition_matrix), "from_regime")), drop = FALSE]
  write_csv_output(
    transition_matrix,
    file.path(config$outputs$tables_dir, "markov_switching", "msgarch_transition_matrix.csv"),
    overwrite = config$outputs$overwrite_existing
  )

  regime_rows <- lapply(seq_len(k), function(regime) {
    sub <- state_probabilities[state_probabilities$dominant_regime == regime, , drop = FALSE]
    probability_col <- paste0("regime_", regime, "_prob")
    data.frame(
      regime = regime,
      regime_label = labels[regime],
      observations = nrow(sub),
      share_of_sample = nrow(sub) / nrow(state_probabilities),
      first_date = if (nrow(sub) == 0) as.Date(NA) else min(sub$date),
      last_date = if (nrow(sub) == 0) as.Date(NA) else max(sub$date),
      mean_target_return = if (nrow(sub) == 0) NA_real_ else mean(sub$target_return, na.rm = TRUE),
      sd_target_return = if (nrow(sub) == 0) NA_real_ else stats::sd(sub$target_return, na.rm = TRUE),
      min_target_return = if (nrow(sub) == 0) NA_real_ else min(sub$target_return, na.rm = TRUE),
      max_target_return = if (nrow(sub) == 0) NA_real_ else max(sub$target_return, na.rm = TRUE),
      mean_conditional_volatility = if (nrow(sub) == 0) NA_real_ else mean(sub$conditional_volatility, na.rm = TRUE),
      mean_regime_probability = if (nrow(sub) == 0) NA_real_ else mean(sub[[probability_col]], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  regime_summary <- do.call(rbind, regime_rows)
  write_csv_output(
    regime_summary,
    file.path(config$outputs$tables_dir, "markov_switching", "msgarch_regime_summary.csv"),
    overwrite = config$outputs$overwrite_existing
  )

  list(
    fit_summary = fit_summary,
    parameter_estimates = parameter_estimates,
    transition_matrix = transition_matrix,
    state_probabilities = state_probabilities,
    regime_summary = regime_summary
  )
}

write_markov_hedging_by_regime <- function(config, state_probabilities) {
  hedge_data <- phase3_8_read_csv("results/tables/hedge_ratios/dynamic_hedge_ratios.csv")
  if (nrow(hedge_data) == 0) {
    output <- data.frame()
    write_csv_output(
      output,
      file.path(config$outputs$tables_dir, "markov_switching", "msgarch_hedging_by_regime.csv"),
      overwrite = config$outputs$overwrite_existing
    )
    return(output)
  }
  hedge_data$date <- as.Date(hedge_data$date)
  numeric_cols <- c(
    "dynamic_correlation", "conditional_covariance", "target_variance",
    "hedge_variance", "hedge_ratio", "target_return", "hedge_return",
    "hedged_return"
  )
  for (col in intersect(numeric_cols, names(hedge_data))) {
    hedge_data[[col]] <- suppressWarnings(as.numeric(hedge_data[[col]]))
  }
  core_assets <- vapply(config$assets$core_hedges, asset_return_name, character(1))
  hedge_data <- hedge_data[hedge_data$hedge %in% core_assets, , drop = FALSE]
  merged <- merge(
    hedge_data,
    state_probabilities[, c("date", "dominant_regime", "regime_label", "high_volatility_probability")],
    by = "date",
    all.x = FALSE,
    all.y = FALSE
  )
  if (nrow(merged) == 0) {
    output <- data.frame()
  } else {
    keys <- unique(merged[, c("hedge", "model", "dominant_regime", "regime_label"), drop = FALSE])
    rows <- lapply(seq_len(nrow(keys)), function(i) {
      key <- keys[i, , drop = FALSE]
      sub <- merged[
        merged$hedge == key$hedge &
          merged$model == key$model &
          merged$dominant_regime == key$dominant_regime,
        ,
        drop = FALSE
      ]
      unhedged_variance <- stats::var(sub$target_return, na.rm = TRUE)
      hedged_variance <- stats::var(sub$hedged_return, na.rm = TRUE)
      data.frame(
        target = unique(sub$target)[1],
        hedge = key$hedge,
        model = key$model,
        dominant_regime = key$dominant_regime,
        regime_label = key$regime_label,
        observations = nrow(sub),
        first_date = min(sub$date),
        last_date = max(sub$date),
        mean_dynamic_correlation = mean(sub$dynamic_correlation, na.rm = TRUE),
        mean_hedge_ratio = mean(sub$hedge_ratio, na.rm = TRUE),
        unhedged_variance = unhedged_variance,
        hedged_variance = hedged_variance,
        hedging_effectiveness = if (is.finite(unhedged_variance) && unhedged_variance > 0) {
          1 - hedged_variance / unhedged_variance
        } else {
          NA_real_
        },
        mean_high_volatility_probability = mean(sub$high_volatility_probability, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    })
    output <- do.call(rbind, rows)
    output <- output[order(output$hedge, output$model, output$dominant_regime), , drop = FALSE]
  }
  write_csv_output(
    output,
    file.path(config$outputs$tables_dir, "markov_switching", "msgarch_hedging_by_regime.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  output
}

write_markov_synthesis <- function(config, fit_outputs, hedging_by_regime) {
  regime_summary <- fit_outputs$regime_summary
  high <- regime_summary[regime_summary$regime_label == "high_volatility_regime", , drop = FALSE]
  low <- regime_summary[regime_summary$regime_label == "low_volatility_regime", , drop = FALSE]
  high_he <- hedging_by_regime[hedging_by_regime$regime_label == "high_volatility_regime", , drop = FALSE]
  low_he <- hedging_by_regime[hedging_by_regime$regime_label == "low_volatility_regime", , drop = FALSE]
  best_high <- if (nrow(high_he) > 0) {
    high_he[order(-as.numeric(high_he$hedging_effectiveness)), , drop = FALSE][1, , drop = FALSE]
  } else {
    data.frame()
  }
  best_low <- if (nrow(low_he) > 0) {
    low_he[order(-as.numeric(low_he$hedging_effectiveness)), , drop = FALSE][1, , drop = FALSE]
  } else {
    data.frame()
  }
  rows <- list(
    data.frame(
      item = "msgarch_regime_identification",
      evidence_table = "msgarch_regime_summary.csv",
      result = if (nrow(high) > 0 && nrow(low) > 0) "two_volatility_regimes_identified" else "regime_identification_incomplete",
      interpretation = "Use as robustness for stress-state heterogeneity, not as replacement for DCC/aDCC.",
      stringsAsFactors = FALSE
    ),
    data.frame(
      item = "high_volatility_state_share",
      evidence_table = "msgarch_regime_summary.csv",
      result = if (nrow(high) == 0) "not_available" else sprintf("%.4f", high$share_of_sample[1]),
      interpretation = "Shows how often VNIndex is classified into the high-volatility regime.",
      stringsAsFactors = FALSE
    ),
    data.frame(
      item = "best_high_volatility_hedge",
      evidence_table = "msgarch_hedging_by_regime.csv",
      result = if (nrow(best_high) == 0) "not_available" else sprintf(
        "%s/%s HE=%.6f",
        best_high$hedge[1],
        best_high$model[1],
        best_high$hedging_effectiveness[1]
      ),
      interpretation = "Use this as regime-conditional hedging evidence.",
      stringsAsFactors = FALSE
    ),
    data.frame(
      item = "best_low_volatility_hedge",
      evidence_table = "msgarch_hedging_by_regime.csv",
      result = if (nrow(best_low) == 0) "not_available" else sprintf(
        "%s/%s HE=%.6f",
        best_low$hedge[1],
        best_low$model[1],
        best_low$hedging_effectiveness[1]
      ),
      interpretation = "Contrasts normal-market and stress-market hedge performance.",
      stringsAsFactors = FALSE
    )
  )
  output <- do.call(rbind, rows)
  write_csv_output(
    output,
    file.path(config$outputs$tables_dir, "markov_switching", "markov_switching_synthesis.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  output
}

plot_markov_probability <- function(config, state_probabilities) {
  ensure_dir(file.path(config$outputs$figures_dir, "markov_switching"))
  png_path <- project_path(file.path(config$outputs$figures_dir, "markov_switching", "figure_09_msgarch_high_volatility_probability.png"))
  svg_path <- project_path(file.path(config$outputs$figures_dir, "markov_switching", "figure_09_msgarch_high_volatility_probability.svg"))
  plot_one <- function(path, device) {
    if (identical(device, "png")) {
      grDevices::png(path, width = 2400, height = 1300, res = 220)
    } else {
      grDevices::svg(path, width = 11, height = 6)
    }
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit({
      graphics::par(old_par)
      grDevices::dev.off()
    }, add = TRUE)
    graphics::par(mar = c(4.8, 5, 3.2, 1.2), family = "sans")
    graphics::plot(
      state_probabilities$date,
      state_probabilities$high_volatility_probability,
      type = "l",
      col = scientific_palette()[["vermillion"]],
      lwd = 1.25,
      xlab = "Date",
      ylab = "Probability",
      main = "MSGARCH high-volatility regime probability",
      ylim = c(0, 1),
      las = 1
    )
    graphics::abline(h = c(0.25, 0.5, 0.75), col = "gray70", lty = c(3, 2, 3), lwd = 0.8)
    graphics::box()
  }
  plot_one(png_path, "png")
  plot_one(svg_path, "svg")
  data.frame(
    figure = c("figure_09_msgarch_high_volatility_probability.png", "figure_09_msgarch_high_volatility_probability.svg"),
    path = c(
      file.path(config$outputs$figures_dir, "markov_switching", "figure_09_msgarch_high_volatility_probability.png"),
      file.path(config$outputs$figures_dir, "markov_switching", "figure_09_msgarch_high_volatility_probability.svg")
    ),
    stringsAsFactors = FALSE
  )
}

write_phase3_8_report <- function(config, outputs) {
  report_path <- project_path("data/metadata/phase3_8_markov_switching_report.md")
  regime_summary <- outputs$fit_outputs$regime_summary
  fit_summary <- outputs$fit_outputs$fit_summary
  hedging <- outputs$hedging_by_regime
  synthesis <- outputs$synthesis
  high <- regime_summary[regime_summary$regime_label == "high_volatility_regime", , drop = FALSE]
  best_high <- hedging[hedging$regime_label == "high_volatility_regime", , drop = FALSE]
  if (nrow(best_high) > 0) {
    best_high <- best_high[order(-as.numeric(best_high$hedging_effectiveness)), , drop = FALSE][1, , drop = FALSE]
  }

  lines <- c(
    "# Phase 3.8 Markov-Switching Robustness Report",
    "",
    "## Mục tiêu",
    "",
    "Phase 3.8 bổ sung kiểm tra Markov-switching bằng MSGARCH hai trạng thái cho lợi suất VNIndex. Mục đích là nhận diện trạng thái biến động thấp/cao và kiểm tra xem hiệu quả phòng ngừa rủi ro của SJC, XAU/VND và Bond TRI có thay đổi theo regime hay không.",
    "",
    "## Model",
    "",
    sprintf("- Target: `%s`", fit_summary$target[1]),
    sprintf("- Model: `%s`", fit_summary$model[1]),
    sprintf("- Regimes: `%s`", fit_summary$regimes[1]),
    sprintf("- Distribution: `%s`", fit_summary$distribution[1]),
    sprintf("- Observations: `%s`", fit_summary$observations[1]),
    sprintf("- Sample: `%s` to `%s`", fit_summary$first_date[1], fit_summary$last_date[1]),
    sprintf("- BIC: `%.6f`", fit_summary$bic[1]),
    "",
    "## Key Signals",
    "",
    sprintf("- Regime summary rows: `%s`", nrow(regime_summary)),
    sprintf("- State-probability rows: `%s`", nrow(outputs$fit_outputs$state_probabilities)),
    sprintf("- Regime-hedging rows: `%s`", nrow(hedging)),
    sprintf("- Synthesis rows: `%s`", nrow(synthesis)),
    sprintf("- High-volatility state share: `%s`", if (nrow(high) == 0) "NA" else sprintf("%.4f", high$share_of_sample[1])),
    sprintf(
      "- Best high-volatility hedge: `%s`",
      if (nrow(best_high) == 0) {
        "NA"
      } else {
        sprintf("%s/%s, HE=%.6f", best_high$hedge[1], best_high$model[1], best_high$hedging_effectiveness[1])
      }
    ),
    "",
    "## Interpretation Rules",
    "",
    "- Markov-switching là robustness layer, không thay thế DCC/aDCC làm mô hình tương quan động chính.",
    "- Regime được gắn nhãn theo độ lệch chuẩn thực nghiệm của lợi suất VNIndex trong trạng thái chiếm ưu thế.",
    "- Kết luận manuscript nên dùng Phase 3.8 để củng cố luận điểm state-dependent hedging, không dùng để phóng đại safe-haven classification.",
    "",
    "## Output Files",
    "",
    sprintf("- `%s`", project_path(file.path(config$outputs$tables_dir, "markov_switching", "msgarch_fit_summary.csv"))),
    sprintf("- `%s`", project_path(file.path(config$outputs$tables_dir, "markov_switching", "msgarch_parameter_estimates.csv"))),
    sprintf("- `%s`", project_path(file.path(config$outputs$tables_dir, "markov_switching", "msgarch_transition_matrix.csv"))),
    sprintf("- `%s`", project_path(file.path(config$outputs$tables_dir, "markov_switching", "msgarch_state_probabilities.csv"))),
    sprintf("- `%s`", project_path(file.path(config$outputs$tables_dir, "markov_switching", "msgarch_regime_summary.csv"))),
    sprintf("- `%s`", project_path(file.path(config$outputs$tables_dir, "markov_switching", "msgarch_hedging_by_regime.csv"))),
    sprintf("- `%s`", project_path(file.path(config$outputs$tables_dir, "markov_switching", "markov_switching_synthesis.csv"))),
    sprintf("- `%s`", project_path(file.path(config$outputs$figures_dir, "markov_switching", "figure_09_msgarch_high_volatility_probability.png"))),
    sprintf("- `%s`", project_path(file.path(config$outputs$figures_dir, "markov_switching", "figure_09_msgarch_high_volatility_probability.svg")))
  )

  con <- file(report_path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw("\ufeff"), con)
  writeLines(enc2utf8(lines), con, useBytes = TRUE)
  invisible(report_path)
}

run_phase3_8_markov_switching <- function(
  control_path = "src/models/model_control.yaml",
  method_map_path = "src/models/model_method_map.yaml",
  strict_packages = FALSE
) {
  safe_package_stop("MSGARCH")
  config <- read_model_control(control_path)
  method_map <- read_model_method_map(method_map_path)
  validation <- validate_model_control(config, method_map, strict_packages = strict_packages)
  if (!validation$ok) {
    stop(paste(validation$errors, collapse = "\n"), call. = FALSE)
  }
  ensure_output_dirs(config)
  ensure_dir(file.path(config$outputs$tables_dir, "markov_switching"))
  ensure_dir(file.path(config$outputs$figures_dir, "markov_switching"))
  append_log("Phase 3.8 Markov-switching robustness started.", config)

  returns <- phase3_8_read_returns(config)
  target <- asset_return_name(config$assets$target)
  if (!target %in% names(returns)) {
    stop(sprintf("Target return column not found: %s", target), call. = FALSE)
  }
  valid <- is.finite(returns[[target]])
  dates <- returns$date[valid]
  target_return <- returns[[target]][valid]
  k <- as.integer(config$robustness$msgarch$regimes)
  spec <- create_msgarch_spec(k, config$robustness$msgarch$distribution)
  fit_result <- fit_status(MSGARCH::FitML(spec = spec, data = target_return, ctr = list(do.se = FALSE)))
  if (!fit_result$ok) {
    write_warning_table(
      sprintf("Phase 3.8 MSGARCH failed: %s", fit_result$error),
      file.path(config$outputs$logs_dir, "convergence_warnings", "msgarch_phase3_8_warnings.csv"),
      config
    )
    stop(sprintf("Phase 3.8 MSGARCH failed: %s", fit_result$error), call. = FALSE)
  }
  fit <- fit_result$value
  conditional_volatility <- as.numeric(MSGARCH::Volatility(fit))
  if (length(conditional_volatility) != length(target_return)) {
    conditional_volatility <- utils::tail(conditional_volatility, length(target_return))
  }
  fit_outputs <- write_msgarch_fit_outputs(fit, config, target, dates, target_return, conditional_volatility)
  hedging_by_regime <- write_markov_hedging_by_regime(config, fit_outputs$state_probabilities)
  synthesis <- write_markov_synthesis(config, fit_outputs, hedging_by_regime)
  figures <- plot_markov_probability(config, fit_outputs$state_probabilities)
  outputs <- list(
    fit_outputs = fit_outputs,
    hedging_by_regime = hedging_by_regime,
    synthesis = synthesis,
    figures = figures
  )
  report <- write_phase3_8_report(config, outputs)

  append_log("Phase 3.8 Markov-switching robustness finished.", config)
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

if (!interactive() && is_direct_rscript_call("src/models/run_phase3_8_markov_switching.R")) {
  result <- run_phase3_8_markov_switching()
  cat("Phase 3.8 Markov-switching robustness completed.\n")
  cat(sprintf("Regime rows: %s\n", nrow(result$outputs$fit_outputs$regime_summary)))
  cat(sprintf("Regime-hedging rows: %s\n", nrow(result$outputs$hedging_by_regime)))
  cat(sprintf("Synthesis rows: %s\n", nrow(result$outputs$synthesis)))
  cat(sprintf("Report: %s\n", result$report))
}
