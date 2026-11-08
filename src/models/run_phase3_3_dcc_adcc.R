source("src/utils/config_loader.R")
source("src/utils/io_helpers.R")
source("src/utils/date_asset_helpers.R")
source("src/models/validate_model_control.R")
source("src/models/R/01_univariate_garch.R")
source("src/models/R/02_dcc_adcc_garch.R")
source("src/visualization/plot_dynamic_correlation.R")

write_phase3_3_univariate_parameter_tables <- function(config, returns) {
  best <- read_best_univariate_specs(config)
  if (nrow(best) == 0) {
    return(list(parameters = data.frame(), fit_stats = data.frame()))
  }

  parameter_rows <- list()
  fit_rows <- list()

  for (i in seq_len(nrow(best))) {
    asset <- best$asset[i]
    if (!asset %in% names(returns)) next
    valid_idx <- which(!is.na(as.numeric(returns[[asset]])))
    x <- as.numeric(returns[[asset]])[valid_idx]
    asset_dates <- returns$date[valid_idx]
    if (length(x) < 30) next

    spec <- build_ugarch_spec(best$model[i], best$distribution[i], config)
    fit_result <- fit_status(
      rugarch::ugarchfit(spec = spec, data = x, solver = "hybrid", fit.control = list(stationarity = 1))
    )
    if (!fit_result$ok) next
    fit <- fit_result$value
    coef_vec <- tryCatch(rugarch::coef(fit), error = function(e) numeric())
    matcoef <- tryCatch(fit@fit$matcoef, error = function(e) NULL)
    parameter_names <- names(coef_vec)
    std_error <- rep(NA_real_, length(coef_vec))
    t_value <- rep(NA_real_, length(coef_vec))
    p_value <- rep(NA_real_, length(coef_vec))
    if (!is.null(matcoef) && length(coef_vec) > 0) {
      colnames(matcoef) <- trimws(colnames(matcoef))
      matched <- match(parameter_names, rownames(matcoef))
      valid <- !is.na(matched)
      mat_cols <- colnames(matcoef)
      if ("Std. Error" %in% mat_cols) {
        std_error[valid] <- suppressWarnings(as.numeric(matcoef[matched[valid], "Std. Error"]))
      }
      if ("t value" %in% mat_cols) {
        t_value[valid] <- suppressWarnings(as.numeric(matcoef[matched[valid], "t value"]))
      }
      if ("Pr(>|t|)" %in% mat_cols) {
        p_value[valid] <- suppressWarnings(as.numeric(matcoef[matched[valid], "Pr(>|t|)"]))
      }
    }
    parameter_rows[[length(parameter_rows) + 1]] <- data.frame(
      asset = asset,
      model = best$model[i],
      distribution = best$distribution[i],
      parameter = parameter_names,
      estimate = as.numeric(coef_vec),
      std_error = std_error,
      t_value = t_value,
      p_value = p_value,
      observations = length(x),
      first_date = min(asset_dates),
      last_date = max(asset_dates),
      convergence = tryCatch(rugarch::convergence(fit), error = function(e) NA_integer_),
      stringsAsFactors = FALSE
    )
    llh <- tryCatch(as.numeric(rugarch::likelihood(fit)), error = function(e) NA_real_)
    k <- length(coef_vec)
    n <- length(x)
    fit_rows[[length(fit_rows) + 1]] <- data.frame(
      asset = asset,
      model = best$model[i],
      distribution = best$distribution[i],
      observations = n,
      first_date = min(asset_dates),
      last_date = max(asset_dates),
      logLik = llh,
      n_parameters = k,
      AIC = information_value(fit, "Akaike"),
      BIC = information_value(fit, "Bayes"),
      HQIC = -2 * llh + 2 * k * log(log(n)),
      convergence = tryCatch(rugarch::convergence(fit), error = function(e) NA_integer_),
      stringsAsFactors = FALSE
    )
  }

  parameters <- if (length(parameter_rows) == 0) data.frame() else do.call(rbind, parameter_rows)
  fit_stats <- if (length(fit_rows) == 0) data.frame() else do.call(rbind, fit_rows)
  if (nrow(parameters) > 0) {
    write_csv_output(
      parameters,
      file.path(config$outputs$tables_dir, "model_selection", "univariate_garch_parameter_estimates.csv"),
      overwrite = config$outputs$overwrite_existing
    )
  }
  if (nrow(fit_stats) > 0) {
    write_csv_output(
      fit_stats,
      file.path(config$outputs$tables_dir, "model_selection", "univariate_garch_fit_statistics.csv"),
      overwrite = config$outputs$overwrite_existing
    )
  }
  list(parameters = parameters, fit_stats = fit_stats)
}

write_phase3_3_report <- function(dcc_output, config, returns) {
  report_path <- project_path("data/metadata/phase3_3_dcc_adcc_report.md")
  summary_path <- project_path(file.path(config$outputs$tables_dir, "dynamic_correlations", "dcc_adcc_summary.csv"))
  parameter_path <- project_path(file.path(config$outputs$tables_dir, "dynamic_correlations", "dcc_adcc_parameter_estimates.csv"))
  fit_stats_path <- project_path(file.path(config$outputs$tables_dir, "dynamic_correlations", "dcc_adcc_model_fit_statistics.csv"))
  residual_path <- project_path(file.path(config$outputs$diagnostics_dir, "residual", "dcc_adcc_residual_diagnostics.csv"))
  uni_parameter_path <- project_path(file.path(config$outputs$tables_dir, "model_selection", "univariate_garch_parameter_estimates.csv"))
  uni_fit_path <- project_path(file.path(config$outputs$tables_dir, "model_selection", "univariate_garch_fit_statistics.csv"))
  dynamic_path <- project_path(file.path(config$outputs$tables_dir, "dynamic_correlations", "dcc_adcc_dynamic_correlations.csv"))
  warning_path <- project_path(file.path(config$outputs$logs_dir, "convergence_warnings", "dcc_adcc_warnings.csv"))
  figure_dir <- project_path(file.path(config$outputs$figures_dir, "dynamic_correlations"))
  figure_count <- if (dir.exists(figure_dir)) {
    length(list.files(figure_dir, pattern = "\\.(png|svg)$", full.names = TRUE))
  } else {
    0L
  }

  summary <- if (file.exists(summary_path)) {
    utils::read.csv(summary_path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    data.frame()
  }
  warnings <- if (file.exists(warning_path)) {
    utils::read.csv(warning_path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    data.frame()
  }
  parameters <- if (file.exists(parameter_path)) utils::read.csv(parameter_path, stringsAsFactors = FALSE, check.names = FALSE) else data.frame()
  fit_stats <- if (file.exists(fit_stats_path)) utils::read.csv(fit_stats_path, stringsAsFactors = FALSE, check.names = FALSE) else data.frame()
  residuals <- if (file.exists(residual_path)) utils::read.csv(residual_path, stringsAsFactors = FALSE, check.names = FALSE) else data.frame()
  uni_parameters <- if (file.exists(uni_parameter_path)) utils::read.csv(uni_parameter_path, stringsAsFactors = FALSE, check.names = FALSE) else data.frame()
  uni_fit <- if (file.exists(uni_fit_path)) utils::read.csv(uni_fit_path, stringsAsFactors = FALSE, check.names = FALSE) else data.frame()

  lines <- c(
    "# Phase 3.3 DCC/ADCC-GARCH Report",
    "",
    "## Mục tiêu",
    "",
    "Phase 3.3 ước lượng tương quan giữa VNIndex và từng tài sản hedge/extension bằng CCC-GARCH, DCC-GARCH và ADCC-GARCH. Benchmark `CCC` được xây theo tinh thần Bollerslev (1990): phương sai có điều kiện đến từ GARCH đơn biến, còn tương quan của standardized residuals được giữ hằng số.",
    "",
    "## Input",
    "",
    sprintf("- Prepared returns: `%s`", config$data$returns_input),
    "- Best univariate specs: `results/tables/model_selection/univariate_garch_best_by_bic.csv`",
    sprintf("- Sample after config filter: `%s` to `%s`", min(returns$date), max(returns$date)),
    sprintf("- Return columns available: `%s`", paste(setdiff(names(returns), "date"), collapse = "`, `")),
    "",
    "## Model Setup",
    "",
    sprintf("- Target: `%s`", asset_return_name(config$assets$target)),
    sprintf("- Pair mode: `%s`", config$dcc_adcc$pair_mode),
    sprintf("- Benchmark models: `%s`", paste(config$dcc_adcc$benchmark_models, collapse = "`, `")),
    sprintf("- Main model: `%s`", config$dcc_adcc$main_model),
    sprintf("- DCC distribution: `%s`", config$dcc_adcc$distribution),
    sprintf("- Univariate spec source: `%s`", config$dcc_adcc$univariate_spec),
    "",
    "## Output Summary",
    "",
    sprintf("- Dynamic correlation rows: `%s`", nrow(dcc_output)),
    sprintf("- Summary rows: `%s`", nrow(summary)),
    sprintf("- CCC/DCC/aDCC parameter rows: `%s`", nrow(parameters)),
    sprintf("- CCC/DCC/aDCC fit-stat rows: `%s`", nrow(fit_stats)),
    sprintf("- CCC/DCC/aDCC residual diagnostic rows: `%s`", nrow(residuals)),
    sprintf("- Best univariate parameter rows: `%s`", nrow(uni_parameters)),
    sprintf("- Best univariate fit-stat rows: `%s`", nrow(uni_fit)),
    sprintf("- Warning rows: `%s`", nrow(warnings)),
    sprintf("- Figure files: `%s`", figure_count),
    "",
    "## Pairwise Summary",
    ""
  )

  if (nrow(summary) == 0) {
    lines <- c(lines, "No pairwise summary was generated.")
  } else {
    lines <- c(
      lines,
      "| Target | Hedge | Model | Observations | First date | Last date | Mean corr. | Min corr. | Max corr. | Convergence |",
      "| --- | --- | --- | ---: | --- | --- | ---: | ---: | ---: | --- |"
    )
    for (i in seq_len(nrow(summary))) {
      lines <- c(lines, sprintf(
        "| %s | %s | %s | %s | %s | %s | %.6f | %.6f | %.6f | %s |",
        summary$target[i],
        summary$hedge[i],
        summary$model[i],
        summary$observations[i],
        summary$first_date[i],
        summary$last_date[i],
        summary$mean_correlation[i],
        summary$min_correlation[i],
        summary$max_correlation[i],
        summary$convergence[i]
      ))
    }
  }

  if (nrow(warnings) > 0) {
    lines <- c(lines, "", "## Warnings", "")
    for (i in seq_len(nrow(warnings))) {
      lines <- c(lines, sprintf("- %s", warnings$message[i]))
    }
  }

  lines <- c(
    lines,
    "",
    "## Output Files",
    "",
    "- `results/tables/dynamic_correlations/dcc_adcc_dynamic_correlations.csv`",
    "- `results/tables/dynamic_correlations/dcc_adcc_summary.csv`",
    "- `results/tables/dynamic_correlations/dcc_adcc_parameter_estimates.csv`",
    "- `results/tables/dynamic_correlations/dcc_adcc_model_fit_statistics.csv`",
    "- `results/diagnostics/residual/dcc_adcc_residual_diagnostics.csv`",
    "- `results/tables/model_selection/univariate_garch_parameter_estimates.csv`",
    "- `results/tables/model_selection/univariate_garch_fit_statistics.csv`",
    "- `results/figures/dynamic_correlations/`",
    "- `results/model_logs/convergence_warnings/dcc_adcc_warnings.csv` if any model failed",
    "",
    "## Scientific Note",
    "",
    "`CCC` is a true CCC-GARCH benchmark built from univariate GARCH conditional variances and the constant correlation of standardized residuals. DCC and ADCC then test whether allowing time-varying and asymmetric correlations improves the hedge analysis."
  )

  con <- file(report_path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw("\ufeff"), con)
  writeLines(enc2utf8(lines), con, useBytes = TRUE)
  invisible(report_path)
}

run_phase3_3_dcc_adcc <- function(
  control_path = "src/models/model_control.yaml",
  method_map_path = "src/models/model_method_map.yaml",
  strict_packages = TRUE
) {
  config <- read_model_control(control_path)
  method_map <- read_model_method_map(method_map_path)
  validation <- validate_model_control(config, method_map, strict_packages = strict_packages)
  if (!validation$ok) {
    stop(paste(validation$errors, collapse = "\n"), call. = FALSE)
  }
  if (length(validation$warnings) > 0) {
    warning(paste(validation$warnings, collapse = "\n"), call. = FALSE)
  }

  ensure_output_dirs(config)
  write_config_snapshot(project_path(control_path), config)
  append_log("Phase 3.3 DCC/ADCC started.", config)

  returns <- load_model_returns(config)
  dcc_output <- run_dcc_adcc_garch(returns, config)
  write_phase3_3_univariate_parameter_tables(config, returns)
  plot_dynamic_correlations(config)
  report <- write_phase3_3_report(dcc_output, config, returns)

  append_log("Phase 3.3 DCC/ADCC finished.", config)
  list(
    returns = returns,
    dcc_adcc = dcc_output,
    report = report
  )
}

is_direct_rscript_call <- function(file) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
  length(file_arg) > 0 && identical(
    normalizePath(file_arg[1], winslash = "/", mustWork = FALSE),
    normalizePath(file, winslash = "/", mustWork = FALSE)
  )
}

if (!interactive() && is_direct_rscript_call("src/models/run_phase3_3_dcc_adcc.R")) {
  result <- run_phase3_3_dcc_adcc()
  cat("Phase 3.3 DCC/ADCC completed.\n")
  cat(sprintf("Dynamic-correlation rows: %s\n", nrow(result$dcc_adcc)))
  cat(sprintf("Report: %s\n", result$report))
}
