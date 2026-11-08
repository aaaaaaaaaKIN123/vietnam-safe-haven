source("src/utils/config_loader.R")
source("src/utils/io_helpers.R")
source("src/models/validate_model_control.R")
source("src/models/R/01_univariate_garch.R")

phase3_2_return_columns <- function(config) {
  hedge_return_names <- paste0("r_", config$assets$core_hedges)
  hedge_return_names[hedge_return_names == "r_bond_tri_10y"] <- "r_bond_tri"
  unique(c(
    paste0("r_", config$assets$target),
    hedge_return_names,
    "r_brent",
    "r_wti",
    paste0("r_", config$assets$extension_hedges)
  ))
}

load_phase3_2_returns <- function(config) {
  input_path <- project_path("data/02_model_input/core/final_daily_returns.csv")
  if (!file.exists(input_path)) {
    stop(sprintf("Phase 3.2 return file not found: %s", input_path), call. = FALSE)
  }

  returns <- utils::read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE)
  returns$date <- as.Date(returns$date)
  returns <- returns[returns$date > as.Date(config$data$sample_start) &
                       returns$date <= as.Date(config$data$sample_end), , drop = FALSE]

  selected <- intersect(phase3_2_return_columns(config), names(returns))
  returns[, c("date", selected), drop = FALSE]
}

write_phase3_2_best_model_table <- function(selection, config) {
  if (nrow(selection) == 0) {
    return(invisible(NULL))
  }
  valid <- selection[!is.na(selection$convergence) &
                       selection$convergence == 0 &
                       !is.na(selection$criterion_value), , drop = FALSE]
  if (nrow(valid) == 0) {
    best <- data.frame()
  } else {
    valid <- valid[order(valid$asset, valid$criterion_value), , drop = FALSE]
    best <- valid[!duplicated(valid$asset), , drop = FALSE]
  }
  write_csv_output(
    best,
    file.path(config$outputs$tables_dir, "model_selection", "univariate_garch_best_by_bic.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  invisible(best)
}

write_phase3_2_report <- function(selection, best, config, returns) {
  report_path <- project_path("data/metadata/phase3_2_univariate_garch_report.md")
  total_fits <- nrow(selection)
  converged <- sum(!is.na(selection$convergence) & selection$convergence == 0)
  failed <- sum(!is.na(selection$error) & selection$error != "")
  non_converged <- sum(!is.na(selection$convergence) & selection$convergence != 0)

  lines <- c(
    "# Phase 3.2 Univariate GARCH Report",
    "",
    "## Mục tiêu",
    "",
    "Phase 3.2 ước lượng và chọn mô hình volatility đơn biến cho các chuỗi return dùng trong nghiên cứu.",
    "",
    "## Input",
    "",
    "- `data/02_model_input/core/final_daily_returns.csv`",
    sprintf("- Sample: `%s` to `%s`", min(returns$date), max(returns$date)),
    sprintf("- Return columns fitted: `%s`", paste(setdiff(names(returns), "date"), collapse = "`, `")),
    "",
    "## Model Grid",
    "",
    sprintf("- Mean model: ARMA(%s,%s) with mean", config$univariate_garch$arma_order$p, config$univariate_garch$arma_order$q),
    sprintf("- Variance models: `%s`", paste(config$univariate_garch$models, collapse = "`, `")),
    sprintf("- Distributions: `%s`", paste(config$univariate_garch$distributions, collapse = "`, `")),
    sprintf("- Selection criterion: `%s`", config$univariate_garch$selection_criterion),
    "",
    "## Run Summary",
    "",
    sprintf("- Total attempted fits: `%s`", total_fits),
    sprintf("- Converged fits: `%s`", converged),
    sprintf("- Non-converged fits: `%s`", non_converged),
    sprintf("- Failed fits: `%s`", failed),
    "",
    "## Best Models By BIC",
    ""
  )

  if (nrow(best) == 0) {
    lines <- c(lines, "No converged models available.")
  } else {
    lines <- c(
      lines,
      "| Asset | Model | Distribution | Observations | First date | Last date | BIC |",
      "| --- | --- | --- | ---: | --- | --- | ---: |"
    )
    for (i in seq_len(nrow(best))) {
      lines <- c(lines, sprintf(
        "| %s | %s | %s | %s | %s | %s | %.6f |",
        best$asset[i],
        best$model[i],
        best$distribution[i],
        best$observations[i],
        best$first_date[i],
        best$last_date[i],
        best$BIC[i]
      ))
    }
  }

  lines <- c(
    lines,
    "",
    "## Output Files",
    "",
    "- `results/tables/model_selection/univariate_garch_selection.csv`",
    "- `results/tables/model_selection/univariate_garch_best_by_bic.csv`",
    "- `results/tables/volatility_estimates/`",
    "- `results/model_logs/convergence_warnings/univariate_garch_warnings.csv` if any model failed",
    "",
    "## Scientific Note",
    "",
    "Student-t and skewed Student-t innovations are retained because Phase 2 normality diagnostics reject Gaussianity for all fitted return series."
  )

  writeLines(lines, report_path, useBytes = TRUE)
  invisible(report_path)
}

run_phase3_2_univariate_garch <- function(
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
  append_log("Phase 3.2 univariate GARCH started.", config)

  returns <- load_phase3_2_returns(config)
  result <- run_univariate_garch(returns, config)
  best <- write_phase3_2_best_model_table(result$selection, config)
  report <- write_phase3_2_report(result$selection, best, config, returns)

  append_log("Phase 3.2 univariate GARCH finished.", config)
  list(
    returns = returns,
    selection = result$selection,
    best = best,
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

if (!interactive() && is_direct_rscript_call("src/models/run_phase3_2_univariate_garch.R")) {
  result <- run_phase3_2_univariate_garch()
  cat("Phase 3.2 univariate GARCH completed.\n")
  cat(sprintf("Attempted fits: %s\n", nrow(result$selection)))
  cat(sprintf("Best-model rows: %s\n", nrow(result$best)))
  cat(sprintf("Report: %s\n", result$report))
}
