source("src/utils/config_loader.R")
source("src/utils/io_helpers.R")
source("src/utils/date_asset_helpers.R")
source("src/models/validate_model_control.R")
source("src/models/R/03_hedging_effectiveness.R")
source("src/visualization/plot_hedge_ratio.R")

read_phase3_3_dynamic_correlations <- function(config) {
  input_path <- project_path(file.path(
    config$outputs$tables_dir,
    "dynamic_correlations",
    "dcc_adcc_dynamic_correlations.csv"
  ))
  if (!file.exists(input_path)) {
    stop(sprintf("Phase 3.3 dynamic-correlation file not found: %s", input_path), call. = FALSE)
  }
  df <- utils::read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE)
  df$date <- as.Date(df$date)
  df
}

format_report_number <- function(x, digits = 6) {
  if (length(x) == 0 || is.na(x) || !is.finite(x)) {
    return("NA")
  }
  formatC(as.numeric(x), format = "f", digits = digits)
}

write_phase3_4_report <- function(hedge_output, config) {
  report_path <- project_path("data/metadata/phase3_4_hedging_report.md")
  hedge_ratio_path <- project_path(file.path(
    config$outputs$tables_dir,
    "hedge_ratios",
    "dynamic_hedge_ratios.csv"
  ))
  hedge_summary_path <- project_path(file.path(
    config$outputs$tables_dir,
    "hedge_ratios",
    "hedge_ratio_summary.csv"
  ))
  weight_path <- project_path(file.path(
    config$outputs$tables_dir,
    "hedge_ratios",
    "dynamic_min_variance_weights.csv"
  ))
  weight_summary_path <- project_path(file.path(
    config$outputs$tables_dir,
    "hedge_ratios",
    "min_variance_weight_summary.csv"
  ))
  effectiveness_path <- project_path(file.path(
    config$outputs$tables_dir,
    "hedging_effectiveness",
    "hedging_effectiveness_summary.csv"
  ))
  figure_dir <- project_path(file.path(config$outputs$figures_dir, "hedge_ratios"))
  figure_count <- if (dir.exists(figure_dir)) {
    length(list.files(figure_dir, pattern = "\\.(png|svg)$", full.names = TRUE))
  } else {
    0L
  }

  hedge_ratios <- hedge_output$hedge_ratios
  hedge_summary <- hedge_output$hedge_ratio_summary
  effectiveness <- hedge_output$effectiveness
  weights <- hedge_output$weights
  weight_summary <- hedge_output$weight_summary

  ranked <- effectiveness
  if (nrow(ranked) > 0) {
    ranked <- ranked[order(-ranked$hedging_effectiveness), , drop = FALSE]
  }

  lines <- c(
    "# Phase 3.4 Dynamic Hedge Ratio And Hedging Effectiveness Report",
    "",
    "## Mục tiêu",
    "",
    "Phase 3.4 tính hedge ratio động theo Kroner-Sultan từ covariance/variance có điều kiện của Phase 3.3, sau đó đánh giá hiệu quả phòng ngừa rủi ro bằng mức giảm phương sai của danh mục đã hedge so với VNIndex chưa hedge.",
    "",
    "## Input",
    "",
    "- Dynamic correlations/covariances: `results/tables/dynamic_correlations/dcc_adcc_dynamic_correlations.csv`",
    sprintf("- Returns: `%s`", config$data$returns_input),
    sprintf("- Hedge-ratio method: `%s`", config$hedging$hedge_ratio_method),
    sprintf("- Effectiveness metric: `%s`", config$hedging$effectiveness_metric),
    "",
    "## Output Summary",
    "",
    sprintf("- Dynamic hedge-ratio rows: `%s`", nrow(hedge_ratios)),
    sprintf("- Hedge-ratio summary rows: `%s`", nrow(hedge_summary)),
    sprintf("- Hedging-effectiveness rows: `%s`", nrow(effectiveness)),
    sprintf("- Dynamic min-variance weight rows: `%s`", nrow(weights)),
    sprintf("- Min-variance weight summary rows: `%s`", nrow(weight_summary)),
    sprintf("- Figure files: `%s`", figure_count),
    "",
    "## Hedging Effectiveness Ranking",
    ""
  )

  if (nrow(ranked) == 0) {
    lines <- c(lines, "No hedging-effectiveness result was generated.")
  } else {
    lines <- c(
      lines,
      "| Rank | Hedge | Model | Observations | Hedging effectiveness | Volatility reduction | Mean hedge ratio |",
      "| ---: | --- | --- | ---: | ---: | ---: | ---: |"
    )
    for (i in seq_len(nrow(ranked))) {
      lines <- c(lines, sprintf(
        "| %s | %s | %s | %s | %s | %s | %s |",
        i,
        ranked$hedge[i],
        ranked$model[i],
        ranked$observations[i],
        format_report_number(ranked$hedging_effectiveness[i]),
        format_report_number(ranked$volatility_reduction[i]),
        format_report_number(ranked$mean_hedge_ratio[i])
      ))
    }
  }

  lines <- c(
    lines,
    "",
    "## Scientific Interpretation Guide",
    "",
    "- `hedging_effectiveness > 0` nghĩa là danh mục hedge có phương sai thấp hơn VNIndex chưa hedge.",
    "- `hedging_effectiveness < 0` nghĩa là chiến lược hedge làm tăng phương sai, nên không được diễn giải là hedge hiệu quả.",
    "- `hedge_ratio > 0` tương ứng với vị thế short tài sản hedge khi đang long VNIndex; `hedge_ratio < 0` tương ứng với việc long thêm tài sản hedge.",
    "- Kết quả chưa bao gồm transaction cost, bid-ask spread, short-sale constraint, margin requirement hoặc giới hạn thanh khoản.",
    "- `dynamic_min_variance_weights.csv` là phân tích trọng số danh mục tối thiểu phương sai, khác với hedge ratio Kroner-Sultan.",
    "",
    "## Output Files",
    "",
    sprintf("- `%s`", hedge_ratio_path),
    sprintf("- `%s`", hedge_summary_path),
    sprintf("- `%s`", effectiveness_path),
    sprintf("- `%s`", weight_path),
    sprintf("- `%s`", weight_summary_path),
    sprintf("- `%s`", figure_dir)
  )

  con <- file(report_path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw("\ufeff"), con)
  writeLines(enc2utf8(lines), con, useBytes = TRUE)
  invisible(report_path)
}

run_phase3_4_hedging_effectiveness <- function(
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
  if (length(validation$warnings) > 0) {
    warning(paste(validation$warnings, collapse = "\n"), call. = FALSE)
  }

  ensure_output_dirs(config)
  write_config_snapshot(project_path(control_path), config)
  append_log("Phase 3.4 hedging effectiveness started.", config)

  returns <- load_model_returns(config)
  dcc_output <- read_phase3_3_dynamic_correlations(config)
  hedge_output <- compute_hedge_ratios(dcc_output, returns, config)
  plot_hedge_ratios(config)
  report <- write_phase3_4_report(hedge_output, config)

  append_log("Phase 3.4 hedging effectiveness finished.", config)
  list(
    returns = returns,
    hedging = hedge_output,
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

if (!interactive() && is_direct_rscript_call("src/models/run_phase3_4_hedging_effectiveness.R")) {
  result <- run_phase3_4_hedging_effectiveness()
  cat("Phase 3.4 hedging effectiveness completed.\n")
  cat(sprintf("Dynamic hedge-ratio rows: %s\n", nrow(result$hedging$hedge_ratios)))
  cat(sprintf("Hedging-effectiveness rows: %s\n", nrow(result$hedging$effectiveness)))
  cat(sprintf("Report: %s\n", result$report))
}
