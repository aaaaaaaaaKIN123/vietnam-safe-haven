source("src/utils/config_loader.R")
source("src/utils/io_helpers.R")
source("src/utils/date_asset_helpers.R")
source("src/models/validate_model_control.R")
source("src/models/R/04_safe_haven_tests.R")

read_phase3_5_returns <- function(config) {
  input_path <- project_path(config$data$returns_input)
  if (!file.exists(input_path)) {
    stop(sprintf("Prepared returns file not found: %s", input_path), call. = FALSE)
  }
  returns <- utils::read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE)
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

phase3_5_format_number <- function(x, digits = 6) {
  if (length(x) == 0 || is.na(x) || !is.finite(x)) {
    return("NA")
  }
  formatC(as.numeric(x), format = "f", digits = digits)
}

phase3_5_best_rows <- function(interpretation, model_spec = "controlled") {
  if (nrow(interpretation) == 0) return(interpretation)
  sub <- interpretation[interpretation$model_spec == model_spec, , drop = FALSE]
  if (nrow(sub) == 0) sub <- interpretation
  sub[order(sub$stress_quantile, sub$hedge), , drop = FALSE]
}

write_phase3_5_report <- function(main_results, premium_results, config) {
  report_path <- project_path("data/metadata/phase3_5_safe_haven_report.md")
  coefficients <- main_results$coefficients
  model_summary <- main_results$summary
  interpretation <- main_results$interpretation
  premium_coefficients <- premium_results$coefficients
  premium_interpretation <- premium_results$interpretation

  controlled <- phase3_5_best_rows(interpretation, "controlled")
  premium_controlled <- phase3_5_best_rows(premium_interpretation, "controlled")

  lines <- c(
    "# Phase 3.5 Safe-Haven Regression Report",
    "",
    "## Mục tiêu",
    "",
    "Phase 3.5 kiểm định hedge và safe haven theo hướng Baur-Lucey bằng hồi quy tail-event, sử dụng sai số chuẩn HAC/Newey-West. Phase này cũng bổ sung kiểm định kênh gold premium để xem phần chênh lệch vàng nội địa có phản ứng với stress của VNIndex hay không.",
    "",
    "## Model",
    "",
    "`r_asset,t = alpha + beta*r_vnindex,t + theta*stress_q,t + gamma*(r_vnindex,t*stress_q,t) + controls + error_t`",
    "",
    "- `beta` đo quan hệ toàn mẫu giữa tài sản và VNIndex.",
    "- `beta + gamma` đo quan hệ trong trạng thái VNIndex lower-tail.",
    "- Sai số chuẩn là HAC/Newey-West.",
    "",
    "## Output Summary",
    "",
    sprintf("- Main coefficient rows: `%s`", nrow(coefficients)),
    sprintf("- Main model-summary rows: `%s`", nrow(model_summary)),
    sprintf("- Main interpretation rows: `%s`", nrow(interpretation)),
    sprintf("- Gold-premium coefficient rows: `%s`", nrow(premium_coefficients)),
    sprintf("- Gold-premium interpretation rows: `%s`", nrow(premium_interpretation)),
    "",
    "## Controlled-Spec Interpretation Matrix",
    ""
  )

  if (nrow(controlled) == 0) {
    lines <- c(lines, "No controlled-spec interpretation was generated.")
  } else {
    lines <- c(
      lines,
      "| Hedge | Tail | Obs. | Beta | Beta p | Stress slope | Stress p | Hedge class | Safe-haven class |",
      "| --- | ---: | ---: | ---: | ---: | ---: | ---: | --- | --- |"
    )
    for (i in seq_len(nrow(controlled))) {
      lines <- c(lines, sprintf(
        "| %s | %.2f | %s | %s | %s | %s | %s | %s | %s |",
        controlled$hedge[i],
        controlled$stress_quantile[i],
        controlled$observations[i],
        phase3_5_format_number(controlled$beta_market[i]),
        phase3_5_format_number(controlled$beta_market_p_value[i]),
        phase3_5_format_number(controlled$stress_slope_beta_plus_gamma[i]),
        phase3_5_format_number(controlled$stress_slope_p_value[i]),
        controlled$hedge_classification[i],
        controlled$safe_haven_classification[i]
      ))
    }
  }

  lines <- c(lines, "", "## Gold Premium Channel", "")
  if (nrow(premium_controlled) == 0) {
    lines <- c(lines, "No gold-premium channel result was generated.")
  } else {
    lines <- c(
      lines,
      "| Tail | Obs. | Beta | Beta p | Stress slope | Stress p | Interpretation |",
      "| ---: | ---: | ---: | ---: | ---: | ---: | --- |"
    )
    for (i in seq_len(nrow(premium_controlled))) {
      lines <- c(lines, sprintf(
        "| %.2f | %s | %s | %s | %s | %s | %s |",
        premium_controlled$stress_quantile[i],
        premium_controlled$observations[i],
        phase3_5_format_number(premium_controlled$beta_market[i]),
        phase3_5_format_number(premium_controlled$beta_market_p_value[i]),
        phase3_5_format_number(premium_controlled$stress_slope_beta_plus_gamma[i]),
        phase3_5_format_number(premium_controlled$stress_slope_p_value[i]),
        premium_controlled$final_interpretation[i]
      ))
    }
  }

  lines <- c(
    lines,
    "",
    "## Scientific Notes",
    "",
    "- `strong_hedge` yêu cầu beta âm và có ý nghĩa thống kê.",
    "- `weak_hedge_or_diversifier` nghĩa là beta không khác 0 có ý nghĩa; đây không phải bằng chứng hedge mạnh.",
    "- `strong_safe_haven` yêu cầu `beta + gamma` âm và có ý nghĩa trong stress tail.",
    "- `weak_safe_haven` nghĩa là stress slope không khác 0 có ý nghĩa; cần diễn giải thận trọng.",
    "- Gold premium không phải tài sản hedge chính; nó là kênh kiểm định domestic safe-haven demand của thị trường vàng Việt Nam.",
    "",
    "## Output Files",
    "",
    sprintf("- `%s`", project_path(file.path(config$outputs$tables_dir, "safe_haven_tests", "baur_lucey_tail_regressions.csv"))),
    sprintf("- `%s`", project_path(file.path(config$outputs$tables_dir, "safe_haven_tests", "baur_lucey_model_summary.csv"))),
    sprintf("- `%s`", project_path(file.path(config$outputs$tables_dir, "safe_haven_tests", "safe_haven_interpretation_matrix.csv"))),
    sprintf("- `%s`", project_path(file.path(config$outputs$tables_dir, "safe_haven_tests", "gold_premium_channel_tests.csv"))),
    sprintf("- `%s`", project_path(file.path(config$outputs$tables_dir, "safe_haven_tests", "gold_premium_interpretation.csv")))
  )

  con <- file(report_path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw("\ufeff"), con)
  writeLines(enc2utf8(lines), con, useBytes = TRUE)
  invisible(report_path)
}

run_phase3_5_safe_haven <- function(
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
    stop("Package 'sandwich' is required for Phase 3.5.", call. = FALSE)
  }

  ensure_output_dirs(config)
  append_log("Phase 3.5 safe-haven regression started.", config)

  returns <- read_phase3_5_returns(config)
  main_results <- run_safe_haven_tests(returns, config)
  premium_results <- run_gold_premium_channel_tests(returns, config)
  report <- write_phase3_5_report(main_results, premium_results, config)

  append_log("Phase 3.5 safe-haven regression finished.", config)
  list(
    returns = returns,
    main = main_results,
    premium = premium_results,
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

if (!interactive() && is_direct_rscript_call("src/models/run_phase3_5_safe_haven.R")) {
  result <- run_phase3_5_safe_haven()
  cat("Phase 3.5 safe-haven regression completed.\n")
  cat(sprintf("Main coefficient rows: %s\n", nrow(result$main$coefficients)))
  cat(sprintf("Main interpretation rows: %s\n", nrow(result$main$interpretation)))
  cat(sprintf("Gold-premium coefficient rows: %s\n", nrow(result$premium$coefficients)))
  cat(sprintf("Report: %s\n", result$report))
}
