source("src/utils/io_helpers.R")

run_residual_diagnostics <- function(univariate_result, config) {
  if (!isTRUE(config$diagnostics$enabled) || !isTRUE(config$diagnostics$residual_tests$enabled)) {
    return(data.frame())
  }

  fits <- univariate_result$fits
  if (length(fits) == 0) {
    return(data.frame())
  }

  lag_count <- as.integer(config$diagnostics$residual_tests$lags)
  rows <- list()
  for (key in names(fits)) {
    residuals <- tryCatch(as.numeric(rugarch::residuals(fits[[key]], standardize = TRUE)), error = function(e) NULL)
    if (is.null(residuals)) next
    lb <- stats::Box.test(stats::na.omit(residuals), lag = lag_count, type = "Ljung-Box")
    rows[[length(rows) + 1]] <- data.frame(
      model_key = key,
      test = "Ljung-Box standardized residuals",
      lags = lag_count,
      statistic = as.numeric(lb$statistic),
      p_value = as.numeric(lb$p.value)
    )
  }

  output <- if (length(rows) == 0) data.frame() else do.call(rbind, rows)
  write_csv_output(
    output,
    file.path(config$outputs$diagnostics_dir, "residual", "residual_ljung_box_tests.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  output
}
