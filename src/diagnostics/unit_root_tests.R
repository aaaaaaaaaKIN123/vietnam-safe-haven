source("src/utils/io_helpers.R")

run_unit_root_tests <- function(returns, config) {
  if (!isTRUE(config$diagnostics$enabled) || !isTRUE(config$diagnostics$unit_root$enabled)) {
    return(data.frame())
  }

  if (!requireNamespace("tseries", quietly = TRUE)) {
    warning("Package 'tseries' is not installed; skipping ADF diagnostics.")
    return(data.frame())
  }

  rows <- list()
  for (asset in setdiff(names(returns), "date")) {
    x <- stats::na.omit(as.numeric(returns[[asset]]))
    if (length(x) < 20) next
    test <- tryCatch(tseries::adf.test(x), error = function(e) NULL)
    if (is.null(test)) next
    rows[[length(rows) + 1]] <- data.frame(
      asset = asset,
      test = "ADF",
      statistic = as.numeric(test$statistic),
      p_value = as.numeric(test$p.value),
      method = test$method,
      observations = length(x)
    )
  }
  output <- if (length(rows) == 0) data.frame() else do.call(rbind, rows)
  write_csv_output(
    output,
    file.path(config$outputs$diagnostics_dir, "unit_root", "adf_tests.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  output
}
