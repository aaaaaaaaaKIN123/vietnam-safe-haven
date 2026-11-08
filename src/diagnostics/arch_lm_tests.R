source("src/utils/io_helpers.R")

run_arch_lm_tests <- function(returns, config) {
  if (!isTRUE(config$diagnostics$enabled) || !isTRUE(config$diagnostics$arch_lm$enabled)) {
    return(data.frame())
  }

  if (!requireNamespace("FinTS", quietly = TRUE)) {
    warning("Package 'FinTS' is not installed; skipping ARCH-LM diagnostics.")
    return(data.frame())
  }

  rows <- list()
  lag_count <- as.integer(config$diagnostics$arch_lm$lags)
  for (asset in setdiff(names(returns), "date")) {
    x <- stats::na.omit(as.numeric(returns[[asset]]))
    if (length(x) <= lag_count + 5) next
    test <- tryCatch(FinTS::ArchTest(x, lags = lag_count), error = function(e) NULL)
    if (is.null(test)) next
    rows[[length(rows) + 1]] <- data.frame(
      asset = asset,
      test = "ARCH-LM",
      lags = lag_count,
      statistic = as.numeric(test$statistic),
      p_value = as.numeric(test$p.value),
      observations = length(x)
    )
  }
  output <- if (length(rows) == 0) data.frame() else do.call(rbind, rows)
  write_csv_output(
    output,
    file.path(config$outputs$diagnostics_dir, "arch_effect", "arch_lm_tests.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  output
}
