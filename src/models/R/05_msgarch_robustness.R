source("src/utils/io_helpers.R")
source("src/models/R/model_helpers.R")

run_msgarch_robustness <- function(returns, config) {
  if (!isTRUE(config$robustness$msgarch$enabled)) {
    return(data.frame())
  }

  safe_package_stop("MSGARCH")
  append_log("Starting MSGARCH robustness.", config)

  target <- asset_return_name(config$assets$target)
  if (!target %in% names(returns)) {
    stop(sprintf("Target return column not found for MSGARCH: %s", target), call. = FALSE)
  }

  x <- stats::na.omit(as.numeric(returns[[target]]))
  k <- as.integer(config$robustness$msgarch$regimes)
  spec <- MSGARCH::CreateSpec(
    variance.spec = list(model = rep("sGARCH", k)),
    distribution.spec = list(distribution = rep(config$robustness$msgarch$distribution, k))
  )

  result <- fit_status(MSGARCH::FitML(spec = spec, data = x, ctr = list(do.se = FALSE)))
  if (!result$ok) {
    write_warning_table(
      sprintf("MSGARCH failed: %s", result$error),
      file.path(config$outputs$logs_dir, "convergence_warnings", "msgarch_warnings.csv"),
      config
    )
    return(data.frame())
  }

  fit <- result$value
  n_par <- length(fit$par)
  n_obs <- length(x)
  output <- data.frame(
    target = target,
    regimes = k,
    log_likelihood = as.numeric(fit$loglik),
    parameters = n_par,
    observations = n_obs,
    aic = -2 * as.numeric(fit$loglik) + 2 * n_par,
    bic = -2 * as.numeric(fit$loglik) + log(n_obs) * n_par,
    stringsAsFactors = FALSE
  )
  write_csv_output(
    output,
    file.path(config$outputs$tables_dir, "model_selection", "msgarch_robustness_summary.csv"),
    overwrite = config$outputs$overwrite_existing
  )

  append_log("Finished MSGARCH robustness.", config)
  output
}
