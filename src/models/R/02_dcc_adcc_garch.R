source("src/utils/io_helpers.R")
source("src/utils/date_asset_helpers.R")
source("src/models/R/model_helpers.R")

read_best_univariate_specs <- function(config) {
  best_path <- project_path("results/tables/model_selection/univariate_garch_best_by_bic.csv")
  if (!identical(config$dcc_adcc$univariate_spec, "best_by_bic") || !file.exists(best_path)) {
    return(data.frame())
  }
  utils::read.csv(best_path, stringsAsFactors = FALSE, check.names = FALSE)
}

default_pair_spec <- function(config) {
  safe_package_stop("rugarch")
  rugarch::ugarchspec(
    variance.model = list(
      model = "sGARCH",
      garchOrder = c(config$univariate_garch$garch_order$p, config$univariate_garch$garch_order$q)
    ),
    mean.model = list(
      armaOrder = c(config$univariate_garch$arma_order$p, config$univariate_garch$arma_order$q),
      include.mean = TRUE
    ),
    distribution.model = "std"
  )
}

asset_spec_from_best <- function(asset, best_specs, config) {
  safe_package_stop("rugarch")
  if (nrow(best_specs) == 0 || !"asset" %in% names(best_specs)) {
    return(default_pair_spec(config))
  }
  row <- best_specs[best_specs$asset == asset, , drop = FALSE]
  if (nrow(row) == 0) {
    return(default_pair_spec(config))
  }
  rugarch::ugarchspec(
    variance.model = list(
      model = row$model[1],
      garchOrder = c(config$univariate_garch$garch_order$p, config$univariate_garch$garch_order$q)
    ),
    mean.model = list(
      armaOrder = c(config$univariate_garch$arma_order$p, config$univariate_garch$arma_order$q),
      include.mean = TRUE
    ),
    distribution.model = row$distribution[1]
  )
}

build_pair_uspec <- function(config, assets, best_specs = data.frame()) {
  specs <- lapply(assets, asset_spec_from_best, best_specs = best_specs, config = config)
  rugarch::multispec(specs)
}

extract_ugarch_parameters_for_ccc <- function(fit, role, target, hedge, pair_returns) {
  coef_vec <- tryCatch(rugarch::coef(fit), error = function(e) numeric())
  matcoef <- tryCatch(fit@fit$matcoef, error = function(e) NULL)
  parameter_names <- names(coef_vec)
  if (is.null(parameter_names)) {
    parameter_names <- paste0("param_", seq_along(coef_vec))
  }

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

  data.frame(
    target = target,
    hedge = hedge,
    model = "CCC",
    parameter = paste(role, parameter_names, sep = "."),
    estimate = as.numeric(coef_vec),
    std_error = std_error,
    t_value = t_value,
    p_value = p_value,
    observations = nrow(pair_returns),
    first_date = min(pair_returns$date),
    last_date = max(pair_returns$date),
    convergence = tryCatch(rugarch::convergence(fit), error = function(e) NA_integer_),
    stringsAsFactors = FALSE
  )
}

residual_diagnostics_from_matrix <- function(z, target, hedge, model_name, lags = 12) {
  z <- as.matrix(z)
  assets <- c(target, hedge)
  rows <- list()
  for (i in seq_len(ncol(z))) {
    asset <- assets[i]
    x <- as.numeric(z[, i])
    x <- x[is.finite(x)]
    if (length(x) <= lags + 1) next
    lb <- stats::Box.test(x, lag = lags, type = "Ljung-Box")
    lb_sq <- stats::Box.test(x^2, lag = lags, type = "Ljung-Box")
    rows[[length(rows) + 1]] <- data.frame(
      target = target,
      hedge = hedge,
      model = model_name,
      asset = asset,
      diagnostic = c("ljung_box_residual", "ljung_box_squared_residual"),
      lag = lags,
      statistic = c(as.numeric(lb$statistic), as.numeric(lb_sq$statistic)),
      p_value = c(as.numeric(lb$p.value), as.numeric(lb_sq$p.value)),
      observations = length(x),
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) data.frame() else do.call(rbind, rows)
}

ccc_quasi_loglik <- function(e1, e2, h11, h22, h12) {
  det_h <- h11 * h22 - h12^2
  valid <- is.finite(e1) & is.finite(e2) & is.finite(h11) & is.finite(h22) &
    is.finite(h12) & is.finite(det_h) & det_h > 0 & h11 > 0 & h22 > 0
  if (!any(valid)) return(NA_real_)
  quad <- (
    h22[valid] * e1[valid]^2 -
      2 * h12[valid] * e1[valid] * e2[valid] +
      h11[valid] * e2[valid]^2
  ) / det_h[valid]
  sum(-0.5 * (2 * log(2 * pi) + log(det_h[valid]) + quad))
}

fit_ccc_garch <- function(pair_returns, target, hedge, config, best_specs = data.frame()) {
  safe_package_stop("rugarch")
  target_spec <- asset_spec_from_best(target, best_specs, config)
  hedge_spec <- asset_spec_from_best(hedge, best_specs, config)
  target_fit <- rugarch::ugarchfit(
    spec = target_spec,
    data = as.numeric(pair_returns[[target]]),
    solver = "hybrid",
    fit.control = list(stationarity = 1)
  )
  hedge_fit <- rugarch::ugarchfit(
    spec = hedge_spec,
    data = as.numeric(pair_returns[[hedge]]),
    solver = "hybrid",
    fit.control = list(stationarity = 1)
  )

  target_sigma <- as.numeric(rugarch::sigma(target_fit))
  hedge_sigma <- as.numeric(rugarch::sigma(hedge_fit))
  target_resid <- as.numeric(rugarch::residuals(target_fit, standardize = FALSE))
  hedge_resid <- as.numeric(rugarch::residuals(hedge_fit, standardize = FALSE))
  target_z <- as.numeric(rugarch::residuals(target_fit, standardize = TRUE))
  hedge_z <- as.numeric(rugarch::residuals(hedge_fit, standardize = TRUE))
  z_valid <- is.finite(target_z) & is.finite(hedge_z)
  if (sum(z_valid) < 5) {
    stop(sprintf("Insufficient standardized residuals for CCC-GARCH: %s-%s", target, hedge), call. = FALSE)
  }

  rho <- as.numeric(stats::cor(target_z[z_valid], hedge_z[z_valid]))
  if (!is.finite(rho)) {
    stop(sprintf("CCC-GARCH constant correlation is not finite: %s-%s", target, hedge), call. = FALSE)
  }
  rho <- max(min(rho, 0.999), -0.999)

  target_variance <- target_sigma^2
  hedge_variance <- hedge_sigma^2
  conditional_covariance <- rho * target_sigma * hedge_sigma
  llh <- ccc_quasi_loglik(target_resid, hedge_resid, target_variance, hedge_variance, conditional_covariance)
  list(
    target_fit = target_fit,
    hedge_fit = hedge_fit,
    rho = rho,
    target_z = target_z,
    hedge_z = hedge_z,
    target_variance = target_variance,
    hedge_variance = hedge_variance,
    conditional_covariance = conditional_covariance,
    logLik = llh
  )
}

run_ccc_benchmark <- function(pair_returns, target, hedge, config, best_specs = data.frame()) {
  ccc_fit <- fit_ccc_garch(pair_returns, target, hedge, config, best_specs)
  data.frame(
    date = pair_returns$date,
    target = target,
    hedge = hedge,
    model = "CCC",
    dynamic_correlation = ccc_fit$rho,
    conditional_covariance = ccc_fit$conditional_covariance,
    target_variance = ccc_fit$target_variance,
    hedge_variance = ccc_fit$hedge_variance,
    convergence = 0L,
    observations = nrow(pair_returns),
    first_date = min(pair_returns$date),
    last_date = max(pair_returns$date)
  )
}

run_ccc_bundle <- function(pair_returns, target, hedge, config, best_specs = data.frame()) {
  ccc_fit <- fit_ccc_garch(pair_returns, target, hedge, config, best_specs)
  dynamic <- data.frame(
    date = pair_returns$date,
    target = target,
    hedge = hedge,
    model = "CCC",
    dynamic_correlation = ccc_fit$rho,
    conditional_covariance = ccc_fit$conditional_covariance,
    target_variance = ccc_fit$target_variance,
    hedge_variance = ccc_fit$hedge_variance,
    convergence = 0L,
    observations = nrow(pair_returns),
    first_date = min(pair_returns$date),
    last_date = max(pair_returns$date)
  )
  target_parameters <- extract_ugarch_parameters_for_ccc(ccc_fit$target_fit, "target", target, hedge, pair_returns)
  hedge_parameters <- extract_ugarch_parameters_for_ccc(ccc_fit$hedge_fit, "hedge", target, hedge, pair_returns)
  rho_se <- if (nrow(pair_returns) > 3) (1 - ccc_fit$rho^2) / sqrt(nrow(pair_returns) - 3) else NA_real_
  rho_t <- ccc_fit$rho / rho_se
  rho_parameter <- data.frame(
    target = target,
    hedge = hedge,
    model = "CCC",
    parameter = "ccc.rho",
    estimate = ccc_fit$rho,
    std_error = rho_se,
    t_value = rho_t,
    p_value = 2 * stats::pnorm(-abs(rho_t)),
    observations = nrow(pair_returns),
    first_date = min(pair_returns$date),
    last_date = max(pair_returns$date),
    convergence = 0L,
    stringsAsFactors = FALSE
  )
  parameters <- rbind(target_parameters, hedge_parameters, rho_parameter)
  k <- length(rugarch::coef(ccc_fit$target_fit)) + length(rugarch::coef(ccc_fit$hedge_fit)) + 1L
  n <- nrow(pair_returns)
  fit_stats <- data.frame(
    target = target,
    hedge = hedge,
    model = "CCC",
    observations = n,
    first_date = min(pair_returns$date),
    last_date = max(pair_returns$date),
    logLik = ccc_fit$logLik,
    n_parameters = k,
    AIC = -2 * ccc_fit$logLik + 2 * k,
    BIC = -2 * ccc_fit$logLik + log(n) * k,
    HQIC = -2 * ccc_fit$logLik + 2 * k * log(log(n)),
    dcc_alpha = NA_real_,
    dcc_beta = NA_real_,
    dcc_gamma = NA_real_,
    dcc_persistence = NA_real_,
    convergence = 0L,
    stringsAsFactors = FALSE
  )
  residual_diagnostics <- residual_diagnostics_from_matrix(
    cbind(ccc_fit$target_z, ccc_fit$hedge_z),
    target,
    hedge,
    "CCC",
    lags = config$diagnostics$residual_tests$lags
  )
  list(dynamic = dynamic, parameters = parameters, fit_stats = fit_stats, residual_diagnostics = residual_diagnostics)
}

safe_coef_value <- function(coef_vec, pattern) {
  idx <- grep(pattern, names(coef_vec), fixed = TRUE)
  if (length(idx) == 0) return(NA_real_)
  as.numeric(coef_vec[idx[1]])
}

extract_dcc_parameters <- function(fit, target, hedge, model_name, pair_returns) {
  coef_vec <- fit@mfit[["coef"]]
  matcoef <- fit@mfit[["matcoef"]]
  parameter_names <- names(coef_vec)
  if (is.null(parameter_names)) {
    parameter_names <- paste0("param_", seq_along(coef_vec))
  }

  std_error <- rep(NA_real_, length(coef_vec))
  t_value <- rep(NA_real_, length(coef_vec))
  p_value <- rep(NA_real_, length(coef_vec))
  if (!is.null(matcoef) && nrow(matcoef) > 0) {
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

  data.frame(
    target = target,
    hedge = hedge,
    model = model_name,
    parameter = parameter_names,
    estimate = as.numeric(coef_vec),
    std_error = std_error,
    t_value = t_value,
    p_value = p_value,
    observations = nrow(pair_returns),
    first_date = min(pair_returns$date),
    last_date = max(pair_returns$date),
    convergence = tryCatch(fit@mfit[["convergence"]], error = function(e) NA_integer_),
    stringsAsFactors = FALSE
  )
}

extract_dcc_fit_stats <- function(fit, target, hedge, model_name, pair_returns) {
  coef_vec <- fit@mfit[["coef"]]
  n <- nrow(pair_returns)
  k <- length(coef_vec)
  llh <- as.numeric(fit@mfit[["llh"]])
  alpha <- safe_coef_value(coef_vec, "[Joint]dcca1")
  beta <- safe_coef_value(coef_vec, "[Joint]dccb1")
  gamma <- safe_coef_value(coef_vec, "[Joint]dccg1")
  persistence <- if (is.na(gamma)) alpha + beta else alpha + beta + gamma / 2
  data.frame(
    target = target,
    hedge = hedge,
    model = model_name,
    observations = n,
    first_date = min(pair_returns$date),
    last_date = max(pair_returns$date),
    logLik = llh,
    n_parameters = k,
    AIC = -2 * llh + 2 * k,
    BIC = -2 * llh + log(n) * k,
    HQIC = -2 * llh + 2 * k * log(log(n)),
    dcc_alpha = alpha,
    dcc_beta = beta,
    dcc_gamma = gamma,
    dcc_persistence = persistence,
    convergence = tryCatch(fit@mfit[["convergence"]], error = function(e) NA_integer_),
    stringsAsFactors = FALSE
  )
}

extract_dcc_residual_diagnostics <- function(fit, target, hedge, model_name, lags = 12) {
  z <- fit@mfit[["stdresid"]]
  if (is.null(z)) return(data.frame())
  z <- as.matrix(z)
  assets <- c(target, hedge)
  rows <- list()
  for (i in seq_len(ncol(z))) {
    asset <- assets[i]
    x <- as.numeric(z[, i])
    x <- x[is.finite(x)]
    if (length(x) <= lags + 1) next
    lb <- stats::Box.test(x, lag = lags, type = "Ljung-Box")
    lb_sq <- stats::Box.test(x^2, lag = lags, type = "Ljung-Box")
    rows[[length(rows) + 1]] <- data.frame(
      target = target,
      hedge = hedge,
      model = model_name,
      asset = asset,
      diagnostic = c("ljung_box_residual", "ljung_box_squared_residual"),
      lag = lags,
      statistic = c(as.numeric(lb$statistic), as.numeric(lb_sq$statistic)),
      p_value = c(as.numeric(lb$p.value), as.numeric(lb_sq$p.value)),
      observations = length(x),
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) data.frame() else do.call(rbind, rows)
}

run_dcc_family_pair <- function(pair_returns, target, hedge, dcc_model, config, best_specs) {
  safe_package_stop("rmgarch")
  uspec <- build_pair_uspec(config, c(target, hedge), best_specs)
  dcc_spec <- rmgarch::dccspec(
    uspec = uspec,
    dccOrder = c(1, 1),
    model = dcc_model,
    distribution = config$dcc_adcc$distribution
  )
  fit <- rmgarch::dccfit(dcc_spec, data = as.matrix(pair_returns[, c(target, hedge)]), fit.control = list(eval.se = TRUE))
  convergence <- tryCatch(fit@mfit$convergence, error = function(e) NA_integer_)
  rcor_arr <- rmgarch::rcor(fit)
  rcov_arr <- rmgarch::rcov(fit)
  data.frame(
    date = pair_returns$date,
    target = target,
    hedge = hedge,
    model = dcc_model,
    dynamic_correlation = as.numeric(rcor_arr[1, 2, ]),
    conditional_covariance = as.numeric(rcov_arr[1, 2, ]),
    hedge_variance = as.numeric(rcov_arr[2, 2, ]),
    convergence = convergence,
    observations = nrow(pair_returns),
    first_date = min(pair_returns$date),
    last_date = max(pair_returns$date)
  )
}

run_dcc_family_bundle <- function(pair_returns, target, hedge, dcc_model, config, best_specs) {
  safe_package_stop("rmgarch")
  uspec <- build_pair_uspec(config, c(target, hedge), best_specs)
  dcc_spec <- rmgarch::dccspec(
    uspec = uspec,
    dccOrder = c(1, 1),
    model = dcc_model,
    distribution = config$dcc_adcc$distribution
  )
  fit <- rmgarch::dccfit(dcc_spec, data = as.matrix(pair_returns[, c(target, hedge)]), fit.control = list(eval.se = TRUE))
  rcor_arr <- rmgarch::rcor(fit)
  rcov_arr <- rmgarch::rcov(fit)
  convergence <- tryCatch(fit@mfit[["convergence"]], error = function(e) NA_integer_)
  dynamic <- data.frame(
    date = pair_returns$date,
    target = target,
    hedge = hedge,
    model = dcc_model,
    dynamic_correlation = as.numeric(rcor_arr[1, 2, ]),
    conditional_covariance = as.numeric(rcov_arr[1, 2, ]),
    target_variance = as.numeric(rcov_arr[1, 1, ]),
    hedge_variance = as.numeric(rcov_arr[2, 2, ]),
    convergence = convergence,
    observations = nrow(pair_returns),
    first_date = min(pair_returns$date),
    last_date = max(pair_returns$date)
  )
  list(
    dynamic = dynamic,
    parameters = extract_dcc_parameters(fit, target, hedge, dcc_model, pair_returns),
    fit_stats = extract_dcc_fit_stats(fit, target, hedge, dcc_model, pair_returns),
    residual_diagnostics = extract_dcc_residual_diagnostics(fit, target, hedge, dcc_model, lags = config$diagnostics$residual_tests$lags)
  )
}

write_dcc_summary <- function(output, config) {
  if (nrow(output) == 0) {
    return(invisible(data.frame()))
  }
  groups <- unique(output[, c("target", "hedge", "model"), drop = FALSE])
  rows <- vector("list", nrow(groups))
  for (i in seq_len(nrow(groups))) {
    sub <- output[
      output$target == groups$target[i] &
        output$hedge == groups$hedge[i] &
        output$model == groups$model[i],
      ,
      drop = FALSE
    ]
    rows[[i]] <- data.frame(
      target = groups$target[i],
      hedge = groups$hedge[i],
      model = groups$model[i],
      observations = nrow(sub),
      first_date = min(sub$date),
      last_date = max(sub$date),
      mean_correlation = mean(sub$dynamic_correlation, na.rm = TRUE),
      min_correlation = min(sub$dynamic_correlation, na.rm = TRUE),
      max_correlation = max(sub$dynamic_correlation, na.rm = TRUE),
      sd_correlation = stats::sd(sub$dynamic_correlation, na.rm = TRUE),
      median_correlation = stats::median(sub$dynamic_correlation, na.rm = TRUE),
      p01_correlation = as.numeric(stats::quantile(sub$dynamic_correlation, 0.01, na.rm = TRUE)),
      p05_correlation = as.numeric(stats::quantile(sub$dynamic_correlation, 0.05, na.rm = TRUE)),
      p95_correlation = as.numeric(stats::quantile(sub$dynamic_correlation, 0.95, na.rm = TRUE)),
      p99_correlation = as.numeric(stats::quantile(sub$dynamic_correlation, 0.99, na.rm = TRUE)),
      share_negative_correlation = mean(sub$dynamic_correlation < 0, na.rm = TRUE),
      share_positive_correlation = mean(sub$dynamic_correlation > 0, na.rm = TRUE),
      mean_conditional_covariance = mean(sub$conditional_covariance, na.rm = TRUE),
      mean_target_variance = if ("target_variance" %in% names(sub)) mean(sub$target_variance, na.rm = TRUE) else NA_real_,
      mean_hedge_variance = mean(sub$hedge_variance, na.rm = TRUE),
      convergence = paste(unique(sub$convergence), collapse = ";")
    )
  }
  summary <- do.call(rbind, rows)
  write_csv_output(
    summary,
    file.path(config$outputs$tables_dir, "dynamic_correlations", "dcc_adcc_summary.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  invisible(summary)
}

run_dcc_adcc_garch <- function(returns, config) {
  if (!isTRUE(config$dcc_adcc$enabled)) {
    return(data.frame())
  }

  safe_package_stop("rmgarch")
  append_log("Starting DCC/ADCC estimation.", config)

  target <- asset_return_name(config$assets$target)
  oil_pairs <- intersect(c("brent", "wti"), config$assets$shock_controls)
  hedge_assets <- unique(c(config$assets$core_hedges, oil_pairs, config$assets$extension_hedges))
  hedges <- vapply(hedge_assets, asset_return_name, character(1))
  hedges <- intersect(hedges, setdiff(names(returns), c("date", target)))
  models <- unique(c(config$dcc_adcc$benchmark_models, config$dcc_adcc$main_model))
  best_specs <- read_best_univariate_specs(config)
  rows <- list()
  parameter_rows <- list()
  fit_stat_rows <- list()
  residual_rows <- list()
  warnings <- character()

  for (hedge in hedges) {
    pair_returns <- returns[, c("date", target, hedge), drop = FALSE]
    pair_returns <- pair_returns[stats::complete.cases(pair_returns), , drop = FALSE]
    for (model_name in models) {
      result <- if (identical(model_name, "CCC")) {
        fit_status(run_ccc_bundle(pair_returns, target, hedge, config, best_specs))
      } else {
        dcc_model <- if (identical(model_name, "aDCC")) "aDCC" else "DCC"
        fit_status(run_dcc_family_bundle(pair_returns, target, hedge, dcc_model, config, best_specs))
      }
      if (result$ok) {
        rows[[length(rows) + 1]] <- result$value$dynamic
        parameter_rows[[length(parameter_rows) + 1]] <- result$value$parameters
        fit_stat_rows[[length(fit_stat_rows) + 1]] <- result$value$fit_stats
        if (nrow(result$value$residual_diagnostics) > 0) {
          residual_rows[[length(residual_rows) + 1]] <- result$value$residual_diagnostics
        }
      } else {
        warnings <- c(warnings, sprintf("%s-%s failed: %s", target, hedge, result$error))
      }
    }
  }

  output <- if (length(rows) == 0) data.frame() else do.call(rbind, rows)
  parameter_output <- if (length(parameter_rows) == 0) data.frame() else do.call(rbind, parameter_rows)
  fit_stats_output <- if (length(fit_stat_rows) == 0) data.frame() else do.call(rbind, fit_stat_rows)
  residual_output <- if (length(residual_rows) == 0) data.frame() else do.call(rbind, residual_rows)
  if (nrow(output) > 0 && isTRUE(config$outputs$save_dynamic_correlation)) {
    write_csv_output(
      output,
      file.path(config$outputs$tables_dir, "dynamic_correlations", "dcc_adcc_dynamic_correlations.csv"),
      overwrite = config$outputs$overwrite_existing
    )
    write_dcc_summary(output, config)
  }
  if (nrow(parameter_output) > 0) {
    write_csv_output(
      parameter_output,
      file.path(config$outputs$tables_dir, "dynamic_correlations", "dcc_adcc_parameter_estimates.csv"),
      overwrite = config$outputs$overwrite_existing
    )
  }
  if (nrow(fit_stats_output) > 0) {
    write_csv_output(
      fit_stats_output,
      file.path(config$outputs$tables_dir, "dynamic_correlations", "dcc_adcc_model_fit_statistics.csv"),
      overwrite = config$outputs$overwrite_existing
    )
  }
  if (nrow(residual_output) > 0) {
    write_csv_output(
      residual_output,
      file.path(config$outputs$diagnostics_dir, "residual", "dcc_adcc_residual_diagnostics.csv"),
      overwrite = config$outputs$overwrite_existing
    )
  }
  write_warning_table(
    warnings,
    file.path(config$outputs$logs_dir, "convergence_warnings", "dcc_adcc_warnings.csv"),
    config
  )

  append_log("Finished DCC/ADCC estimation.", config)
  output
}
