source("src/utils/io_helpers.R")
source("src/utils/date_asset_helpers.R")

safe_haven_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

safe_haven_p_value <- function(t_value, df) {
  if (!is.finite(t_value) || !is.finite(df) || df <= 0) {
    return(NA_real_)
  }
  2 * stats::pt(abs(t_value), df = df, lower.tail = FALSE)
}

safe_haven_hac_vcov <- function(fit, lag = NULL) {
  if (!requireNamespace("sandwich", quietly = TRUE)) {
    stop("Package 'sandwich' is required for HAC/Newey-West standard errors.", call. = FALSE)
  }
  n <- stats::nobs(fit)
  if (is.null(lag)) {
    lag <- max(1L, floor(4 * (n / 100)^(2 / 9)))
  }
  sandwich::NeweyWest(fit, lag = lag, prewhite = FALSE, adjust = TRUE)
}

safe_haven_controls_for_asset <- function(asset, returns, config) {
  controls <- character()
  if ("d_vix" %in% names(returns)) {
    controls <- c(controls, "d_vix")
  }

  if (isTRUE(config$safe_haven$controls$use_oil_price_shock)) {
    if (!identical(asset, "r_brent") && "r_brent" %in% names(returns)) {
      controls <- c(controls, "r_brent")
    } else if (!identical(asset, "r_wti") && "r_wti" %in% names(returns)) {
      controls <- c(controls, "r_wti")
    }
  }

  if (isTRUE(config$safe_haven$controls$use_monetary_tightening)) {
    monetary_controls <- c("d_policy_rate", "d_interbank_rate")
    controls <- c(controls, intersect(monetary_controls, names(returns)))
  }

  if (!identical(asset, "r_bond_tri") && "d_vn10y_yield" %in% names(returns)) {
    controls <- c(controls, "d_vn10y_yield")
  }

  unique(controls)
}

safe_haven_prepare_data <- function(returns, target, asset, controls, quantile_level) {
  columns <- unique(c("date", target, asset, controls))
  data <- returns[, columns, drop = FALSE]
  data$date <- as.Date(data$date)
  numeric_columns <- setdiff(names(data), "date")
  for (col in numeric_columns) {
    data[[col]] <- safe_haven_numeric(data[[col]])
  }
  data <- data[stats::complete.cases(data), , drop = FALSE]
  if (nrow(data) < 30) {
    return(data.frame())
  }

  threshold <- as.numeric(stats::quantile(data[[target]], probs = quantile_level, na.rm = TRUE, names = FALSE))
  data$hedge_return <- data[[asset]]
  data$market_return <- data[[target]]
  data$stress_dummy <- as.integer(data$market_return <= threshold)
  data$market_stress <- data$market_return * data$stress_dummy
  attr(data, "threshold") <- threshold
  data
}

safe_haven_fit_regression <- function(data, controls) {
  rhs <- c("market_return", "stress_dummy", "market_stress", controls)
  formula <- stats::as.formula(sprintf("hedge_return ~ %s", paste(rhs, collapse = " + ")))
  stats::lm(formula, data = data)
}

safe_haven_coefficient_table <- function(fit, hac_lag, metadata) {
  vcov_hac <- safe_haven_hac_vcov(fit, lag = hac_lag)
  coef_vec <- stats::coef(fit)
  standard_errors <- sqrt(diag(vcov_hac))
  terms <- names(coef_vec)
  t_values <- coef_vec / standard_errors
  p_values <- vapply(t_values, safe_haven_p_value, numeric(1), df = stats::df.residual(fit))

  data.frame(
    asset_role = metadata$asset_role,
    hedge = metadata$hedge,
    model_spec = metadata$model_spec,
    stress_quantile = metadata$stress_quantile,
    stress_threshold = metadata$stress_threshold,
    term = terms,
    estimate = as.numeric(coef_vec),
    std_error_hac = as.numeric(standard_errors),
    t_value_hac = as.numeric(t_values),
    p_value_hac = as.numeric(p_values),
    observations = metadata$observations,
    first_date = metadata$first_date,
    last_date = metadata$last_date,
    controls_used = metadata$controls_used,
    hac_lag = metadata$hac_lag,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

safe_haven_linear_combo <- function(fit, term_names, hac_lag) {
  vcov_hac <- safe_haven_hac_vcov(fit, lag = hac_lag)
  coef_vec <- stats::coef(fit)
  weights <- rep(0, length(coef_vec))
  names(weights) <- names(coef_vec)
  for (term in term_names) {
    if (term %in% names(weights)) {
      weights[[term]] <- 1
    }
  }
  estimate <- sum(weights * coef_vec)
  variance <- as.numeric(t(weights) %*% vcov_hac %*% weights)
  std_error <- if (is.finite(variance) && variance >= 0) sqrt(variance) else NA_real_
  t_value <- estimate / std_error
  p_value <- safe_haven_p_value(t_value, stats::df.residual(fit))
  c(estimate = estimate, std_error_hac = std_error, t_value_hac = t_value, p_value_hac = p_value)
}

classify_hedge_property <- function(beta, p_value, alpha = 0.05) {
  if (!is.finite(beta) || !is.finite(p_value)) {
    return("insufficient_evidence")
  }
  if (beta < 0 && p_value < alpha) {
    return("strong_hedge")
  }
  if (p_value >= alpha) {
    return("weak_hedge_or_diversifier")
  }
  "not_a_hedge_positive_comovement"
}

classify_safe_haven_property <- function(stress_slope, p_value, alpha = 0.05) {
  if (!is.finite(stress_slope) || !is.finite(p_value)) {
    return("insufficient_evidence")
  }
  if (stress_slope < 0 && p_value < alpha) {
    return("strong_safe_haven")
  }
  if (p_value >= alpha) {
    return("weak_safe_haven")
  }
  "not_a_safe_haven_positive_stress_comovement"
}

safe_haven_interpretation_row <- function(fit, metadata) {
  hac_lag <- metadata$hac_lag
  coef_table <- safe_haven_coefficient_table(fit, hac_lag, metadata)
  beta_row <- coef_table[coef_table$term == "market_return", , drop = FALSE]
  gamma_row <- coef_table[coef_table$term == "market_stress", , drop = FALSE]
  stress_combo <- safe_haven_linear_combo(fit, c("market_return", "market_stress"), hac_lag)

  beta <- if (nrow(beta_row) == 1) beta_row$estimate[1] else NA_real_
  beta_p <- if (nrow(beta_row) == 1) beta_row$p_value_hac[1] else NA_real_
  gamma <- if (nrow(gamma_row) == 1) gamma_row$estimate[1] else NA_real_
  gamma_p <- if (nrow(gamma_row) == 1) gamma_row$p_value_hac[1] else NA_real_
  stress_slope <- as.numeric(stress_combo[["estimate"]])
  stress_p <- as.numeric(stress_combo[["p_value_hac"]])

  hedge_class <- classify_hedge_property(beta, beta_p)
  safe_haven_class <- classify_safe_haven_property(stress_slope, stress_p)

  data.frame(
    asset_role = metadata$asset_role,
    hedge = metadata$hedge,
    model_spec = metadata$model_spec,
    stress_quantile = metadata$stress_quantile,
    observations = metadata$observations,
    first_date = metadata$first_date,
    last_date = metadata$last_date,
    beta_market = beta,
    beta_market_p_value = beta_p,
    gamma_interaction = gamma,
    gamma_interaction_p_value = gamma_p,
    stress_slope_beta_plus_gamma = stress_slope,
    stress_slope_std_error_hac = as.numeric(stress_combo[["std_error_hac"]]),
    stress_slope_p_value = stress_p,
    hedge_classification = hedge_class,
    safe_haven_classification = safe_haven_class,
    final_interpretation = sprintf("%s; %s", hedge_class, safe_haven_class),
    controls_used = metadata$controls_used,
    hac_lag = metadata$hac_lag,
    stringsAsFactors = FALSE
  )
}

safe_haven_model_summary_row <- function(fit, metadata) {
  data.frame(
    asset_role = metadata$asset_role,
    hedge = metadata$hedge,
    model_spec = metadata$model_spec,
    stress_quantile = metadata$stress_quantile,
    observations = metadata$observations,
    first_date = metadata$first_date,
    last_date = metadata$last_date,
    r_squared = summary(fit)$r.squared,
    adj_r_squared = summary(fit)$adj.r.squared,
    aic = stats::AIC(fit),
    bic = stats::BIC(fit),
    controls_used = metadata$controls_used,
    hac_lag = metadata$hac_lag,
    stringsAsFactors = FALSE
  )
}

safe_haven_run_asset <- function(returns, target, asset, config, asset_role = "hedge_asset") {
  if (!asset %in% names(returns) || !target %in% names(returns)) {
    return(list(coefficients = data.frame(), summary = data.frame(), interpretation = data.frame()))
  }

  quantiles <- as.numeric(config$safe_haven$stress_quantiles)
  control_candidates <- safe_haven_controls_for_asset(asset, returns, config)
  model_specs <- list(
    baseline = character(),
    controlled = control_candidates
  )
  rows_coef <- list()
  rows_summary <- list()
  rows_interpretation <- list()

  for (quantile_level in quantiles) {
    for (model_spec in names(model_specs)) {
      controls <- model_specs[[model_spec]]
      data <- safe_haven_prepare_data(returns, target, asset, controls, quantile_level)
      if (nrow(data) < 30) next
      hac_lag <- max(1L, floor(4 * (nrow(data) / 100)^(2 / 9)))
      fit <- safe_haven_fit_regression(data, controls)
      metadata <- list(
        asset_role = asset_role,
        hedge = asset,
        model_spec = model_spec,
        stress_quantile = quantile_level,
        stress_threshold = attr(data, "threshold"),
        observations = nrow(data),
        first_date = min(data$date),
        last_date = max(data$date),
        controls_used = if (length(controls) == 0) "none" else paste(controls, collapse = ";"),
        hac_lag = hac_lag
      )
      rows_coef[[length(rows_coef) + 1]] <- safe_haven_coefficient_table(fit, hac_lag, metadata)
      rows_summary[[length(rows_summary) + 1]] <- safe_haven_model_summary_row(fit, metadata)
      rows_interpretation[[length(rows_interpretation) + 1]] <- safe_haven_interpretation_row(fit, metadata)
    }
  }

  list(
    coefficients = if (length(rows_coef) == 0) data.frame() else do.call(rbind, rows_coef),
    summary = if (length(rows_summary) == 0) data.frame() else do.call(rbind, rows_summary),
    interpretation = if (length(rows_interpretation) == 0) data.frame() else do.call(rbind, rows_interpretation)
  )
}

safe_haven_test_assets <- function(config, returns) {
  core_and_extension <- unique(c(config$assets$core_hedges, config$assets$extension_hedges))
  assets <- vapply(core_and_extension, asset_return_name, character(1))
  intersect(assets, names(returns))
}

run_safe_haven_tests <- function(returns, config) {
  if (!isTRUE(config$safe_haven$enabled)) {
    return(list(coefficients = data.frame(), summary = data.frame(), interpretation = data.frame()))
  }

  target <- asset_return_name(config$assets$target)
  assets <- safe_haven_test_assets(config, returns)
  asset_results <- lapply(assets, function(asset) {
    safe_haven_run_asset(returns, target, asset, config, asset_role = "main_hedge_asset")
  })

  coefficients <- if (length(asset_results) == 0) data.frame() else do.call(rbind, lapply(asset_results, `[[`, "coefficients"))
  model_summary <- if (length(asset_results) == 0) data.frame() else do.call(rbind, lapply(asset_results, `[[`, "summary"))
  interpretation <- if (length(asset_results) == 0) data.frame() else do.call(rbind, lapply(asset_results, `[[`, "interpretation"))

  write_csv_output(
    coefficients,
    file.path(config$outputs$tables_dir, "safe_haven_tests", "baur_lucey_tail_regressions.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  write_csv_output(
    model_summary,
    file.path(config$outputs$tables_dir, "safe_haven_tests", "baur_lucey_model_summary.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  write_csv_output(
    interpretation,
    file.path(config$outputs$tables_dir, "safe_haven_tests", "safe_haven_interpretation_matrix.csv"),
    overwrite = config$outputs$overwrite_existing
  )

  list(coefficients = coefficients, summary = model_summary, interpretation = interpretation)
}

run_gold_premium_channel_tests <- function(returns, config) {
  target <- asset_return_name(config$assets$target)
  premium_asset <- "r_gold_premium"
  if (!premium_asset %in% names(returns) || !target %in% names(returns)) {
    empty <- list(coefficients = data.frame(), interpretation = data.frame())
    return(empty)
  }

  result <- safe_haven_run_asset(returns, target, premium_asset, config, asset_role = "gold_premium_channel")
  write_csv_output(
    result$coefficients,
    file.path(config$outputs$tables_dir, "safe_haven_tests", "gold_premium_channel_tests.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  write_csv_output(
    result$interpretation,
    file.path(config$outputs$tables_dir, "safe_haven_tests", "gold_premium_interpretation.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  result
}
