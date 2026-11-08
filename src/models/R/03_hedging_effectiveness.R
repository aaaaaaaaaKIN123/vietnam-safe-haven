source("src/utils/io_helpers.R")
source("src/utils/date_asset_helpers.R")

safe_numeric_quantile <- function(x, probs) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(rep(NA_real_, length(probs)))
  }
  as.numeric(stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE, type = 7))
}

summarise_hedge_ratios <- function(hedge_ratios) {
  if (nrow(hedge_ratios) == 0) {
    return(data.frame())
  }

  groups <- unique(hedge_ratios[, c("target", "hedge", "model"), drop = FALSE])
  rows <- list()
  for (i in seq_len(nrow(groups))) {
    sub <- hedge_ratios[
      hedge_ratios$target == groups$target[i] &
        hedge_ratios$hedge == groups$hedge[i] &
        hedge_ratios$model == groups$model[i],
      ,
      drop = FALSE
    ]
    h <- as.numeric(sub$hedge_ratio)
    q <- safe_numeric_quantile(h, c(0.01, 0.05, 0.50, 0.95, 0.99))
    valid_h <- h[is.finite(h)]
    rows[[length(rows) + 1]] <- data.frame(
      target = groups$target[i],
      hedge = groups$hedge[i],
      model = groups$model[i],
      observations = length(valid_h),
      first_date = min(sub$date, na.rm = TRUE),
      last_date = max(sub$date, na.rm = TRUE),
      mean_hedge_ratio = mean(valid_h, na.rm = TRUE),
      sd_hedge_ratio = stats::sd(valid_h, na.rm = TRUE),
      min_hedge_ratio = min(valid_h, na.rm = TRUE),
      p01_hedge_ratio = q[1],
      p05_hedge_ratio = q[2],
      median_hedge_ratio = q[3],
      p95_hedge_ratio = q[4],
      p99_hedge_ratio = q[5],
      max_hedge_ratio = max(valid_h, na.rm = TRUE),
      share_negative = mean(valid_h < 0, na.rm = TRUE),
      share_abs_gt_1 = mean(abs(valid_h) > 1, na.rm = TRUE),
      share_abs_gt_2 = mean(abs(valid_h) > 2, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

compute_min_variance_weights <- function(hedge_ratios, config) {
  if (!isTRUE(config$hedging$portfolio_weight) || nrow(hedge_ratios) == 0) {
    return(list(weights = data.frame(), summary = data.frame()))
  }

  weights <- hedge_ratios[, c(
    "date", "target", "hedge", "model", "target_variance",
    "hedge_variance", "conditional_covariance"
  )]
  denominator <- with(weights, target_variance - 2 * conditional_covariance + hedge_variance)
  weights$target_weight_unconstrained <- with(
    weights,
    (hedge_variance - conditional_covariance) / denominator
  )
  weights$target_weight_unconstrained[!is.finite(weights$target_weight_unconstrained)] <- NA_real_
  weights$hedge_weight_unconstrained <- 1 - weights$target_weight_unconstrained
  weights$target_weight_capped_0_1 <- pmin(pmax(weights$target_weight_unconstrained, 0), 1)
  weights$hedge_weight_capped_0_1 <- 1 - weights$target_weight_capped_0_1

  groups <- unique(weights[, c("target", "hedge", "model"), drop = FALSE])
  rows <- list()
  for (i in seq_len(nrow(groups))) {
    sub <- weights[
      weights$target == groups$target[i] &
        weights$hedge == groups$hedge[i] &
        weights$model == groups$model[i],
      ,
      drop = FALSE
    ]
    w <- as.numeric(sub$target_weight_unconstrained)
    q <- safe_numeric_quantile(w, c(0.01, 0.05, 0.50, 0.95, 0.99))
    valid_w <- w[is.finite(w)]
    rows[[length(rows) + 1]] <- data.frame(
      target = groups$target[i],
      hedge = groups$hedge[i],
      model = groups$model[i],
      observations = length(valid_w),
      mean_target_weight = mean(valid_w, na.rm = TRUE),
      sd_target_weight = stats::sd(valid_w, na.rm = TRUE),
      min_target_weight = min(valid_w, na.rm = TRUE),
      p01_target_weight = q[1],
      p05_target_weight = q[2],
      median_target_weight = q[3],
      p95_target_weight = q[4],
      p99_target_weight = q[5],
      max_target_weight = max(valid_w, na.rm = TRUE),
      share_outside_0_1 = mean(valid_w < 0 | valid_w > 1, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }

  list(weights = weights, summary = do.call(rbind, rows))
}

compute_hedge_ratios <- function(dcc_output, returns, config) {
  if (!isTRUE(config$hedging$enabled) || nrow(dcc_output) == 0) {
    return(list(
      hedge_ratios = data.frame(),
      hedge_ratio_summary = data.frame(),
      effectiveness = data.frame(),
      weights = data.frame(),
      weight_summary = data.frame()
    ))
  }

  target <- asset_return_name(config$assets$target)
  required_cols <- c(
    "date", "target", "hedge", "model", "conditional_covariance",
    "target_variance", "hedge_variance"
  )
  missing_cols <- setdiff(required_cols, names(dcc_output))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "DCC/ADCC output is missing required columns: %s",
      paste(missing_cols, collapse = ", ")
    ), call. = FALSE)
  }

  dcc_output$date <- as.Date(dcc_output$date)
  returns$date <- as.Date(returns$date)
  dcc_output$hedge_ratio <- with(dcc_output, conditional_covariance / hedge_variance)
  dcc_output$hedge_ratio[
    !is.finite(dcc_output$hedge_ratio) |
      !is.finite(dcc_output$hedge_variance) |
      dcc_output$hedge_variance <= 0
  ] <- NA_real_

  he_rows <- list()
  hedge_ratio_rows <- list()
  for (hedge in unique(dcc_output$hedge)) {
    if (!hedge %in% names(returns)) next
    for (model_name in unique(dcc_output$model)) {
      sub <- dcc_output[dcc_output$hedge == hedge & dcc_output$model == model_name, , drop = FALSE]
      merged <- merge(
        sub[, c(
          "date", "target", "hedge", "model", "dynamic_correlation",
          "conditional_covariance", "target_variance", "hedge_variance",
          "hedge_ratio", "convergence", "observations", "first_date", "last_date"
        )],
        returns[, c("date", target, hedge), drop = FALSE],
        by = "date",
        all = FALSE
      )
      if (nrow(merged) < 5) next
      names(merged)[names(merged) == target] <- "target_return"
      names(merged)[names(merged) == hedge] <- "hedge_return"
      merged$hedged_return <- merged$target_return - merged$hedge_ratio * merged$hedge_return
      hedge_ratio_rows[[length(hedge_ratio_rows) + 1]] <- merged

      valid <- stats::complete.cases(merged[, c("target_return", "hedge_return", "hedge_ratio", "hedged_return")])
      eval_df <- merged[valid, , drop = FALSE]
      if (nrow(eval_df) < 5) next
      unhedged_var <- stats::var(eval_df$target_return, na.rm = TRUE)
      hedged_var <- stats::var(eval_df$hedged_return, na.rm = TRUE)
      unhedged_vol <- sqrt(unhedged_var)
      hedged_vol <- sqrt(hedged_var)
      he_rows[[length(he_rows) + 1]] <- data.frame(
        target = target,
        hedge = hedge,
        model = model_name,
        observations = nrow(eval_df),
        first_date = min(eval_df$date),
        last_date = max(eval_df$date),
        unhedged_variance = unhedged_var,
        hedged_variance = hedged_var,
        hedging_effectiveness = 1 - hedged_var / unhedged_var,
        unhedged_volatility = unhedged_vol,
        hedged_volatility = hedged_vol,
        volatility_reduction = 1 - hedged_vol / unhedged_vol,
        mean_hedge_ratio = mean(eval_df$hedge_ratio, na.rm = TRUE),
        mean_hedged_return = mean(eval_df$hedged_return, na.rm = TRUE),
        mean_unhedged_return = mean(eval_df$target_return, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }
  }

  hedge_ratios <- if (length(hedge_ratio_rows) == 0) data.frame() else do.call(rbind, hedge_ratio_rows)
  he <- if (length(he_rows) == 0) data.frame() else do.call(rbind, he_rows)
  hedge_ratio_summary <- summarise_hedge_ratios(hedge_ratios)
  weight_output <- compute_min_variance_weights(hedge_ratios, config)

  if (isTRUE(config$outputs$save_hedge_ratio)) {
    write_csv_output(
      hedge_ratios,
      file.path(config$outputs$tables_dir, "hedge_ratios", "dynamic_hedge_ratios.csv"),
      overwrite = config$outputs$overwrite_existing
    )
    write_csv_output(
      hedge_ratio_summary,
      file.path(config$outputs$tables_dir, "hedge_ratios", "hedge_ratio_summary.csv"),
      overwrite = config$outputs$overwrite_existing
    )
    if (nrow(weight_output$weights) > 0) {
      write_csv_output(
        weight_output$weights,
        file.path(config$outputs$tables_dir, "hedge_ratios", "dynamic_min_variance_weights.csv"),
        overwrite = config$outputs$overwrite_existing
      )
      write_csv_output(
        weight_output$summary,
        file.path(config$outputs$tables_dir, "hedge_ratios", "min_variance_weight_summary.csv"),
        overwrite = config$outputs$overwrite_existing
      )
    }
  }
  write_csv_output(
    he,
    file.path(config$outputs$tables_dir, "hedging_effectiveness", "hedging_effectiveness_summary.csv"),
    overwrite = config$outputs$overwrite_existing
  )

  list(
    hedge_ratios = hedge_ratios,
    hedge_ratio_summary = hedge_ratio_summary,
    effectiveness = he,
    weights = weight_output$weights,
    weight_summary = weight_output$summary
  )
}
