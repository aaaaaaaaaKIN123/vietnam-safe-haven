source("src/utils/io_helpers.R")
source("src/models/R/model_helpers.R")

build_ugarch_spec <- function(model_name, distribution_name, config) {
  rugarch::ugarchspec(
    variance.model = list(
      model = model_name,
      garchOrder = c(config$univariate_garch$garch_order$p, config$univariate_garch$garch_order$q)
    ),
    mean.model = list(
      armaOrder = c(config$univariate_garch$arma_order$p, config$univariate_garch$arma_order$q),
      include.mean = TRUE
    ),
    distribution.model = distribution_name
  )
}

run_univariate_garch <- function(returns, config) {
  if (!isTRUE(config$univariate_garch$enabled)) {
    return(list(selection = data.frame(), fits = list()))
  }

  safe_package_stop("rugarch")
  append_log("Starting univariate GARCH estimation.", config)

  assets <- setdiff(names(returns), "date")
  rows <- list()
  fits <- list()
  warnings <- character()
  criterion <- config$univariate_garch$selection_criterion

  for (asset in assets) {
    valid_idx <- which(!is.na(as.numeric(returns[[asset]])))
    x <- as.numeric(returns[[asset]])[valid_idx]
    asset_dates <- returns$date[valid_idx]
    for (model_name in config$univariate_garch$models) {
      for (dist_name in config$univariate_garch$distributions) {
        spec <- build_ugarch_spec(model_name, dist_name, config)
        fit_result <- fit_status(
          rugarch::ugarchfit(spec = spec, data = x, solver = "hybrid", fit.control = list(stationarity = 1))
        )
        key <- paste(asset, model_name, dist_name, sep = "__")
        if (fit_result$ok) {
          fit <- fit_result$value
          fits[[key]] <- fit
          rows[[length(rows) + 1]] <- data.frame(
            asset = asset,
            model = model_name,
            distribution = dist_name,
            observations = length(x),
            first_date = if (length(asset_dates) > 0) as.character(min(asset_dates)) else NA_character_,
            last_date = if (length(asset_dates) > 0) as.character(max(asset_dates)) else NA_character_,
            convergence = tryCatch(rugarch::convergence(fit), error = function(e) NA_integer_),
            AIC = information_value(fit, "Akaike"),
            BIC = information_value(fit, "Bayes"),
            selected_criterion = criterion,
            criterion_value = information_value(fit, criterion),
            error = NA_character_,
            stringsAsFactors = FALSE
          )

          if (isTRUE(config$outputs$save_conditional_volatility)) {
            sigma_df <- data.frame(
              date = asset_dates[seq_along(rugarch::sigma(fit))],
              asset = asset,
              model = model_name,
              distribution = dist_name,
              conditional_sigma = as.numeric(rugarch::sigma(fit))
            )
            out <- file.path(config$outputs$tables_dir, "volatility_estimates",
                             sprintf("%s_%s_%s_sigma.csv", asset, model_name, dist_name))
            write_csv_output(sigma_df, out, overwrite = config$outputs$overwrite_existing)
          }
        } else {
          warnings <- c(warnings, sprintf("%s failed: %s", key, fit_result$error))
          rows[[length(rows) + 1]] <- data.frame(
            asset = asset,
            model = model_name,
            distribution = dist_name,
            observations = length(x),
            first_date = if (length(asset_dates) > 0) as.character(min(asset_dates)) else NA_character_,
            last_date = if (length(asset_dates) > 0) as.character(max(asset_dates)) else NA_character_,
            convergence = NA_integer_,
            AIC = NA_real_,
            BIC = NA_real_,
            selected_criterion = criterion,
            criterion_value = NA_real_,
            error = fit_result$error,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  selection <- do.call(rbind, rows)
  if (nrow(selection) > 0) {
    selection <- selection[order(selection$asset, selection$criterion_value), , drop = FALSE]
  }

  write_csv_output(
    selection,
    file.path(config$outputs$tables_dir, "model_selection", "univariate_garch_selection.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  write_warning_table(
    warnings,
    file.path(config$outputs$logs_dir, "convergence_warnings", "univariate_garch_warnings.csv"),
    config
  )

  append_log("Finished univariate GARCH estimation.", config)
  list(selection = selection, fits = fits)
}
