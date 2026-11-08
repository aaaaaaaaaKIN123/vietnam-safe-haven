# run_univariate_garch_diagnostics.R
# Estimate the best GARCH models and output residual diagnostics

source("src/utils/config_loader.R")
source("src/utils/io_helpers.R")
source("src/models/R/01_univariate_garch.R")
source("src/models/run_phase3_2_univariate_garch.R")

config <- read_model_control("src/models/model_control.yaml")
returns <- load_phase3_2_returns(config)

best_models <- utils::read.csv("results/tables/model_selection/univariate_garch_best_by_bic.csv", stringsAsFactors = FALSE)

results <- list()

for (i in 1:nrow(best_models)) {
  asset_name <- best_models$asset[i]
  model_name <- best_models$model[i]
  dist_name <- best_models$distribution[i]
  
  cat(sprintf("Fitting model for %s (%s, %s)...\n", asset_name, model_name, dist_name))
  
  spec <- build_ugarch_spec(model_name, dist_name, config)
  
  valid_idx <- which(!is.na(as.numeric(returns[[asset_name]])))
  x <- as.numeric(returns[[asset_name]])[valid_idx]
  
  fit <- rugarch::ugarchfit(spec = spec, data = x, solver = "hybrid", fit.control = list(stationarity = 1))
  
  # Extract standardized residuals
  std_resid <- as.numeric(rugarch::residuals(fit, standardize = TRUE))
  
  # 1. Ljung-Box on standardized residuals (lag 12)
  lb_res <- stats::Box.test(std_resid, lag = 12, type = "Ljung-Box")
  
  # 2. Ljung-Box on squared standardized residuals (lag 12)
  lb_sq_res <- stats::Box.test(std_resid^2, lag = 12, type = "Ljung-Box")
  
  # 3. ARCH-LM test on standardized residuals (lag 12)
  arch_test_stat <- NA_real_
  arch_p_val <- NA_real_
  if (requireNamespace("FinTS", quietly = TRUE)) {
    arch_res <- tryCatch(FinTS::ArchTest(std_resid, lags = 12), error = function(e) NULL)
    if (!is.null(arch_res)) {
      arch_test_stat <- as.numeric(arch_res$statistic)
      arch_p_val <- as.numeric(arch_res$p.value)
    }
  }
  
  results[[length(results) + 1]] <- data.frame(
    asset = asset_name,
    model = model_name,
    distribution = dist_name,
    observations = length(x),
    lb_stat = lb_res$statistic,
    lb_p_value = lb_res$p.value,
    lb_sq_stat = lb_sq_res$statistic,
    lb_sq_p_value = lb_sq_res$p.value,
    arch_stat = arch_test_stat,
    arch_p_value = arch_p_val,
    stringsAsFactors = FALSE
  )
}

diagnostics_df <- do.call(rbind, results)
write.csv(diagnostics_df, "results/diagnostics/residual/univariate_garch_residual_diagnostics.csv", row.names = FALSE)
cat("Diagnostics completed and saved.\n")
