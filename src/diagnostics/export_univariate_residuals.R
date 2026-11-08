# export_univariate_residuals.R
source("src/utils/config_loader.R")
source("src/utils/io_helpers.R")
source("src/models/R/01_univariate_garch.R")
source("src/models/run_phase3_2_univariate_garch.R")

config <- read_model_control("src/models/model_control.yaml")
returns <- load_phase3_2_returns(config)
best_models <- utils::read.csv("results/tables/model_selection/univariate_garch_best_by_bic.csv", stringsAsFactors = FALSE)

residuals_list <- list()

for (i in 1:nrow(best_models)) {
  asset_name <- best_models$asset[i]
  model_name <- best_models$model[i]
  dist_name <- best_models$distribution[i]
  
  spec <- build_ugarch_spec(model_name, dist_name, config)
  
  valid_idx <- which(!is.na(as.numeric(returns[[asset_name]])))
  x <- as.numeric(returns[[asset_name]])[valid_idx]
  
  fit <- rugarch::ugarchfit(spec = spec, data = x, solver = "hybrid", fit.control = list(stationarity = 1))
  std_resid <- as.numeric(rugarch::residuals(fit, standardize = TRUE))
  
  # Align residuals with date
  df <- data.frame(
    date = returns$date[valid_idx],
    asset = asset_name,
    std_resid = std_resid,
    stringsAsFactors = FALSE
  )
  residuals_list[[length(residuals_list) + 1]] <- df
}

# Combine and save
all_residuals <- do.call(rbind, residuals_list)
write.csv(all_residuals, "results/diagnostics/residual/univariate_garch_std_residuals.csv", row.names = FALSE)
cat("Exported residuals to results/diagnostics/residual/univariate_garch_std_residuals.csv\n")
