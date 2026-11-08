source("src/utils/config_loader.R")
source("src/utils/io_helpers.R")
source("src/utils/date_asset_helpers.R")
source("src/models/validate_model_control.R")
source("src/diagnostics/unit_root_tests.R")
source("src/diagnostics/arch_lm_tests.R")
source("src/diagnostics/residual_diagnostics.R")
source("src/models/R/01_univariate_garch.R")
source("src/models/R/02_dcc_adcc_garch.R")
source("src/models/R/03_hedging_effectiveness.R")
source("src/models/R/04_safe_haven_tests.R")
source("src/models/R/05_msgarch_robustness.R")

run_model_pipeline <- function(
  control_path = "src/models/model_control.yaml",
  method_map_path = "src/models/model_method_map.yaml",
  strict_packages = FALSE
) {
  config <- read_model_control(control_path)
  method_map <- read_model_method_map(method_map_path)

  validation <- validate_model_control(config, method_map, strict_packages = strict_packages)
  if (length(validation$warnings) > 0) {
    cat("Model-control warnings:\n")
    cat(paste0("- ", validation$warnings, collapse = "\n"), "\n")
  }
  if (!validation$ok) {
    stop(paste(validation$errors, collapse = "\n"), call. = FALSE)
  }

  ensure_output_dirs(config)
  write_config_snapshot(project_path(control_path), config)
  append_log("Model pipeline started.", config)

  returns <- load_model_returns(config)

  run_unit_root_tests(returns, config)
  run_arch_lm_tests(returns, config)

  univariate_result <- run_univariate_garch(returns[, c("date", return_asset_columns(config)), drop = FALSE], config)
  run_residual_diagnostics(univariate_result, config)

  dcc_output <- run_dcc_adcc_garch(returns, config)
  hedge_output <- compute_hedge_ratios(dcc_output, returns, config)
  safe_haven_output <- run_safe_haven_tests(returns, config)
  msgarch_output <- run_msgarch_robustness(returns, config)

  append_log("Model pipeline finished.", config)
  list(
    config = config,
    method_map = method_map,
    returns = returns,
    univariate = univariate_result$selection,
    dcc_adcc = dcc_output,
    hedging = hedge_output$effectiveness,
    safe_haven = safe_haven_output,
    msgarch = msgarch_output
  )
}

is_direct_rscript_call <- function(file) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
  length(file_arg) > 0 && identical(
    normalizePath(file_arg[1], winslash = "/", mustWork = FALSE),
    normalizePath(file, winslash = "/", mustWork = FALSE)
  )
}

if (!interactive() && is_direct_rscript_call("src/models/run_model_pipeline.R")) {
  run_model_pipeline()
}
