ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

ensure_output_dirs <- function(config) {
  dirs <- c(
    config$outputs$diagnostics_dir,
    config$outputs$tables_dir,
    config$outputs$figures_dir,
    config$outputs$logs_dir,
    file.path(config$outputs$tables_dir, "model_selection"),
    file.path(config$outputs$tables_dir, "volatility_estimates"),
    file.path(config$outputs$tables_dir, "dynamic_correlations"),
    file.path(config$outputs$tables_dir, "hedge_ratios"),
    file.path(config$outputs$tables_dir, "hedging_effectiveness"),
    file.path(config$outputs$tables_dir, "safe_haven_tests"),
    file.path(config$outputs$diagnostics_dir, "unit_root"),
    file.path(config$outputs$diagnostics_dir, "arch_effect"),
    file.path(config$outputs$diagnostics_dir, "residual"),
    file.path(config$outputs$logs_dir, "run_config_snapshot"),
    file.path(config$outputs$logs_dir, "estimation_logs"),
    file.path(config$outputs$logs_dir, "convergence_warnings")
  )
  invisible(lapply(dirs, ensure_dir))
}

write_csv_output <- function(x, path, overwrite = TRUE) {
  if (file.exists(path) && !isTRUE(overwrite)) {
    stop(sprintf("Output exists and overwrite_existing is false: %s", path), call. = FALSE)
  }
  ensure_dir(dirname(path))
  utils::write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(path)
}

append_log <- function(message, config, filename = "model_pipeline.log") {
  log_path <- file.path(config$outputs$logs_dir, "estimation_logs", filename)
  ensure_dir(dirname(log_path))
  line <- sprintf("[%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), message)
  cat(line, "\n", file = log_path, append = TRUE)
  invisible(log_path)
}

write_config_snapshot <- function(config_path, config) {
  if (!isTRUE(config$outputs$save_config_snapshot)) {
    return(invisible(NULL))
  }
  destination <- file.path(
    config$outputs$logs_dir,
    "run_config_snapshot",
    sprintf("model_control_%s.yaml", format(Sys.time(), "%Y%m%d_%H%M%S"))
  )
  ensure_dir(dirname(destination))
  file.copy(config_path, destination, overwrite = TRUE)
  invisible(destination)
}
