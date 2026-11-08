source("src/utils/config_loader.R")
source("src/utils/io_helpers.R")
source("src/utils/date_asset_helpers.R")

validate_model_control <- function(config, method_map = NULL, strict_packages = FALSE) {
  errors <- character()
  warnings <- character()

  required_top <- c("project", "data", "assets", "diagnostics", "univariate_garch",
                    "dcc_adcc", "hedging", "safe_haven", "robustness", "outputs")
  missing_top <- setdiff(required_top, names(config))
  if (length(missing_top) > 0) {
    errors <- c(errors, sprintf("Missing top-level config keys: %s", paste(missing_top, collapse = ", ")))
  }

  input_path <- project_path(config$data$model_input)
  if (!file.exists(input_path)) {
    warnings <- c(warnings, sprintf("Model input file does not exist yet: %s", input_path))
  }
  if (!is.null(config$data$returns_input)) {
    returns_input_path <- project_path(config$data$returns_input)
    if (!file.exists(returns_input_path)) {
      warnings <- c(warnings, sprintf("Prepared return file does not exist yet: %s", returns_input_path))
    }
  }

  if (!is.null(method_map)) {
    required_methods <- c("univariate_garch", "dcc_adcc_garch", "hedging_effectiveness",
                          "safe_haven_tests", "markov_switching_robustness")
    missing_methods <- setdiff(required_methods, names(method_map$method_map))
    if (length(missing_methods) > 0) {
      errors <- c(errors, sprintf("Missing method-map keys: %s", paste(missing_methods, collapse = ", ")))
    }
  }

  package_checks <- list(
    yaml = TRUE,
    rugarch = isTRUE(config$univariate_garch$enabled),
    rmgarch = isTRUE(config$dcc_adcc$enabled),
    MSGARCH = isTRUE(config$robustness$msgarch$enabled)
  )

  for (pkg in names(package_checks)) {
    if (isTRUE(package_checks[[pkg]]) && !requireNamespace(pkg, quietly = TRUE)) {
      msg <- sprintf("R package '%s' is not installed.", pkg)
      if (isTRUE(strict_packages)) errors <- c(errors, msg) else warnings <- c(warnings, msg)
    }
  }

  list(ok = length(errors) == 0, errors = errors, warnings = warnings)
}

is_direct_rscript_call <- function(file) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
  length(file_arg) > 0 && identical(
    normalizePath(file_arg[1], winslash = "/", mustWork = FALSE),
    normalizePath(file, winslash = "/", mustWork = FALSE)
  )
}

if (!interactive() && is_direct_rscript_call("src/models/validate_model_control.R")) {
  config <- read_model_control()
  method_map <- read_model_method_map()
  result <- validate_model_control(config, method_map, strict_packages = FALSE)
  if (length(result$warnings) > 0) {
    cat("WARNINGS:\n")
    cat(paste0("- ", result$warnings, collapse = "\n"), "\n")
  }
  if (!result$ok) {
    cat("ERRORS:\n")
    cat(paste0("- ", result$errors, collapse = "\n"), "\n")
    quit(status = 1)
  }
  cat("Model control validation completed.\n")
}
