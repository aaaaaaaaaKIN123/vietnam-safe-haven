safe_package_stop <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(sprintf("Required package '%s' is not installed.", package), call. = FALSE)
  }
}

information_value <- function(fit, criterion) {
  ic <- tryCatch(rugarch::infocriteria(fit), error = function(e) NULL)
  if (is.null(ic)) return(NA_real_)
  criterion_normalized <- tolower(criterion)
  if (criterion_normalized %in% c("bic", "sbic")) {
    criterion_normalized <- "bayes"
  }
  if (criterion_normalized == "aic") {
    criterion_normalized <- "akaike"
  }
  names_ic <- rownames(ic)
  match_idx <- match(criterion_normalized, tolower(names_ic))
  if (is.na(match_idx)) return(NA_real_)
  as.numeric(ic[match_idx, 1])
}

fit_status <- function(expr) {
  tryCatch(
    list(ok = TRUE, value = force(expr), error = NA_character_),
    error = function(e) list(ok = FALSE, value = NULL, error = conditionMessage(e))
  )
}

write_warning_table <- function(warnings, path, config) {
  if (length(warnings) == 0) {
    if (file.exists(path)) {
      file.remove(path)
    }
    return(invisible(NULL))
  }
  warning_df <- data.frame(message = warnings, stringsAsFactors = FALSE)
  write_csv_output(warning_df, path, overwrite = config$outputs$overwrite_existing)
}
