required_asset_columns <- function(config, include_extensions = TRUE) {
  hedges <- config$assets$core_hedges
  if (isTRUE(include_extensions)) {
    hedges <- unique(c(hedges, config$assets$extension_hedges))
  }
  unique(c(config$assets$target, hedges, config$assets$shock_controls))
}

asset_return_name <- function(asset) {
  if (identical(asset, "bond_tri_10y")) {
    return("r_bond_tri")
  }
  if (grepl("^r_", asset)) {
    return(asset)
  }
  paste0("r_", asset)
}

strip_bom_from_names <- function(x) {
  names(x) <- sub("^\xef\xbb\xbf", "", names(x), useBytes = TRUE)
  x
}

return_asset_columns <- function(config, include_extensions = TRUE, include_controls = FALSE) {
  assets <- c(config$assets$target, config$assets$core_hedges)
  if (isTRUE(include_extensions)) {
    assets <- unique(c(assets, config$assets$extension_hedges))
  }
  out <- vapply(assets, asset_return_name, character(1))
  if (isTRUE(include_controls)) {
    controls <- c("r_brent", "r_wti", "d_vix", "d_vn10y_yield", "d_vn5y_yield",
                  "d_interbank_rate", "d_policy_rate")
    out <- unique(c(out, controls))
  }
  unique(out)
}

load_model_returns <- function(config, input_path = NULL) {
  if (is.null(input_path)) {
    input_path <- if (!is.null(config$data$returns_input)) {
      config$data$returns_input
    } else {
      "data/02_model_input/core/final_daily_returns.csv"
    }
  }
  full_path <- project_path(input_path)
  if (!file.exists(full_path)) {
    stop(sprintf("Prepared return file not found: %s", full_path), call. = FALSE)
  }

  returns <- utils::read.csv(full_path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
  returns <- strip_bom_from_names(returns)
  date_col <- config$data$date_column
  if (!date_col %in% names(returns)) {
    stop(sprintf("Date column '%s' not found in prepared return file.", date_col), call. = FALSE)
  }

  returns[[date_col]] <- as.Date(returns[[date_col]])
  returns <- returns[returns[[date_col]] > as.Date(config$data$sample_start) &
                       returns[[date_col]] <= as.Date(config$data$sample_end), , drop = FALSE]

  selected <- intersect(return_asset_columns(config, include_extensions = TRUE, include_controls = TRUE), names(returns))
  returns[, c(date_col, selected), drop = FALSE]
}

load_model_panel <- function(config) {
  input_path <- project_path(config$data$model_input)
  if (!file.exists(input_path)) {
    stop(sprintf("Model input file not found: %s", input_path), call. = FALSE)
  }

  panel <- utils::read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
  panel <- strip_bom_from_names(panel)
  date_col <- config$data$date_column
  if (!date_col %in% names(panel)) {
    stop(sprintf("Date column '%s' not found in model input.", date_col), call. = FALSE)
  }

  panel[[date_col]] <- as.Date(panel[[date_col]])
  panel <- panel[panel[[date_col]] >= as.Date(config$data$sample_start) &
                   panel[[date_col]] <= as.Date(config$data$sample_end), , drop = FALSE]

  if (identical(config$data$missing_policy, "inner_join")) {
    candidate_cols <- intersect(unique(c(config$assets$target, config$assets$core_hedges)), names(panel))
    panel <- panel[stats::complete.cases(panel[, c(date_col, candidate_cols), drop = FALSE]), , drop = FALSE]
  }

  panel
}

return_columns <- function(config) {
  unique(c(config$assets$target, config$assets$core_hedges, config$assets$extension_hedges))
}

compute_log_returns <- function(panel, config) {
  date_col <- config$data$date_column
  cols <- intersect(return_columns(config), names(panel))
  scale <- config$data$return_scale

  out <- data.frame(date = panel[[date_col]][-1])
  for (col in cols) {
    values <- suppressWarnings(as.numeric(panel[[col]]))
    out[[col]] <- c(diff(log(values))) * scale
  }
  out
}
