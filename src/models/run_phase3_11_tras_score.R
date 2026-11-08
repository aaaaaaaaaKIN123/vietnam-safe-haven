source("src/utils/config_loader.R")
source("src/utils/io_helpers.R")
source("src/utils/date_asset_helpers.R")
source("src/models/validate_model_control.R")
source("src/models/R/model_helpers.R")
source("src/visualization/scientific_theme.R")

phase3_11_core_hedges <- c("r_sjc_gold", "r_xau_vnd", "r_bond_tri")
phase3_11_alphas <- c(0.01, 0.05, 0.10)
phase3_11_metrics <- c(
  "hedging_effectiveness",
  "mean_var_reduction",
  "mean_es_reduction",
  "drawdown_reduction",
  "portfolio_delta_covar_improvement",
  "lower_tail_absorption_score"
)
phase3_11_layer_metrics <- list(
  direct_protection = c("hedging_effectiveness", "mean_var_reduction", "mean_es_reduction", "drawdown_reduction"),
  systemic_absorption = c("lower_tail_absorption_score"),
  conditional_tail = c("portfolio_delta_covar_improvement")
)

phase3_11_read_csv <- function(path, required = TRUE) {
  full_path <- project_path(path)
  if (!file.exists(full_path)) {
    if (isTRUE(required)) {
      stop(sprintf("Required input file not found: %s", full_path), call. = FALSE)
    }
    return(data.frame())
  }
  out <- utils::read.csv(full_path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
  strip_bom_from_names(out)
}

phase3_11_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

phase3_11_prepare_hedge_ratios <- function(path = "results/tables/hedge_ratios/dynamic_hedge_ratios.csv") {
  x <- phase3_11_read_csv(path)
  required_cols <- c("date", "target", "hedge", "model", "target_return", "hedge_return", "hedged_return")
  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0) {
    stop(sprintf("Dynamic hedge-ratio file is missing columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  x$date <- as.Date(x$date)
  for (col in setdiff(names(x), c("date", "target", "hedge", "model", "first_date", "last_date"))) {
    x[[col]] <- phase3_11_numeric(x[[col]])
  }
  x <- x[x$hedge %in% phase3_11_core_hedges, , drop = FALSE]
  x <- x[stats::complete.cases(x[, required_cols, drop = FALSE]), , drop = FALSE]
  x[order(x$target, x$hedge, x$model, x$date), , drop = FALSE]
}

phase3_11_prepare_returns <- function(config) {
  x <- phase3_11_read_csv(config$data$returns_input)
  date_col <- config$data$date_column
  if (!date_col %in% names(x)) {
    stop(sprintf("Date column '%s' not found in prepared returns.", date_col), call. = FALSE)
  }
  x[[date_col]] <- as.Date(x[[date_col]])
  x <- x[
    x[[date_col]] > as.Date(config$data$sample_start) &
      x[[date_col]] <= as.Date(config$data$sample_end),
    ,
    drop = FALSE
  ]
  for (col in setdiff(names(x), date_col)) {
    x[[col]] <- phase3_11_numeric(x[[col]])
  }
  x
}

phase3_11_group_key <- function(x) {
  paste(x$target, x$hedge, x$model, sep = "|")
}

phase3_11_common_dates <- function(hedge_ratios) {
  groups <- unique(hedge_ratios[, c("target", "hedge", "model"), drop = FALSE])
  date_sets <- vector("list", nrow(groups))
  for (i in seq_len(nrow(groups))) {
    sub <- hedge_ratios[
      hedge_ratios$target == groups$target[i] &
        hedge_ratios$hedge == groups$hedge[i] &
        hedge_ratios$model == groups$model[i],
      ,
      drop = FALSE
    ]
    date_sets[[i]] <- unique(sub$date)
  }
  common <- Reduce(intersect, date_sets)
  sort(as.Date(common, origin = "1970-01-01"))
}

phase3_11_var_es_loss <- function(x, alpha) {
  x <- x[is.finite(x)]
  if (length(x) < 30) {
    return(c(var_loss = NA_real_, es_loss = NA_real_))
  }
  q <- as.numeric(stats::quantile(x, probs = alpha, na.rm = TRUE, names = FALSE, type = 7))
  tail_x <- x[x <= q]
  es <- if (length(tail_x) == 0) NA_real_ else mean(tail_x, na.rm = TRUE)
  c(var_loss = -q, es_loss = -es)
}

phase3_11_safe_reduction <- function(unhedged_loss, hedged_loss) {
  if (!is.finite(unhedged_loss) || unhedged_loss <= 0 || !is.finite(hedged_loss)) {
    return(NA_real_)
  }
  1 - hedged_loss / unhedged_loss
}

phase3_11_max_drawdown_loss <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 30) return(NA_real_)
  wealth <- exp(cumsum(x / 100))
  running_peak <- cummax(wealth)
  drawdown <- wealth / running_peak - 1
  -min(drawdown, na.rm = TRUE)
}

phase3_11_tail_metrics <- function(target_return, hedged_return) {
  var_reductions <- numeric(length(phase3_11_alphas))
  es_reductions <- numeric(length(phase3_11_alphas))
  for (i in seq_along(phase3_11_alphas)) {
    alpha <- phase3_11_alphas[i]
    unhedged <- phase3_11_var_es_loss(target_return, alpha)
    hedged <- phase3_11_var_es_loss(hedged_return, alpha)
    var_reductions[i] <- phase3_11_safe_reduction(unhedged[["var_loss"]], hedged[["var_loss"]])
    es_reductions[i] <- phase3_11_safe_reduction(unhedged[["es_loss"]], hedged[["es_loss"]])
  }
  unhedged_dd <- phase3_11_max_drawdown_loss(target_return)
  hedged_dd <- phase3_11_max_drawdown_loss(hedged_return)
  c(
    mean_var_reduction = mean(var_reductions, na.rm = TRUE),
    mean_es_reduction = mean(es_reductions, na.rm = TRUE),
    var_reduction_1pct = var_reductions[1],
    es_reduction_1pct = es_reductions[1],
    var_reduction_5pct = var_reductions[2],
    es_reduction_5pct = es_reductions[2],
    drawdown_reduction = phase3_11_safe_reduction(unhedged_dd, hedged_dd)
  )
}

phase3_11_portfolio_delta_covar_common <- function(hedge_ratios, common_dates) {
  if (!requireNamespace("quantreg", quietly = TRUE)) {
    stop("Package 'quantreg' is required for Phase 3.11 TRAS portfolio Delta-CoVaR-style component.", call. = FALSE)
  }
  rows <- list()
  common_hedges <- hedge_ratios[hedge_ratios$date %in% common_dates, , drop = FALSE]
  groups <- unique(common_hedges[, c("target", "hedge", "model"), drop = FALSE])
  for (g in seq_len(nrow(groups))) {
    sub <- common_hedges[
      common_hedges$target == groups$target[g] &
        common_hedges$hedge == groups$hedge[g] &
        common_hedges$model == groups$model[g],
      ,
      drop = FALSE
    ]
    sub <- sub[stats::complete.cases(sub[, c("target_return", "hedged_return"), drop = FALSE]), , drop = FALSE]
    if (nrow(sub) < 120) next
    improvements <- numeric(length(phase3_11_alphas))
    unhedged_deltas <- numeric(length(phase3_11_alphas))
    hedged_deltas <- numeric(length(phase3_11_alphas))
    status <- "ok"
    for (i in seq_along(phase3_11_alphas)) {
      alpha <- phase3_11_alphas[i]
      target_var <- as.numeric(stats::quantile(sub$target_return, probs = alpha, na.rm = TRUE, names = FALSE))
      target_median <- as.numeric(stats::quantile(sub$target_return, probs = 0.50, na.rm = TRUE, names = FALSE))
      unhedged_delta <- target_var - target_median
      fit <- tryCatch(
        quantreg::rq(hedged_return ~ target_return, tau = alpha, data = sub, method = "fn"),
        error = function(e) e
      )
      if (inherits(fit, "error")) {
        improvements[i] <- NA_real_
        unhedged_deltas[i] <- unhedged_delta
        hedged_deltas[i] <- NA_real_
        status <- paste("partial_failed:", fit$message)
        next
      }
      hedged_covar_distress <- as.numeric(stats::predict(fit, newdata = data.frame(target_return = target_var)))
      hedged_covar_median <- as.numeric(stats::predict(fit, newdata = data.frame(target_return = target_median)))
      hedged_delta <- hedged_covar_distress - hedged_covar_median
      unhedged_deltas[i] <- unhedged_delta
      hedged_deltas[i] <- hedged_delta
      improvements[i] <- hedged_delta - unhedged_delta
    }
    rows[[length(rows) + 1]] <- data.frame(
      target = groups$target[g],
      hedge = groups$hedge[g],
      model = groups$model[g],
      portfolio_delta_covar_improvement = mean(improvements, na.rm = TRUE),
      delta_covar_style_improvement = mean(improvements, na.rm = TRUE),
      unhedged_delta_covar_alpha_1pct = unhedged_deltas[1],
      hedged_delta_covar_alpha_1pct = hedged_deltas[1],
      portfolio_delta_covar_improvement_alpha_1pct = improvements[1],
      unhedged_delta_covar_alpha_5pct = unhedged_deltas[2],
      hedged_delta_covar_alpha_5pct = hedged_deltas[2],
      portfolio_delta_covar_improvement_alpha_5pct = improvements[2],
      unhedged_delta_covar_alpha_10pct = unhedged_deltas[3],
      hedged_delta_covar_alpha_10pct = hedged_deltas[3],
      portfolio_delta_covar_improvement_alpha_10pct = improvements[3],
      delta_covar_status = status,
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) data.frame() else do.call(rbind, rows)
}

phase3_11_connectedness_common <- function(path = "results/tables/connectedness/quantile_connectedness_net.csv") {
  x <- phase3_11_read_csv(path, required = FALSE)
  if (nrow(x) == 0) return(data.frame())
  x$tau <- phase3_11_numeric(x$tau)
  x$net <- phase3_11_numeric(x$net)
  x <- x[abs(x$tau - 0.05) < 1e-8 & x$asset %in% phase3_11_core_hedges, c("asset", "net"), drop = FALSE]
  if (nrow(x) == 0) return(data.frame())
  data.frame(
    hedge = x$asset,
    lower_tail_net_connectedness = x$net,
    lower_tail_absorption_score = -x$net,
    stringsAsFactors = FALSE
  )
}

phase3_11_component_table <- function(hedge_ratios, returns, common_dates) {
  common_hedges <- hedge_ratios[hedge_ratios$date %in% common_dates, , drop = FALSE]
  groups <- unique(common_hedges[, c("target", "hedge", "model"), drop = FALSE])
  delta <- phase3_11_portfolio_delta_covar_common(hedge_ratios, common_dates)
  connectedness <- phase3_11_connectedness_common()
  rows <- list()
  for (i in seq_len(nrow(groups))) {
    sub <- common_hedges[
      common_hedges$target == groups$target[i] &
        common_hedges$hedge == groups$hedge[i] &
        common_hedges$model == groups$model[i],
      ,
      drop = FALSE
    ]
    sub <- sub[stats::complete.cases(sub[, c("target_return", "hedged_return"), drop = FALSE]), , drop = FALSE]
    if (nrow(sub) < 120) next
    he <- 1 - stats::var(sub$hedged_return, na.rm = TRUE) / stats::var(sub$target_return, na.rm = TRUE)
    tail <- phase3_11_tail_metrics(sub$target_return, sub$hedged_return)
    dc <- delta[
      delta$target == groups$target[i] &
        delta$hedge == groups$hedge[i] &
        delta$model == groups$model[i],
      ,
      drop = FALSE
    ]
    cn <- connectedness[connectedness$hedge == groups$hedge[i], , drop = FALSE]
    rows[[length(rows) + 1]] <- data.frame(
      sample_label = "core_common_sample",
      target = groups$target[i],
      hedge = groups$hedge[i],
      model = groups$model[i],
      observations = nrow(sub),
      first_date = min(sub$date, na.rm = TRUE),
      last_date = max(sub$date, na.rm = TRUE),
      hedging_effectiveness = he,
      mean_var_reduction = tail[["mean_var_reduction"]],
      mean_es_reduction = tail[["mean_es_reduction"]],
      var_reduction_1pct = tail[["var_reduction_1pct"]],
      es_reduction_1pct = tail[["es_reduction_1pct"]],
      var_reduction_5pct = tail[["var_reduction_5pct"]],
      es_reduction_5pct = tail[["es_reduction_5pct"]],
      drawdown_reduction = tail[["drawdown_reduction"]],
      portfolio_delta_covar_improvement = if (nrow(dc) == 0) NA_real_ else dc$portfolio_delta_covar_improvement[1],
      delta_covar_style_improvement = if (nrow(dc) == 0) NA_real_ else dc$delta_covar_style_improvement[1],
      lower_tail_net_connectedness = if (nrow(cn) == 0) NA_real_ else cn$lower_tail_net_connectedness[1],
      lower_tail_absorption_score = if (nrow(cn) == 0) NA_real_ else cn$lower_tail_absorption_score[1],
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) data.frame() else do.call(rbind, rows)
}

phase3_11_rank_scores <- function(x, metrics = phase3_11_metrics) {
  out <- x
  component_rows <- list()
  n <- nrow(out)
  for (metric in metrics) {
    values <- phase3_11_numeric(out[[metric]])
    ranks <- rep(NA_real_, n)
    scores <- rep(NA_real_, n)
    valid <- is.finite(values)
    if (sum(valid) > 0) {
      ranks[valid] <- rank(-values[valid], ties.method = "average")
      scores[valid] <- if (sum(valid) == 1) 1 else (sum(valid) - ranks[valid]) / (sum(valid) - 1)
    }
    out[[paste0(metric, "_rank")]] <- ranks
    out[[paste0(metric, "_score")]] <- scores
    component_rows[[length(component_rows) + 1]] <- data.frame(
      sample_label = out$sample_label,
      target = out$target,
      hedge = out$hedge,
      model = out$model,
      metric = metric,
      metric_value = values,
      metric_rank = ranks,
      normalized_component_score = scores,
      direction = "higher_is_better",
      stringsAsFactors = FALSE
    )
  }
  score_cols <- paste0(metrics, "_score")
  out$tras_flat_score <- rowMeans(out[, score_cols, drop = FALSE], na.rm = TRUE)
  out$tras_flat_rank <- rank(-out$tras_flat_score, ties.method = "average")
  layer_rows <- list()
  for (layer_name in names(phase3_11_layer_metrics)) {
    layer_metric_cols <- paste0(phase3_11_layer_metrics[[layer_name]], "_score")
    layer_metric_cols <- layer_metric_cols[layer_metric_cols %in% names(out)]
    layer_score <- if (length(layer_metric_cols) == 0) {
      rep(NA_real_, nrow(out))
    } else {
      rowMeans(out[, layer_metric_cols, drop = FALSE], na.rm = TRUE)
    }
    out[[paste0(layer_name, "_layer_score")]] <- layer_score
    out[[paste0(layer_name, "_layer_rank")]] <- rank(-layer_score, ties.method = "average")
    layer_rows[[length(layer_rows) + 1]] <- data.frame(
      sample_label = out$sample_label,
      target = out$target,
      hedge = out$hedge,
      model = out$model,
      layer = layer_name,
      layer_score = layer_score,
      layer_rank = out[[paste0(layer_name, "_layer_rank")]],
      stringsAsFactors = FALSE
    )
  }
  layer_score_cols <- paste0(names(phase3_11_layer_metrics), "_layer_score")
  out$tras_layered_score <- rowMeans(out[, layer_score_cols, drop = FALSE], na.rm = TRUE)
  out$tras_layered_rank <- rank(-out$tras_layered_score, ties.method = "average")
  out$tras_score <- out$tras_layered_score
  out$tras_component_count <- rowSums(is.finite(as.matrix(out[, score_cols, drop = FALSE])))
  out$tras_rank <- out$tras_layered_rank
  out <- out[order(out$tras_rank, out$hedge, out$model), , drop = FALSE]
  rownames(out) <- NULL
  component <- do.call(rbind, component_rows)
  layers <- do.call(rbind, layer_rows)
  list(final = out, component = component, layers = layers)
}

phase3_11_leave_one_metric_out <- function(ranked, metrics = phase3_11_metrics) {
  rows <- list()
  score_cols <- paste0(metrics, "_score")
  base <- ranked[, c("sample_label", "target", "hedge", "model", "tras_score", "tras_rank"), drop = FALSE]
  names(base)[names(base) %in% c("tras_score", "tras_rank")] <- c("base_tras_score", "base_tras_rank")
  for (metric in metrics) {
    temp <- ranked
    temp[[paste0(metric, "_score")]] <- NA_real_
    layer_scores <- list()
    for (layer_name in names(phase3_11_layer_metrics)) {
      layer_metric_cols <- paste0(phase3_11_layer_metrics[[layer_name]], "_score")
      layer_metric_cols <- layer_metric_cols[layer_metric_cols %in% names(temp)]
      layer_scores[[layer_name]] <- if (length(layer_metric_cols) == 0) {
        rep(NA_real_, nrow(temp))
      } else {
        rowMeans(temp[, layer_metric_cols, drop = FALSE], na.rm = TRUE)
      }
    }
    score <- rowMeans(as.data.frame(layer_scores), na.rm = TRUE)
    rank_without <- rank(-score, ties.method = "average")
    rows[[length(rows) + 1]] <- data.frame(
      sample_label = ranked$sample_label,
      target = ranked$target,
      hedge = ranked$hedge,
      model = ranked$model,
      omitted_metric = metric,
      tras_score_without_metric = score,
      tras_rank_without_metric = rank_without,
      base_tras_score = base$base_tras_score,
      base_tras_rank = base$base_tras_rank,
      rank_change_vs_base = rank_without - base$base_tras_rank,
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  out <- out[order(out$omitted_metric, out$tras_rank_without_metric), , drop = FALSE]
  rownames(out) <- NULL
  out
}

phase3_11_plot_outputs <- function(ranked, component, config) {
  output_dir <- file.path(config$outputs$figures_dir, "tras")
  ensure_dir(output_dir)
  files <- character()

  draw_bar <- function() {
    y <- ranked$tras_score
    names(y) <- paste(ranked$hedge, ranked$model, sep = "_")
    graphics::par(mar = c(8, 4.8, 3, 2.2), family = "sans")
    graphics::barplot(y, col = scientific_asset_colors(ranked$hedge), border = "white", las = 2, ylim = c(0, 1), ylab = "TRAS score", main = "TRAS final ranking")
    graphics::grid(nx = NA, ny = NULL, col = "gray85", lty = "solid")
    scientific_asset_legend(ranked$hedge, x = "topright", title = "Hedge asset")
  }
  png_path <- file.path(output_dir, "tras_ranking_bar.png")
  grDevices::png(png_path, width = 1600, height = 950, res = 150)
  draw_bar()
  grDevices::dev.off()
  svg_path <- file.path(output_dir, "tras_ranking_bar.svg")
  grDevices::svg(svg_path, width = 10.5, height = 6.3)
  draw_bar()
  grDevices::dev.off()
  files <- c(files, png_path, svg_path)

  heat <- component[, c("hedge", "model", "metric", "normalized_component_score"), drop = FALSE]
  heat$combo <- paste(heat$hedge, heat$model, sep = "_")
  combos <- unique(paste(ranked$hedge, ranked$model, sep = "_"))
  metrics <- phase3_11_metrics
  matrix_values <- matrix(NA_real_, nrow = length(metrics), ncol = length(combos), dimnames = list(metrics, combos))
  for (i in seq_len(nrow(heat))) {
    matrix_values[heat$metric[i], heat$combo[i]] <- heat$normalized_component_score[i]
  }
  draw_heat <- function() {
    heat_cols <- scientific_heat_colors(12)
    graphics::layout(matrix(c(1, 2), nrow = 1), widths = c(9.2, 1.0))
    graphics::par(mar = c(8.5, 12, 3, 0.8), family = "sans")
    graphics::image(
      x = seq_len(ncol(matrix_values)),
      y = seq_len(nrow(matrix_values)),
      z = t(matrix_values[nrow(matrix_values):1, , drop = FALSE]),
      col = heat_cols,
      axes = FALSE,
      xlab = "",
      ylab = "",
      main = "TRAS component-score heatmap"
    )
    graphics::mtext("Color scale shows normalized component score; higher is better", side = 3, line = 0.2, cex = 0.8)
    graphics::axis(1, at = seq_len(ncol(matrix_values)), labels = colnames(matrix_values), las = 2, cex.axis = 0.75)
    graphics::axis(2, at = seq_len(nrow(matrix_values)), labels = rev(rownames(matrix_values)), las = 2, cex.axis = 0.75)
    graphics::box()
    scientific_draw_colorbar_panel(
      zlim = c(0, 1),
      colors = heat_cols,
      label = "Normalized score"
    )
    graphics::layout(1)
  }
  png_path <- file.path(output_dir, "tras_heatmap.png")
  grDevices::png(png_path, width = 1700, height = 1050, res = 150)
  draw_heat()
  grDevices::dev.off()
  svg_path <- file.path(output_dir, "tras_heatmap.svg")
  grDevices::svg(svg_path, width = 11.2, height = 7.0)
  draw_heat()
  grDevices::dev.off()
  files <- c(files, png_path, svg_path)

  files
}

phase3_11_write_report <- function(outputs, common_dates, config) {
  report_path <- project_path("data/metadata/phase3_11_tras_report.md")
  ranked <- outputs$ranked
  loo <- outputs$leave_one_out
  top_line <- if (nrow(ranked) == 0) {
    "No TRAS ranking was generated."
  } else {
    sprintf(
      "Top TRAS-layered combination: `%s` under `%s` (TRAS-layered = %.6f, rank = %.1f).",
      ranked$hedge[1], ranked$model[1], ranked$tras_score[1], ranked$tras_rank[1]
    )
  }
  max_abs_rank_change <- if (nrow(loo) == 0) NA_real_ else max(abs(loo$rank_change_vs_base), na.rm = TRUE)
  stability_line <- if (!is.finite(max_abs_rank_change)) {
    "Leave-one-metric-out stability could not be evaluated."
  } else if (max_abs_rank_change >= 3) {
    sprintf("Maximum absolute rank change under leave-one-metric-out check: `%.1f`; therefore the ranking is informative but metric-sensitive and should be read together with component ranks.", max_abs_rank_change)
  } else {
    sprintf("Maximum absolute rank change under leave-one-metric-out check: `%.1f`.", max_abs_rank_change)
  }
  lines <- c(
    "# Phase 3.11 TRAS Report",
    "",
    "## Purpose",
    "",
    "Phase 3.11 builds a rank-based Tail Risk Absorption Score (TRAS) for investable core safe-haven candidates. The ranking is computed on a common sample to avoid mixing assets with different histories. Brent is excluded from the main TRAS ranking because it is treated as an oil-price shock/risk channel, not as a core safe-haven asset.",
    "",
    "## Inputs",
    "",
    "- `results/tables/hedge_ratios/dynamic_hedge_ratios.csv`",
    "- `results/tables/connectedness/quantile_connectedness_net.csv`",
    sprintf("- `%s`", config$data$returns_input),
    "",
    "## Common Sample",
    "",
    sprintf("- Hedges: `%s`", paste(phase3_11_core_hedges, collapse = "`, `")),
    sprintf("- Common observations: `%s`", length(common_dates)),
    sprintf("- Common sample: `%s` to `%s`", min(common_dates), max(common_dates)),
    "",
    "## Components",
    "",
    "- Hedging effectiveness",
    "- Mean VaR reduction",
    "- Mean ES reduction",
    "- Drawdown reduction",
    "- Portfolio Delta-CoVaR-style conditional tail-risk improvement",
    "- Lower-tail connectedness absorption score",
    "",
    "## TRAS Aggregation",
    "",
    "- `TRAS-flat`: average of all six metric scores; kept as a robustness/reference score.",
    "- `TRAS-layered`: average of three layer scores and used as the main TRAS result.",
    "- Direct Protection Layer: HE, VaR reduction, ES reduction, and drawdown reduction.",
    "- Systemic Absorption Layer: lower-tail connectedness absorption.",
    "- Conditional Tail Layer: portfolio Delta-CoVaR-style improvement of the hedged portfolio versus unhedged VNIndex.",
    "",
    "## Outputs",
    "",
    sprintf("- Final ranking rows: `%s`", nrow(ranked)),
    sprintf("- Component-rank rows: `%s`", nrow(outputs$component)),
    sprintf("- Layer-score rows: `%s`", nrow(outputs$layers)),
    sprintf("- Leave-one-metric-out rows: `%s`", nrow(loo)),
    sprintf("- Figure files: `%s`", length(outputs$figures)),
    "",
    "## Main Diagnostic Notes",
    "",
    top_line,
    stability_line,
    "",
    "## Scientific Cautions",
    "",
    "- TRAS is a transparent rank-based score, not a structural causal estimate.",
    "- Layered equal-rank averaging avoids subjective metric weights while preserving the three-layer TRAS design.",
    "- A strong TRAS rank should be interpreted together with component ranks.",
    "- Brent is excluded from the main TRAS ranking because it is an oil-price shock/risk channel, not a core safe-haven asset.",
    "- Crypto is excluded from the main TRAS ranking because Phase 3.9 connectedness was defined for the core system only."
  )
  con <- file(report_path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw("\ufeff"), con)
  writeLines(enc2utf8(lines), con, useBytes = TRUE)
  invisible(report_path)
}

run_phase3_11_tras_score <- function(
  control_path = "src/models/model_control.yaml",
  method_map_path = "src/models/model_method_map.yaml",
  strict_packages = TRUE
) {
  config <- read_model_control(control_path)
  method_map <- read_model_method_map(method_map_path)
  validation <- validate_model_control(config, method_map, strict_packages = strict_packages)
  if (!validation$ok) {
    stop(paste(validation$errors, collapse = "\n"), call. = FALSE)
  }
  if (length(validation$warnings) > 0) {
    warning(paste(validation$warnings, collapse = "\n"), call. = FALSE)
  }
  safe_package_stop("quantreg")
  ensure_output_dirs(config)
  ensure_dir(file.path(config$outputs$tables_dir, "tras"))
  ensure_dir(file.path(config$outputs$figures_dir, "tras"))
  append_log("Phase 3.11 TRAS started.", config)

  hedge_ratios <- phase3_11_prepare_hedge_ratios()
  returns <- phase3_11_prepare_returns(config)
  common_dates <- phase3_11_common_dates(hedge_ratios)
  if (length(common_dates) < 120) {
    stop("Phase 3.11 requires at least 120 common-sample observations.", call. = FALSE)
  }
  components_wide <- phase3_11_component_table(hedge_ratios, returns, common_dates)
  ranked_result <- phase3_11_rank_scores(components_wide)
  leave_one <- phase3_11_leave_one_metric_out(ranked_result$final)

  write_csv_output(ranked_result$component, file.path(config$outputs$tables_dir, "tras", "tras_component_ranks.csv"), overwrite = config$outputs$overwrite_existing)
  write_csv_output(ranked_result$layers, file.path(config$outputs$tables_dir, "tras", "tras_layer_scores.csv"), overwrite = config$outputs$overwrite_existing)
  write_csv_output(ranked_result$final, file.path(config$outputs$tables_dir, "tras", "tras_final_ranking.csv"), overwrite = config$outputs$overwrite_existing)
  write_csv_output(leave_one, file.path(config$outputs$tables_dir, "tras", "tras_leave_one_metric_out.csv"), overwrite = config$outputs$overwrite_existing)

  figures <- phase3_11_plot_outputs(ranked_result$final, ranked_result$component, config)
  report <- phase3_11_write_report(
    list(ranked = ranked_result$final, component = ranked_result$component, layers = ranked_result$layers, leave_one_out = leave_one, figures = figures),
    common_dates,
    config
  )
  append_log("Phase 3.11 TRAS finished.", config)
  list(
    components_wide = components_wide,
    ranked = ranked_result$final,
    component = ranked_result$component,
    layers = ranked_result$layers,
    leave_one_out = leave_one,
    common_dates = common_dates,
    figures = figures,
    report = report
  )
}

is_direct_rscript_call <- function(file) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
  length(file_arg) > 0 &&
    identical(
      normalizePath(file_arg[1], winslash = "/", mustWork = FALSE),
      normalizePath(file, winslash = "/", mustWork = FALSE)
    )
}

if (!interactive() && is_direct_rscript_call("src/models/run_phase3_11_tras_score.R")) {
  result <- run_phase3_11_tras_score()
  cat("Phase 3.11 TRAS completed.\n")
  cat(sprintf("Common observations: %s\n", length(result$common_dates)))
  cat(sprintf("Final ranking rows: %s\n", nrow(result$ranked)))
  cat(sprintf("Component-rank rows: %s\n", nrow(result$component)))
  cat(sprintf("Layer-score rows: %s\n", nrow(result$layers)))
  cat(sprintf("Leave-one-metric-out rows: %s\n", nrow(result$leave_one_out)))
  cat(sprintf("Report: %s\n", result$report))
}
