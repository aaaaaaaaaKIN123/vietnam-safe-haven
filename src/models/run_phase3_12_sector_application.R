source("src/utils/config_loader.R")
source("src/utils/io_helpers.R")
source("src/utils/date_asset_helpers.R")
source("src/models/validate_model_control.R")
source("src/models/R/model_helpers.R")
source("src/visualization/scientific_theme.R")

phase3_12_sector_cols <- c(
  "r_vnfin", "r_vnreal", "r_vnind", "r_vnit", "r_vnheal",
  "r_vnuti", "r_vncons", "r_vncond", "r_vnmat", "r_vnene"
)
phase3_12_safe_haven_hedges <- c("r_sjc_gold", "r_xau_vnd", "r_bond_tri")
phase3_12_alphas <- c(0.01, 0.05, 0.10)
phase3_12_rank_metrics <- c(
  "hedging_effectiveness",
  "mean_var_reduction",
  "mean_es_reduction",
  "drawdown_reduction"
)

phase3_12_read_csv <- function(path) {
  full_path <- project_path(path)
  if (!file.exists(full_path)) {
    stop(sprintf("Required input file not found: %s", full_path), call. = FALSE)
  }
  out <- utils::read.csv(full_path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
  strip_bom_from_names(out)
}

phase3_12_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

phase3_12_prepare_panel <- function(path) {
  x <- phase3_12_read_csv(path)
  required <- c("date", phase3_12_sector_cols, phase3_12_safe_haven_hedges)
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols) > 0) {
    stop(sprintf("Sector panel is missing columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  x$date <- as.Date(x$date)
  for (col in setdiff(names(x), "date")) {
    x[[col]] <- phase3_12_numeric(x[[col]])
  }
  x <- x[stats::complete.cases(x[, required, drop = FALSE]), required, drop = FALSE]
  x[order(x$date), , drop = FALSE]
}

phase3_12_var_es_loss <- function(x, alpha) {
  x <- x[is.finite(x)]
  if (length(x) < 30) {
    return(c(var_loss = NA_real_, es_loss = NA_real_))
  }
  q <- as.numeric(stats::quantile(x, probs = alpha, na.rm = TRUE, names = FALSE, type = 7))
  tail_x <- x[x <= q]
  es <- if (length(tail_x) == 0) NA_real_ else mean(tail_x, na.rm = TRUE)
  c(var_loss = -q, es_loss = -es)
}

phase3_12_safe_reduction <- function(unhedged_loss, hedged_loss) {
  if (!is.finite(unhedged_loss) || unhedged_loss <= 0 || !is.finite(hedged_loss)) {
    return(NA_real_)
  }
  1 - hedged_loss / unhedged_loss
}

phase3_12_max_drawdown_loss <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 30) return(NA_real_)
  wealth <- exp(cumsum(x / 100))
  running_peak <- cummax(wealth)
  drawdown <- wealth / running_peak - 1
  -min(drawdown, na.rm = TRUE)
}

phase3_12_static_hedge_metrics <- function(target_return, hedge_return) {
  valid <- is.finite(target_return) & is.finite(hedge_return)
  target_return <- target_return[valid]
  hedge_return <- hedge_return[valid]
  if (length(target_return) < 120 || stats::var(hedge_return, na.rm = TRUE) <= 0) {
    return(NULL)
  }
  hedge_ratio <- as.numeric(stats::cov(target_return, hedge_return, use = "complete.obs") / stats::var(hedge_return, na.rm = TRUE))
  hedged_return <- target_return - hedge_ratio * hedge_return
  unhedged_variance <- stats::var(target_return, na.rm = TRUE)
  hedged_variance <- stats::var(hedged_return, na.rm = TRUE)
  var_reductions <- numeric(length(phase3_12_alphas))
  es_reductions <- numeric(length(phase3_12_alphas))
  for (i in seq_along(phase3_12_alphas)) {
    alpha <- phase3_12_alphas[i]
    unhedged <- phase3_12_var_es_loss(target_return, alpha)
    hedged <- phase3_12_var_es_loss(hedged_return, alpha)
    var_reductions[i] <- phase3_12_safe_reduction(unhedged[["var_loss"]], hedged[["var_loss"]])
    es_reductions[i] <- phase3_12_safe_reduction(unhedged[["es_loss"]], hedged[["es_loss"]])
  }
  unhedged_dd <- phase3_12_max_drawdown_loss(target_return)
  hedged_dd <- phase3_12_max_drawdown_loss(hedged_return)
  c(
    observations = length(target_return),
    hedge_ratio = hedge_ratio,
    unhedged_variance = unhedged_variance,
    hedged_variance = hedged_variance,
    hedging_effectiveness = 1 - hedged_variance / unhedged_variance,
    mean_var_reduction = mean(var_reductions, na.rm = TRUE),
    mean_es_reduction = mean(es_reductions, na.rm = TRUE),
    var_reduction_1pct = var_reductions[1],
    es_reduction_1pct = es_reductions[1],
    var_reduction_5pct = var_reductions[2],
    es_reduction_5pct = es_reductions[2],
    drawdown_reduction = phase3_12_safe_reduction(unhedged_dd, hedged_dd),
    mean_unhedged_return = mean(target_return, na.rm = TRUE),
    mean_hedged_return = mean(hedged_return, na.rm = TRUE)
  )
}

phase3_12_compute_frequency <- function(panel, frequency) {
  rows <- list()
  for (sector in phase3_12_sector_cols) {
    for (hedge in phase3_12_safe_haven_hedges) {
      sub <- panel[, c("date", sector, hedge), drop = FALSE]
      sub <- sub[stats::complete.cases(sub), , drop = FALSE]
      metrics <- phase3_12_static_hedge_metrics(sub[[sector]], sub[[hedge]])
      if (is.null(metrics)) next
      rows[[length(rows) + 1]] <- data.frame(
        frequency = frequency,
        sector = sector,
        hedge = hedge,
        model = "static_min_variance",
        first_date = min(sub$date, na.rm = TRUE),
        last_date = max(sub$date, na.rm = TRUE),
        as.list(metrics),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) data.frame() else do.call(rbind, rows)
}

phase3_12_sector_rank_scores <- function(x) {
  daily <- x[x$frequency == "daily", , drop = FALSE]
  if (nrow(daily) == 0) return(data.frame())
  rows <- list()
  for (sector in unique(daily$sector)) {
    sub <- daily[daily$sector == sector, , drop = FALSE]
    for (metric in phase3_12_rank_metrics) {
      values <- phase3_12_numeric(sub[[metric]])
      ranks <- rank(-values, ties.method = "average", na.last = "keep")
      scores <- if (sum(is.finite(values)) <= 1) {
        rep(1, length(values))
      } else {
        (sum(is.finite(values)) - ranks) / (sum(is.finite(values)) - 1)
      }
      sub[[paste0(metric, "_rank")]] <- ranks
      sub[[paste0(metric, "_score")]] <- scores
    }
    score_cols <- paste0(phase3_12_rank_metrics, "_score")
    sub$sector_tras_score <- rowMeans(sub[, score_cols, drop = FALSE], na.rm = TRUE)
    sub$sector_tras_rank <- rank(-sub$sector_tras_score, ties.method = "average")
    rows[[length(rows) + 1]] <- sub
  }
  out <- do.call(rbind, rows)
  out <- out[order(out$sector, out$sector_tras_rank, out$hedge), , drop = FALSE]
  rownames(out) <- NULL
  out
}

phase3_12_plot_heatmap <- function(heatmap_table, config) {
  output_dir <- file.path(config$outputs$figures_dir, "sector_extension")
  ensure_dir(output_dir)
  sectors <- unique(heatmap_table$sector)
  hedges <- phase3_12_safe_haven_hedges
  mat <- matrix(NA_real_, nrow = length(sectors), ncol = length(hedges), dimnames = list(sectors, hedges))
  for (i in seq_len(nrow(heatmap_table))) {
    mat[heatmap_table$sector[i], heatmap_table$hedge[i]] <- heatmap_table$sector_tras_score[i]
  }
  draw <- function() {
    heat_cols <- scientific_heat_colors(12)
    graphics::layout(matrix(c(1, 2), nrow = 1), widths = c(7.8, 1.6))
    graphics::par(mar = c(7, 6.5, 3, 0.8), family = "sans")
    graphics::image(
      x = seq_len(ncol(mat)),
      y = seq_len(nrow(mat)),
      z = t(mat[nrow(mat):1, , drop = FALSE]),
      col = heat_cols,
      axes = FALSE,
      xlab = "",
      ylab = "",
      main = "Sector TRAS heatmap"
    )
    graphics::mtext("Color scale shows sector TRAS score; higher is better", side = 3, line = 0.2, cex = 0.8)
    graphics::axis(1, at = seq_len(ncol(mat)), labels = colnames(mat), las = 2, cex.axis = 0.85)
    graphics::axis(2, at = seq_len(nrow(mat)), labels = rev(rownames(mat)), las = 2, cex.axis = 0.85)
    graphics::box()
    scientific_draw_colorbar_panel(
      zlim = c(0, 1),
      colors = heat_cols,
      label = "Sector TRAS score"
    )
    graphics::layout(1)
  }
  png_path <- file.path(output_dir, "sector_tras_heatmap.png")
  grDevices::png(png_path, width = 1300, height = 950, res = 150)
  draw()
  grDevices::dev.off()
  svg_path <- file.path(output_dir, "sector_tras_heatmap.svg")
  grDevices::svg(svg_path, width = 8.7, height = 6.3)
  draw()
  grDevices::dev.off()
  c(png_path, svg_path)
}

phase3_12_plot_best_hedge <- function(heatmap_table, config) {
  output_dir <- file.path(config$outputs$figures_dir, "sector_extension")
  ensure_dir(output_dir)
  best <- heatmap_table[order(heatmap_table$sector, heatmap_table$sector_tras_rank), , drop = FALSE]
  best <- best[!duplicated(best$sector), , drop = FALSE]
  best <- best[order(best$sector_tras_score), , drop = FALSE]
  y <- best$sector_tras_score
  names(y) <- paste(best$sector, best$hedge, sep = " / ")
  draw <- function() {
    graphics::par(mar = c(4.5, 9, 3, 2.8), family = "sans")
    graphics::barplot(y, horiz = TRUE, col = scientific_asset_colors(best$hedge), border = "white", las = 1, xlim = c(0, 1), xlab = "Best sector TRAS score", main = "Best safe-haven candidate by sector")
    graphics::grid(nx = NULL, ny = NA, col = "gray85", lty = "solid")
    scientific_asset_legend(best$hedge, x = "bottomright", title = "Best hedge")
  }
  png_path <- file.path(output_dir, "sector_best_hedge_by_sector.png")
  grDevices::png(png_path, width = 1300, height = 950, res = 150)
  draw()
  grDevices::dev.off()
  svg_path <- file.path(output_dir, "sector_best_hedge_by_sector.svg")
  grDevices::svg(svg_path, width = 8.7, height = 6.3)
  draw()
  grDevices::dev.off()
  c(png_path, svg_path)
}

phase3_12_write_report <- function(outputs, daily_panel, weekly_panel, config) {
  report_path <- project_path("data/metadata/phase3_12_sector_application_report.md")
  heatmap_table <- outputs$heatmap_table
  best <- heatmap_table[order(heatmap_table$sector, heatmap_table$sector_tras_rank), , drop = FALSE]
  best <- best[!duplicated(best$sector), , drop = FALSE]
  hedge_counts <- as.data.frame(table(best$hedge), stringsAsFactors = FALSE)
  names(hedge_counts) <- c("hedge", "best_sector_count")
  top_hedge_line <- if (nrow(hedge_counts) == 0) {
    "No best-hedge summary was generated."
  } else {
    hedge_counts <- hedge_counts[order(-hedge_counts$best_sector_count, hedge_counts$hedge), , drop = FALSE]
    sprintf("Most frequent best sector hedge: `%s` across `%s` sectors.", hedge_counts$hedge[1], hedge_counts$best_sector_count[1])
  }
  lines <- c(
    "# Phase 3.12 Sector Application Report",
    "",
    "## Purpose",
    "",
    "Phase 3.12 applies the safe-haven ranking framework to 10 HOSE/VNAllshare sector indices. It is an application layer, not a second full DCC-GARCH sector paper.",
    "",
    "## Scope",
    "",
    "- Main safe-haven candidates: `r_sjc_gold`, `r_xau_vnd`, `r_bond_tri`.",
    "- Brent is excluded from sector safe-haven ranking and remains an oil-price shock/risk channel.",
    "- Crypto is excluded from sector ranking.",
    "- Hedge method: static minimum-variance hedge ratio for a compact sector-level application.",
    "",
    "## Input Sample",
    "",
    sprintf("- Daily observations: `%s` from `%s` to `%s`.", nrow(daily_panel), min(daily_panel$date), max(daily_panel$date)),
    sprintf("- Weekly observations: `%s` from `%s` to `%s`.", nrow(weekly_panel), min(weekly_panel$date), max(weekly_panel$date)),
    "",
    "## Outputs",
    "",
    sprintf("- Sector hedging-effectiveness rows: `%s`", nrow(outputs$hedging)),
    sprintf("- Sector tail-risk rows: `%s`", nrow(outputs$tail)),
    sprintf("- Sector TRAS heatmap rows: `%s`", nrow(outputs$heatmap_table)),
    sprintf("- Figure files: `%s`", length(outputs$figures)),
    "",
    "## Main Diagnostic Note",
    "",
    top_hedge_line,
    "",
    "## Scientific Cautions",
    "",
    "- Sector results apply to HOSE/VNAllshare sector indices, not the entire Vietnamese economy.",
    "- Static hedge ratios are used for application-level comparison; they do not replace the core DCC/aDCC results.",
    "- Sector TRAS is rank-based within each sector and should be read with component metrics."
  )
  con <- file(report_path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw("\ufeff"), con)
  writeLines(enc2utf8(lines), con, useBytes = TRUE)
  invisible(report_path)
}

run_phase3_12_sector_application <- function(
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
  ensure_output_dirs(config)
  ensure_dir(file.path(config$outputs$tables_dir, "sector_extension"))
  ensure_dir(file.path(config$outputs$figures_dir, "sector_extension"))
  append_log("Phase 3.12 sector application started.", config)

  daily_panel <- phase3_12_prepare_panel("data/02_model_input/sector_extension/final_sector_daily_returns.csv")
  weekly_panel <- phase3_12_prepare_panel("data/02_model_input/sector_extension/final_sector_weekly_returns.csv")
  daily_metrics <- phase3_12_compute_frequency(daily_panel, "daily")
  weekly_metrics <- phase3_12_compute_frequency(weekly_panel, "weekly")
  all_metrics <- rbind(daily_metrics, weekly_metrics)
  heatmap_table <- phase3_12_sector_rank_scores(all_metrics)

  hedging <- all_metrics[, c(
    "frequency", "sector", "hedge", "model", "first_date", "last_date",
    "observations", "hedge_ratio", "unhedged_variance", "hedged_variance",
    "hedging_effectiveness", "mean_unhedged_return", "mean_hedged_return"
  ), drop = FALSE]
  tail <- all_metrics[, c(
    "frequency", "sector", "hedge", "model", "observations",
    "mean_var_reduction", "mean_es_reduction", "var_reduction_1pct",
    "es_reduction_1pct", "var_reduction_5pct", "es_reduction_5pct",
    "drawdown_reduction"
  ), drop = FALSE]

  write_csv_output(hedging, file.path(config$outputs$tables_dir, "sector_extension", "sector_hedging_effectiveness.csv"), overwrite = config$outputs$overwrite_existing)
  write_csv_output(tail, file.path(config$outputs$tables_dir, "sector_extension", "sector_tail_risk_reduction.csv"), overwrite = config$outputs$overwrite_existing)
  write_csv_output(heatmap_table, file.path(config$outputs$tables_dir, "sector_extension", "sector_tras_heatmap_table.csv"), overwrite = config$outputs$overwrite_existing)

  figures <- c(
    phase3_12_plot_heatmap(heatmap_table, config),
    phase3_12_plot_best_hedge(heatmap_table, config)
  )
  report <- phase3_12_write_report(
    list(hedging = hedging, tail = tail, heatmap_table = heatmap_table, figures = figures),
    daily_panel,
    weekly_panel,
    config
  )
  append_log("Phase 3.12 sector application finished.", config)
  list(
    daily_panel = daily_panel,
    weekly_panel = weekly_panel,
    hedging = hedging,
    tail = tail,
    heatmap_table = heatmap_table,
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

if (!interactive() && is_direct_rscript_call("src/models/run_phase3_12_sector_application.R")) {
  result <- run_phase3_12_sector_application()
  cat("Phase 3.12 sector application completed.\n")
  cat(sprintf("Daily rows: %s\n", nrow(result$daily_panel)))
  cat(sprintf("Weekly rows: %s\n", nrow(result$weekly_panel)))
  cat(sprintf("Sector hedging rows: %s\n", nrow(result$hedging)))
  cat(sprintf("Sector tail rows: %s\n", nrow(result$tail)))
  cat(sprintf("Sector TRAS rows: %s\n", nrow(result$heatmap_table)))
  cat(sprintf("Report: %s\n", result$report))
}
