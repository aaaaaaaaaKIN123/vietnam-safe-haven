source("src/utils/config_loader.R")
source("src/utils/io_helpers.R")
source("src/utils/date_asset_helpers.R")
source("src/models/validate_model_control.R")
source("src/models/R/model_helpers.R")
source("src/visualization/scientific_theme.R")

phase3_10_alphas <- c(0.01, 0.05, 0.10)
phase3_10_core_hedges <- c("r_sjc_gold", "r_xau_vnd", "r_bond_tri")

phase3_10_read_csv <- function(path, required = TRUE) {
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

phase3_10_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

phase3_10_prepare_hedge_ratios <- function(path = "results/tables/hedge_ratios/dynamic_hedge_ratios.csv") {
  x <- phase3_10_read_csv(path)
  required_cols <- c("date", "target", "hedge", "model", "target_return", "hedge_return", "hedged_return")
  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0) {
    stop(sprintf("Dynamic hedge-ratio file is missing columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  x$date <- as.Date(x$date)
  for (col in setdiff(names(x), c("date", "target", "hedge", "model", "first_date", "last_date"))) {
    x[[col]] <- phase3_10_numeric(x[[col]])
  }
  x <- x[stats::complete.cases(x[, required_cols, drop = FALSE]), , drop = FALSE]
  x[order(x$target, x$hedge, x$model, x$date), , drop = FALSE]
}

phase3_10_prepare_returns <- function(config) {
  x <- phase3_10_read_csv(config$data$returns_input)
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
    x[[col]] <- phase3_10_numeric(x[[col]])
  }
  x
}

phase3_10_weekly_from_hedge_ratios <- function(daily) {
  if (nrow(daily) == 0) return(daily)
  daily$week_id <- format(daily$date, "%Y-%U")
  groups <- unique(daily[, c("target", "hedge", "model", "week_id"), drop = FALSE])
  rows <- vector("list", nrow(groups))
  for (i in seq_len(nrow(groups))) {
    sub <- daily[
      daily$target == groups$target[i] &
        daily$hedge == groups$hedge[i] &
        daily$model == groups$model[i] &
        daily$week_id == groups$week_id[i],
      ,
      drop = FALSE
    ]
    rows[[i]] <- data.frame(
      date = max(sub$date, na.rm = TRUE),
      target = groups$target[i],
      hedge = groups$hedge[i],
      model = groups$model[i],
      target_return = sum(sub$target_return, na.rm = TRUE),
      hedge_return = sum(sub$hedge_return, na.rm = TRUE),
      hedged_return = sum(sub$hedged_return, na.rm = TRUE),
      observations_in_week = nrow(sub),
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  out[order(out$target, out$hedge, out$model, out$date), , drop = FALSE]
}

phase3_10_var_es_loss <- function(x, alpha) {
  x <- x[is.finite(x)]
  if (length(x) < 30) {
    return(c(var_return = NA_real_, es_return = NA_real_, var_loss = NA_real_, es_loss = NA_real_))
  }
  q <- as.numeric(stats::quantile(x, probs = alpha, na.rm = TRUE, names = FALSE, type = 7))
  tail_x <- x[x <= q]
  es <- if (length(tail_x) == 0) NA_real_ else mean(tail_x, na.rm = TRUE)
  c(
    var_return = q,
    es_return = es,
    var_loss = -q,
    es_loss = -es
  )
}

phase3_10_max_drawdown_loss <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 30) return(NA_real_)
  wealth <- exp(cumsum(x / 100))
  running_peak <- cummax(wealth)
  drawdown <- wealth / running_peak - 1
  -min(drawdown, na.rm = TRUE)
}

phase3_10_safe_reduction <- function(unhedged_loss, hedged_loss) {
  if (!is.finite(unhedged_loss) || unhedged_loss <= 0 || !is.finite(hedged_loss)) {
    return(NA_real_)
  }
  1 - hedged_loss / unhedged_loss
}

phase3_10_var_es_table <- function(data, frequency) {
  groups <- unique(data[, c("target", "hedge", "model"), drop = FALSE])
  rows <- list()
  for (i in seq_len(nrow(groups))) {
    sub <- data[
      data$target == groups$target[i] &
        data$hedge == groups$hedge[i] &
        data$model == groups$model[i],
      ,
      drop = FALSE
    ]
    sub <- sub[stats::complete.cases(sub[, c("target_return", "hedged_return"), drop = FALSE]), , drop = FALSE]
    if (nrow(sub) < 30) next
    for (alpha in phase3_10_alphas) {
      unhedged <- phase3_10_var_es_loss(sub$target_return, alpha)
      hedged <- phase3_10_var_es_loss(sub$hedged_return, alpha)
      rows[[length(rows) + 1]] <- data.frame(
        frequency = frequency,
        target = groups$target[i],
        hedge = groups$hedge[i],
        model = groups$model[i],
        alpha = alpha,
        observations = nrow(sub),
        first_date = min(sub$date, na.rm = TRUE),
        last_date = max(sub$date, na.rm = TRUE),
        unhedged_var_return = unhedged[["var_return"]],
        hedged_var_return = hedged[["var_return"]],
        unhedged_var_loss = unhedged[["var_loss"]],
        hedged_var_loss = hedged[["var_loss"]],
        var_reduction = phase3_10_safe_reduction(unhedged[["var_loss"]], hedged[["var_loss"]]),
        unhedged_es_return = unhedged[["es_return"]],
        hedged_es_return = hedged[["es_return"]],
        unhedged_es_loss = unhedged[["es_loss"]],
        hedged_es_loss = hedged[["es_loss"]],
        es_reduction = phase3_10_safe_reduction(unhedged[["es_loss"]], hedged[["es_loss"]]),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) data.frame() else do.call(rbind, rows)
}

phase3_10_drawdown_table <- function(data, frequency) {
  groups <- unique(data[, c("target", "hedge", "model"), drop = FALSE])
  rows <- list()
  for (i in seq_len(nrow(groups))) {
    sub <- data[
      data$target == groups$target[i] &
        data$hedge == groups$hedge[i] &
        data$model == groups$model[i],
      ,
      drop = FALSE
    ]
    sub <- sub[stats::complete.cases(sub[, c("target_return", "hedged_return"), drop = FALSE]), , drop = FALSE]
    if (nrow(sub) < 30) next
    unhedged <- phase3_10_max_drawdown_loss(sub$target_return)
    hedged <- phase3_10_max_drawdown_loss(sub$hedged_return)
    rows[[length(rows) + 1]] <- data.frame(
      frequency = frequency,
      target = groups$target[i],
      hedge = groups$hedge[i],
      model = groups$model[i],
      observations = nrow(sub),
      first_date = min(sub$date, na.rm = TRUE),
      last_date = max(sub$date, na.rm = TRUE),
      unhedged_max_drawdown_loss = unhedged,
      hedged_max_drawdown_loss = hedged,
      drawdown_reduction = phase3_10_safe_reduction(unhedged, hedged),
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) data.frame() else do.call(rbind, rows)
}

phase3_10_delta_covar_style <- function(hedge_ratios) {
  if (!requireNamespace("quantreg", quietly = TRUE)) {
    stop("Package 'quantreg' is required for Phase 3.10 portfolio Delta-CoVaR-style quantile regression.", call. = FALSE)
  }
  rows <- list()
  groups <- unique(hedge_ratios[, c("target", "hedge", "model"), drop = FALSE])
  for (g in seq_len(nrow(groups))) {
    sub <- hedge_ratios[
      hedge_ratios$target == groups$target[g] &
        hedge_ratios$hedge == groups$hedge[g] &
        hedge_ratios$model == groups$model[g],
      ,
      drop = FALSE
    ]
    sub <- sub[stats::complete.cases(sub[, c("target_return", "hedged_return"), drop = FALSE]), , drop = FALSE]
    if (nrow(sub) < 120) next
    for (alpha in phase3_10_alphas) {
      target_var <- as.numeric(stats::quantile(sub$target_return, probs = alpha, na.rm = TRUE, names = FALSE))
      target_median <- as.numeric(stats::quantile(sub$target_return, probs = 0.50, na.rm = TRUE, names = FALSE))
      unhedged_delta <- target_var - target_median
      fit <- tryCatch(
        quantreg::rq(hedged_return ~ target_return, tau = alpha, data = sub, method = "fn"),
        error = function(e) e
      )
      if (inherits(fit, "error")) {
        rows[[length(rows) + 1]] <- data.frame(
          target = groups$target[g],
          hedge = groups$hedge[g],
          model = groups$model[g],
          alpha = alpha,
          observations = nrow(sub),
          first_date = min(sub$date),
          last_date = max(sub$date),
          intercept = NA_real_,
          target_beta = NA_real_,
          target_var_return = target_var,
          target_median_return = target_median,
          unhedged_delta_covar_style = unhedged_delta,
          hedged_covar_at_target_var = NA_real_,
          hedged_covar_at_target_median = NA_real_,
          hedged_delta_covar_style = NA_real_,
          delta_covar_style = NA_real_,
          delta_covar_style_improvement = NA_real_,
          portfolio_delta_covar_improvement = NA_real_,
          estimation_status = paste("failed:", fit$message),
          stringsAsFactors = FALSE
        )
        next
      }
      coefs <- stats::coef(fit)
      covar_distress <- as.numeric(stats::predict(fit, newdata = data.frame(target_return = target_var)))
      covar_median <- as.numeric(stats::predict(fit, newdata = data.frame(target_return = target_median)))
      hedged_delta <- covar_distress - covar_median
      improvement <- hedged_delta - unhedged_delta
      rows[[length(rows) + 1]] <- data.frame(
        target = groups$target[g],
        hedge = groups$hedge[g],
        model = groups$model[g],
        alpha = alpha,
        observations = nrow(sub),
        first_date = min(sub$date),
        last_date = max(sub$date),
        intercept = unname(coefs[["(Intercept)"]]),
        target_beta = unname(coefs[["target_return"]]),
        target_var_return = target_var,
        target_median_return = target_median,
        unhedged_delta_covar_style = unhedged_delta,
        hedged_covar_at_target_var = covar_distress,
        hedged_covar_at_target_median = covar_median,
        hedged_delta_covar_style = hedged_delta,
        delta_covar_style = improvement,
        delta_covar_style_improvement = improvement,
        portfolio_delta_covar_improvement = improvement,
        estimation_status = "ok",
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) data.frame() else do.call(rbind, rows)
}

phase3_10_connectedness_absorption <- function(path = "results/tables/connectedness/quantile_connectedness_net.csv") {
  x <- phase3_10_read_csv(path, required = FALSE)
  if (nrow(x) == 0) return(data.frame())
  if (!all(c("tau", "asset", "net") %in% names(x))) return(data.frame())
  x$tau <- phase3_10_numeric(x$tau)
  x$net <- phase3_10_numeric(x$net)
  x <- x[abs(x$tau - 0.05) < 1e-8, c("asset", "net"), drop = FALSE]
  if (nrow(x) == 0) return(data.frame())
  names(x) <- c("hedge", "lower_tail_net_connectedness")
  x$lower_tail_absorption_score <- -x$lower_tail_net_connectedness
  x
}

phase3_10_summary_table <- function(var_es, drawdown, delta_covar, connectedness) {
  if (nrow(var_es) == 0) return(data.frame())
  daily <- var_es[var_es$frequency == "daily", , drop = FALSE]
  groups <- unique(daily[, c("target", "hedge", "model"), drop = FALSE])
  rows <- list()
  for (i in seq_len(nrow(groups))) {
    sub <- daily[
      daily$target == groups$target[i] &
        daily$hedge == groups$hedge[i] &
        daily$model == groups$model[i],
      ,
      drop = FALSE
    ]
    dd <- drawdown[
      drawdown$frequency == "daily" &
        drawdown$target == groups$target[i] &
        drawdown$hedge == groups$hedge[i] &
        drawdown$model == groups$model[i],
      ,
      drop = FALSE
    ]
    dc <- delta_covar[
      delta_covar$target == groups$target[i] &
        delta_covar$hedge == groups$hedge[i] &
        delta_covar$model == groups$model[i],
      ,
      drop = FALSE
    ]
    cn <- connectedness[connectedness$hedge == groups$hedge[i], , drop = FALSE]
    rows[[length(rows) + 1]] <- data.frame(
      target = groups$target[i],
      hedge = groups$hedge[i],
      model = groups$model[i],
      observations = max(sub$observations, na.rm = TRUE),
      mean_var_reduction = mean(sub$var_reduction, na.rm = TRUE),
      mean_es_reduction = mean(sub$es_reduction, na.rm = TRUE),
      var_reduction_1pct = sub$var_reduction[sub$alpha == 0.01][1],
      es_reduction_1pct = sub$es_reduction[sub$alpha == 0.01][1],
      var_reduction_5pct = sub$var_reduction[sub$alpha == 0.05][1],
      es_reduction_5pct = sub$es_reduction[sub$alpha == 0.05][1],
      drawdown_reduction = if (nrow(dd) == 0) NA_real_ else dd$drawdown_reduction[1],
      mean_delta_covar_style_improvement = if (nrow(dc) == 0) NA_real_ else mean(dc$delta_covar_style_improvement, na.rm = TRUE),
      mean_portfolio_delta_covar_improvement = if (nrow(dc) == 0) NA_real_ else mean(dc$portfolio_delta_covar_improvement, na.rm = TRUE),
      lower_tail_net_connectedness = if (nrow(cn) == 0) NA_real_ else cn$lower_tail_net_connectedness[1],
      lower_tail_absorption_score = if (nrow(cn) == 0) NA_real_ else cn$lower_tail_absorption_score[1],
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  out <- out[order(-out$mean_es_reduction, -out$drawdown_reduction, out$hedge, out$model), , drop = FALSE]
  rownames(out) <- NULL
  out
}

phase3_10_plot_tail_risk <- function(summary, var_es, config) {
  output_dir <- file.path(config$outputs$figures_dir, "tail_risk")
  ensure_dir(output_dir)
  files <- character()
  core_hedges <- phase3_10_core_hedges
  df <- summary[summary$hedge %in% core_hedges, , drop = FALSE]
  if (nrow(df) > 0) {
    df <- df[order(df$hedge, df$model), , drop = FALSE]
    y <- df$mean_es_reduction
    names(y) <- paste(df$hedge, df$model, sep = "_")
    draw <- function() {
      y_min <- min(0, y, na.rm = TRUE)
      y_max <- max(0, y, na.rm = TRUE)
      ylim <- c(if (y_min < 0) y_min * 1.18 else 0, if (y_max > 0) y_max * 1.35 else 0)
      graphics::par(mar = c(8, 6.2, 4.5, 1.4), family = "sans")
      graphics::barplot(y, col = scientific_asset_colors(df$hedge), border = "white", las = 2, ylab = "", main = "", ylim = ylim)
      graphics::mtext("Tail-risk reduction by hedge and model", side = 3, line = 2.0, font = 2, cex = 1.2)
      graphics::mtext("Mean ES reduction", side = 2, line = 4.2)
      graphics::abline(h = 0, col = "black", lty = "dashed")
      graphics::grid(nx = NA, ny = NULL, col = "gray85", lty = "solid")
      scientific_asset_legend(df$hedge, x = "topright", inset = 0.02, title = "Hedge asset")
    }
    png_path <- file.path(output_dir, "figure_phase3_10_tail_risk_summary.png")
    grDevices::png(png_path, width = 1600, height = 950, res = 150)
    draw()
    grDevices::dev.off()
    svg_path <- file.path(output_dir, "figure_phase3_10_tail_risk_summary.svg")
    grDevices::svg(svg_path, width = 10.5, height = 6.3)
    draw()
    grDevices::dev.off()
    files <- c(files, png_path, svg_path)
  }

  es5 <- var_es[var_es$frequency == "daily" & abs(var_es$alpha - 0.05) < 1e-8 & var_es$hedge %in% core_hedges, , drop = FALSE]
  if (nrow(es5) > 0) {
    es5 <- es5[order(es5$hedge, es5$model), , drop = FALSE]
    y <- es5$es_reduction
    names(y) <- paste(es5$hedge, es5$model, sep = "_")
    draw <- function() {
      y_min <- min(0, y, na.rm = TRUE)
      y_max <- max(0, y, na.rm = TRUE)
      ylim <- c(if (y_min < 0) y_min * 1.18 else 0, if (y_max > 0) y_max * 1.35 else 0)
      graphics::par(mar = c(8, 6.2, 4.5, 1.4), family = "sans")
      graphics::barplot(y, col = scientific_asset_colors(es5$hedge), border = "white", las = 2, ylab = "", main = "", ylim = ylim)
      graphics::mtext("Daily 5% ES reduction", side = 3, line = 2.0, font = 2, cex = 1.2)
      graphics::mtext("ES reduction at 5% tail", side = 2, line = 4.2)
      graphics::abline(h = 0, col = "black", lty = "dashed")
      graphics::grid(nx = NA, ny = NULL, col = "gray85", lty = "solid")
      scientific_asset_legend(es5$hedge, x = "topright", inset = 0.02, title = "Hedge asset")
    }
    png_path <- file.path(output_dir, "figure_phase3_10_es5_reduction.png")
    grDevices::png(png_path, width = 1600, height = 950, res = 150)
    draw()
    grDevices::dev.off()
    svg_path <- file.path(output_dir, "figure_phase3_10_es5_reduction.svg")
    grDevices::svg(svg_path, width = 10.5, height = 6.3)
    draw()
    grDevices::dev.off()
    files <- c(files, png_path, svg_path)
  }
  files
}

phase3_10_write_report <- function(outputs, config) {
  report_path <- project_path("data/metadata/phase3_10_tail_risk_report.md")
  summary <- outputs$summary
  core_hedges <- phase3_10_core_hedges
  top <- summary[order(-summary$mean_es_reduction, -summary$drawdown_reduction), , drop = FALSE]
  top_overall_line <- if (nrow(top) == 0) {
    "No valid tail-risk summary was generated."
  } else {
    sprintf(
      "Best overall daily mean ES reduction: `%s` with `%s` under `%s` (mean ES reduction = %.6f; drawdown reduction = %.6f).",
      top$target[1], top$hedge[1], top$model[1], top$mean_es_reduction[1], top$drawdown_reduction[1]
    )
  }
  core_top <- summary[summary$hedge %in% core_hedges, , drop = FALSE]
  core_top <- core_top[order(-core_top$mean_es_reduction, -core_top$drawdown_reduction), , drop = FALSE]
  top_core_line <- if (nrow(core_top) == 0) {
    "No valid core-hedge tail-risk summary was generated."
  } else {
    sprintf(
      "Best core-hedge daily mean ES reduction: `%s` with `%s` under `%s` (mean ES reduction = %.6f; drawdown reduction = %.6f).",
      core_top$target[1], core_top$hedge[1], core_top$model[1], core_top$mean_es_reduction[1], core_top$drawdown_reduction[1]
    )
  }
  lines <- c(
    "# Phase 3.10 Tail Risk and Delta-CoVaR-Style Report",
    "",
    "## Purpose",
    "",
    "Phase 3.10 evaluates whether dynamic hedge portfolios reduce lower-tail risk, not only variance. Positive reduction values mean the hedged portfolio has lower risk than the unhedged VNIndex return on the same sample.",
    "",
    "## Inputs",
    "",
    "- `results/tables/hedge_ratios/dynamic_hedge_ratios.csv`",
    sprintf("- `%s`", config$data$returns_input),
    "- `results/tables/connectedness/quantile_connectedness_net.csv`",
    "",
    "## Outputs",
    "",
    sprintf("- VaR/ES rows: `%s`", nrow(outputs$var_es)),
    sprintf("- Drawdown rows: `%s`", nrow(outputs$drawdown)),
    sprintf("- Delta-CoVaR-style rows: `%s`", nrow(outputs$delta_covar)),
    sprintf("- Summary rows: `%s`", nrow(outputs$summary)),
    sprintf("- Figure files: `%s`", length(outputs$figures)),
    "",
    "## Main Diagnostic Note",
    "",
    top_overall_line,
    top_core_line,
    "",
    "## Scientific Cautions",
    "",
    "- VaR/ES reduction is computed on the same sample for each hedge-model pair.",
    "- Delta-CoVaR-style is a conditional tail-risk diagnostic, not a full systemic-risk identification design.",
    "- A hedge should not be called superior if it improves variance but worsens ES or drawdown.",
    "- Brent is excluded from hedge-asset tail-risk outputs and remains an oil-price shock/control variable.",
    "- Bond TRI remains a simulated investable bond index proxy, not an official index."
  )
  con <- file(report_path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw("\ufeff"), con)
  writeLines(enc2utf8(lines), con, useBytes = TRUE)
  invisible(report_path)
}

run_phase3_10_tail_risk <- function(
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
  ensure_dir(file.path(config$outputs$tables_dir, "tail_risk"))
  ensure_dir(file.path(config$outputs$figures_dir, "tail_risk"))
  append_log("Phase 3.10 tail risk started.", config)

  hedge_ratios <- phase3_10_prepare_hedge_ratios()
  hedge_ratios <- hedge_ratios[hedge_ratios$hedge %in% phase3_10_core_hedges, , drop = FALSE]
  if (nrow(hedge_ratios) == 0) {
    stop("No core hedge assets are available in dynamic hedge-ratio inputs.", call. = FALSE)
  }
  weekly_hedges <- phase3_10_weekly_from_hedge_ratios(hedge_ratios)
  hedges <- phase3_10_core_hedges[phase3_10_core_hedges %in% unique(hedge_ratios$hedge)]

  var_es <- rbind(
    phase3_10_var_es_table(hedge_ratios, "daily"),
    phase3_10_var_es_table(weekly_hedges, "weekly")
  )
  drawdown <- rbind(
    phase3_10_drawdown_table(hedge_ratios, "daily"),
    phase3_10_drawdown_table(weekly_hedges, "weekly")
  )
  delta_covar <- phase3_10_delta_covar_style(hedge_ratios)
  connectedness <- phase3_10_connectedness_absorption()
  summary <- phase3_10_summary_table(var_es, drawdown, delta_covar, connectedness)

  write_csv_output(var_es, file.path(config$outputs$tables_dir, "tail_risk", "var_es_reduction.csv"), overwrite = config$outputs$overwrite_existing)
  write_csv_output(drawdown, file.path(config$outputs$tables_dir, "tail_risk", "drawdown_reduction.csv"), overwrite = config$outputs$overwrite_existing)
  write_csv_output(delta_covar, file.path(config$outputs$tables_dir, "tail_risk", "delta_covar_style.csv"), overwrite = config$outputs$overwrite_existing)
  write_csv_output(summary, file.path(config$outputs$tables_dir, "tail_risk", "tail_risk_summary.csv"), overwrite = config$outputs$overwrite_existing)

  figures <- phase3_10_plot_tail_risk(summary, var_es, config)
  report <- phase3_10_write_report(
    list(var_es = var_es, drawdown = drawdown, delta_covar = delta_covar, summary = summary, figures = figures),
    config
  )
  append_log("Phase 3.10 tail risk finished.", config)
  list(
    var_es = var_es,
    drawdown = drawdown,
    delta_covar = delta_covar,
    connectedness = connectedness,
    summary = summary,
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

if (!interactive() && is_direct_rscript_call("src/models/run_phase3_10_tail_risk.R")) {
  result <- run_phase3_10_tail_risk()
  cat("Phase 3.10 tail risk completed.\n")
  cat(sprintf("VaR/ES rows: %s\n", nrow(result$var_es)))
  cat(sprintf("Drawdown rows: %s\n", nrow(result$drawdown)))
  cat(sprintf("Delta-CoVaR-style rows: %s\n", nrow(result$delta_covar)))
  cat(sprintf("Summary rows: %s\n", nrow(result$summary)))
  cat(sprintf("Report: %s\n", result$report))
}
