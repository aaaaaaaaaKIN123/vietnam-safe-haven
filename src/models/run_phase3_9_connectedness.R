source("src/utils/config_loader.R")
source("src/utils/io_helpers.R")
source("src/utils/date_asset_helpers.R")
source("src/models/validate_model_control.R")
source("src/models/R/model_helpers.R")
source("src/visualization/scientific_theme.R")

phase3_9_core_assets <- c("r_vnindex", "r_sjc_gold", "r_xau_vnd", "r_bond_tri")
phase3_9_quantiles <- c(0.05, 0.10, 0.50, 0.90, 0.95)
phase3_9_rolling_quantiles <- c(0.05, 0.50, 0.95)

phase3_9_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

phase3_9_read_returns <- function(config) {
  returns <- load_model_returns(config)
  missing_assets <- setdiff(phase3_9_core_assets, names(returns))
  if (length(missing_assets) > 0) {
    stop(sprintf(
      "Phase 3.9 missing required core return columns: %s",
      paste(missing_assets, collapse = ", ")
    ), call. = FALSE)
  }
  returns$date <- as.Date(returns$date)
  for (col in setdiff(names(returns), "date")) {
    returns[[col]] <- phase3_9_numeric(returns[[col]])
  }
  panel <- returns[, c("date", phase3_9_core_assets), drop = FALSE]
  panel <- panel[stats::complete.cases(panel), , drop = FALSE]
  if (nrow(panel) < 300) {
    stop("Phase 3.9 requires at least 300 complete daily observations.", call. = FALSE)
  }
  panel
}

phase3_9_weekly_returns <- function(daily_panel) {
  daily_panel$week_id <- format(daily_panel$date, "%Y-%U")
  week_ids <- unique(daily_panel$week_id)
  rows <- lapply(week_ids, function(week_id) {
    sub <- daily_panel[daily_panel$week_id == week_id, , drop = FALSE]
    out <- data.frame(date = max(sub$date), stringsAsFactors = FALSE)
    for (asset in phase3_9_core_assets) {
      out[[asset]] <- sum(sub[[asset]], na.rm = TRUE)
    }
    out
  })
  out <- do.call(rbind, rows)
  out <- out[stats::complete.cases(out[, phase3_9_core_assets, drop = FALSE]), , drop = FALSE]
  out[order(out$date), , drop = FALSE]
}

phase3_9_as_zoo <- function(panel) {
  zoo::zoo(panel[, phase3_9_core_assets, drop = FALSE], panel$date)
}

phase3_9_connectedness <- function(
  z,
  model = "QVAR",
  connectedness = "Time",
  tau = 0.50,
  window.size = NULL,
  nlag = 1,
  nfore = 10
) {
  if (identical(model, "QVAR")) {
    ConnectednessApproach::ConnectednessApproach(
      z,
      nlag = nlag,
      nfore = nfore,
      window.size = window.size,
      model = "QVAR",
      connectedness = connectedness,
      VAR_config = list(QVAR = list(tau = tau, method = "fn"))
    )
  } else {
    ConnectednessApproach::ConnectednessApproach(
      z,
      nlag = nlag,
      nfore = nfore,
      window.size = window.size,
      model = model,
      connectedness = connectedness,
      Connectedness_config = list(
        FrequencyConnectedness = list(
          partition = c(pi, pi / 5, 0),
          generalized = TRUE,
          scenario = "ABS"
        )
      )
    )
  }
}

phase3_9_tci_long <- function(tci, tau = NA_real_, sample_type, model, connectedness) {
  if (is.null(tci)) return(data.frame())
  m <- as.matrix(tci)
  dates <- rownames(m)
  if (is.null(dates)) dates <- rep(NA_character_, nrow(m))
  horizons <- colnames(m)
  if (is.null(horizons)) horizons <- paste0("series_", seq_len(ncol(m)))
  rows <- list()
  for (j in seq_len(ncol(m))) {
    rows[[length(rows) + 1]] <- data.frame(
      date = dates,
      sample_type = sample_type,
      model = model,
      connectedness = connectedness,
      tau = tau,
      horizon = horizons[j],
      tci = as.numeric(m[, j]),
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  out$date <- suppressWarnings(as.Date(out$date))
  out
}

phase3_9_net_long <- function(net, tau = NA_real_, sample_type, model, connectedness) {
  if (is.null(net)) return(data.frame())
  dims <- dim(net)
  rows <- list()
  if (length(dims) == 2) {
    m <- as.matrix(net)
    dates <- rownames(m)
    if (is.null(dates)) dates <- rep(NA_character_, nrow(m))
    assets <- colnames(m)
    for (j in seq_len(ncol(m))) {
      rows[[length(rows) + 1]] <- data.frame(
        date = dates,
        sample_type = sample_type,
        model = model,
        connectedness = connectedness,
        tau = tau,
        horizon = "Total",
        asset = assets[j],
        net = as.numeric(m[, j]),
        stringsAsFactors = FALSE
      )
    }
  } else if (length(dims) == 3) {
    dn <- dimnames(net)
    dates <- dn[[1]]
    if (is.null(dates)) dates <- rep(NA_character_, dims[1])
    assets <- dn[[2]]
    if (is.null(assets)) assets <- paste0("asset_", seq_len(dims[2]))
    horizons <- dn[[3]]
    if (is.null(horizons)) horizons <- paste0("horizon_", seq_len(dims[3]))
    for (h in seq_len(dims[3])) {
      m <- as.matrix(net[, , h])
      for (j in seq_len(ncol(m))) {
        rows[[length(rows) + 1]] <- data.frame(
          date = dates,
          sample_type = sample_type,
          model = model,
          connectedness = connectedness,
          tau = tau,
          horizon = horizons[h],
          asset = assets[j],
          net = as.numeric(m[, j]),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(rows) == 0) return(data.frame())
  out <- do.call(rbind, rows)
  out$date <- suppressWarnings(as.Date(out$date))
  out
}

phase3_9_table_long <- function(table_obj, tau = NA_real_, sample_type, model, connectedness) {
  if (is.null(table_obj)) return(data.frame())
  dims <- dim(table_obj)
  rows <- list()
  add_matrix <- function(m, horizon) {
    row_names <- rownames(m)
    col_names <- colnames(m)
    local_rows <- list()
    for (i in seq_len(nrow(m))) {
      for (j in seq_len(ncol(m))) {
        local_rows[[length(local_rows) + 1]] <- data.frame(
          sample_type = sample_type,
          model = model,
          connectedness = connectedness,
          tau = tau,
          horizon = horizon,
          row = row_names[i],
          column = col_names[j],
          value = suppressWarnings(as.numeric(trimws(as.character(m[i, j])))),
          value_raw = trimws(as.character(m[i, j])),
          stringsAsFactors = FALSE
        )
      }
    }
    local_rows
  }
  if (length(dims) == 2) {
    rows <- add_matrix(as.matrix(table_obj), "Total")
  } else if (length(dims) == 3) {
    horizons <- dimnames(table_obj)[[3]]
    if (is.null(horizons)) horizons <- paste0("horizon_", seq_len(dims[3]))
    for (h in seq_len(dims[3])) {
      rows <- c(rows, add_matrix(as.matrix(table_obj[, , h]), horizons[h]))
    }
  }
  if (length(rows) == 0) data.frame() else do.call(rbind, rows)
}

phase3_9_write_quantile_outputs <- function(daily_z, weekly_z, config) {
  tci_rows <- list()
  net_rows <- list()
  table_rows <- list()
  rolling_tci_rows <- list()
  rolling_net_rows <- list()
  warnings <- character()

  for (tau in phase3_9_quantiles) {
    result <- fit_status(phase3_9_connectedness(
      daily_z,
      model = "QVAR",
      connectedness = "Time",
      tau = tau,
      window.size = NULL
    ))
    if (!result$ok) {
      warnings <- c(warnings, sprintf("daily full-sample QVAR tau=%s failed: %s", tau, result$error))
      next
    }
    fit <- result$value
    tci_rows[[length(tci_rows) + 1]] <- phase3_9_tci_long(
      fit$TCI, tau, "daily_full_sample", "QVAR", "Time"
    )
    net_rows[[length(net_rows) + 1]] <- phase3_9_net_long(
      fit$NET, tau, "daily_full_sample", "QVAR", "Time"
    )
    table_rows[[length(table_rows) + 1]] <- phase3_9_table_long(
      fit$TABLE, tau, "daily_full_sample", "QVAR", "Time"
    )
  }

  rolling_window <- min(104L, max(52L, floor(nrow(weekly_z) / 3)))
  for (tau in phase3_9_rolling_quantiles) {
    result <- fit_status(phase3_9_connectedness(
      weekly_z,
      model = "QVAR",
      connectedness = "Time",
      tau = tau,
      window.size = rolling_window
    ))
    if (!result$ok) {
      warnings <- c(warnings, sprintf("weekly rolling QVAR tau=%s failed: %s", tau, result$error))
      next
    }
    fit <- result$value
    rolling_tci_rows[[length(rolling_tci_rows) + 1]] <- phase3_9_tci_long(
      fit$TCI, tau, "weekly_rolling", "QVAR", "Time"
    )
    rolling_net_rows[[length(rolling_net_rows) + 1]] <- phase3_9_net_long(
      fit$NET, tau, "weekly_rolling", "QVAR", "Time"
    )
  }

  out <- list(
    tci = if (length(tci_rows) == 0) data.frame() else do.call(rbind, tci_rows),
    net = if (length(net_rows) == 0) data.frame() else do.call(rbind, net_rows),
    table = if (length(table_rows) == 0) data.frame() else do.call(rbind, table_rows),
    rolling_tci = if (length(rolling_tci_rows) == 0) data.frame() else do.call(rbind, rolling_tci_rows),
    rolling_net = if (length(rolling_net_rows) == 0) data.frame() else do.call(rbind, rolling_net_rows),
    warnings = warnings,
    rolling_window = rolling_window
  )

  write_csv_output(
    out$tci,
    file.path(config$outputs$tables_dir, "connectedness", "quantile_connectedness_tci.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  write_csv_output(
    out$net,
    file.path(config$outputs$tables_dir, "connectedness", "quantile_connectedness_net.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  write_csv_output(
    out$table,
    file.path(config$outputs$tables_dir, "connectedness", "quantile_connectedness_table.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  write_csv_output(
    out$rolling_tci,
    file.path(config$outputs$tables_dir, "connectedness", "quantile_connectedness_rolling_weekly_tci.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  write_csv_output(
    out$rolling_net,
    file.path(config$outputs$tables_dir, "connectedness", "quantile_connectedness_rolling_weekly_net.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  out
}

phase3_9_write_frequency_outputs <- function(daily_z, config) {
  warnings <- character()
  result <- fit_status(phase3_9_connectedness(
    daily_z,
    model = "VAR",
    connectedness = "Frequency",
    tau = NA_real_,
    window.size = NULL
  ))
  if (!result$ok) {
    warnings <- c(warnings, sprintf("daily full-sample frequency VAR failed: %s", result$error))
    out <- list(tci = data.frame(), net = data.frame(), table = data.frame(), warnings = warnings)
  } else {
    fit <- result$value
    out <- list(
      tci = phase3_9_tci_long(fit$TCI, NA_real_, "daily_full_sample", "VAR", "Frequency"),
      net = phase3_9_net_long(fit$NET, NA_real_, "daily_full_sample", "VAR", "Frequency"),
      table = phase3_9_table_long(fit$TABLE, NA_real_, "daily_full_sample", "VAR", "Frequency"),
      warnings = warnings
    )
  }

  write_csv_output(
    out$tci,
    file.path(config$outputs$tables_dir, "connectedness", "frequency_connectedness_tci.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  write_csv_output(
    out$net,
    file.path(config$outputs$tables_dir, "connectedness", "frequency_connectedness_net.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  write_csv_output(
    out$table,
    file.path(config$outputs$tables_dir, "connectedness", "frequency_connectedness_table.csv"),
    overwrite = config$outputs$overwrite_existing
  )
  out
}

phase3_9_plot_tci <- function(rolling_tci, config) {
  if (nrow(rolling_tci) == 0) return(character())
  output_dir <- file.path(config$outputs$figures_dir, "connectedness")
  ensure_dir(output_dir)
  df <- rolling_tci[rolling_tci$horizon == "TCI" | rolling_tci$horizon == "Total", , drop = FALSE]
  if (nrow(df) == 0) df <- rolling_tci
  taus <- sort(unique(df$tau))
  pal <- scientific_palette()
  tau_col <- c("0.05" = pal[["vermillion"]], "0.1" = pal[["orange"]], "0.10" = pal[["orange"]], "0.5" = pal[["blue"]], "0.50" = pal[["blue"]], "0.9" = pal[["bluish_green"]], "0.90" = pal[["bluish_green"]], "0.95" = pal[["reddish_purple"]])
  lty_map <- c("0.05" = "solid", "0.1" = "dashed", "0.10" = "dashed", "0.5" = "solid", "0.50" = "solid", "0.9" = "dotdash", "0.90" = "dotdash", "0.95" = "longdash")
  y_lim <- range(df$tci, na.rm = TRUE)
  if (!all(is.finite(y_lim)) || diff(y_lim) == 0) y_lim <- c(0, 100)

  draw <- function() {
    graphics::par(mar = c(4.5, 4.8, 3.0, 1.0), family = "sans")
    graphics::plot(
      range(df$date, na.rm = TRUE),
      y_lim,
      type = "n",
      xlab = "Date",
      ylab = "Total connectedness index",
      main = "Weekly rolling quantile connectedness",
      las = 1,
      bty = "l"
    )
    graphics::grid(col = "gray85", lty = "solid")
    for (tau in taus) {
      sub <- df[df$tau == tau, , drop = FALSE]
      graphics::lines(
        sub$date,
        sub$tci,
        col = ifelse(as.character(tau) %in% names(tau_col), tau_col[[as.character(tau)]], pal[["charcoal"]]),
        lty = ifelse(as.character(tau) %in% names(lty_map), lty_map[[as.character(tau)]], "dotdash"),
        lwd = ifelse(abs(tau - 0.50) < 1e-8, 2.0, 1.5)
      )
    }
    graphics::legend(
      "topright",
      legend = paste0("tau=", taus),
      col = ifelse(as.character(taus) %in% names(tau_col), tau_col[as.character(taus)], pal[["charcoal"]]),
      lty = ifelse(as.character(taus) %in% names(lty_map), lty_map[as.character(taus)], "dotdash"),
      lwd = ifelse(abs(taus - 0.50) < 1e-8, 2.0, 1.5),
      bty = "n",
      cex = 0.85
    )
  }

  png_path <- file.path(output_dir, "figure_phase3_9_rolling_quantile_tci.png")
  grDevices::png(png_path, width = 1500, height = 900, res = 150)
  draw()
  grDevices::dev.off()

  svg_path <- file.path(output_dir, "figure_phase3_9_rolling_quantile_tci.svg")
  grDevices::svg(svg_path, width = 10, height = 6)
  draw()
  grDevices::dev.off()
  c(png_path, svg_path)
}

phase3_9_plot_tail_net <- function(net, config) {
  if (nrow(net) == 0) return(character())
  output_dir <- file.path(config$outputs$figures_dir, "connectedness")
  ensure_dir(output_dir)
  df <- net[abs(net$tau - 0.05) < 1e-8 & net$sample_type == "daily_full_sample", , drop = FALSE]
  if (nrow(df) == 0) return(character())
  df <- df[order(df$net), , drop = FALSE]
  y <- df$net
  names(y) <- df$asset

  draw <- function() {
    graphics::par(mar = c(7.5, 4.8, 3.0, 1.0), family = "sans")
    graphics::barplot(
      y,
      col = scientific_asset_colors(names(y)),
      border = "black",
      las = 2,
      ylab = "Net connectedness",
      main = "Lower-tail net connectedness (tau=0.05)"
    )
    graphics::abline(h = 0, col = "black", lty = "dashed")
  }

  png_path <- file.path(output_dir, "figure_phase3_9_lower_tail_net.png")
  grDevices::png(png_path, width = 1300, height = 900, res = 150)
  draw()
  grDevices::dev.off()

  svg_path <- file.path(output_dir, "figure_phase3_9_lower_tail_net.svg")
  grDevices::svg(svg_path, width = 8.5, height = 6)
  draw()
  grDevices::dev.off()
  c(png_path, svg_path)
}

phase3_9_network_edges <- function(table, net, tau = 0.05, sample_type = "daily_full_sample", horizon = "Total", top_n = 10L) {
  if (nrow(table) == 0) return(data.frame())
  assets <- phase3_9_core_assets
  x <- table[
    table$sample_type == sample_type &
      table$horizon == horizon &
      abs(table$tau - tau) < 1e-8 &
      table$row %in% assets &
      table$column %in% assets &
      table$row != table$column,
    ,
    drop = FALSE
  ]
  if (nrow(x) == 0) return(data.frame())
  # In ConnectednessApproach TABLE, row i / column j is the shock contribution
  # from j to i; therefore the directed edge is column -> row.
  edges <- data.frame(
    sample_type = x$sample_type,
    model = x$model,
    connectedness = x$connectedness,
    tau = x$tau,
    horizon = x$horizon,
    from = x$column,
    to = x$row,
    spillover = phase3_9_numeric(x$value),
    stringsAsFactors = FALSE
  )
  net_sub <- net[
    nrow(net) > 0 &
      net$sample_type == sample_type &
      net$horizon == horizon &
      abs(net$tau - tau) < 1e-8 &
      net$asset %in% assets,
    c("asset", "net"),
    drop = FALSE
  ]
  names(net_sub) <- c("from", "from_net")
  edges <- merge(edges, net_sub, by = "from", all.x = TRUE)
  names(net_sub) <- c("to", "to_net")
  edges <- merge(edges, net_sub, by = "to", all.x = TRUE)
  edges <- edges[order(-edges$spillover, edges$from, edges$to), , drop = FALSE]
  edges$rank <- seq_len(nrow(edges))
  edges$plotted <- edges$rank <= min(top_n, nrow(edges))
  edges[, c("sample_type", "model", "connectedness", "tau", "horizon", "from", "to", "spillover", "rank", "plotted", "from_net", "to_net"), drop = FALSE]
}

phase3_9_draw_network_legend <- function() {
  pal <- scientific_palette()
  graphics::legend(
    "bottom",
    inset = c(0, -0.02),
    horiz = TRUE,
    legend = c("Blue border: net transmitter", "Orange border: net receiver", "Thicker arrow: stronger spillover"),
    pt.bg = c("white", "white", NA),
    pch = c(21, 21, NA),
    lty = c(NA, NA, "solid"),
    lwd = c(NA, NA, 2.6),
    col = c(pal[["blue"]], pal[["orange"]], pal[["grey"]]),
    bty = "n",
    cex = 0.78,
    xpd = NA
  )
}

phase3_9_plot_tail_network <- function(table, net, config) {
  edges <- phase3_9_network_edges(table, net, tau = 0.05, top_n = 10L)
  if (nrow(edges) == 0) return(list(files = character(), edges = data.frame()))
  output_dir <- file.path(config$outputs$figures_dir, "connectedness")
  ensure_dir(output_dir)
  edge_path <- file.path(config$outputs$tables_dir, "connectedness", "lower_tail_network_edges.csv")
  write_csv_output(edges, edge_path, overwrite = config$outputs$overwrite_existing)

  assets <- phase3_9_core_assets
  net_sub <- net[
    nrow(net) > 0 &
      net$sample_type == "daily_full_sample" &
      net$horizon == "Total" &
      abs(net$tau - 0.05) < 1e-8 &
      net$asset %in% assets,
    ,
    drop = FALSE
  ]
  net_values <- stats::setNames(net_sub$net, net_sub$asset)
  net_values <- net_values[assets]
  net_values[is.na(net_values)] <- 0

  theta <- pi / 2 + 2 * pi * (seq_along(assets) - 1) / length(assets)
  coords <- data.frame(
    asset = assets,
    x = 0.48 + 0.36 * cos(theta),
    y = 0.56 + 0.34 * sin(theta),
    stringsAsFactors = FALSE
  )
  rownames(coords) <- coords$asset
  label_map <- stats::setNames(scientific_asset_labels(assets), assets)
  role_label <- c(
    r_vnindex = "Market",
    r_sjc_gold = "Hedge",
    r_xau_vnd = "Hedge",
    r_bond_tri = "Hedge"
  )
  plot_edges <- edges[edges$plotted, , drop = FALSE]
  value_range <- range(plot_edges$spillover, na.rm = TRUE)
  scale_width <- function(v) {
    if (!all(is.finite(value_range)) || diff(value_range) == 0) return(rep(2.0, length(v)))
    0.9 + 3.5 * (v - value_range[1]) / diff(value_range)
  }

  draw <- function() {
    pal <- scientific_palette()
    graphics::par(mar = c(3.2, 1.0, 3.8, 1.0), family = "sans", xpd = NA)
    graphics::plot(
      c(0, 1),
      c(0, 1),
      type = "n",
      axes = FALSE,
      xlab = "",
      ylab = "",
      main = "Lower-tail directional connectedness network (tau=0.05)"
    )
    graphics::mtext("Top 10 directional spillovers; arrow direction follows shock transmission", side = 3, line = 0.4, cex = 0.85)
    for (i in seq_len(nrow(plot_edges))) {
      edge <- plot_edges[i, , drop = FALSE]
      p1 <- coords[edge$from, c("x", "y")]
      p2 <- coords[edge$to, c("x", "y")]
      dx <- p2$x - p1$x
      dy <- p2$y - p1$y
      d <- sqrt(dx^2 + dy^2)
      if (!is.finite(d) || d == 0) next
      ux <- dx / d
      uy <- dy / d
      px <- -uy
      py <- ux
      pair_key <- paste(sort(c(edge$from, edge$to)), collapse = "_")
      offset_sign <- ifelse(edge$from < edge$to, 1, -1)
      offset <- 0.018 * offset_sign
      start_x <- p1$x + ux * 0.075 + px * offset
      start_y <- p1$y + uy * 0.075 + py * offset
      end_x <- p2$x - ux * 0.082 + px * offset
      end_y <- p2$y - uy * 0.082 + py * offset
      graphics::arrows(
        start_x,
        start_y,
        end_x,
        end_y,
        length = 0.09,
        angle = 22,
        lwd = scale_width(edge$spillover),
        col = grDevices::adjustcolor(pal[["grey"]], alpha.f = 0.62)
      )
    }
    node_radius <- stats::setNames(rep(0.078, length(assets)), assets)
    for (asset in assets) {
      row <- coords[asset, , drop = FALSE]
      role <- role_label[[asset]]
      fill <- if (asset == "r_vnindex") pal[["black"]] else scientific_asset_colors(asset)
      border <- if (net_values[[asset]] >= 0) pal[["blue"]] else pal[["orange"]]
      text_col <- if (asset == "r_sjc_gold") pal[["charcoal"]] else "white"
      graphics::symbols(
        row$x,
        row$y,
        circles = node_radius[asset],
        inches = FALSE,
        add = TRUE,
        bg = fill,
        fg = border,
        lwd = 2.2
      )
      graphics::text(row$x, row$y + 0.018, labels = label_map[[asset]], col = text_col, cex = 0.72, font = 2)
      graphics::text(row$x, row$y - 0.026, labels = sprintf("NET %.2f", net_values[[asset]]), cex = 0.62, col = text_col)
    }
    phase3_9_draw_network_legend()
  }

  png_path <- file.path(output_dir, "figure_phase3_9_lower_tail_network.png")
  grDevices::png(png_path, width = 1500, height = 1000, res = 150)
  draw()
  grDevices::dev.off()

  svg_path <- file.path(output_dir, "figure_phase3_9_lower_tail_network.svg")
  grDevices::svg(svg_path, width = 10, height = 6.7)
  draw()
  grDevices::dev.off()
  list(files = c(png_path, svg_path), edges = edges)
}

phase3_9_plot_package_network <- function(daily_z, config) {
  output_dir <- file.path(config$outputs$figures_dir, "connectedness")
  ensure_dir(output_dir)
  result <- fit_status(phase3_9_connectedness(
    daily_z,
    model = "QVAR",
    connectedness = "Time",
    tau = 0.05,
    window.size = NULL
  ))
  if (!result$ok) {
    warning(
      sprintf("ConnectednessApproach::PlotNetwork reference figure failed: %s", result$error),
      call. = FALSE
    )
    return(character())
  }
  fit <- result$value

  draw <- function() {
    graphics::par(family = "sans")
    ConnectednessApproach::PlotNetwork(fit)
  }

  png_path <- file.path(output_dir, "figure_phase3_9_lower_tail_network_package_native.png")
  grDevices::png(png_path, width = 1500, height = 1000, res = 150)
  draw()
  grDevices::dev.off()

  svg_path <- file.path(output_dir, "figure_phase3_9_lower_tail_network_package_native.svg")
  grDevices::svg(svg_path, width = 10, height = 6.7)
  draw()
  grDevices::dev.off()
  c(png_path, svg_path)
}

phase3_9_plot_frequency_tci <- function(freq_tci, config) {
  if (nrow(freq_tci) == 0) return(character())
  output_dir <- file.path(config$outputs$figures_dir, "connectedness")
  ensure_dir(output_dir)
  df <- freq_tci[order(freq_tci$horizon), , drop = FALSE]
  y <- df$tci
  names(y) <- df$horizon

  draw <- function() {
    graphics::par(mar = c(5.8, 4.8, 3.0, 1.0), family = "sans")
    graphics::barplot(
      y,
      col = c(scientific_palette()[["blue"]], scientific_palette()[["orange"]], scientific_palette()[["bluish_green"]], scientific_palette()[["reddish_purple"]])[seq_along(y)],
      border = "black",
      las = 2,
      ylab = "Total connectedness index",
      main = "Frequency connectedness by horizon"
    )
    graphics::grid(nx = NA, ny = NULL, col = "gray85", lty = "solid")
  }

  png_path <- file.path(output_dir, "figure_phase3_9_frequency_tci.png")
  grDevices::png(png_path, width = 1200, height = 850, res = 150)
  draw()
  grDevices::dev.off()

  svg_path <- file.path(output_dir, "figure_phase3_9_frequency_tci.svg")
  grDevices::svg(svg_path, width = 8, height = 5.7)
  draw()
  grDevices::dev.off()
  c(png_path, svg_path)
}

phase3_9_write_report <- function(outputs, config, daily_panel, weekly_panel, figure_files) {
  report_path <- project_path("data/metadata/phase3_9_connectedness_report.md")
  quantile_tci <- outputs$quantile$tci
  quantile_net <- outputs$quantile$net
  rolling_tci <- outputs$quantile$rolling_tci
  frequency_tci <- outputs$frequency$tci
  frequency_net <- outputs$frequency$net
  network_edges <- outputs$network_edges
  warnings <- c(outputs$quantile$warnings, outputs$frequency$warnings)

  lower_tail_net <- quantile_net[
    nrow(quantile_net) > 0 &
      abs(quantile_net$tau - 0.05) < 1e-8 &
      quantile_net$sample_type == "daily_full_sample",
    ,
    drop = FALSE
  ]
  lower_tail_note <- "No lower-tail NET result was generated."
  if (nrow(lower_tail_net) > 0) {
    lower_tail_net <- lower_tail_net[order(lower_tail_net$net), , drop = FALSE]
    lower_tail_note <- sprintf(
      "Lower-tail tau=0.05 strongest net receiver/absorber: `%s` (NET=%.4f); strongest net transmitter: `%s` (NET=%.4f).",
      lower_tail_net$asset[1],
      lower_tail_net$net[1],
      lower_tail_net$asset[nrow(lower_tail_net)],
      lower_tail_net$net[nrow(lower_tail_net)]
    )
  }

  lines <- c(
    "# Phase 3.9 Tail/Frequency Connectedness Report",
    "",
    "## M\u1ee5c ti\u00eau",
    "",
    "Phase 3.9 \u0111o core connectedness gi\u1eefa VNIndex, SJC, XAU/VND v\u00e0 Bond TRI. Brent \u0111\u01b0\u1ee3c lo\u1ea1i kh\u1ecfi core connectedness theo h\u01b0\u1edbng ch\u1ed1t c\u1ee7a b\u00e0i: Brent l\u00e0 oil-price shock/control, kh\u00f4ng ph\u1ea3i t\u00e0i s\u1ea3n tr\u00fa \u1ea9n c\u1ed1t l\u00f5i. K\u1ebft qu\u1ea3 d\u00f9ng \u0111\u1ec3 b\u1ed5 sung l\u1edbp tail/frequency connectedness cho khung TRAS, kh\u00f4ng d\u00f9ng \u0111\u1ec3 k\u1ebft lu\u1eadn quan h\u1ec7 nh\u00e2n qu\u1ea3.",
    "",
    "## Input",
    "",
    sprintf("- Prepared returns: `%s`", config$data$returns_input),
    sprintf("- Core variables: `%s`", paste(phase3_9_core_assets, collapse = "`, `")),
    sprintf("- Daily complete observations: `%s`", nrow(daily_panel)),
    sprintf("- Daily sample: `%s` to `%s`", min(daily_panel$date), max(daily_panel$date)),
    sprintf("- Weekly complete observations: `%s`", nrow(weekly_panel)),
    sprintf("- Weekly sample: `%s` to `%s`", min(weekly_panel$date), max(weekly_panel$date)),
    "",
    "## Method",
    "",
    "- Daily full-sample QVAR connectedness at tau = 0.05, 0.10, 0.50, 0.90, 0.95.",
    sprintf("- Weekly rolling QVAR connectedness at tau = 0.05, 0.50, 0.95 with window size `%s` weeks.", outputs$quantile$rolling_window),
    "- Daily full-sample VAR frequency connectedness with horizons labelled by the package output.",
    "- Package: `ConnectednessApproach`; frequency layer follows Barunik-Krehlik style decomposition through the package interface.",
    "",
    "## Output Summary",
    "",
    sprintf("- Quantile TCI rows: `%s`", nrow(quantile_tci)),
    sprintf("- Quantile NET rows: `%s`", nrow(quantile_net)),
    sprintf("- Weekly rolling TCI rows: `%s`", nrow(rolling_tci)),
    sprintf("- Frequency TCI rows: `%s`", nrow(frequency_tci)),
    sprintf("- Frequency NET rows: `%s`", nrow(frequency_net)),
    sprintf("- Lower-tail network edge rows: `%s`", nrow(network_edges)),
    sprintf("- Lower-tail network plotted edges: `%s`", if (nrow(network_edges) == 0) 0 else sum(network_edges$plotted)),
    sprintf("- Figure files: `%s`", length(figure_files)),
    "",
    "## Main Diagnostic Note",
    "",
    lower_tail_note,
    "",
    "## Output Files",
    "",
    "- `results/tables/connectedness/quantile_connectedness_tci.csv`",
    "- `results/tables/connectedness/quantile_connectedness_net.csv`",
    "- `results/tables/connectedness/quantile_connectedness_table.csv`",
    "- `results/tables/connectedness/quantile_connectedness_rolling_weekly_tci.csv`",
    "- `results/tables/connectedness/quantile_connectedness_rolling_weekly_net.csv`",
    "- `results/tables/connectedness/frequency_connectedness_tci.csv`",
    "- `results/tables/connectedness/frequency_connectedness_net.csv`",
    "- `results/tables/connectedness/frequency_connectedness_table.csv`",
    "- `results/tables/connectedness/lower_tail_network_edges.csv`",
    "- `results/figures/connectedness/`",
    "",
    "## Scientific Cautions",
    "",
    "- Connectedness is not causal identification.",
    "- The network figure displays the strongest directional spillovers for readability; the full edge table remains in CSV output.",
    "- A package-native reference network can also be exported with `ConnectednessApproach::PlotNetwork`, following the style used in the imported WS time-series example.",
    "- Crypto is not included in the core Phase 3.9 system.",
    "- Brent is excluded from the core connectedness system and should be used only as an oil-price shock/control or robustness extension.",
    "- Bond TRI is a simulated investable bond index proxy and must not be called an official index."
  )

  if (length(warnings) > 0) {
    lines <- c(lines, "", "## Warnings", "", paste0("- ", warnings))
  }

  con <- file(report_path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw("\ufeff"), con)
  writeLines(enc2utf8(lines), con, useBytes = TRUE)
  invisible(report_path)
}

run_phase3_9_connectedness <- function(
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
  safe_package_stop("zoo")
  safe_package_stop("ConnectednessApproach")
  ensure_output_dirs(config)
  ensure_dir(file.path(config$outputs$tables_dir, "connectedness"))
  ensure_dir(file.path(config$outputs$figures_dir, "connectedness"))
  append_log("Phase 3.9 connectedness started.", config)

  daily_panel <- phase3_9_read_returns(config)
  weekly_panel <- phase3_9_weekly_returns(daily_panel)
  daily_z <- phase3_9_as_zoo(daily_panel)
  weekly_z <- phase3_9_as_zoo(weekly_panel)

  quantile_outputs <- phase3_9_write_quantile_outputs(daily_z, weekly_z, config)
  frequency_outputs <- phase3_9_write_frequency_outputs(daily_z, config)
  network_output <- phase3_9_plot_tail_network(quantile_outputs$table, quantile_outputs$net, config)
  package_network_files <- phase3_9_plot_package_network(daily_z, config)
  figure_files <- c(
    phase3_9_plot_tci(quantile_outputs$rolling_tci, config),
    phase3_9_plot_tail_net(quantile_outputs$net, config),
    network_output$files,
    package_network_files,
    phase3_9_plot_frequency_tci(frequency_outputs$tci, config)
  )
  report <- phase3_9_write_report(
    list(quantile = quantile_outputs, frequency = frequency_outputs, network_edges = network_output$edges),
    config,
    daily_panel,
    weekly_panel,
    figure_files
  )

  append_log("Phase 3.9 connectedness finished.", config)
  list(
    daily_panel = daily_panel,
    weekly_panel = weekly_panel,
    quantile = quantile_outputs,
    frequency = frequency_outputs,
    figures = figure_files,
    report = report
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

if (!interactive() && is_direct_rscript_call("src/models/run_phase3_9_connectedness.R")) {
  result <- run_phase3_9_connectedness()
  cat("Phase 3.9 connectedness completed.\n")
  cat(sprintf("Daily complete rows: %s\n", nrow(result$daily_panel)))
  cat(sprintf("Weekly complete rows: %s\n", nrow(result$weekly_panel)))
  cat(sprintf("Quantile TCI rows: %s\n", nrow(result$quantile$tci)))
  cat(sprintf("Rolling weekly TCI rows: %s\n", nrow(result$quantile$rolling_tci)))
  cat(sprintf("Frequency TCI rows: %s\n", nrow(result$frequency$tci)))
  cat(sprintf("Report: %s\n", result$report))
}
