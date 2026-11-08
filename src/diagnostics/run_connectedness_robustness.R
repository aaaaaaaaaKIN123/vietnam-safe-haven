# run_connectedness_robustness.R
# Estimate 5-node connectedness networks for Brent and WTI robustness checks

source("src/utils/config_loader.R")
source("src/utils/io_helpers.R")
source("src/utils/date_asset_helpers.R")
source("src/models/validate_model_control.R")
source("src/models/R/model_helpers.R")
source("src/visualization/scientific_theme.R")

# Source the original connectedness functions
source("src/models/run_phase3_9_connectedness.R")

# Redefine the plotting function to support Brent and WTI in role_label
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
  
  # Corrected role_label supporting all assets
  role_label <- c(
    r_vnindex = "Market",
    r_sjc_gold = "Hedge",
    r_xau_vnd = "Hedge",
    r_bond_tri = "Hedge",
    r_brent = "Control",
    r_wti = "Control"
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

config_base <- read_model_control("src/models/model_control.yaml")

# 1. Brent 5-node network
cat("Running 5-node connectedness with Brent...\n")
phase3_9_core_assets <- c("r_vnindex", "r_sjc_gold", "r_xau_vnd", "r_bond_tri", "r_brent")

# Modify config to redirect outputs
config_brent <- config_base
config_brent$outputs$tables_dir <- "results/tables/connectedness_robustness_brent"
config_brent$outputs$figures_dir <- "results/figures/connectedness_robustness_brent"

# Recreate folders
dir.create(config_brent$outputs$tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(config_brent$outputs$figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(config_brent$outputs$tables_dir, "connectedness"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(config_brent$outputs$figures_dir, "connectedness"), recursive = TRUE, showWarnings = FALSE)

# Run Brent 5-node
daily_panel_b <- phase3_9_read_returns(config_brent)
weekly_panel_b <- phase3_9_weekly_returns(daily_panel_b)
daily_z_b <- phase3_9_as_zoo(daily_panel_b)
weekly_z_b <- phase3_9_as_zoo(weekly_panel_b)

# Save tables and figures using the original functions (which write to config paths)
quantile_b <- phase3_9_write_quantile_outputs(daily_z_b, weekly_z_b, config_brent)
frequency_b <- phase3_9_write_frequency_outputs(daily_z_b, config_brent)
network_b <- phase3_9_plot_tail_network(quantile_b$table, quantile_b$net, config_brent)
package_net_b <- phase3_9_plot_package_network(daily_z_b, config_brent)

cat("Completed 5-node connectedness with Brent.\n")

# 2. WTI 5-node network
cat("Running 5-node connectedness with WTI...\n")
phase3_9_core_assets <- c("r_vnindex", "r_sjc_gold", "r_xau_vnd", "r_bond_tri", "r_wti")

# Modify config to redirect outputs
config_wti <- config_base
config_wti$outputs$tables_dir <- "results/tables/connectedness_robustness_wti"
config_wti$outputs$figures_dir <- "results/figures/connectedness_robustness_wti"

# Recreate folders
dir.create(config_wti$outputs$tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(config_wti$outputs$figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(config_wti$outputs$tables_dir, "connectedness"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(config_wti$outputs$figures_dir, "connectedness"), recursive = TRUE, showWarnings = FALSE)

# Run WTI 5-node
daily_panel_w <- phase3_9_read_returns(config_wti)
weekly_panel_w <- phase3_9_weekly_returns(daily_panel_w)
daily_z_w <- phase3_9_as_zoo(daily_panel_w)
weekly_z_w <- phase3_9_as_zoo(weekly_panel_w)

# Save tables and figures
quantile_w <- phase3_9_write_quantile_outputs(daily_z_w, weekly_z_w, config_wti)
frequency_w <- phase3_9_write_frequency_outputs(daily_z_w, config_wti)
network_w <- phase3_9_plot_tail_network(quantile_w$table, quantile_w$net, config_wti)
package_net_w <- phase3_9_plot_package_network(daily_z_w, config_wti)

cat("Completed 5-node connectedness with WTI.\n")
