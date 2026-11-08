source("src/utils/io_helpers.R")
source("src/visualization/scientific_theme.R")

plot_single_hedge_ratio <- function(sub, output_path, device = c("png", "svg")) {
  device <- match.arg(device)
  models <- unique(sub$model)
  model_order <- c("CCC", "DCC", "aDCC")
  models <- c(model_order[model_order %in% models], setdiff(models, model_order))
  finite_y <- sub$hedge_ratio[is.finite(sub$hedge_ratio)]
  if (length(finite_y) == 0) {
    return(invisible(FALSE))
  }
  ylim <- range(finite_y, na.rm = TRUE)
  if (diff(ylim) == 0) {
    ylim <- ylim + c(-0.01, 0.01)
  }

  if (device == "png") {
    grDevices::png(output_path, width = 1600, height = 950, res = 150)
  } else {
    grDevices::svg(output_path, width = 10.7, height = 6.3)
  }
  on.exit(grDevices::dev.off(), add = TRUE)

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mar = c(4.5, 4.8, 3.0, 1.0), family = "sans")

  graphics::plot(
    sub$date,
    sub$hedge_ratio,
    type = "n",
    xlab = "Date",
    ylab = "Dynamic hedge ratio",
    main = sprintf("Dynamic hedge ratio: %s", unique(sub$hedge)[1]),
    ylim = ylim,
    las = 1,
    bty = "l"
  )
  graphics::grid(col = "gray85", lty = "solid")
  graphics::abline(h = 0, col = "black", lty = "dashed", lwd = 1)
  for (model_name in models) {
    model_sub <- sub[sub$model == model_name, , drop = FALSE]
    graphics::lines(
      model_sub$date,
      model_sub$hedge_ratio,
      col = scientific_model_colors(model_name),
      lty = scientific_model_lty(model_name),
      lwd = scientific_model_lwd(model_name)
    )
  }
  graphics::legend(
    "topright",
    legend = models,
    col = scientific_model_colors(models),
    lty = scientific_model_lty(models),
    lwd = scientific_model_lwd(models),
    bty = "n",
    cex = 0.9
  )
  invisible(TRUE)
}

plot_hedge_ratios <- function(config) {
  input <- file.path(config$outputs$tables_dir, "hedge_ratios", "dynamic_hedge_ratios.csv")
  if (!file.exists(input)) {
    warning(sprintf("Hedge-ratio file not found: %s", input))
    return(invisible(NULL))
  }
  df <- utils::read.csv(input, stringsAsFactors = FALSE)
  df$date <- as.Date(df$date)
  ensure_dir(file.path(config$outputs$figures_dir, "hedge_ratios"))

  for (hedge in unique(df$hedge)) {
    sub <- df[df$hedge == hedge, , drop = FALSE]
    png_path <- file.path(config$outputs$figures_dir, "hedge_ratios", sprintf("%s_hedge_ratio.png", hedge))
    svg_path <- file.path(config$outputs$figures_dir, "hedge_ratios", sprintf("%s_hedge_ratio.svg", hedge))
    plot_single_hedge_ratio(sub, png_path, device = "png")
    plot_single_hedge_ratio(sub, svg_path, device = "svg")
  }
  invisible(TRUE)
}
