source("src/utils/io_helpers.R")
source("src/visualization/scientific_theme.R")

plot_dynamic_correlations <- function(config) {
  input <- file.path(config$outputs$tables_dir, "dynamic_correlations", "dcc_adcc_dynamic_correlations.csv")
  if (!file.exists(input)) {
    warning(sprintf("Dynamic correlation file not found: %s", input))
    return(invisible(NULL))
  }
  df <- utils::read.csv(input, stringsAsFactors = FALSE)
  df$date <- as.Date(df$date)
  output_dir <- file.path(config$outputs$figures_dir, "dynamic_correlations")
  ensure_dir(output_dir)

  written <- character()
  for (hedge in unique(df$hedge)) {
    sub <- df[df$hedge == hedge, , drop = FALSE]
    models <- intersect(c("DCC", "aDCC", "CCC"), unique(sub$model))
    y_range <- range(sub$dynamic_correlation, na.rm = TRUE)
    y_pad <- diff(y_range) * 0.08
    if (!is.finite(y_pad) || y_pad == 0) y_pad <- 0.02
    y_lim <- c(y_range[1] - y_pad, y_range[2] + y_pad)

    draw_plot <- function() {
      graphics::plot(
        range(sub$date),
        y_lim,
        type = "n",
        xlab = "Date",
        ylab = "Conditional correlation",
        main = sprintf("VNIndex and %s", hedge),
        las = 1
      )
      graphics::grid(col = "gray85", lty = 1)
      graphics::abline(h = 0, col = "gray35", lty = 3)
      for (model_name in models) {
        model_sub <- sub[sub$model == model_name, , drop = FALSE]
        graphics::lines(
          model_sub$date,
          model_sub$dynamic_correlation,
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
    }

    png_path <- file.path(output_dir, sprintf("%s_dynamic_correlation.png", hedge))
    grDevices::png(png_path, width = 1400, height = 800, res = 150)
    draw_plot()
    grDevices::dev.off()
    written <- c(written, png_path)

    svg_path <- file.path(output_dir, sprintf("%s_dynamic_correlation.svg", hedge))
    grDevices::svg(svg_path, width = 9.5, height = 5.5)
    draw_plot()
    grDevices::dev.off()
    written <- c(written, svg_path)
  }
  invisible(written)
}
