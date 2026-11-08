source("src/utils/io_helpers.R")

plot_volatility_series <- function(config) {
  input_dir <- file.path(config$outputs$tables_dir, "volatility_estimates")
  if (!dir.exists(input_dir)) {
    warning(sprintf("Volatility estimate directory not found: %s", input_dir))
    return(invisible(NULL))
  }
  files <- list.files(input_dir, pattern = "_sigma\\.csv$", full.names = TRUE)
  ensure_dir(file.path(config$outputs$figures_dir, "volatility"))

  for (file in files) {
    df <- utils::read.csv(file, stringsAsFactors = FALSE)
    df$date <- as.Date(df$date)
    asset <- unique(df$asset)[1]
    model <- unique(df$model)[1]
    dist <- unique(df$distribution)[1]
    png_path <- file.path(config$outputs$figures_dir, "volatility", sprintf("%s_%s_%s_sigma.png", asset, model, dist))
    grDevices::png(png_path, width = 1200, height = 700)
    graphics::plot(df$date, df$conditional_sigma, type = "l", xlab = "Date", ylab = "Conditional sigma",
                   main = sprintf("Conditional volatility: %s (%s, %s)", asset, model, dist))
    grDevices::dev.off()
  }
  invisible(TRUE)
}
