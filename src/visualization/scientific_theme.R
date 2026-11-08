scientific_palette <- function() {
  c(
    black = "#000000",
    charcoal = "#1F2937",
    grey = "#999999",
    light_grey = "#D9D9D9",
    orange = "#E69F00",
    sky_blue = "#56B4E9",
    bluish_green = "#009E73",
    yellow = "#F0E442",
    blue = "#0072B2",
    vermillion = "#D55E00",
    reddish_purple = "#CC79A7"
  )
}

scientific_asset_colors <- function(keys) {
  pal <- scientific_palette()
  color_map <- c(
    r_vnindex = pal[["black"]],
    vnindex = pal[["black"]],
    r_sjc_gold = pal[["orange"]],
    sjc_gold = pal[["orange"]],
    r_xau_vnd = pal[["blue"]],
    xau_vnd = pal[["blue"]],
    r_bond_tri = pal[["bluish_green"]],
    r_bond_tri_10y = pal[["bluish_green"]],
    bond_tri = pal[["bluish_green"]],
    bond_tri_10y = pal[["bluish_green"]],
    r_brent = pal[["vermillion"]],
    brent = pal[["vermillion"]],
    r_wti = pal[["reddish_purple"]],
    wti = pal[["reddish_purple"]],
    r_btc = pal[["reddish_purple"]],
    btc = pal[["reddish_purple"]],
    r_eth = pal[["sky_blue"]],
    eth = pal[["sky_blue"]],
    r_gold_premium = pal[["yellow"]],
    gold_premium = pal[["yellow"]]
  )
  out <- unname(color_map[keys])
  out[is.na(out)] <- pal[["grey"]]
  out
}

scientific_asset_labels <- function(keys) {
  label_map <- c(
    r_vnindex = "VNIndex",
    vnindex = "VNIndex",
    r_sjc_gold = "SJC Gold",
    sjc_gold = "SJC Gold",
    r_xau_vnd = "XAU/VND",
    xau_vnd = "XAU/VND",
    r_bond_tri = "Bond TRI",
    r_bond_tri_10y = "Bond TRI",
    bond_tri = "Bond TRI",
    bond_tri_10y = "Bond TRI",
    r_brent = "Brent oil shock",
    brent = "Brent oil shock",
    r_wti = "WTI oil shock",
    wti = "WTI oil shock",
    r_btc = "Bitcoin",
    btc = "Bitcoin",
    r_eth = "Ethereum",
    eth = "Ethereum",
    r_gold_premium = "SJC premium",
    gold_premium = "SJC premium"
  )
  out <- unname(label_map[keys])
  out[is.na(out)] <- keys[is.na(out)]
  out
}

scientific_asset_legend <- function(keys, x = "topright", inset = 0.02, title = "Color key") {
  unique_keys <- unique(keys)
  graphics::legend(
    x,
    inset = inset,
    legend = scientific_asset_labels(unique_keys),
    fill = scientific_asset_colors(unique_keys),
    title = title,
    bty = "n",
    cex = 0.78,
    border = NA,
    xpd = NA
  )
}

scientific_model_colors <- function(keys) {
  pal <- scientific_palette()
  color_map <- c(
    CCC = pal[["grey"]],
    DCC = pal[["blue"]],
    aDCC = pal[["vermillion"]]
  )
  out <- unname(color_map[keys])
  out[is.na(out)] <- pal[["charcoal"]]
  out
}

scientific_model_lty <- function(keys) {
  lty_map <- c(CCC = "dotted", DCC = "solid", aDCC = "longdash")
  out <- unname(lty_map[keys])
  out[is.na(out)] <- "dotdash"
  out
}

scientific_model_lwd <- function(keys) {
  lwd_map <- c(CCC = 1.5, DCC = 2.1, aDCC = 2.0)
  out <- unname(lwd_map[keys])
  out[is.na(out)] <- 1.5
  out
}

scientific_heat_colors <- function(n = 12, option = "viridis") {
  if ("hcl.colors" %in% getNamespaceExports("grDevices")) {
    palette_name <- if (identical(option, "diverging")) "Blue-Red 3" else "Viridis"
    return(grDevices::hcl.colors(n, palette_name, rev = FALSE))
  }
  grDevices::heat.colors(n)
}

scientific_draw_vertical_colorbar <- function(
    zlim = c(0, 1),
    colors = scientific_heat_colors(12),
    label = "Score",
    xleft,
    ybottom,
    height,
    width = 0.18,
    ticks = seq(0, 1, by = 0.25),
    cex = 0.7) {
  old_xpd <- graphics::par("xpd")
  graphics::par(xpd = NA)
  n <- length(colors)
  breaks <- seq(zlim[1], zlim[2], length.out = n + 1)
  y_breaks <- ybottom + (breaks - zlim[1]) / diff(zlim) * height
  for (i in seq_len(n)) {
    graphics::rect(
      xleft,
      y_breaks[i],
      xleft + width,
      y_breaks[i + 1],
      col = colors[i],
      border = NA
    )
  }
  graphics::rect(xleft, ybottom, xleft + width, ybottom + height, border = scientific_palette()[["charcoal"]])
  tick_y <- ybottom + (ticks - zlim[1]) / diff(zlim) * height
  graphics::segments(xleft + width, tick_y, xleft + width + width * 0.35, tick_y, col = scientific_palette()[["charcoal"]])
  graphics::text(xleft + width * 1.65, tick_y, labels = sprintf("%.2f", ticks), adj = 0, cex = cex, col = scientific_palette()[["charcoal"]])
  graphics::text(xleft + width * 4.5, ybottom + height / 2, labels = label, srt = 90, cex = cex, col = scientific_palette()[["charcoal"]])
  graphics::par(xpd = old_xpd)
}

scientific_draw_colorbar_panel <- function(
    zlim = c(0, 1),
    colors = scientific_heat_colors(12),
    label = "Score",
    ticks = seq(0, 1, by = 0.25),
    cex = 0.8) {
  graphics::par(mar = c(7, 0.2, 3, 0.2), family = "sans")
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1.25), ylim = zlim)
  n <- length(colors)
  breaks <- seq(zlim[1], zlim[2], length.out = n + 1)
  for (i in seq_len(n)) {
    graphics::rect(0.16, breaks[i], 0.48, breaks[i + 1], col = colors[i], border = NA)
  }
  graphics::rect(0.16, zlim[1], 0.48, zlim[2], border = scientific_palette()[["charcoal"]])
  tick_values <- ticks[ticks >= zlim[1] & ticks <= zlim[2]]
  graphics::segments(
    0.48,
    tick_values,
    0.58,
    tick_values,
    col = scientific_palette()[["charcoal"]]
  )
  graphics::text(
    0.63,
    tick_values,
    labels = sprintf("%.2f", tick_values),
    adj = 0,
    cex = cex,
    col = scientific_palette()[["charcoal"]]
  )
  graphics::text(
    1.13,
    mean(zlim),
    labels = label,
    srt = 90,
    cex = cex,
    col = scientific_palette()[["charcoal"]]
  )
}

scientific_base_par <- function(mar = c(4.8, 5, 3.2, 1.2)) {
  graphics::par(mar = mar, family = "sans", fg = "#1F2937", col.axis = "#1F2937", col.lab = "#1F2937")
}

scientific_grid <- function(x = NULL, y = NULL) {
  graphics::grid(nx = x, ny = y, col = scientific_palette()[["light_grey"]], lty = "solid")
}
