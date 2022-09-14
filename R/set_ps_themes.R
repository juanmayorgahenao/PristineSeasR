ps_palettes <- list(
  main = c(`dark blue` = "#003761",
           `blue` = "#2074a4",
           `turquoise` = "#43b4c4",
           `green` = "#aae4d8",
           `yellow` = "#ffcc00",
           `white` = "#ffffff"))

ps_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (ps_palettes)

  ps_palettes[cols]
}


ps_pal <- function(palette = "main",
                         reverse = FALSE, ...) {

  pal <- ps_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}

#' Color scale constructor for Pristine Seas colors
#'
#' @param palette Character name of palette in ps_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_pristine_seas <- function(palette = "main",
                                      discrete = TRUE,
                                      reverse = FALSE, ...) {

  pal <- ps_pal(palette = palette,
                reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("ps_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}


#' Fill scale constructor for drsimonj colors
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_pristine_seas <- function(palette = "main",
                                     discrete = TRUE,
                                     reverse = FALSE, ...) {

  pal <- ps_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("drsimonj_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}
