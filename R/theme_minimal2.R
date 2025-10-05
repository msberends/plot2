# ===================================================================== #
#  An R package for Fast 'ggplot2' Plotting:                            #
#  https://github.com/msberends/plot2                                   #
#                                                                       #
#  This R package is free software; you can freely use and distribute   #
#  it for both personal and commercial purposes under the terms of the  #
#  GNU General Public License version 2.0 (GNU GPL-2), as published by  #
#  the Free Software Foundation.                                        #
#                                                                       #
#  We created this package for both routine data analysis and academic  #
#  research and it was publicly released in the hope that it will be    #
#  useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ===================================================================== #

#' An Even More Minimal Theme
#' 
#' This `ggplot2` theme provides even more white area and less clutter than [`theme_minimal()`][ggplot2::theme_minimal()].
#' @param ... Arguments passed on to [ggplot2::theme()].
#' @param colour_font_primary Colour to set for the plot title and tag.
#' @param colour_font_secondary Colour to set for the plot subtitle and caption.
#' @param colour_font_axis Colour to set for the axis titles on both x and y.
#' @param colour_background Colour to set for the background.
#' @importFrom ggplot2 element_text `%+replace%` theme_bw theme margin element_line element_blank unit element_rect
#' @export
#' @examples 
#' library(ggplot2)
#' ggplot(mtcars, aes(hp, mpg)) +
#'   geom_point()
#'   
#' ggplot(mtcars, aes(hp, mpg)) +
#'   geom_point() +
#'   theme_minimal2()
#'   
#' # in plot2(), the 'theme' argument defaults to theme_minimal2():
#' mtcars |>
#'   plot2(hp, mpg)
#'   
#' # set to NULL to use the ggplot2 default:
#' mtcars |>
#'   plot2(hp, mpg, theme = NULL)
theme_minimal2 <- function(...,
                           colour_font_primary = getOption("plot2.colour_font_primary", "black"),
                           colour_font_secondary = getOption("plot2.colour_font_secondary", "grey35"),
                           colour_font_axis = getOption("plot2.colour_font_axis", "grey25"),
                           colour_background = getOption("plot2.colour_background", "white")) {
  
  colour_font_primary <- get_colour(colour_font_primary, length = 1)
  colour_font_secondary <- get_colour(colour_font_secondary, length = 1)
  colour_font_axis <- get_colour(colour_font_axis, length = 1)
  colour_background <- get_colour(colour_background, length = 1)
  
  t <- theme_bw(base_size = 11) %+replace%
    theme(
      axis.text.x = element_text(margin = margin(3, 0, 0, 0)),
      axis.title.x = element_text(margin = margin(14, 0, 0, 0), colour = colour_font_axis),
      axis.title.y = element_text(margin = margin(0, 14, 0, 0), angle = 90, colour = colour_font_axis),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(linewidth = 0.75, colour = "grey75"),
      axis.ticks.length = unit(2, "pt"),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.key.size = unit(11, "pt"), # squares and lines left to legend text
      legend.text = element_text(size = unit(9, "pt"), # text itself
                                 margin = margin(l = 1, r = 6, unit = "pt")), # left and right of text
      legend.title = element_text(face = "bold", size = unit(10, "pt")),
      panel.background = element_rect(fill = colour_background, linetype = 0),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.375, colour = "grey75"),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_line(linewidth = 0.25, colour = "grey85"),
      axis.line = element_line(linewidth = 0.375, colour = "grey75"),
      axis.line.y = element_blank(),
      plot.margin = unit(c(5, 12, 5, 5), units = "pt"),
      plot.background = element_rect(fill = colour_background, linetype = 0),
      plot.subtitle = element_text(size = unit(11, "pt"),
                                   hjust = 0.5,
                                   colour = colour_font_secondary,
                                   margin = margin(0, 0, 10, 0)),
      plot.title = element_text(size = unit(13, "pt"),
                                hjust = 0.5,
                                colour = colour_font_primary,
                                margin = margin(0, 0, 10, 0)),
      plot.caption = element_text(size = unit(10, "pt"),
                                  hjust = 1,
                                  colour = colour_font_secondary),
      plot.tag = element_text(size = unit(14, "pt"),
                              hjust = 0,
                              colour = colour_font_primary,
                              margin = margin(0, 0, 0, 0),
                              face = "bold"),
      strip.background = element_rect(colour = colour_background),
      strip.switch.pad.wrap = unit(-5, "pt"),
      strip.placement = "outside",
      complete = TRUE)
  
  if (length(list(...)) > 0) {
    t <- t %+replace%
      theme(...)
  }
  
  t
}
