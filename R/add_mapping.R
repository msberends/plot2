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

#' Add Additional Mapping
#' 
#' This function can be used to adjust the mapping of a plot.
#' @param plot a `ggplot2` plot
#' @param ... arguments passed on to [ggplot2::aes()]
#' @importFrom ggplot2 is.ggplot aes
#' @export
#' @examples 
#' p <- iris |> plot2(Sepal.Length, Sepal.Width, Species, zoom = TRUE)
#' p
#' 
#' p |> add_mapping(shape = Species)
add_mapping <- function(plot, ...) {
  if (!is.ggplot(plot)) {
    stop("`plot` must be a ggplot2 object.", call. = FALSE)
  }
  plot$mapping <- utils::modifyList(plot$mapping, aes(...))
  plot
}
