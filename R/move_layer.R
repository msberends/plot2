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

#' Move a `ggplot` Layer
#' 
#' Use this function to move a certain plot layer up or down.
#' @param plot A `ggplot` object.
#' @param move Number of layers to move `layer`: a negative value moves it down (drawn earlier, so below other layers), a positive value moves it up. The new position is limited to the existing layers. Defaults to `-1`, one layer down.
#' @param layer The number of the layer to move, defaults to the top layer.
#' @return A `ggplot` object with the layers in the new order.
#' @importFrom ggplot2 is_ggplot
#' @export
#' @examples
#' p <- plot2(iris, Sepal.Length, Sepal.Width, colour = "grey75") |>
#'   add_line(y = mean(Sepal.Width), colour = "red", linewidth = 2)
#' 
#' # the line is drawn on top of the points
#' p
#' 
#' # move the line one layer down, below the points
#' move_layer(p)
move_layer <- function(plot, move = -1, layer = length(plot$layers)) {
  
  if (!is_ggplot(plot)) {
    stop("`plot` must be a ggplot2 object.", call. = FALSE)
  }
  
  layers <- plot$layers
  layer_old <- layer
  
  if (!layer_old %in% seq_len(length(layers))) {
    stop("This plot contains only ", length(layers), " layers. Layer ",
         layer_old, " does not exist.", call. = FALSE)
  }
  # clamp the new position to the existing layers
  layer_new <- min(length(layers), max(1, layer + move))
  
  # new order by index: take out layer_old and insert it at layer_new (F083)
  ord <- append(setdiff(seq_along(layers), layer_old), layer_old, after = layer_new - 1)
  layers <- layers[ord]
  plot$layers <- layers
  
  plot
}
