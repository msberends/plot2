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
#' Use this function to move a certain plot layer up or down. This function returns a `ggplot` object.
#' @param plot A `ggplot` object.
#' @param move Number of layers to move `layer` up or down.
#' @param layer The layer to affect, defaults to top layer.
#' @importFrom ggplot2 is_ggplot
#' @export
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
