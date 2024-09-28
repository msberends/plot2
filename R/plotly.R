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

#' Create Interactive Plotly
#' 
#' Transform a `ggplot2`/`plot2` object to an interactive plot using the [Plotly R Open Source Graphing Library](https://plotly.com/r/).
#' @param plot a `ggplot2` plot
#' @param ...
#' In case of [as_plotly()]: arguments to pass on to [`layout()`][plotly::layout()] to change the Plotly layout object
#' 
#' In case of [plotly_style()]: arguments to pass on to [`style()`][plotly::style()] to change the Plotly style object
#' @importFrom ggplot2 is.ggplot
#' @rdname plotly
#' @export
#' @examples 
#' mtcars |>
#'   plot2(mpg, hp) |> 
#'   as_plotly()
#'   
#' mtcars |>
#'   plot2(mpg, hp) |> 
#'   as_plotly(dragmode = "pan") |>
#'   plotly_style(marker.line.color = "red",
#'                hoverinfo = "y")
as_plotly <- function(plot, ...) {
  if (!is.ggplot(plot)) {
    stop("`plot` must be a ggplot2 object.", call. = FALSE)
  }
  if (!"plotly" %in% rownames(utils::installed.packages())) {
    stop("This function requires the 'plotly' package - install it with install.packages(\"plotly\")", call. = FALSE)
  }
  
  plotly::ggplotly(plot) |>
    plotly::layout(...)
}

#' @importFrom ggplot2 is.ggplot
#' @rdname plotly
#' @export
plotly_style <- function(plot, ...) {
  if (is.ggplot(plot)) {
    plot <- as_plotly(plot)
  }
  plot |> 
    plotly::style(...)
}
