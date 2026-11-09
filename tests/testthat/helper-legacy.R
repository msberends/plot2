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

# Data and accessors of the legacy smoke tests in test-legacy-*.R (#50).

plotdata <- data.frame(x = seq_len(8) + 10,
                       x_char = letters[1:8],
                       x_date = Sys.Date() - seq_len(8),
                       n = seq_len(8),
                       stringsAsFactors = FALSE)

get_mapping <- function(plot) gsub("~", "", sapply(plot$mapping, deparse))
get_layers <- function(plot) plot$layers
get_labels <- function(plot) unlist(plot$labels)
get_data <- function(plot) plot$data
get_range_x <- function(plot) {
  ggplot2::ggplot_build(plot)$layout$panel_scales_x[[1]]$limits %or% 
    ggplot2::ggplot_build(plot)$layout$panel_scales_x[[1]]$range$range
}
get_range_y <- function(plot) {
  ggplot2::ggplot_build(plot)$layout$panel_scales_y[[1]]$limits %or%
    ggplot2::ggplot_build(plot)$layout$panel_scales_y[[1]]$range$range
}
