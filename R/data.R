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

#' Example Geography Data Set: the Netherlands
#'
#' A data set containing the geometries of the twelve provinces of the Netherlands, according to Statistics Netherlands (2021).
#' @format A [data.frame] with `r format(nrow(netherlands), big.mark = ",")` observations and `r ncol(netherlands)` variables:
#' - `province`\cr name of the Dutch province
#' - `area_km2`\cr area in square kilometres
#' - `geometry`\cr geometry of the province, of class `r paste0(class(netherlands$geometry), collapse = "/")`
"netherlands"
