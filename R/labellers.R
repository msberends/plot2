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

#' Label Currencies
#' 
#' Format numbers as currency, rounding values to dollars or cents using a convenient heuristic.
#' @details The euro sign of [euros()] cannot be encoded by the default [pdf()][grDevices::pdf()] device. To save such plots as PDF, use the Cairo device, e.g. `ggplot2::ggsave("plot.pdf", device = grDevices::cairo_pdf)`, or in R Markdown the chunk option `dev = "cairo_pdf"`.
#' @param x Values.
#' @inheritParams plot2
#' @name labellers
#' @rdname labellers
#' @export
#' @examples
#' profit <- data.frame(group = LETTERS[1:4],
#'                      profit = runif(4, 10000, 25000))
#' 
#' profit |>
#'   plot2(y.labels = euros,
#'         datalabels = FALSE)
#'         
#' profit |>
#'   plot2(y.labels = euros,
#'         datalabels.format = euros)
#' 
#' dollars(c(1500, 2499.5))
euros <- function(x,
                  big.mark = big_mark(),
                  decimal.mark = dec_mark(),
                  ...) {
  if (all(is.finite(x) & (x == round(x)), na.rm = TRUE)) {
    # no cents
    trimws(paste0("\u20ac ", trimws(format(round(x, 0), decimal.mark = decimal.mark, big.mark = big.mark, scientific = FALSE))))
  } else {
    trimws(paste0("\u20ac ", trimws(format(round(x, 2), decimal.mark = decimal.mark, big.mark = big.mark, nsmall = 2, scientific = FALSE))))
  }
}

#' @rdname labellers
#' @export
dollars <- function(x,
                    big.mark = big_mark(),
                    decimal.mark = dec_mark(),
                    ...) {
  if (all(is.finite(x) & (x == round(x)), na.rm = TRUE)) {
    # no cents
    trimws(paste0("$", trimws(format(round(x, 0), decimal.mark = decimal.mark, big.mark = big.mark, scientific = FALSE))))
  } else {
    trimws(paste0("$", trimws(format(round(x, 2), decimal.mark = decimal.mark, big.mark = big.mark, nsmall = 2, scientific = FALSE))))
  }
}
