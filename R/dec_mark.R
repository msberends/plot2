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

#' Use Decimal Comma?
#' 
#' These functions determine which characters the decimal mark and big mark should be that are used in plotting. They base the determination on `getOption("OutDec")`, which is also what [base::format()] uses.
#' @details If the [option][options()] `"plot2.decimal.mark"` is set, that value will be used for [dec_mark()] if it is either a comma or a full stop.
#' 
#' At default, [big_mark()] returns a full stop if [dec_mark()] returns a comma, and a space otherwise. If the [option][options()] `"plot2.big.mark"` is set, that value will be used if it is either a comma (`","`) or a full stop (`"."`) or a space (`" "`) or an empty character (`""`).
#' @export
#' @examples
#' # at default, this follows `getOption("OutDec")`:
#' dec_mark()
#' # and big_mark() returns a space if dec_mark() returns ".":
#' big_mark()
#' 
#' # you you can set options to alter behaviour:
#' options(plot2.decimal.mark = ",", plot2.big.mark = ".")
#' dec_mark()
#' big_mark()
#' 
#' options(plot2.decimal.mark = ",", plot2.big.mark = NULL)
#' dec_mark()
#' big_mark()
#' 
#' options(plot2.big.mark = ",")
#' dec_mark()
#' big_mark()
#' 
#' # clean up
#' options(plot2.decimal.mark = NULL, plot2.big.mark = NULL)
dec_mark <- function() {
  option_set <- getOption("plot2.decimal.mark", default = NULL)
  if (!is.null(option_set)) {
    if (option_set %in% c(",", ".")) {
      return(option_set)
    }
    warning("Option 'plot2.decimal.mark' has an invalid value and was ignored. Use a \",\" or \".\".", call. = FALSE)
  }
  getOption("OutDec", default = ".")
}

#' @rdname dec_mark
#' @export
big_mark <- function() {
  option_set <- getOption("plot2.big.mark", default = NULL)
  if (!is.null(option_set)) {
    if (option_set %in% c(",", ".", "", " ")) {
      return(option_set)
    } else {
      warning("Option 'plot2.big.mark' has an invalid value and was ignored. Use a \",\" or \".\" or \" \" or \"\".", call. = FALSE)
    }
  }
  
  switch(dec_mark(),
         "," = ".",
         "." = " ")
}
