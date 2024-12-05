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
#' @details If the [option][options()] `"dec_mark"` is set, that value will be used for [dec_mark()] if it is either a comma or a full stop.
#' 
#' At default, [big_mark()] returns a full stop if [dec_mark()] returns a comma, and a space otherwise. If the [option][options()] `"big_mark"` is set, that value will be used if it is either a comma (`","`) or a full stop (`"."`) or a space (`" "`) or an empty character (`""`).
#' @export
#' @examples
#' # at default, this follows `getOption("OutDec")`:
#' dec_mark()
#' # and big_mark() returns a space if dec_mark() returns ".":
#' big_mark()
#' 
#' # you you can set options to alter behaviour:
#' options(dec_mark = ",", big_mark = ".")
#' dec_mark()
#' big_mark()
#' 
#' options(dec_mark = ",", big_mark = NULL)
#' dec_mark()
#' big_mark()
#' 
#' options(big_mark = ",")
#' dec_mark()
#' big_mark()
#' 
#' # clean up
#' options(dec_mark = NULL, big_mark = NULL)
dec_mark <- function() {
  option_set <- getOption("dec_mark", default = NULL)
  if (!is.null(option_set)) {
    if (option_set %in% c(",", ".")) {
      return(option_set)
    } else {
      plot2_warning("Option 'dec_mark' has an invalid value and was ignored. Use a \",\" or \".\".")
    }
  } else {
    getOption("OutDec", default = ".")
  }
}

#' @rdname dec_mark
#' @export
big_mark <- function() {
  option_set <- getOption("big_mark", default = NULL)
  if (!is.null(option_set)) {
    if (option_set %in% c(",", ".", "", " ")) {
      return(option_set)
    } else {
      plot2_warning("Option 'big_mark' has an invalid value and was ignored. Use a \",\" or \".\" or \" \" or \"\".")
    }
  }
  
  switch(dec_mark(),
         "," = ".",
         "." = " ")
}
