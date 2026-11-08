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

#' `plot2()` Options
#' 
#' These are the package options for usage in [plot2()]. These can all be set using [options()], and their values can be retrieved using [getOption()].
#' @section Package Options: 
#' * `big_mark`\cr
#'   Default character for a thousands separator: `","`, `"."`, `" "` or `""`. Defaults to `"."` if the decimal mark is a comma, and `" "` otherwise
#' * `dec_mark`\cr
#'   Default character for a decimal separator: `","` or `"."`. Defaults to the [option][options()] `OutDec`, which is `"."` in a standard R session
#' * `plot2.colour`\cr
#'   Default colour to use in plots, defaults to `"ggplot2"` (the colours of `ggplot2` itself)
#' * `plot2.colour_background`\cr
#'   Default background colour to use in plots, defaults to `"white"`
#' * `plot2.colour_font_axis`\cr
#'   Default colour of the axis text (`axis.title.x` and `axis.title.y`), defaults to `"grey25"`
#' * `plot2.colour_font_primary`\cr
#'   Default colour of the plot title and plot tag, defaults to `"black"`
#' * `plot2.colour_font_secondary`\cr
#'   Default colour of the plot subtitle and caption, defaults to `"grey35"`
#' * `plot2.colour_sf`\cr
#'   Default colour of the borders in sf plots, defaults to `"grey50"`
#' * `plot2.colour_sf_fill`\cr
#'   Default colour of the area fills in sf plots, defaults to the option `plot2.colour`
#' * `plot2.default_type`\cr
#'   Default plot type when `type` is left blank and cannot be determined from the data, defaults to `"col"`. Any value of `type` can be used, such as `"line"` or `"l"`
#' * `plot2.font`\cr
#'   Default plot font family, defaults to the font of the graphics device. See the `font` argument of [plot2()]
#' * `plot2.language`\cr
#'   Language of the words that plot2 generates (such as automatic titles and the labels of `y.24h` and `y.age`) and of month and weekday names in date labels: `"en"` (default) or `"nl"` (Dutch). If not set, date labels follow the system locale.
#' * `plot2.silent`\cr
#'   A [logical] to hide the console messages when plotting (`TRUE`), default is `FALSE`. Messages are printed only in interactive sessions (and pkgdown builds), at the end of the [plot2()] call. They are signalled as R conditions of class `plot2_message` (and `plot2_caution` for cautions), so they can also be caught with [withCallingHandlers()] or hidden with [suppressMessages()]. Warnings are R warnings of class `plot2_warning` and are not hidden by this option.
#' * `plot2.text_factor`\cr
#'   Default factor to multiply all text sizes with, defaults to `1`
#' * `plot2.theme`\cr
#'   Default plot theme, defaults to `"theme_minimal2"` ([theme_minimal2()])
#' @rdname plot2-options
#' @name plot2-options
NULL
