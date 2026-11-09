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

# Run once before all tests; everything is restored after the test run.
# Goal: identical results on every machine (locale, time zone, number
# formatting, plot2 options), independent of the user's settings.

withr::local_options(
  list(
    OutDec = ".",
    plot2.decimal.mark = NULL,
    plot2.big.mark = NULL,
    plot2.colour = NULL,
    plot2.colour_font_primary = NULL,
    plot2.colour_font_secondary = NULL,
    plot2.colour_background = NULL,
    plot2.colour_font_axis = NULL,
    plot2.colour_sf = NULL,
    plot2.colour_sf_fill = NULL,
    plot2.default_type = NULL,
    plot2.font = NULL,
    plot2.silent = NULL,
    plot2.text_factor = NULL,
    plot2.theme = NULL
  ),
  .local_envir = testthat::teardown_env()
)
withr::local_timezone("UTC", .local_envir = testthat::teardown_env())
withr::local_envvar(c(IN_PKGDOWN = ""), .local_envir = testthat::teardown_env())
withr::local_locale(c(LC_TIME = "C", LC_COLLATE = "C"), .local_envir = testthat::teardown_env())
# fonts downloaded by tests go to a temporary folder, not to the user cache (#23)
withr::local_envvar(c(R_USER_CACHE_DIR = file.path(tempdir(), "plot2-cache")), .local_envir = testthat::teardown_env())
