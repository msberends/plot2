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

# Not part of the CRAN core, which must finish within 1 minute (#50)
skip_on_cran()

# Code in diagnostics: arguments, values, columns and functions are formatted
# with the inline classes of cli as ggplot2 uses them, in plot2_cli_theme.

test_that("code in diagnostics is quoted with backticks in plain text", {
  expect_plot2_message(plot2(fx_xy), "Using `type = \"point\"` since both axes are numeric", fixed = TRUE)
  expect_identical(cli::ansi_strip(plot2:::font_code("type = \"", "point", "\"")), "`type = \"point\"`")
  expect_identical(cli::ansi_strip(plot2:::font_arg("x.limits")), "`x.limits`")
  expect_identical(cli::ansi_strip(plot2:::font_fn("sf::st_as_sf")), "`sf::st_as_sf()`")
  expect_identical(cli::ansi_strip(plot2:::font_var(c("a", "b"), collapse = NULL)), c("`a`", "`b`"))
})

test_that("code in diagnostics is green when the console supports colours", {
  withr::local_options(cli.num_colors = 256)
  expect_identical(plot2:::font_code("y = n()"), paste0("\033[32m`y = n()`\033[39m"))
  expect_identical(plot2:::font_arg("x"), paste0("\033[32m`x`\033[39m"))
})

test_that("the theme of plot2 does not change the user's own cli output", {
  withr::local_options(cli.num_colors = 256)
  plot2:::font_code("x = 1")
  expect_identical(cli::format_inline("{.code x = 1}"), "`x = 1`")
})

test_that("diagnostics with braces are printed as they are", {
  withr::local_options(plot2.silent = FALSE)
  withr::local_envvar(IN_PKGDOWN = "true")
  out <- cli::cli_fmt(plot2:::emit_diagnostic("Values {a} and {b}", "infos"))
  expect_match(paste(out, collapse = " "), "Values {a} and {b}", fixed = TRUE)
})
