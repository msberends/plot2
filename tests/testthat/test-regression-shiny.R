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

# Regression tests for the Shiny app and its security.
# Each test_that() starts with the GitHub issue number.

test_that("#148 column names are never parsed as code", {
  skip_if_not_installed("patchwork")
  marker <- tempfile()
  nm <- sprintf('writeLines("x", "%s")', marker)
  d <- data.frame(a = c("p", "q", "p", "q"), g = c("m", "m", "f", "f"), n = 1:4)
  names(d)[1] <- nm
  p <- plot2(d, x = !!rlang::sym(nm), y = n, facet = g, type = "back-to-back")
  expect_false(file.exists(marker))
  m <- setup_aes(colour = sprintf('`a`+writeLines("x", "%s")', marker))
  expect_true(is.symbol(rlang::quo_get_expr(m$colour)))
  # non-syntactic names work, also in back-to-back plots
  names(d)[1] <- "my col"
  expect_s3_class(plot2(d, `my col`, n, facet = g, type = "back-to-back"), "patchwork")
  expect_identical(rlang::quo_get_expr(setup_aes(x = "`a b`")$x), as.name("a b"))
  expect_identical(rlang::eval_tidy(setup_aes(x = "2")$x), 2)
})

test_that("#148 a selection label is parsed only into column names", {
  expect_identical(column_selection_expr("c(a, b)"), quote(c(a, b)))
  expect_identical(column_selection_expr("a"), quote(a))
  expect_identical(column_selection_expr("my col"), as.name("my col"))
  expect_identical(column_selection_expr('c(a, system("x"))'), as.name('c(a, system("x"))'))
})
