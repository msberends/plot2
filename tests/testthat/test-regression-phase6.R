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

# Phase 6 of the review and overhaul: the Shiny app and its security.
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

test_that("#74 max_items counts the rest item, %p is the share of y, %n the number of merged values", {
  d <- data.frame(g = letters[1:5], v = c(50, 20, 10, 10, 10))
  p <- plot2(d, g, v, x.max_items = 3, x.max_txt = "rest (%p, %n)")
  expect_identical(levels(p$data$g), c("a", "b", "rest (30%, 3)"))
  expect_identical(p$data$v, c(50, 20, 30))
  # without a numeric y, the share of rows
  p <- plot2(data.frame(g = c("a", "a", "b", "c", "d")), g, x.max_items = 2, x.max_txt = "rest (%p)")
  expect_identical(levels(p$data$g), c("a", "rest (60%)"))
})
