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

# get_colour() methods and colour registration

local_registered <- function(..., .env = parent.frame()) {
  before <- plot2:::plot2_env$reg_cols
  suppressMessages(register_colour(...))
  withr::defer(assign("reg_cols", before, envir = plot2:::plot2_env), envir = .env)
}

test_that("class, names and case-insensitive R colour names", {
  x <- get_colour(c(a = "red", b = "Blue"))
  expect_s3_class(x, "colour")
  expect_identical(names(x), c("a", "b"))
  expect_identical(as.character(unname(x)), c("#FF0000", "#0000FF"))
})

test_that("c(), rev() and rep() keep the class", {
  x <- get_colour(c(a = "red", b = "blue"))
  expect_s3_class(c(x, get_colour("green")), "colour")
  expect_identical(names(rev(x)), c("b", "a"))
  expect_s3_class(rep(x, 2), "colour")
  expect_length(rep(x, 2), 4)
})

test_that("vctrs and tibble keep the class", {
  skip_if_not_installed("vctrs")
  x <- get_colour(c("red", "blue"))
  expect_s3_class(vctrs::vec_c(x, x), "colour")
  expect_s3_class(dplyr::tibble(col = x)$col, "colour")
})

test_that("register a colour and a set; usable in plot2()", {
  local_registered(test_brand = "#123456", test_set = c("red", "blue"))
  expect_identical(as.character(get_colour("test_brand")), "#123456")
  expect_identical(as.character(get_colour("test_set", 2)), c("#FF0000", "#0000FF"))
  p <- plot2(fx_counts, group, n, colour = "test_brand")
  expect_identical(unique(col_hex(as.character(layer_df(p, "bar")$fill))), "#123456")
})

test_that("registration requires names; unregister by pattern", {
  expect_error(register_colour("red"), "must be named")
  local_registered(test_tmp_a = "red", test_tmp_b = "blue")
  expect_message(unregister_colour("^test_tmp_"), "2 colours unregistered")
  expect_false(any(grepl("^test_tmp_", names(plot2:::plot2_env$reg_cols))))
})

test_that("registered colours survive the internal clean-up", {
  local_registered(test_keep = "#ABCDEF")
  plot2:::clear_plot2_env()
  expect_true("test_keep" %in% names(plot2:::plot2_env$reg_cols))
})

test_that("one invalid colour does not replace the valid ones (#100)", {
  expect_warning(x <- get_colour(c(a = "red", b = "nope", c = "blue")), "nope", class = "plot2_warning")
  expect_length(x, 3)
  expect_identical(as.character(unname(x[c(1, 3)])), c("#FF0000", "#0000FF"))
})

test_that("registering an invalid colour errors (#100)", {
  expect_error(register_colour(test_bad = "notacolour"))
})

test_that("subsetting keeps the class (#102)", {
  expect_s3_class(get_colour(c("red", "blue"))[1], "colour")
})
