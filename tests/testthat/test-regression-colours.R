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

# Regression tests for colours.
# Each test_that() starts with the GitHub issue number.

test_that("#28 short hex colours are expanded each on their own; invalid hex codes are rejected", {
  expect_identical(as.character(get_colour(c("#f0a", "#123", "abc"))), c("#FF00AA", "#112233", "#AABBCC"))
  expect_identical(toupper(as.character(get_colour("#AbCdEf"))), "#ABCDEF")
  # a real warning since #100
  expect_warning(plot2(fx_cat, x, y, colour = "#12:4;6"), "Invalid colour", class = "plot2_warning")
})

test_that("#79 category.focus keeps the focused category's own colour and greys the rest", {
  d <- data.frame(x = c("a", "b", "c"), y = 1:3, k = c("a", "b", "c"))
  unfocused <- layer_df(expect_plot2(plot2(d, x, y, k)), "bar")$fill
  focused <- layer_df(expect_plot2(plot2(d, x, y, k, category.focus = "b")), "bar")$fill
  expect_identical(unname(col_hex(as.character(focused[2]))), unname(col_hex(as.character(unfocused[2]))))
  expect_identical(col_hex(focused[c(1, 3)]), rep(col_hex("grey85"), 2))
})

test_that("#100 invalid colours are replaced one by one with a warning; registration is strict and names are case-insensitive", {
  expect_warning(x <- get_colour(c(a = "red", b = "nope", c = "blue")), "nope", class = "plot2_warning")
  expect_identical(names(x), c("a", "b", "c"))
  expect_identical(as.character(unname(x)), c("#FF0000", "#595959", "#0000FF"))
  expect_error(register_colour(test_bad = "notacolour"), "test_bad")
  suppressMessages(register_colour(test_red = "#AA0000"))
  withr::defer(suppressMessages(unregister_colour("^test_red$")))
  expect_identical(as.character(get_colour("TEST_RED")), "#AA0000")
})

test_that("#102 subsetting a colour vector keeps the class and the names", {
  cols <- get_colour(c(a = "red", b = "blue"))
  expect_s3_class(cols[1], "colour")
  expect_s3_class(cols[[2]], "colour")
  expect_identical(names(cols[2]), "b")
  expect_identical(unname(as.character(cols[2:1])), c("#0000FF", "#FF0000"))
})

test_that("#108 the 'ggplot2' palette gives ggplot2's own colours, also with an opacity", {
  for (n in 2:5) expect_identical(unname(as.character(get_colour("ggplot2", n))), toupper(scales::hue_pal()(n)))
  d <- data.frame(x = rep(c("a", "b"), 2), y = 1:4, k = rep(c("u", "v"), each = 2))
  plain <- layer_df(expect_plot2(plot2(d, x, y, k)), "bar")$fill
  transparent <- layer_df(expect_plot2(plot2(d, x, y, k, colour_transparency = 0.5)), "bar")$fill
  expect_identical(unname(substr(as.character(transparent), 1, 7)), toupper(plain))
})
