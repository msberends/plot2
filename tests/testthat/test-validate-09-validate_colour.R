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

# I002, R02 sheet 09: validate_colour()
# review_and_overhaul/reviews/R02_validate/09_validate_colour.md

d_three <- data.frame(x = c("a", "b", "c"), y = 1:3, k = c("a", "b", "c"))

test_that("09.01 without a category, columns get the first ggplot2 colour of plot2", {
  p <- expect_plot2(plot2(fx_cat, x, y))
  expect_identical(unname(col_hex(unique(layer_df(p, "bar")$fill))), unname(col_hex(as.character(get_colour("ggplot2", 1)))))
})

test_that("09.02 colour = 'red' on columns colours both fill and outline", {
  p <- expect_plot2(plot2(fx_cat, x, y, colour = "red"))
  expect_identical(unname(col_hex(unique(layer_df(p, "bar")$fill))), "#FF0000")
  expect_identical(unname(col_hex(unique(layer_df(p, "bar")$colour))), "#FF0000")
})

test_that("09.03 boxplots without a category get a white fill and a dark outline", {
  d <- data.frame(x = rep(c("a", "b"), each = 6), y = c(1:6, 2:7))
  p <- expect_plot2(plot2(d, x, y, type = "boxplot"))
  df <- layer_df(p, "boxplot")
  expect_identical(unique(df$fill), "white")
  expect_lt(sum(grDevices::col2rgb(unique(df$colour))), sum(grDevices::col2rgb("white")))
})

test_that("09.04 a palette name gives one distinct colour per category", {
  p <- expect_plot2(plot2(fx_cat, x, y, category, colour = "viridis"))
  expect_scale_values(p, "fill", c(u = "#440154", v = "#FDE725"))
})

test_that("09.05 a named colour vector maps names to categories regardless of order", {
  p <- expect_plot2(plot2(fx_cat, x, y, category, colour = c(v = "red", u = "blue")))
  expect_scale_values(p, "fill", c(u = "#0000FF", v = "#FF0000"))
})

test_that("09.06 fewer colours than categories are recycled or give a clear warning", {
  expect_plot2_message(plot2(d_three, x, y, k, colour = c("red", "blue")), "recycled", type = "warning")
})

test_that("09.07 colour_opacity adds transparency, also for palette names", {
  p <- expect_plot2(plot2(fx_cat, x, y, category, colour = c("red", "blue"), colour_opacity = 0.5))
  expect_true(all(nchar(scale_values(p, "fill")) == 9))
  p <- expect_plot2(plot2(fx_cat, x, y, category, colour_opacity = 0.5))
  expect_true(all(nchar(scale_values(p, "fill")) == 9))
})

test_that("09.08 horizontal = TRUE reverses the colours so the legend follows the bars", {
  normal <- scale_values(plot2(fx_cat, x, y, category), "fill")
  flipped <- scale_values(expect_plot2(plot2(fx_cat, x, y, category, horizontal = TRUE)), "fill")
  expect_identical(names(flipped), rev(names(normal)))
  expect_identical(unname(flipped[["u"]]), unname(normal[["v"]]))
})

test_that("09.09 a registered colour set with a numeric category gives a gradient through its colours", {
  register_colour(testset_0909 = c("#111111", "#555555", "#999999", "#DDDDDD"))
  withr::defer(unregister_colour("testset_0909"))
  p <- expect_plot2(plot2(fx_numcat, x, y, value, colour = "testset_0909"))
  expect_identical(unname(col_hex(scale_of(p, "colour")$map(0.9))), "#DDDDDD")
})

test_that("09.10 category.focus colours the focused category and greys the others", {
  p <- expect_plot2(plot2(d_three, x, y, k, category.focus = "b"))
  v <- scale_values(p, "fill")
  expect_identical(unname(col_hex(v[c("a", "c")])), rep(unname(col_hex("grey85")), 2))
  expect_false(col_hex(v[["b"]]) == col_hex("grey85"))
})

test_that("09.11 an invalid colour name is reported and replaced by the default colours", {
  p <- expect_plot2_message(plot2(fx_cat, x, y, category, colour = "notacolourxyz"), "Invalid colour set")
  expect_identical(length(unique(scale_values(p, "fill"))), 2L)
})
