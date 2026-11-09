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

# E07: sizes, widths and colour vectors

fx_focus <- data.frame(x = c("p", "q", "r"), y = c(1, 3, 2), category = c("b", "a", "c"))

test_that("E07.01 default point size 2 and column width 0.5", {
  expect_identical(unique(layer_df(plot2(fx_xy, x, y, type = "point"), "point")$size), 2)
  bars <- layer_df(plot2(fx_counts, group, n), "bar")
  expect_equal(unique(round(bars$xmax - bars$xmin, 6)), 0.5)
})

test_that("E07.02 spider plots have a transparent fill by default", {
  sp <- data.frame(dim = c("a", "b", "c", "d"), score = c(3, 5, 2, 4))
  expect_identical(toupper(as.character(unique(layer_df(plot2(sp, dim, score, type = "spider"), "polygon")$fill))), "#FFFFFF00")
})

test_that("E07.03 category.focus with an explicit colour greys out the rest", {
  v <- scale_values(plot2(fx_focus, x, y, category, category.focus = "a", colour = "red"), "fill")
  expect_identical(col_hex(v[["a"]]), "#FF0000")
  expect_true(all(col_hex(v[c("b", "c")]) == col_hex("grey85")))
})

test_that("E07.04 category.focus with default colours (#79)", {
  v <- scale_values(plot2(fx_focus, x, y, category, category.focus = "a"), "fill")
  expect_true(all(col_hex(v[c("b", "c")]) == col_hex("grey85")))
  expect_false(col_hex(v[["a"]]) == col_hex("grey85"))
})

test_that("E07.05 category.focus not found gives a plot2 warning", {
  expect_plot2_message(plot2(fx_cat_unique, x, y, category, category.focus = "zz", colour = "red"), "zz", type = "warning")
})

test_that("E07.06 category.focus = 2 selects the second category", {
  v <- scale_values(plot2(fx_focus, x, y, category, category.focus = 2, colour = "red"), "fill")
  expect_identical(names(v)[col_hex(v) == "#FF0000"], "b")
})

test_that("E07.07 the secondary y-axis series takes y_secondary.colour (with colour given)", {
  df <- data.frame(x = c("a", "b", "c"), y = c(1, 2, 3), z = c(10, 30, 20))
  p <- expect_plot2(plot2(df, x, y, y_secondary = z, y_secondary.type = "line", colour = "blue", y_secondary.colour = "red"))
  expect_identical(unique(col_hex(as.character(layer_df(p, "line")$colour))), "#FF0000")
})

test_that("E07.07b y_secondary.colour applies without colour (#89)", {
  df <- data.frame(x = c("a", "b", "c"), y = c(1, 2, 3), z = c(10, 30, 20))
  p <- plot2(df, x, y, y_secondary = z, y_secondary.type = "line", y_secondary.colour = "red")
  expect_identical(unique(col_hex(as.character(layer_df(p, "line")$colour))), "#FF0000")
})
