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

# R03 sheet E03: type shortcuts and early normalisation
# review_and_overhaul/reviews/R03_plot2_exec/E03_type-shortcuts.md

test_that("E03.01 barpercent: horizontal, sorted by frequency, labels n (p%)", {
  p <- expect_plot2(plot2(fx_counts, group, n, type = "barpercent"))
  expect_identical(coord_class(p), "CoordFlip")
  expect_setequal(datalabels_of(p), c("30 (60%)", "12 (24%)", "7 (14%)", "1 (2%)"))
  # freq-desc: the largest group first, i.e. at the top of a flipped plot
  expect_identical(axis_labels(p, "y")[4], "c")
})

test_that("E03.02 barpercent: at most 10 items plus a rest group by default", {
  df <- data.frame(g = letters[1:12], n = 12:1)
  p <- plot2(df, g, n, type = "bp")
  expect_lte(length(unique(layer_df(p, 1)$x)), 11)
})

test_that("E03.03 'bar' gives horizontal columns", {
  p <- expect_plot2(plot2(fx_counts, group, n, type = "bar"))
  expect_geoms(p, "bar")
  expect_identical(coord_class(p), "CoordFlip")
})

test_that("E03.04 linedot and its aliases: a line and two point layers", {
  for (t in c("linedot", "ld", "line-dot", "pointline")) {
    p <- plot2(fx_xy, x, y, type = t)
    expect_identical(geoms(p)[1:3], c("GeomLine", "GeomPoint", "GeomPoint"), label = t)
  }
})

test_that("E03.05 dumbbell: points and segments, horizontal", {
  p <- expect_plot2(plot2(fx_cat_unique, x, y, category, type = "d"))
  expect_geoms(p, c("point", "segment"))
  expect_identical(coord_class(p), "CoordFlip")
})

test_that("E03.06 sankey: parallel-sets layers", {
  skip_if_not_installed("ggforce")
  sk <- data.frame(from = c("A", "A", "B", "B"), to = c("X", "Y", "X", "Y"), n = c(5, 3, 2, 6), grp = c("p", "q", "p", "q"))
  p <- expect_plot2(plot2(sk, x = c(from, to), y = n, category = grp, type = "s"))
  expect_geoms(p, c("GeomShape", "GeomParallelSetsAxes"))
})

test_that("E03.07 spider and radar: polygon in CoordSpider", {
  sp <- data.frame(dim = c("a", "b", "c", "d"), score = c(3, 5, 2, 4))
  for (t in c("spider", "radar", "sp")) {
    p <- plot2(sp, dim, score, type = t)
    expect_true("GeomPolygon" %in% geoms(p), label = t)
    expect_identical(coord_class(p), "CoordSpider", label = t)
  }
})

test_that("E03.08 upset: patchwork of four plots", {
  up <- data.frame(a = c(TRUE, TRUE, FALSE), b = c(TRUE, FALSE, TRUE))
  p <- plot2(up, x = c(a, b), type = "u")
  expect_s3_class(p, "patchwork")
  expect_length(p, 4)
})

test_that("E03.09 'errorbar' is not turned into horizontal columns (F009)", {
  df <- data.frame(x = c("a", "b"), y = c(2, 3))
  # without ymin and ymax this plot cannot be drawn, so it is not rendered here
  expect_message(p <- plot2(df, x, y, type = "errorbar"), "loosely supported", class = "plot2_caution")
  expect_false(identical(coord_class(p), "CoordFlip"))
  expect_true("GeomErrorbar" %in% geoms(p))
})

test_that("E03.10 decimal.mark equal to big.mark: big mark becomes a space", {
  p <- plot2(data.frame(x = c("a", "b"), y = c(12345.5, 56789.25)), x, y, decimal.mark = ",", big.mark = ",")
  labs <- axis_labels(p, "y")
  expect_true(any(grepl(" ", labs[nchar(labs) > 4])))
})

test_that("E03.11 category.type 'color' is accepted; invalid values error", {
  p <- plot2(fx_cat_unique, x, y, category, category.type = "color", type = "point")
  expect_true(all(c("colour", "fill") %in% names(plot_of(p)$mapping)))
  expect_error(plot2(fx_cat_unique, x, y, category, category.type = "colours"), "category.type")
})

test_that("E03.12 a non-character type errors", {
  expect_error(plot2(fx_xy, x, y, type = 1), "must be a character")
})
