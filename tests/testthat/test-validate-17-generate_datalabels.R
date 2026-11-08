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

# I002, R02 sheet 17: generate_datalabels()
# review_and_overhaul/reviews/R02_validate/17_generate_datalabels.md

test_that("17.01 columns get a label and a text layer, with the formatted y values", {
  p <- plot2(fx_counts, group, n)
  expect_geoms(p, c("label", "bar", "text"), exact = TRUE)
  expect_identical(datalabels_of(p), c("12", "7", "30", "1"))
})

test_that("17.02 more than 25 bars: no labels with a caution; datalabels = TRUE forces them", {
  d26 <- data.frame(g = sprintf("g%02d", 1:26), v = 1:26)
  expect_plot2_message(plot2(d26, g, v), "Omitting printing of 26 datalabels", type = "caution")
  expect_geoms(plot2(d26, g, v), "bar", exact = TRUE)
  expect_length(datalabels_of(plot2(d26, g, v, datalabels = TRUE)), 26)
  expect_geoms(plot2(d26[1:25, ], g, v), "text")
})

test_that("17.03 stacked: labels in the middle of their segments", {
  p <- plot2(fx_cat_unique, x, y, category, stacked = TRUE)
  b <- layer_df(p, "bar")
  expect_equal(sort(layer_df(p, "text")$y), sort((b$ymin + b$ymax) / 2))
})

test_that("17.04 stacked_fill: labels in the middle of their segments", {
  p <- plot2(fx_cat_unique, x, y, category, stacked_fill = TRUE)
  b <- layer_df(p, "bar")
  expect_equal(sort(layer_df(p, "text")$y), sort((b$ymin + b$ymax) / 2))
})

test_that("17.05 horizontal: labels beyond the end of the bars", {
  p <- plot2(fx_counts, group, n, horizontal = TRUE)
  t <- layer_df(p, "text")
  expect_s3_class(p$coordinates, "CoordFlip")
  expect_equal(sort(t$y), sort(layer_df(p, "bar")$ymax))
  # anchored at the bar, with a fixed padding instead of a gap relative to the label length (F140)
  expect_identical(unique(t$hjust), 0)
  expect_true(all(startsWith(t$label, "  ")))
})

test_that("17.06 rotated labels are anchored at the bar with the same gap for every length (F140)", {
  d <- data.frame(g = c("a", "b", "c"), n = c(1, 4567, 1234567))
  t90 <- layer_df(plot2(d, g, n, datalabels.angle = 90), "text")
  t270 <- layer_df(plot2(d, g, n, datalabels.angle = 270), "text")
  t45 <- layer_df(plot2(d, g, n, datalabels.angle = 45), "text")
  t270r <- layer_df(plot2(d, g, n, datalabels.angle = 270, y.transform = "reverse"), "text")
  expect_identical(unique(t90$angle), 90)
  expect_identical(unique(t270$angle), 270)
  # upwards text starts at the bar, downwards text ends at it; a reversed axis mirrors this
  expect_identical(unique(c(t90$hjust, t45$hjust, t270r$hjust)), 0)
  expect_identical(unique(t270$hjust), 1)
  expect_identical(unique(c(t90$vjust, t270$vjust)), 0.5)
  # the gap is a fixed padding on the side of the bar, not a share of the label length
  expect_true(all(startsWith(t90$label, "  ")))
  expect_true(all(endsWith(t270$label, "  ")))
  # unrotated labels above vertical bars are unchanged
  t0 <- layer_df(plot2(d, g, n), "text")
  expect_identical(unique(t0$hjust), 0.5)
  expect_false(any(startsWith(t0$label, " ")))
})

test_that("17.07 colour, fill, size and line height of the datalabels", {
  p <- plot2(fx_counts, group, n, datalabels.colour = "red", datalabels.colour_fill = "yellow",
             datalabels.size = 5, datalabels.lineheight = 2)
  t <- layer_df(p, "text")
  expect_identical(unique(col_hex(t$colour)), "#FF0000")
  expect_identical(unique(col_hex(layer_df(p, "label")$fill)), "#FFFF00")
  expect_identical(unique(t$size), 5)
  expect_identical(unique(t$lineheight), 2)
})

test_that("17.08 tile plots: labels from the category, centred", {
  d <- data.frame(x = rep(c("a", "b"), 2), y = rep(c("p", "q"), each = 2), v = c(1, 2, 3, 4))
  t <- layer_df(plot2(d, x, y, v, type = "tile"), "text")
  expect_setequal(t$label, c("1", "2", "3", "4"))
  expect_identical(unique(c(t$hjust, t$vjust)), 0.5)
})

test_that("17.09 the background label layer comes before the text layer", {
  g <- geoms(plot2(fx_counts, group, n))
  expect_lt(match("GeomLabel", g), match("GeomText", g))
})

test_that("17.10 replacing the plot data afterwards updates the labels (F055)", {
  p <- plot2(fx_counts, group, n)
  new <- p$data
  new$n <- new$n * 2
  new$datalabels <- new$n
  q <- p + new
  expect_identical(datalabels_of(q), c("24", "14", "60", "2"))
  expect_identical(layer_df(q, "label")$label, c("24-", "14-", "60-", "2-"))
})

test_that("17.11 sf: labels at centroids, or on the surface with datalabels.centroid = FALSE", {
  skip_if_not_installed("sf")
  fun_text <- function(p) paste(deparse(p$layers[[layer_index(p, "GeomLabel")]]$stat_params$fun.geometry), collapse = " ")
  expect_match(fun_text(plot2(netherlands)), "st_centroid")
  expect_match(fun_text(plot2(netherlands, datalabels.centroid = FALSE)), "st_point_on_surface")
})

test_that("17.12 rotated datalabels get room for their length on the value axis (F143)", {
  d <- data.frame(g = c("a", "b", "c"), n = c(1, 4567, 1234567))
  upper <- function(p) scale_of(p, "y")$expand[3]
  expect_gt(upper(plot2(d, g, n, datalabels.angle = 90)), upper(plot2(d, g, n)))
  expect_equal(upper(plot2(d, g, n, datalabels.angle = 90)), upper(plot2(d, g, n, datalabels.angle = 270)))
  # a reversed axis gets the room on its other side
  expect_gt(scale_of(plot2(d, g, n, datalabels.angle = 270, y.transform = "reverse"), "y")$expand[1], 0.25)
  # short labels keep the default room; a set y.expand is respected
  expect_equal(upper(plot2(data.frame(g = c("a", "b"), n = c(1, 2)), g, n, datalabels.angle = 90)), 0.25)
  expect_equal(upper(plot2(d, g, n, datalabels.angle = 90, y.expand = 0.1)), 0.1)
})
