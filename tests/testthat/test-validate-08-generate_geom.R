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

# I002, R02 sheet 08: generate_geom()
# review_and_overhaul/reviews/R02_validate/08_generate_geom.md

# several observations per x and category, for distribution types
d_dist <- data.frame(x = rep(c("a", "b"), each = 6),
                     y = c(1, 2, 3, 4, 5, 6, 2, 3, 4, 5, 6, 7),
                     category = rep(c("u", "v"), 6))
d_tile <- data.frame(x = rep(c("a", "b"), 2), y = rep(c("p", "q"), each = 2), v = 1:4)
d_hist <- data.frame(v = c(1, 2, 2, 3, 5, 6, 6, 7))

test_that("08.01 every supported type gets its geom and builds without warnings", {
  expected <- list(col = "GeomBar", bar = "GeomBar", line = "GeomLine", point = "GeomPoint",
                   jitter = "GeomPoint", boxplot = "GeomBoxplot", violin = "GeomViolin",
                   area = "GeomArea", linedot = c("GeomLine", "GeomPoint"),
                   dumbbell = c("GeomSegment", "GeomPoint"), blank = "GeomBlank")
  for (t in names(expected)) {
    p <- expect_plot2(plot2(d_dist, x, y, category, type = t))
    expect_geoms(p, expected[[t]])
  }
  expect_geoms(expect_plot2(plot2(d_hist, v, type = "histogram")), "GeomBar")
  expect_geoms(expect_plot2(plot2(d_hist, v, type = "density")), "GeomDensity")
  expect_geoms(expect_plot2(plot2(d_tile, x, y, v, type = "tile")), "GeomTile")
  expect_geoms(expect_plot2(plot2(d_tile, x, y, v, type = "raster")), "GeomRaster")
})

test_that("08.02 columns: width 0.5, dodged by category, stacked, filled and reversed on request", {
  p <- expect_plot2(plot2(fx_cat, x, y, category))
  expect_identical(layer_params(p, "bar")$width, 0.5)
  expect_identical(positions(p)[layer_index(p, "bar")], "PositionDodge2")
  p <- expect_plot2(plot2(fx_cat, x, y, category, stacked = TRUE))
  expect_identical(positions(p)[layer_index(p, "bar")], "PositionStack")
  p <- expect_plot2(plot2(fx_cat, x, y, category, stacked_fill = TRUE))
  expect_identical(positions(p)[layer_index(p, "bar")], "PositionFill")
  p <- expect_plot2(plot2(fx_cat, x, y, category, stacked = TRUE, reverse = TRUE))
  expect_true(plot_of(p)$layers[[layer_index(p, "bar")]]$position$reverse)
})

test_that("08.03 areas with a category are not dodged when not stacked", {
  p <- expect_plot2(plot2(fx_cat, x, y, category, type = "area"))
  expect_false(positions(p)[layer_index(p, "area")] == "PositionDodge2")
})

test_that("08.04 lines have round ends; linetype and linewidth are honoured", {
  p <- expect_plot2(plot2(fx_cat, x, y, category, type = "line", linetype = 2, linewidth = 2))
  prm <- layer_params(p, "line")
  expect_identical(prm$lineend, "round")
  expect_identical(prm$linetype, 2)
  expect_identical(prm$linewidth, 2)
})

test_that("08.05 points have size 2; jitter_seed gives reproducible positions", {
  expect_identical(layer_params(expect_plot2(plot2(fx_cat, x, y, category, type = "point")), "point")$size, 2)
  p1 <- plot2(fx_cat, x, y, type = "jitter", jitter_seed = 1)
  p2 <- plot2(fx_cat, x, y, type = "jitter", jitter_seed = 1)
  expect_identical(layer_df(p1)$x, layer_df(p2)$x)
})

test_that("08.06 boxplots have whiskers, semi-transparent outliers and a white fill with a category", {
  p <- expect_plot2(plot2(d_dist, x, y, category, type = "boxplot"))
  expect_geoms(p, c("GeomErrorbar", "GeomBoxplot"))
  prm <- layer_params(p, "boxplot")
  expect_identical(prm$fill, "white")
  expect_identical(plot_of(p)$layers[[layer_index(p, "boxplot")]]$geom_params$outlier_gp$alpha, 0.75)
})

test_that("08.07 violins draw quartiles and honour violin_scale", {
  p <- expect_plot2(plot2(d_dist, x, y, type = "violin", violin_scale = "width"))
  prm <- layer_params(p, "violin")
  expect_identical(prm$scale, "width")
  expect_identical(prm$quantiles, c(0.25, 0.5, 0.75))
})

test_that("08.07b violins with groups of one value do not error", {
  expect_plot2(plot2(fx_cat, x, y, category, type = "violin"), allow_warnings = TRUE)
})

test_that("08.08 histograms: automatic binwidth with a message, binwidth honoured, constant data works", {
  expect_plot2_message(plot2(d_hist, v, type = "histogram"), "binwidth = 0.488", type = "info")
  expect_identical(layer_params(expect_plot2(plot2(d_hist, v, type = "histogram", binwidth = 2)), "bar")$binwidth, 2)
  expect_plot2(plot2(data.frame(v = c(3, 3, 3)), v, type = "histogram"))
})

test_that("08.09 density ignores y, with a message", {
  p <- expect_plot2_message(plot2(fx_xy, x, y, type = "density"), "Ignoring y", type = "info")
  expect_geoms(p, "density", exact = TRUE)
})

test_that("08.10 tile and raster use the category as fill", {
  for (t in c("tile", "raster")) {
    p <- expect_plot2(plot2(d_tile, x, y, v, type = t))
    expect_identical(rlang::as_label(plot_of(p)$mapping$fill), "v")
  }
})

test_that("08.11 an extra argument in ... reaches the layer, with a message", {
  p <- expect_plot2_message(plot2(fx_cat, x, y, alpha = 0.3), "additional argument was given to the geom: alpha")
  expect_identical(layer_params(p, "bar")$alpha, 0.3)
})

test_that("08.12 a loosely supported geom gives a caution and builds without warnings", {
  expect_plot2_message(plot2(fx_cat, x, y, category, type = "step"), "only loosely supported")
  expect_plot2(plot2(fx_cat, x, y, category, type = "step"))
})

test_that("08.13 beeswarm uses ggbeeswarm", {
  skip_if_not_installed("ggbeeswarm")
  p <- expect_plot2(plot2(d_dist, x, y, type = "beeswarm"))
  # ggbeeswarm draws points with a beeswarm position
  expect_geoms(p, "point", exact = TRUE)
  expect_s3_class(p$layers[[1]]$position, "PositionBeeswarm")
})
