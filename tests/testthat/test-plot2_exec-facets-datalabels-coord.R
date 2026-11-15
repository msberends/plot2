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

# facets, datalabels and coordinates

test_that("one panel per facet value; facet keeps its original name", {
  p <- expect_plot2(plot2(fx_facet, x, y, facet = facet))
  expect_facets(p, n_panels = 2L)
  expect_identical(names(plot_of(p)$facet$params$facets), "facet")
})

test_that("columns get label and text datalabel layers with the values", {
  p <- expect_plot2(plot2(fx_counts, group, n))
  expect_geoms(p, c("label", "text"))
  expect_setequal(datalabels_of(p), c("12", "7", "30", "1"))
})

test_that("horizontal plots use coord_flip(); clipping off with a title", {
  p <- plot2(fx_counts, group, n, horizontal = TRUE, title = "T")
  expect_identical(coord_class(p), "CoordFlip")
  expect_identical(plot_of(p)$coordinates$clip, "off")
})

test_that("vertical plot with datalabels and a title: clipping off", {
  p <- plot2(fx_counts, group, n, title = "T")
  expect_identical(coord_class(p), "CoordCartesian")
  expect_identical(plot_of(p)$coordinates$clip, "off")
})

test_that("vertical plot without title or facets: default coordinates", {
  p <- plot2(fx_counts, group, n)
  expect_identical(coord_class(p), "CoordCartesian")
  expect_identical(plot_of(p)$coordinates$clip, "on")
})

test_that("type = 'blank' has no datalabels", {
  expect_false("GeomText" %in% geoms(plot2(fx_counts, group, n, type = "blank")))
})

test_that("fixed or free x scales over facets are decided after the axis transformation", {
  d <- data.frame(x = c(10, 100, 1000, 30, 300, 3000), y = 1:6, f = rep(c("a", "b"), each = 3))
  x_ranges <- function(p) {
    b <- suppressMessages(ggplot2::ggplot_build(p))
    lapply(b$layout$panel_params, function(pp) round(pp$x.range, 6))
  }
  # maxima 1000 and 3000 differ much, but not on a log10 axis: one shared, valid scale
  ranges <- x_ranges(plot2(d, x, y, facet = f, x.transform = "log10"))
  expect_identical(ranges[[1]], ranges[[2]])
  expect_true(all(is.finite(unlist(ranges))))
  # without the transformation, the scales are free
  ranges <- x_ranges(plot2(d, x, y, facet = f, facet.repeat_labels_x = TRUE))
  expect_false(identical(ranges[[1]], ranges[[2]]))
  # fixed scales with a log axis do not start at zero (formerly an error)
  expect_plot2(plot2(d, x, y, facet = f, facet.fixed_x = TRUE, x.transform = "log10"))
  expect_true(is.na(plot2:::maxima_variation(c(0, 10), "log10")))
})

test_that("colour_transparency does not fade the lines of a smooth", {
  p <- plot2(iris, Sepal.Length, Sepal.Width, Species, smooth = TRUE, colour_transparency = 0.6)
  b <- suppressMessages(ggplot2::ggplot_build(p))
  is_smooth <- vapply(p$layers, function(l) inherits(l$geom, "GeomSmooth"), logical(1))
  expect_true(all(nchar(unique(b$data[[which(is_smooth)]]$colour)) == 7))
  expect_true(all(substr(unique(b$data[[which(!is_smooth)[1]]]$colour), 8, 9) == "66"))
  # the staged mapping refers to the readable column name (#49)
  expect_match(rlang::as_label(p$layers[[which(is_smooth)]]$mapping$colour), "Species")
})

test_that("smooth.method and smooth.formula are set as in ggplot2, so ggplot2 reports nothing", {
  p <- plot2(iris, Sepal.Length, Sepal.Width, Species, smooth = TRUE)
  is_smooth <- vapply(p$layers, function(l) inherits(l$geom, "GeomSmooth"), logical(1))
  expect_identical(p$layers[[which(is_smooth)]]$stat_params$method, "loess")
  expect_no_message(ggplot2::ggplot_build(p))
  p <- plot2(iris, Sepal.Length, Sepal.Width, smooth = TRUE, smooth.method = "lm")
  is_smooth <- vapply(p$layers, function(l) inherits(l$geom, "GeomSmooth"), logical(1))
  expect_identical(p$layers[[which(is_smooth)]]$stat_params$method, "lm")
})

test_that("a smooth with a numeric category is one neutral line on top, not one line per value", {
  d <- data.frame(x = rep(1:20, 3), y = rep(1:20, 3) + rep(0:2, each = 20), yr = rep(c(2000, 2010, 2020), each = 20))
  p <- plot2(d, x, y, yr, smooth = TRUE, category.character = FALSE)
  is_smooth <- vapply(p$layers, function(l) inherits(l$geom, "GeomSmooth"), logical(1))
  expect_identical(unname(which(is_smooth)), length(p$layers))
  b <- suppressMessages(ggplot2::ggplot_build(p))
  expect_identical(length(unique(b$data[[which(is_smooth)]]$group)), 1L)
  expect_identical(unique(b$data[[which(is_smooth)]]$colour), "grey35")
})

test_that("years in a continuous legend have no thousands separator", {
  d <- data.frame(x = 1:6, y = 1:6, yr = c(1990, 1995, 2000, 2005, 2010, 2015))
  p <- plot2(d, x, y, yr, category.character = FALSE)
  b <- suppressMessages(ggplot2::ggplot_build(p))
  labels <- b$plot$scales$get_scales("colour")$get_labels()
  expect_true(all(grepl("^[0-9]{4}$", labels[!is.na(labels)])))
})

test_that("a smooth beyond the shared y limits of facets is kept, not removed with a warning", {
  # the fitted line ends at about 9.6, above the highest value of 9
  d <- data.frame(x = rep(1:10, 2), y = rep(c(1:9, 8.5), 2), f = rep(c("a", "b"), each = 10))
  p <- plot2(d, x, y, facet = f, smooth = TRUE, smooth.method = "lm", facet.fixed_y = TRUE)
  expect_no_warning(ggplot2::ggplotGrob(p))
})
