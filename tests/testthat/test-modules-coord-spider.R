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

# coord_spider()

fx_radar <- data.frame(dim = c("a", "b", "c", "d"), score = c(3, 5, 2, 4))
radar <- function(..., data = fx_radar) {
  ggplot2::ggplot(data, ggplot2::aes(dim, score, group = 1)) +
    ggplot2::geom_polygon(fill = NA, colour = "black") +
    coord_spider(...)
}
# transformed (npc) coordinates of the polygon
radar_npc <- function(p) {
  b <- built(p)
  b$layout$coord$transform(b$data[[1]], b$layout$panel_params[[1]])
}

test_that("works on a plain ggplot2 plot; axis titles removed", {
  p <- expect_plot2(radar())
  expect_identical(coord_class(p), "CoordSpider")
  expect_true(label_text(p, "x") %in% c("", NA_character_))
})

test_that("all points lie within outer.radius", {
  d <- radar_npc(radar(outer.radius = 0.8))
  r <- sqrt((d$x - 0.5)^2 + (d$y - 0.5)^2) * 2
  expect_true(all(r <= 0.8 + 1e-8))
})

test_that("the first category lies on the vertical spoke for start = 0", {
  d <- radar_npc(radar())
  first <- d[which(d$x == d$x[1] & d$y == d$y[1])[1], ]
  a <- d[1, ]
  expect_equal(a$x, 0.5)
  expect_gt(a$y, 0.5)
})

test_that("zero maps to the centre for non-negative data", {
  d <- radar_npc(radar(data = transform(fx_radar, score = c(0, 5, 2, 4))))
  expect_equal(c(d$x[1], d$y[1]), c(0.5, 0.5))
})

test_that("outer.radius outside (0, 1] errors", {
  expect_error(coord_spider(outer.radius = 2))
  expect_error(coord_spider(outer.radius = 0))
})

test_that("theta = 'y' works", {
  p <- ggplot2::ggplot(fx_radar, ggplot2::aes(score, dim, group = 1)) +
    ggplot2::geom_polygon(fill = NA, colour = "black") +
    coord_spider(theta = "y")
  expect_plot2(p)
})

test_that("facets work in plain ggplot2 (#158)", {
  df <- rbind(transform(fx_radar, g = "p"), transform(fx_radar, g = "q"))
  expect_plot2(radar(data = df) + ggplot2::facet_wrap(~g))
})

test_that("radial labels follow start (#98)", {
  b <- built(radar(start = pi / 2))
  bg <- b$layout$coord$render_bg(b$layout$panel_params[[1]], ggplot2::theme_grey())
  txt <- Filter(function(k) inherits(k, "text"), bg$children)
  r_lbls <- txt[-seq_len(nrow(fx_radar))]
  pos <- vapply(r_lbls, function(k) paste(as.character(k$x), as.character(k$y)), "")
  expect_identical(length(unique(pos)), length(pos))
})
