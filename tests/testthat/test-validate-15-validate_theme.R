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

# I002, R02 sheet 15: validate_theme()
# review_and_overhaul/reviews/R02_validate/15_validate_theme.md

panel_fill <- function(p) col_hex(theme_el(p, "panel.background")$fill)
el_class <- function(p, el) class(theme_el(p, el))[1]

test_that("15.01 theme_minimal2() by default; a theme as name, call, function or object (F005)", {
  expect_identical(el_class(plot2(fx_xy, x, y), "panel.grid.major.x"), "ggplot2::element_blank")
  for (t in list("theme_bw", "theme_bw()", ggplot2::theme_bw, ggplot2::theme_bw())) {
    p <- plot2(fx_xy, x, y, theme = t)
    expect_identical(col_hex(theme_el(p, "panel.border")$colour), "#333333")
  }
})

test_that("15.02 theme = NULL gives the grey theme", {
  expect_identical(panel_fill(plot2(fx_xy, x, y, theme = NULL)), "#EBEBEB")
})

test_that("15.03 an unknown theme name errors, an invalid object warns", {
  expect_error(plot2(fx_xy, x, y, theme = "theme_nonsense"), "unknown theme: theme_nonsense")
  expect_warning(plot2(fx_xy, x, y, theme = 42), "No valid ggplot2 theme", class = "plot2_warning")
})

test_that("15.04 a partial theme object is completed (F032)", {
  p <- expect_plot2(plot2(fx_xy, x, y, theme = ggplot2::theme(panel.background = ggplot2::element_rect(fill = "red"))))
  expect_identical(panel_fill(p), "#FF0000")
  expect_true(is.numeric(theme_el(p, "axis.text.x")$size))
})

test_that("15.05 background colours the panel and the plot", {
  p <- plot2(fx_xy, x, y, background = "lightyellow")
  expect_identical(panel_fill(p), "#FFFFE0")
  expect_identical(col_hex(theme_el(p, "plot.background")$fill), "#FFFFE0")
})

test_that("15.06 x.remove, y.remove and x.label_italic follow the orientation", {
  expect_identical(el_class(plot2(fx_cat_unique, x, y, x.remove = TRUE), "axis.text.x"), "ggplot2::element_blank")
  expect_identical(el_class(plot2(fx_cat_unique, x, y, y.remove = TRUE), "axis.text.y"), "ggplot2::element_blank")
  p <- plot2(fx_cat_unique, x, y, x.remove = TRUE, horizontal = TRUE)
  expect_identical(c(el_class(p, "axis.text.y"), el_class(p, "axis.text.x")), c("ggplot2::element_blank", "ggplot2::element_text"))
  expect_identical(theme_el(plot2(fx_cat_unique, x, y, x.label_italic = TRUE), "axis.text.x")$face, "italic")
  p <- plot2(fx_cat_unique, x, y, x.label_italic = TRUE, horizontal = TRUE)
  expect_identical(c(theme_el(p, "axis.text.y")$face, theme_el(p, "axis.text.x")$face), c("italic", "plain"))
})

test_that("15.07 x.label_angle rotates the category labels, also when horizontal (F051)", {
  el <- theme_el(plot2(fx_cat_unique, x, y, x.label_angle = 45), "axis.text.x")
  expect_identical(c(el$angle, el$hjust), c(45, 1))
  p <- plot2(fx_cat_unique, x, y, x.label_angle = 45, horizontal = TRUE)
  expect_identical(c(theme_el(p, "axis.text.y")$angle, theme_el(p, "axis.text.y")$hjust), c(45, 1))
  expect_identical(theme_el(p, "axis.text.x")$angle, 0)
})

test_that("15.08 title.colour and subtitle.colour", {
  p <- plot2(fx_xy, x, y, title = "T", subtitle = "S", title.colour = "red", subtitle.colour = "blue")
  expect_identical(col_hex(theme_el(p, "plot.title")$colour), "#FF0000")
  expect_identical(col_hex(theme_el(p, "plot.subtitle")$colour), "#0000FF")
})

test_that("15.09 facet strip settings", {
  p <- plot2(fx_facet, x, y, facet = facet, facet.colour_fill = "yellow", facet.bold = FALSE,
             facet.italic = TRUE, facet.size = 14, facet.margin = 10)
  expect_identical(col_hex(theme_el(p, "strip.background")$fill), "#FFFF00")
  st <- theme_el(p, "strip.text")
  expect_identical(st$face, "italic")
  expect_identical(st$size, 14)
  expect_equal(as.numeric(st$margin)[1], 10)
})

test_that("15.10 text_factor = 2 doubles all text sizes, also in theme_bw (F057)", {
  sizes <- function(p) c(theme_el(p, "axis.text.x")$size, theme_el(p, "plot.title")$size)
  expect_equal(sizes(plot2(fx_xy, x, y, text_factor = 2)), 2 * sizes(plot2(fx_xy, x, y)))
  bw <- ggplot2::theme_bw()
  expect_equal(sizes(plot2(fx_xy, x, y, theme = bw, text_factor = 2)), 2 * sizes(plot2(fx_xy, x, y, theme = bw)))
})

test_that("15.11 horizontal plots move the major grid lines to the other axis", {
  p <- plot2(fx_cat_unique, x, y)
  expect_identical(c(el_class(p, "panel.grid.major.x"), el_class(p, "panel.grid.major.y")), c("ggplot2::element_blank", "ggplot2::element_line"))
  p <- plot2(fx_cat_unique, x, y, horizontal = TRUE)
  expect_identical(c(el_class(p, "panel.grid.major.x"), el_class(p, "panel.grid.major.y")), c("ggplot2::element_line", "ggplot2::element_blank"))
})

test_that("15.12 with a secondary y-axis, the axis titles take the colours of their series", {
  d <- data.frame(x = c("a", "b", "c"), y = c(1, 2, 3), z = c(10, 30, 20))
  p <- plot2(d, x, y, y_secondary = z, colour = "blue", y_secondary.colour = "red")
  expect_identical(col_hex(theme_el(p, "axis.title.y.left")$colour), "#0000FF")
  expect_identical(col_hex(theme_el(p, "axis.title.y.right")$colour), "#FF0000")
})
