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

# R06 sheet T07: graphics devices
# review_and_overhaul/reviews/R06_themes/T07_graphics-devices.md

render_warnings <- function(device, p) {
  f <- tempfile()
  w <- character(0)
  withCallingHandlers({ device(f); print(p); grDevices::dev.off() },
                      warning = function(c) { w <<- c(w, conditionMessage(c)); invokeRestart("muffleWarning") })
  unique(w)
}
p_sci <- function() plot2(data.frame(x = c("a", "b"), y = c(1e7, 3e7)), x, y, y.scientific = TRUE)

test_that("T07.01 scientific labels do not depend on the console's Unicode support", {
  a <- withr::with_options(list(cli.unicode = TRUE), axis_labels(p_sci(), "y"))
  b <- withr::with_options(list(cli.unicode = FALSE), axis_labels(p_sci(), "y"))
  expect_identical(a, b)
})

test_that("T07.02 plot2's own labels render on the default pdf() device without encoding warnings", {
  expect_identical(render_warnings(function(f) grDevices::pdf(f), p_sci()), character(0))
})

test_that("T07.02b euros() labels render on the default pdf() device (F134)", {
  skip_open_finding("F134")
  p <- plot2(data.frame(x = c("a", "b"), y = c(1500, 2500)), x, y, datalabels.format = euros)
  expect_identical(render_warnings(function(f) grDevices::pdf(f), p), character(0))
})

test_that("T07.03 non-ASCII text renders without warnings on cairo, png and svg devices", {
  skip_if_not(capabilities("cairo"))
  p <- plot2(data.frame(x = c("\u20ac caf\u00e9", "\u03b1 \u2264 0.05"), y = 1:2), x, y, title = "Pr\u00e9valence \u2265 5%")
  expect_identical(render_warnings(function(f) grDevices::cairo_pdf(f), p), character(0))
  expect_identical(render_warnings(function(f) grDevices::png(f, type = "cairo"), p), character(0))
  expect_identical(render_warnings(function(f) grDevices::svg(f), p), character(0))
})
