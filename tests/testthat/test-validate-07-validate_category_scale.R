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

# I002, R02 sheet 07: validate_category_scale()
# review_and_overhaul/reviews/R02_validate/07_validate_category_scale.md

cat_cols <- function(p, aes = "colour", at = c(0.1, 0.5, 0.9)) {
  unname(col_hex(scale_of(p, aes)$map(at)))
}

test_that("07.01 a numeric category with default colours gives a colour bar on the right", {
  p <- expect_plot2(plot2(fx_numcat, x, y, value))
  expect_s3_class(scale_of(p, "colour"), "ScaleContinuous")
  expect_true(inherits(scale_of(p, "colour")$guide, "GuideColourbar") || identical(scale_of(p, "colour")$guide, "colourbar"))
  expect_identical(legend_position(p), "right")
})

test_that("07.02 one colour gives a gradient from white", {
  p <- expect_plot2(plot2(fx_numcat, x, y, value, colour = "red"))
  cols <- cat_cols(p, at = c(0.1, 0.9))
  expect_identical(cols[2], "#FF0000")
  expect_gt(sum(grDevices::col2rgb(cols[1])), sum(grDevices::col2rgb(cols[2])))
})

test_that("07.03 two colours give a low and a high colour", {
  p <- expect_plot2(plot2(fx_numcat, x, y, value, colour = c("red", "blue")))
  expect_identical(cat_cols(p)[3], "#0000FF")
  expect_identical(substr(cat_cols(p)[1], 1, 3), "#F5")
})

test_that("07.04 three colours diverge around the centre, with a message; category.midpoint is honoured", {
  p <- expect_plot2_message(plot2(fx_numcat, x, y, value, colour = c("red", "white", "blue")), "category.midpoint = 0.45", type = "info")
  expect_identical(cat_cols(p, at = 0.45), "#FFFFFF")
  p <- expect_plot2(plot2(fx_numcat, x, y, value, colour = c("red", "white", "blue"), category.midpoint = 0.1))
  expect_identical(cat_cols(p, at = 0.1), "#FFFFFF")
})

test_that("07.05 a viridis option name gives the viridis gradient", {
  p <- expect_plot2(plot2(fx_numcat, x, y, value, colour = "viridis"))
  expect_identical(cat_cols(p, at = 0.9), "#FDE725")
})

test_that("07.06 category.percent = TRUE gives percent labels", {
  p <- expect_plot2(plot2(fx_numcat, x, y, value, category.percent = TRUE))
  expect_true(all(endsWith(scale_labels(p, "colour"), "%")))
})

test_that("07.07 a date category gets sensible breaks; a POSIXct category likewise", {
  d <- data.frame(x = c(1.5, 2, 3, 4, 5), y = c(2, 1, 4, 3, 5), d = as.Date("2024-01-01") + c(0, 30, 60, 90, 120))
  p <- expect_plot2_message(plot2(d, x, y, d), "category.date_breaks = \"2 weeks\"", type = "info")
  b <- suppressWarnings(built(p)) # the Dutch default locale warns on systems without it (F003)
  expect_lte(length(scale_breaks(b, "colour")), 12)
  skip_open_finding("F004") # without the fix this case takes minutes
  d$d <- as.POSIXct(d$d)
  p <- expect_plot2(plot2(d, x, y, d))
  expect_lte(length(scale_breaks(p, "colour")), 12)
})

test_that("07.08 category.transform = 'log10' with a zero in category.limits is reported", {
  expect_plot2_message(plot2(fx_numcat, x, y, value, category.transform = "log10", category.limits = c(0, 1)),
                       "Ignoring category.limits since it contains 0")
})

test_that("07.09 category.breaks and category.labels are honoured", {
  p <- expect_plot2(plot2(fx_numcat, x, y, value, category.breaks = c(0.2, 0.6), category.labels = c("lo", "hi")))
  expect_identical(scale_breaks(p, "colour"), c(0.2, 0.6))
  expect_identical(scale_labels(p, "colour"), c("lo", "hi"))
})

test_that("07.10 an integer category from -2 to 3 gets negative breaks", {
  p <- expect_plot2(plot2(data.frame(x = c(1.5, 2, 3, 4, 5, 6), y = c(2, 1, 4, 3, 6, 5), v = -2:3), x, y, v))
  expect_true(any(scale_breaks(p, "colour") < 0))
})

test_that("07.11 legend settings reach the colour bar; a top legend is supported", {
  p <- expect_plot2(plot2(fx_numcat, x, y, value, legend.reverse = TRUE, legend.barheight = 9, legend.barwidth = 2, legend.nbin = 50))
  g <- scale_of(p, "colour")$guide
  expect_true(g$params$reverse)
  expect_identical(g$params$nbin, 50)
  expect_identical(as.numeric(g$params$theme$legend.key.height), 9)
  expect_identical(legend_position(expect_plot2(plot2(fx_numcat, x, y, value, legend.position = "top"))), "top")
})

test_that("07.12 category.type = 'size' maps a numeric category to size", {
  p <- expect_plot2(plot2(fx_numcat, x, y, value, category.type = "size"))
  expect_true("size" %in% names(plot_of(p)$mapping))
  expect_s3_class(scale_of(p, "size"), "ScaleContinuous")
})

test_that("07.13 building a numeric-category plot gives no ggplot2 deprecation warnings", {
  expect_plot2(plot2(fx_numcat, x, y, value, colour = c("red", "white", "blue")))
  expect_plot2(plot2(fx_numcat, x, y, value, legend.position = "bottom"))
})
