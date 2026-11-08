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

# Regression tests for Phase 1 findings that had no end-to-end test case yet.
# Each test_that() starts with the finding ID; see review_and_overhaul/findings/.

test_that("F015 an invalid dec_mark or big_mark option warns and falls back", {
  withr::local_options(dec_mark = "x", big_mark = "y", OutDec = ".")
  expect_warning(expect_identical(dec_mark(), "."), "dec_mark")
  expect_warning(expect_identical(suppressWarnings(dec_mark()), "."), NA)
  expect_warning(bm <- big_mark(), "big_mark")
  expect_identical(bm, " ")
})

test_that("F016 short hex colours are expanded each on their own; invalid hex codes are rejected", {
  expect_identical(as.character(get_colour(c("#f0a", "#123", "abc"))), c("#FF00AA", "#112233", "#AABBCC"))
  expect_identical(toupper(as.character(get_colour("#AbCdEf"))), "#ABCDEF")
  expect_plot2_message(plot2(fx_cat, x, y, colour = "#12:4;6"), "Invalid colour", type = "caution")
})
