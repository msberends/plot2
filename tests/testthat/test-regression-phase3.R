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

# Regression tests for Phase 3 findings (dependencies, locale, fonts).
# Each test_that() starts with the finding ID; see review_and_overhaul/findings/.

test_that("F029 date formats keep words and quoted text, and always return character", {
  d <- as.Date(c("2024-01-15", "2024-05-03"))
  withr::local_locale(c(LC_TIME = "C"))
  expect_identical(format2(d, format = "Week ww"), c("Week 03", "Week 18"))
  expect_identical(format2(d, format = "Month mm"), c("Month 01", "Month 05"))
  expect_identical(format2(d, format = "'d' d"), c("d 15", "d 3"))
  expect_identical(format2(d, format = "yyyymmdd"), c("20240115", "20240503"))
  expect_identical(format2(d, format = "qq yyyy"), c("Q1 2024", "Q2 2024"))
  expect_identical(format2(d, format = "q"), c("1", "2"))
  expect_identical(format2(d, format = "mmm"), c("Jan", "May"))
  expect_type(format2(d, format = "mmmm"), "character")
  expect_identical(format2(as.Date(NA), format = "mmmm"), NA_character_)
})

test_that("F029 unify_years() keeps the time zone", {
  x <- as.POSIXct("2024-03-01 14:30", tz = "America/New_York")
  u <- plot2:::unify_years(x)
  expect_identical(attr(u, "tzone"), "America/New_York")
  expect_identical(format(u, "%m-%d %H:%M"), "03-01 14:30")
})

test_that("F003 dates follow the system locale unless plot2.language is set, and LC_TIME is restored", {
  withr::local_locale(c(LC_TIME = "C"))
  d <- as.Date("2024-05-01")
  expect_identical(format2(d, format = "d mmmm"), "1 May")
  withr::local_options(plot2.language = "nl")
  nl <- format2(d, format = "d mmmm")
  skip_if(nl == "1 May", "no Dutch locale installed")
  expect_identical(nl, "1 mei")
  expect_identical(Sys.getlocale("LC_TIME"), "C")
})

test_that("F003 generated words follow plot2.language, not the machine's locale", {
  d <- data.frame(x = c("a", "b"), y = c(5, 20))
  withr::local_locale(c(LC_COLLATE = "C"))
  expect_true(all(grepl("yrs$", axis_labels(plot2(d, x, y, y.age = TRUE), "y")[-1])))
  withr::local_options(plot2.language = "nl")
  expect_true(all(grepl("jr$", axis_labels(plot2(d, x, y, y.age = TRUE), "y")[-1])))
})

test_that("F003 an unsupported plot2.language warns and falls back to English", {
  withr::local_options(plot2.language = "de")
  expect_warning(plot2:::plot2_language(), "plot2.language", class = "plot2_warning")
})

test_that("F003 the x axis formats dates with plot2's tokens", {
  d <- data.frame(dt = as.Date(c("2024-01-15", "2024-04-15", "2024-07-15")), v = 1:3)
  withr::local_locale(c(LC_TIME = "C"))
  expect_true(all(grepl("^Q[1-4] 2024$", stats::na.omit(axis_labels(plot2(d, dt, v, x.date_labels = "qq yyyy", x.date_breaks = "3 months"), "x")))))
})
