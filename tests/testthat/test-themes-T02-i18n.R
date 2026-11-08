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

# R06 sheet T02: internationalisation (locale, dates, decimal marks, Unicode)
# review_and_overhaul/reviews/R06_themes/T02_i18n.md

fx_two_dates <- data.frame(x = c("a", "b", "a", "b"), dt = as.Date(c("2024-01-01", "2024-01-01", "2024-02-01", "2024-02-01")), v = 1:4)

test_that("T02.01 plot2 leaves the session locale unchanged, also with date formatting", {
  # the OS warning about "nl_NL" is F003, tested in T02.09
  suppressWarnings(expect_clean_state(plot2(fx_two_dates, x, v, dt), check = "locale"))
})

test_that("T02.02 date category labels follow the session locale, not Dutch", {
  skip_open_finding("F003")
  withr::local_locale(LC_TIME = "C")
  expect_false(any(grepl("mrt|mei|okt", format2(as.Date(c("2024-03-01", "2024-05-01", "2024-10-01")), "d mmm"))))
})

test_that("T02.03 a date category with two dates gives two legend entries", {
  skip_open_finding("F004")
  expect_length(scale_labels(plot2(fx_two_dates, x, v, dt), "fill"), 2)
})

test_that("T02.04 OutDec = ',' gives comma decimals and full-stop thousands", {
  withr::local_options(OutDec = ",")
  d <- data.frame(x = c("a", "b"), y = c(1500.5, 2500.25))
  expect_identical(datalabels_of(plot2(d, x, y)), c("1.500,5", "2.500,2"))
})

test_that("T02.05 non-ASCII column names and titles are kept", {
  nm <- "Gr\u00f6\u00dfe"
  d <- stats::setNames(data.frame(c("a", "b"), 1:2), c(nm, "y"))
  p <- eval(parse(text = paste0("plot2(d, `", nm, "`, y)")))
  expect_labels(p, x = nm)
  expect_true(grepl("\u03b1", label_text(plot2(iris, title = "\u03b1 test"), "title")))
})

test_that("T02.06 y.age and y.24h labels do not depend on LC_COLLATE", {
  skip_open_finding("F003")
  d <- data.frame(x = c("a", "b"), y = c(5, 20))
  a <- withr::with_locale(c(LC_COLLATE = "C"), axis_labels(plot2(d, x, y, y.age = TRUE), "y"))
  b <- withr::with_locale(c(LC_COLLATE = "nl_NL.UTF-8"), axis_labels(plot2(d, x, y, y.age = TRUE), "y"))
  expect_identical(a, b)
})

test_that("T02.07 date-times in a non-UTC time zone are labelled in their own time zone", {
  d <- data.frame(t = as.POSIXct(c("2024-01-01 09:00", "2024-01-01 15:00"), tz = "America/New_York"), v = 1:2)
  labs <- axis_labels(plot2(d, t, v, type = "l"), "x")
  expect_true(any(grepl("^09:00", labs)))
})

test_that("T02.08 date labels have no leading space", {
  skip_open_finding("F029")
  d <- data.frame(dt = as.Date(c("2024-01-05", "2024-03-15")), v = 1:2)
  expect_false(any(grepl("^ ", axis_labels(plot2(d, dt, v, x.date_labels = "d mmm"), "x"))))
})

test_that("T02.09 a date category gives no operating-system locale warning", {
  skip_open_finding("F003")
  withr::local_locale(c(LC_TIME = "C"))
  expect_no_warning(ggplot2::ggplot_build(plot2(fx_two_dates, x, v, dt)))
})
