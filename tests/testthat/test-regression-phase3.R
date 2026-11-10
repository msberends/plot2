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

# Regression tests for Phase 3 issues (dependencies, locale, fonts).
# Each test_that() starts with the GitHub issue number.

test_that("#41 date formats keep words and quoted text, and always return character", {
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

test_that("#41 unify_years() keeps the time zone", {
  x <- as.POSIXct("2024-03-01 14:30", tz = "America/New_York")
  u <- plot2:::unify_years(x)
  expect_identical(attr(u, "tzone"), "America/New_York")
  expect_identical(format(u, "%m-%d %H:%M"), "03-01 14:30")
})

test_that("#15 dates follow the system locale unless plot2.language is set, and LC_TIME is restored", {
  withr::local_locale(c(LC_TIME = "C"))
  d <- as.Date("2024-05-01")
  expect_identical(format2(d, format = "d mmmm"), "1 May")
  withr::local_options(plot2.language = "nl")
  nl <- format2(d, format = "d mmmm")
  skip_if(nl == "1 May", "no Dutch locale installed")
  expect_identical(nl, "1 mei")
  expect_identical(Sys.getlocale("LC_TIME"), "C")
})

test_that("#15 generated words follow plot2.language, not the machine's locale", {
  d <- data.frame(x = c("a", "b"), y = c(5, 20))
  withr::local_locale(c(LC_COLLATE = "C"))
  expect_true(all(grepl("yrs$", axis_labels(plot2(d, x, y, y.age = TRUE), "y")[-1])))
  withr::local_options(plot2.language = "nl")
  expect_true(all(grepl("jr$", axis_labels(plot2(d, x, y, y.age = TRUE), "y")[-1])))
})

test_that("#15 an unsupported plot2.language warns and falls back to English", {
  withr::local_options(plot2.language = "de")
  expect_warning(plot2:::plot2_language(), "plot2.language", class = "plot2_warning")
})

test_that("#15 the x axis formats dates with plot2's tokens", {
  d <- data.frame(dt = as.Date(c("2024-01-15", "2024-04-15", "2024-07-15")), v = 1:3)
  withr::local_locale(c(LC_TIME = "C"))
  expect_true(all(grepl("^Q[1-4] 2024$", stats::na.omit(axis_labels(plot2(d, dt, v, x.date_labels = "qq yyyy", x.date_breaks = "3 months"), "x")))))
})

test_that("#23 fonts unused for 90 days are pruned from the cache", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "old_font"))
  dir.create(file.path(d, "new_font"))
  file.create(file.path(d, c("old_font", "new_font"), ".last_used"))
  Sys.setFileTime(file.path(d, "old_font", ".last_used"), Sys.time() - 91 * 24 * 3600)
  expect_identical(plot2:::prune_font_cache(d), "old_font")
  expect_identical(list.files(d), "new_font")
})

test_that("#23 the font cache is in the user cache folder, and a cached font is registered", {
  skip_if_not_installed("sysfonts")
  ttf <- "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
  skip_if_not(file.exists(ttf))
  expect_identical(plot2:::font_cache_dir(), file.path(tools::R_user_dir("plot2", "cache"), "fonts"))
  d <- withr::local_tempdir()
  dir.create(file.path(d, "plot2_test_font"))
  file.copy(ttf, file.path(d, "plot2_test_font", "regular.ttf"))
  expect_false(plot2:::font_from_cache("Not Cached", d))
  expect_true(plot2:::font_from_cache("Plot2 Test Font", d))
  expect_true("Plot2 Test Font" %in% sysfonts::font_families())
  expect_true(file.exists(file.path(d, "plot2_test_font", ".last_used")))
})

test_that("#23 a custom font is used only while its plot is drawn", {
  skip_if_not_installed("showtext")
  skip_if_not_installed("sysfonts")
  skip_if_not("Plot2 Test Font" %in% sysfonts::font_families())
  hooks_before <- getHook("grid.newpage")
  p <- plot2(fx_xy, x, y, font = "Plot2 Test Font")
  expect_s3_class(p, "plot2_font")
  expect_false(inherits(plot2(fx_xy, x, y), "plot2_font"))
  withr::local_pdf(withr::local_tempfile(fileext = ".pdf"))
  print(p)
  expect_identical(getHook("grid.newpage"), hooks_before)
  expect_identical(tools::R_user_dir("plot2", "cache"),
                   file.path(Sys.getenv("R_USER_CACHE_DIR"), "R", "plot2"))
})
