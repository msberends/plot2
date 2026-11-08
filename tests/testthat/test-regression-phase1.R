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
  withr::local_options(dec_mark = NULL)
  expect_warning(bm <- big_mark(), "big_mark")
  expect_identical(bm, " ")
})

test_that("F016 short hex colours are expanded each on their own; invalid hex codes are rejected", {
  expect_identical(as.character(get_colour(c("#f0a", "#123", "abc"))), c("#FF00AA", "#112233", "#AABBCC"))
  expect_identical(toupper(as.character(get_colour("#AbCdEf"))), "#ABCDEF")
  expect_plot2_message(plot2(fx_cat, x, y, colour = "#12:4;6"), "Invalid colour", type = "caution")
})

test_that("F033 every stop() and warning() in R/ uses call. = FALSE", {
  files <- list.files(source_file("R"), pattern = "\\.R$", full.names = TRUE)
  offenders <- character(0)
  walk <- function(e, file) {
    if (is.call(e)) {
      fn <- e[[1]]
      if (is.symbol(fn) && as.character(fn) %in% c("stop", "warning") && !"call." %in% names(as.list(e))) {
        offenders <<- c(offenders, paste0(basename(file), ": ", paste(deparse(e, width.cutoff = 60L)[1], collapse = "")))
      }
      for (el in as.list(e)[-1]) if (!missing(el) && !is.null(el)) walk(el, file)
    }
  }
  for (f in files) for (e in parse(f, keep.source = FALSE)) walk(e, f)
  expect_identical(offenders, character(0))
})

test_that("F043 transformation objects are accepted for x, y and category", {
  d <- data.frame(x = c(1, 10, 100, 1000), y = c(3, 1, 4, 2), v = c(1, 10, 100, 1000))
  expect_identical(axis_labels(expect_plot2(plot2(d, x, y, x.transform = scales::transform_log10())), "x"),
                   c("1", "10", "100", "1 000"))
  expect_plot2(plot2(fx_cat, x, y, y.transform = scales::transform_reverse()))
  expect_plot2(plot2(d, x, y, v, category.transform = scales::transform_log10()))
})

test_that("F051 x.lbl_angle and x.lbl_align apply to the category labels of horizontal plots", {
  d <- data.frame(g = c("a", "b"), v = 1:2)
  p <- expect_plot2(plot2(d, g, v, horizontal = TRUE, x.lbl_angle = 45))
  expect_identical(theme_el(p, "axis.text.y")$angle, 45)
  expect_null(p$theme$axis.text.x$angle)
  p <- plot2(d, g, v, horizontal = TRUE, x.lbl_align = 0)
  expect_identical(theme_el(p, "axis.text.y")$hjust, 0)
  p <- plot2(d, g, v, x.lbl_angle = 45)
  expect_identical(theme_el(p, "axis.text.x")$angle, 45)
})

test_that("F054 x.limits on a discrete x selects and orders the items, also horizontally", {
  p <- expect_plot2(plot2(fx_cat_unique, x, y, category, x.limits = c("c", "a")))
  expect_identical(axis_labels(p, "x"), c("c", "a"))
  p <- expect_plot2(plot2(fx_cat_unique, x, y, category, x.limits = c("a", "b"), horizontal = TRUE))
  expect_setequal(axis_labels(p, "y"), c("a", "b"))
  expect_plot2_message(plot2(fx_cat_unique, x, y, category, x.limits = c("a", "z")), "not found", type = "warning")
})

test_that("F056 titles with non-ASCII characters are wrapped like ASCII titles", {
  expect_identical(validate_title("Résumé ééééé ééééé ééééé ééééé", markdown = FALSE, max_length = 20),
                   "Résumé ééééé ééééé\nééééé ééééé")
  expect_identical(validate_title("Resume eeeee eeeee eeeee eeeee", markdown = FALSE, max_length = 20),
                   "Resume eeeee eeeee\neeeee eeeee")
  expect_identical(validate_title("αβγ δεζ", markdown = FALSE, max_length = 20), "αβγ δεζ")
})

test_that("F067 category.focus keeps the focused category's own colour and greys the rest", {
  d <- data.frame(x = c("a", "b", "c"), y = 1:3, k = c("a", "b", "c"))
  unfocused <- layer_df(expect_plot2(plot2(d, x, y, k)), "bar")$fill
  focused <- layer_df(expect_plot2(plot2(d, x, y, k, category.focus = "b")), "bar")$fill
  expect_identical(focused[2], unfocused[2])
  expect_identical(col_hex(focused[c(1, 3)]), rep(col_hex("grey85"), 2))
})

test_that("F071 type requirements are checked before the data are prepared", {
  d <- data.frame(from = c("A", "A", "B"), to = c("X", "Y", "X"), n = 1:3, grp = c("A", "A", "B"))
  expect_error(plot2(d, x = from, y = n, category = grp, type = "sankey"), "at least two variables")
  expect_error(plot2(d, x = c(from, to), category = grp, type = "sankey"), "require `y`")
  expect_error(plot2(d, x = c(from, to), y = n, type = "sankey"), "require `category`")
})

test_that("F072 character vectors given as a long expression or named y are plotted", {
  p <- expect_plot2(plot2(c("a", "b", "b", "c", "c", "c", "a", "a", "a", "b")))
  expect_identical(sort(layer_df(p, 1)$y), c(3, 3, 4))
  y <- c("p", "q", "q")
  expect_plot2(plot2(y))
})

test_that("F074 a histogram is chosen by the arguments, not by identical values", {
  expect_geoms(plot2(data.frame(x = 1:4, y = 1:4), x, y), "point", exact = TRUE)
  expect_true("GeomBar" %in% geoms(plot2(data.frame(x = c(1, 2, 2, 3, 3, 3)), x, x)))
})

test_that("F079 a missing x is guessed by column name, and left empty when no column is left", {
  expect_geoms(plot2(data.frame(value = c(1, 5, 3, 8, 2)), y = value), c("errorbar", "boxplot"), exact = TRUE)
  p <- expect_plot2(plot2(data.frame(g = c("a", "b", "a"), value = c(1, 5, 3)), y = value))
  expect_labels(p, x = "g")
})
