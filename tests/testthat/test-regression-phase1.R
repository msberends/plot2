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

# Regression tests for Phase 1 issues that had no end-to-end test case yet.
# Each test_that() starts with the GitHub issue number.

test_that("#27 an invalid dec_mark or big_mark option warns and falls back", {
  withr::local_options(plot2.decimal.mark = "x", plot2.big.mark = "y", OutDec = ".")
  expect_warning(expect_identical(dec_mark(), "."), "plot2.decimal.mark")
  expect_warning(expect_identical(suppressWarnings(dec_mark()), "."), NA)
  withr::local_options(plot2.decimal.mark = NULL)
  expect_warning(bm <- big_mark(), "plot2.big.mark")
  expect_identical(bm, " ")
})

test_that("#28 short hex colours are expanded each on their own; invalid hex codes are rejected", {
  expect_identical(as.character(get_colour(c("#f0a", "#123", "abc"))), c("#FF00AA", "#112233", "#AABBCC"))
  expect_identical(toupper(as.character(get_colour("#AbCdEf"))), "#ABCDEF")
  # a real warning since #100
  expect_warning(plot2(fx_cat, x, y, colour = "#12:4;6"), "Invalid colour", class = "plot2_warning")
})

test_that("#45 every stop() and warning() in R/ uses call. = FALSE", {
  files <- list.files(source_file("R"), pattern = "\\.R$", full.names = TRUE)
  offenders <- character(0)
  walk <- function(e, file) {
    if (is.call(e)) {
      fn <- e[[1]]
      # a condition object made with warningCondition() carries its own call
      is_condition <- length(e) > 1 && is.call(e[[2]]) && identical(e[[2]][[1]], as.name("warningCondition"))
      if (is.symbol(fn) && as.character(fn) %in% c("stop", "warning") && !"call." %in% names(as.list(e)) && !is_condition) {
        offenders <<- c(offenders, paste0(basename(file), ": ", paste(deparse(e, width.cutoff = 60L)[1], collapse = "")))
      }
      for (el in as.list(e)[-1]) if (!missing(el) && !is.null(el)) walk(el, file)
    }
  }
  for (f in files) for (e in parse(f, keep.source = FALSE)) walk(e, f)
  expect_identical(offenders, character(0))
})

test_that("#55 transformation objects are accepted for x, y and category", {
  d <- data.frame(x = c(1, 10, 100, 1000), y = c(3, 1, 4, 2), v = c(1, 10, 100, 1000))
  expect_identical(axis_labels(expect_plot2(plot2(d, x, y, x.transform = scales::transform_log10())), "x"),
                   c("1", "10", "100", "1 000"))
  expect_plot2(plot2(fx_cat, x, y, y.transform = scales::transform_reverse()))
  expect_plot2(plot2(d, x, y, v, category.transform = scales::transform_log10()))
})

test_that("#63 x.label_angle and x.label_align apply to the category labels of horizontal plots", {
  d <- data.frame(g = c("a", "b"), v = 1:2)
  # the angle is set by guide_axis() on the left axis after coord_flip() (#154)
  p <- expect_plot2(plot2(d, g, v, horizontal = TRUE, x.label_angle = 45))
  expect_identical(axis_guide_angle(p, "y"), 45)
  expect_null(axis_guide_angle(p, "x"))
  p <- plot2(d, g, v, horizontal = TRUE, x.label_align = 0)
  expect_identical(theme_el(p, "axis.text.y")$hjust, 0)
  p <- plot2(d, g, v, x.label_angle = 45)
  expect_identical(axis_guide_angle(p, "x"), 45)
  # a user-set alignment uses the theme
  p <- plot2(d, g, v, horizontal = TRUE, x.label_angle = 45, x.label_align = 0)
  expect_identical(theme_el(p, "axis.text.y")$angle, 45)
})

test_that("#66 x.limits on a discrete x selects and orders the items, also horizontally", {
  p <- expect_plot2(plot2(fx_cat_unique, x, y, category, x.limits = c("c", "a")))
  expect_identical(axis_labels(p, "x"), c("c", "a"))
  p <- expect_plot2(plot2(fx_cat_unique, x, y, category, x.limits = c("a", "b"), horizontal = TRUE))
  expect_setequal(axis_labels(p, "y"), c("a", "b"))
  expect_plot2_message(plot2(fx_cat_unique, x, y, category, x.limits = c("a", "z")), "not found", type = "warning")
})

test_that("#68 titles with non-ASCII characters are wrapped like ASCII titles", {
  expect_identical(validate_title("Résumé ééééé ééééé ééééé ééééé", markdown = FALSE, max_length = 20),
                   "Résumé ééééé ééééé\nééééé ééééé")
  expect_identical(validate_title("Resume eeeee eeeee eeeee eeeee", markdown = FALSE, max_length = 20),
                   "Resume eeeee eeeee\neeeee eeeee")
  expect_identical(validate_title("αβγ δεζ", markdown = FALSE, max_length = 20), "αβγ δεζ")
})

test_that("#79 category.focus keeps the focused category's own colour and greys the rest", {
  d <- data.frame(x = c("a", "b", "c"), y = 1:3, k = c("a", "b", "c"))
  unfocused <- layer_df(expect_plot2(plot2(d, x, y, k)), "bar")$fill
  focused <- layer_df(expect_plot2(plot2(d, x, y, k, category.focus = "b")), "bar")$fill
  expect_identical(unname(col_hex(as.character(focused[2]))), unname(col_hex(as.character(unfocused[2]))))
  expect_identical(col_hex(focused[c(1, 3)]), rep(col_hex("grey85"), 2))
})

test_that("#83 type requirements are checked before the data are prepared", {
  skip_if_not_installed("ggforce")
  d <- data.frame(from = c("A", "A", "B"), to = c("X", "Y", "X"), n = 1:3, grp = c("A", "A", "B"))
  expect_error(plot2(d, x = from, y = n, category = grp, type = "sankey"), "at least two variables")
  # without y, a Sankey plot counts rows (#118)
  expect_plot2(plot2(d, x = c(from, to), category = grp, type = "sankey"))
  expect_error(plot2(d, x = c(from, to), y = n, type = "sankey"), "require `category`")
})

test_that("#84 character vectors given as a long expression or named y are plotted", {
  p <- expect_plot2(plot2(c("a", "b", "b", "c", "c", "c", "a", "a", "a", "b")))
  expect_identical(sort(layer_df(p, 1)$y), c(3, 3, 4))
  y <- c("p", "q", "q")
  expect_plot2(plot2(y))
})

test_that("#86 a histogram is chosen by the arguments, not by identical values", {
  expect_geoms(plot2(data.frame(x = 1:4, y = 1:4), x, y), "point", exact = TRUE)
  expect_true("GeomBar" %in% geoms(plot2(data.frame(x = c(1, 2, 2, 3, 3, 3)), x, x)))
})

test_that("#91 a missing x is guessed by column name, and left empty when no column is left", {
  expect_geoms(plot2(data.frame(value = c(1, 5, 3, 8, 2)), y = value), c("errorbar", "boxplot"), exact = TRUE)
  p <- expect_plot2(plot2(data.frame(g = c("a", "b", "a"), value = c(1, 5, 3)), y = value))
  expect_labels(p, x = "g")
})

test_that("#94 add_errorbar() works without a category and keeps the facets", {
  d <- data.frame(g = c("a", "b", "a", "b"), n = c(3, 4, 5, 6), f = c("p", "p", "q", "q"))
  p <- expect_plot2(add_errorbar(plot2(d, g, n, facet = f), min = n - 1, max = n + 1))
  eb <- layer_df(p, "errorbar")
  expect_identical(nrow(eb), 4L)
  expect_identical(sort(unique(as.integer(eb$PANEL))), 1:2)
})

test_that("#95 move_layer() reorders by index and clamps the new position", {
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point() + ggplot2::geom_line() + ggplot2::geom_rug()
  expect_identical(geoms(move_layer(p, layer = 1, move = 2)), c("GeomLine", "GeomRug", "GeomPoint"))
  expect_identical(geoms(move_layer(p, layer = 1, move = 10)), c("GeomLine", "GeomRug", "GeomPoint"))
  expect_identical(geoms(move_layer(p, layer = 2, move = 0)), c("GeomPoint", "GeomLine", "GeomRug"))
})

test_that("#99 theme_minimal2(...) changes only the given properties", {
  t_default <- theme_minimal2()
  t <- theme_minimal2(plot.title = ggplot2::element_text(face = "italic"))
  expect_identical(ggplot2::calc_element("plot.title", t)$face, "italic")
  expect_identical(ggplot2::calc_element("plot.title", t)$colour, ggplot2::calc_element("plot.title", t_default)$colour)
  expect_identical(ggplot2::calc_element("plot.title", t)$size, ggplot2::calc_element("plot.title", t_default)$size)
})

test_that("#102 subsetting a colour vector keeps the class and the names", {
  cols <- get_colour(c(a = "red", b = "blue"))
  expect_s3_class(cols[1], "colour")
  expect_s3_class(cols[[2]], "colour")
  expect_identical(names(cols[2]), "b")
  expect_identical(unname(as.character(cols[2:1])), c("#0000FF", "#FF0000"))
})

test_that("#103 theme_minimal2() text sizes are plain numbers", {
  size <- ggplot2::calc_element("plot.title", theme_minimal2())$size
  expect_false(inherits(size, "unit"))
  expect_equal(as.numeric(size), 13)
})

test_that("#143 violin plots with single-observation groups render", {
  d <- data.frame(x = c("a", "a", "a", "b"), y = c(1, 2, 3, 4))
  p <- expect_plot2(plot2(d, x, y, type = "violin"), allow_warnings = TRUE)
  expect_true(nrow(suppressWarnings(layer_df(p, "violin"))) > 0)
})

test_that("#108 the 'ggplot2' palette gives ggplot2's own colours, also with an opacity", {
  for (n in 2:5) expect_identical(unname(as.character(get_colour("ggplot2", n))), toupper(scales::hue_pal()(n)))
  d <- data.frame(x = rep(c("a", "b"), 2), y = 1:4, k = rep(c("u", "v"), each = 2))
  plain <- layer_df(expect_plot2(plot2(d, x, y, k)), "bar")$fill
  transparent <- layer_df(expect_plot2(plot2(d, x, y, k, colour_transparency = 0.5)), "bar")$fill
  expect_identical(unname(substr(as.character(transparent), 1, 7)), toupper(plain))
})

test_that("#60 x.complete works for character and date-time values", {
  p <- expect_plot2(plot2(data.frame(g = c("a", "c", "a"), v = 1:3, k = c("u", "w", "w")), g, v, k, x.complete = TRUE))
  expect_identical(nrow(plot_data(p)), 4L)
  t <- as.POSIXct(c("2024-01-01 00:00", "2024-01-01 12:00", "2024-01-01 18:00"), tz = "UTC")
  p <- expect_plot2(plot2(data.frame(t = t, v = 1:3), t, v, x.complete = TRUE))
  expect_identical(nrow(plot_data(p)), 4L)
  p <- expect_plot2(plot2(data.frame(x = c(1, 2, 5), y = 1:3), x, y, x.complete = TRUE))
  expect_identical(nrow(plot_data(p)), 5L)
})
