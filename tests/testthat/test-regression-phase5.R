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

# Regression tests for the issues fixed in Phase 5 of the review and overhaul.
# Each test_that() starts with the GitHub issue number.

test_that("#47 the methods and plot2_exec() take their arguments from the generic", {
  generic <- formals(plot2)
  generic$data <- NULL
  for (m in c("plot2.default", "plot2.data.frame", "plot2.formula", "plot2.matrix", "plot2.sf", "plot2.freq", "plot2_exec")) {
    f <- formals(getFromNamespace(m, "plot2"))
    expect_identical(setdiff(names(generic), names(f)), character(0))
  }
  # method-specific defaults
  expect_identical(formals(plot2:::plot2.freq)$x.sort, "freq-desc")
  expect_identical(formals(plot2:::plot2.matrix)$x.title, FALSE)
  expect_true(all(c("crs", "datalabels.centroid") %in% names(formals(plot2:::plot2.sf))))
  # every method forwards each argument once
  cl <- as.list(plot2:::plot2_exec_call())
  expect_false(anyDuplicated(names(cl)[nzchar(names(cl))]) > 0)
})

test_that("#117 titles show the column name in functions and loops", {
  h <- function(v) plot2(iris, !!rlang::sym(v), n())
  expect_labels(h("Species"), x = "Species")
  ps <- lapply(c("Sepal.Length", "Sepal.Width"), function(v) plot2(iris, Species, .data[[v]]))
  expect_identical(vapply(ps, function(p) p$labels$y, ""), c("Sepal.Length", "Sepal.Width"))
  k <- function(df, col) plot2(df, Species, {{ col }}, y.title = TRUE)
  expect_labels(k(iris, Petal.Width), y = "Petal.Width")
  # in-line calculations keep their code as title
  expect_labels(plot2(iris, Species, mean(.data$Sepal.Length)), y = "mean(Sepal.Length)")
})

test_that("#46 the generic shows the defaults the methods use; matrix plots say that x, y and category are ignored", {
  expect_identical(formals(plot2)$y_secondary.colour, formals(plot2:::plot2.data.frame)$y_secondary.colour)
  expect_message(plot2(matrix(1:6, 2), x = 1), class = "plot2_caution")
})

test_that("#106 automatic defaults are NULL, and explicit values mean what they say", {
  d <- data.frame(x = paste0("v", 1:30), y = 1:30)
  n_labels <- function(p) length(datalabels_of(p))
  expect_identical(n_labels(plot2(d, x, y)), 0L)
  expect_identical(n_labels(plot2(d, x, y, datalabels = NULL)), 0L)
  expect_identical(n_labels(plot2(d, x, y, datalabels = TRUE)), 30L)
  # summarise_function: NULL sums, FALSE does not summarise
  dup <- data.frame(x = c("a", "a", "b"), y = c(1, 2, 3))
  expect_identical(nrow(plot_data(plot2(dup, x, y))), 2L)
  expect_identical(nrow(plot_data(plot2(dup, x, y, summarise_function = sum))), 2L)
  expect_identical(nrow(plot_data(plot2(dup, x, y, summarise_function = FALSE))), 3L)
  expect_error(plot2(dup, x, y, summarise_function = "sum"), "must be a function, or FALSE")
  # several y columns are not summarised automatically
  wide <- data.frame(x = c("a", "a"), y1 = 1:2, y2 = 3:4)
  expect_identical(nrow(plot_data(plot2(wide, x, c(y1, y2)))), 4L)
  # density plots use percentages automatically, unless y.percent = FALSE
  expect_identical(plot2(iris, Sepal.Length, type = "density")$labels$y, "Density")
  expect_false(identical(axis_labels(plot2(iris, Sepal.Length, type = "density"), "y"),
                         axis_labels(plot2(iris, Sepal.Length, type = "density", y.percent = FALSE), "y")))
  # every default shown in the usage of the automatic arguments is NULL
  auto <- c("datalabels", "summarise_function", "x.max_items", "x.zoom", "y.percent", "y.percent_break",
            "y_secondary.colour", "y_secondary.colour_fill", "x.expand", "y.expand", "x.sort", "facet.fixed_x")
  expect_true(all(vapply(formals(plot2)[auto], is.null, logical(1))))
})

test_that("API decisions: renamed and removed arguments give a clear error", {
  expect_error(plot2(iris, colour_opacity = 0.5), "renamed to `colour_transparency`")
  expect_error(plot2(iris, x.lbl_angle = 90), "renamed to `x.label_angle`")
  expect_error(plot2(iris, facet.fill = "red"), "renamed to `facet.colour_fill`")
  expect_error(plot2(iris, x.mic = TRUE), "AMR::scale_x_mic")
  expect_error(plot2(iris, x.lbl_taxonomy = TRUE), "label_taxonomy")
  expect_identical(get_colour("red", transparency = 0.5), get_colour("#FF000080"))
  expect_identical(names(formals(get_colour)), c("x", "length", "transparency"))
})

test_that("API decisions: options plot2.decimal.mark and plot2.big.mark", {
  withr::local_options(plot2.decimal.mark = ",", plot2.big.mark = ".")
  expect_identical(c(dec_mark(), big_mark()), c(",", "."))
})

test_that("API decisions: legend = FALSE hides the legend, TRUE conflicts with legend.position = 'none'", {
  expect_identical(theme_el(plot2(iris, Sepal.Length, Sepal.Width, Species, legend = FALSE), "legend.position"), "none")
  expect_identical(theme_el(plot2(iris, Sepal.Length, Sepal.Width, Species, legend = TRUE), "legend.position"), "top")
  expect_error(plot2(iris, Sepal.Length, Sepal.Width, Species, legend = TRUE, legend.position = "none"), "legend = TRUE")
  expect_error(plot2(iris, Sepal.Length, Sepal.Width, Species, legend = "yes"), "NULL, TRUE or FALSE")
})

test_that("API decisions: stacked = 'fill' equals stacked_fill = TRUE", {
  d <- data.frame(x = rep(c("a", "b"), each = 2), y = c(1, 3, 2, 2), g = rep(c("p", "q"), 2))
  expect_identical(layer_df(plot2(d, x, y, g, stacked = "fill"), 1)$ymax,
                   layer_df(plot2(d, x, y, g, stacked_fill = TRUE), 1)$ymax)
  expect_error(plot2(d, x, y, g, stacked = "yes"), "TRUE, FALSE or")
})

test_that("API decisions: category.title and legend.title cannot both be given", {
  expect_error(plot2(iris, Sepal.Length, Sepal.Width, Species, category.title = "A", legend.title = "B"), "same setting")
  expect_labels(plot2(iris, Sepal.Length, Sepal.Width, Species, legend.title = "B"), colour = "B")
})

test_that("API decisions: label_taxonomy() italicises taxonomic names", {
  skip_if_not_installed("AMR")
  out <- label_taxonomy(c("Escherichia coli", "Other", "S. aureus"))
  expect_true(is.expression(out))
  expect_match(as.character(out[[1]]), "italic", all = FALSE)
  expect_identical(label_taxonomy(c("a", "b")), c("a", "b"))
  # species names only after a genus (#70)
  out <- label_taxonomy(c("coli alone", "Escherichia coli"))
  expect_false(grepl("italic", deparse(out[[1]])))
  expect_true(grepl("italic", deparse(out[[2]])))
  d <- data.frame(name = c("Escherichia coli", "Other"), n = c(2, 3))
  expect_plot2(plot2(d, name, n, x.labels = label_taxonomy))
})

test_that("#37 datalabels are formatted once, on the final rows, with all placeholders", {
  d <- data.frame(x = rep(c("a", "b"), each = 50), y = 1:100)
  counter <- new.env()
  counter$n <- integer(0)
  trace("format_datalabels", tracer = bquote(assign("n", c(get("n", envir = .(counter)), length(datalabels)), envir = .(counter))), print = FALSE, where = asNamespace("plot2"))
  withr::defer(untrace("format_datalabels", where = asNamespace("plot2")))
  p <- plot2(d, x, y, type = "col", datalabels.format = "%n (%p) of %n")
  expect_identical(counter$n, 2L)
  expect_identical(datalabels_of(p), c("1 275 (25.2%) of 1 275", "3 775 (74.8%) of 3 775"))
})

test_that("#122 the default theme and the list of geoms are built once per session", {
  invisible(plot2(data.frame(g = "a", v = 1), g, v))
  expect_true(length(plot2:::plot2_session$theme_cache) >= 1)
  expect_true(length(plot2:::plot2_session$validated_themes) >= 1)
  expect_true("geom_col" %in% plot2:::plot2_session$valid_geoms)
  # a changed option gives another theme, not the cached one
  withr::local_options(plot2.colour_font_primary = "red")
  p <- plot2(data.frame(g = "a", v = 1), g, v, title = "t")
  expect_identical(col_hex(p$theme$plot.title$colour), col_hex("red"))
})
