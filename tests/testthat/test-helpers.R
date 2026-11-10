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

# Self-tests of the helpers in helper-plot2.R. These use plain ggplot2 plots
# with known output, so a failure here means a helper is broken, not plot2.

library(ggplot2)

gg_bar <- ggplot(data.frame(x = c("a", "b", "c"), y = c(1, 5, 10)), aes(x, y)) +
  geom_col() +
  geom_text(aes(label = y)) +
  scale_y_continuous(breaks = c(0, 5, 10), limits = c(0, 10)) +
  labs(title = "My title", x = "Group")

gg_colour <- ggplot(data.frame(x = 1:3, y = 1:3, g = c("p", "q", "r")), aes(x, y, colour = g)) +
  geom_point() +
  scale_colour_manual(values = c(p = "#FF0000", q = "#00FF00", r = "#0000FF"))

gg_facet <- ggplot(data.frame(x = 1:4, y = 1:4, f = c("a", "a", "b", "b")), aes(x, y)) +
  geom_point() +
  facet_wrap(~f, scales = "free_y", nrow = 1)

test_that("layer accessors", {
  expect_identical(geoms(gg_bar), c("GeomCol", "GeomText"))
  expect_identical(positions(gg_bar)[1], "PositionStack")
  expect_length(positions(gg_bar), 2)
  expect_identical(layer_stats(gg_bar)[1], "StatIdentity")
  expect_identical(layer_index(gg_bar, "text"), 2L)
  expect_error(layer_index(gg_bar, "point"), "No layer with GeomPoint")
  expect_identical(layer_df(gg_bar, "col")$y, c(1, 5, 10))
  expect_identical(datalabels_of(gg_bar), c("1", "5", "10"))
  expect_identical(nrow(plot_data(gg_bar)), 3L)
})

test_that("accessors accept built plots", {
  b <- built(gg_bar)
  expect_true(is_built(b))
  expect_identical(built(b), b)
  expect_identical(geoms(b), geoms(gg_bar))
  expect_identical(axis_breaks(b, "y"), axis_breaks(gg_bar, "y"))
})

test_that("label accessors", {
  expect_identical(label_text(gg_bar, "title"), "My title")
  expect_identical(label_text(gg_bar, "x"), "Group")
  expect_identical(label_text(gg_bar, "subtitle"), NA_character_)
  expect_identical(label_text(gg_bar + labs(title = expression(alpha^2)), "title"), "alpha^2")
})

test_that("axis accessors", {
  expect_identical(axis_breaks(gg_bar, "y"), c(0, 5, 10))
  expect_identical(axis_labels(gg_bar, "y"), c("0", "5", "10"))
  expect_identical(axis_labels(gg_bar, "x"), c("a", "b", "c"))
  expect_identical(axis_class(gg_bar, "x"), "discrete")
  expect_identical(axis_class(gg_bar, "y"), "continuous")
  expect_identical(axis_class(ggplot(fx_dates, aes(date, y)) + geom_line(), "x"), "date")
  expect_identical(axis_class(ggplot(fx_datetimes, aes(time, y)) + geom_line(), "x"), "datetime")
  expect_length(axis_range(gg_bar, "y"), 2)
  # coord_flip(): axes as drawn
  flipped <- gg_bar + coord_flip()
  expect_identical(axis_labels(flipped, "y"), c("a", "b", "c"))
  expect_identical(axis_breaks(flipped, "x"), c(0, 5, 10))
  expect_identical(coord_class(flipped), "CoordFlip")
})

test_that("non-position scale accessors", {
  expect_identical(scale_breaks(gg_colour, "colour"), c("p", "q", "r"))
  expect_identical(scale_labels(gg_colour, "colour"), c("p", "q", "r"))
  expect_identical(scale_values(gg_colour, "colour"), c(p = "#FF0000", q = "#00FF00", r = "#0000FF"))
  expect_null(scale_of(gg_bar, "colour"))
})

test_that("theme and facet accessors", {
  p <- gg_bar + theme(axis.text.x = element_text(angle = 45), legend.position = "bottom")
  expect_identical(theme_el(p, "axis.text.x")$angle, 45)
  expect_identical(legend_position(p), "bottom")
  info <- facet_info(gg_facet)
  expect_identical(info$type, "FacetWrap")
  expect_false(info$free_x)
  expect_true(info$free_y)
  expect_identical(info$n_panels, 2L)
  expect_identical(info$nrow, 1L)
  expect_identical(facet_info(gg_bar)$type, "FacetNull")
})

test_that("expectations pass on correct input", {
  expect_success(expect_geoms(gg_bar, c("col", "text"), exact = TRUE))
  expect_success(expect_geoms(gg_bar, "text"))
  expect_success(expect_labels(gg_bar, title = "My title", x = "Group", subtitle = NULL))
  expect_success(expect_axis_breaks(gg_bar, "y", c(0, 5, 10)))
  expect_success(expect_axis_labels(gg_bar, "x", c("a", "b", "c")))
  expect_success(expect_scale_values(gg_colour, "colour", c(p = "#ff0000", q = "#00ff00", r = "#0000ff")))
  expect_success(expect_facets(gg_facet, n_panels = 2L, free_y = TRUE, free_x = FALSE))
})

test_that("expectations fail on wrong input", {
  expect_failure(expect_geoms(gg_bar, "point"), "expected to include")
  expect_failure(expect_geoms(gg_bar, "col", exact = TRUE), "expected exactly")
  expect_failure(expect_labels(gg_bar, title = "Other"), "Label `title`")
  expect_failure(expect_axis_breaks(gg_bar, "y", c(0, 10)), "Breaks of the y axis")
  expect_failure(expect_axis_labels(gg_bar, "x", c("a", "b")), "Labels of the x axis")
  expect_failure(expect_scale_values(gg_colour, "colour", c("#FF0000", "#FF0000", "#FF0000")), "maps to")
  expect_failure(expect_facets(gg_facet, n_panels = 3L), "Facet `n_panels`")
})

test_that("expect_plot2() checks class, errors and R warnings", {
  expect_success(expect_plot2(gg_bar))
  expect_failure(expect_plot2(stop("boom")), "threw an error")
  expect_failure(expect_plot2(data.frame()), "is not a ggplot")
  # a warning raised when the layer is created ("Ignoring unknown parameters")
  warn_plot <- quote(ggplot(data.frame(x = 1, y = 1), aes(x, y)) + geom_point(nonexistent = 1))
  expect_failure(expect_plot2(eval(warn_plot)), "R warning")
  expect_success(expect_plot2(eval(warn_plot), allow_warnings = TRUE))
  # a warning raised only when rendering (rows with missing values removed)
  na_plot <- ggplot(data.frame(x = c(1, NA), y = c(1, 2)), aes(x, y)) + geom_point()
  expect_failure(expect_plot2(na_plot), "Removed 1 row")
  expect_identical(geoms(expect_plot2(gg_bar)), c("GeomCol", "GeomText"))
})

test_that("plot2 diagnostics are captured and classified", {
  d <- plot2_diagnostics(plot2(fx_xy))
  expect_true(ggplot2::is_ggplot(d$value))
  expect_true(all(c("type", "text") %in% names(d$messages)))
  expect_true(any(d$messages$type == "info"))
  expect_success(expect_plot2_message(plot2(fx_xy), "Using .*type", type = "info"))
  expect_failure(expect_plot2_message(plot2(fx_xy), "this text never appears"), "did not report")
  expect_success(expect_no_plot2_message(plot2(fx_xy), "this text never appears"))
  # IN_PKGDOWN is restored afterwards
  expect_identical(Sys.getenv("IN_PKGDOWN"), "")
})

test_that("rendering catches messages queued at print time (#13)", {
  # plot2's default y labels decide on scientific notation at render time
  d <- plot2_diagnostics(plot2(data.frame(x = c("a", "b"), y = c(1e7, 3e7)), x, y))
  expect_true(is.data.frame(d$messages))
  plot2:::clear_plot2_env()
})

test_that("expect_clean_state() detects leftover state and RNG use", {
  expect_success(expect_clean_state(gg_bar))
  expect_failure(expect_clean_state({ assign("leftover", 1, envir = plot2:::plot2_env); gg_bar }), "left internal state")
  expect_failure(expect_clean_state({ stats::runif(1); gg_bar }), "random-number stream")
  expect_success(expect_clean_state({ stats::runif(1); gg_bar }, check = "state"))
})

test_that("col_hex() normalises colour names and hex codes", {
  expect_identical(col_hex(c(a = "red", b = "#00ff0080")), c(a = "#FF0000", b = "#00FF00"))
})

test_that("local_known_deprecation() silences lifecycle warnings locally", {
  f <- function() {
    local_known_deprecation("#0")
    getOption("lifecycle_verbosity")
  }
  before <- getOption("lifecycle_verbosity")
  expect_identical(f(), "quiet")
  expect_identical(getOption("lifecycle_verbosity"), before)
})

test_that("setup.R makes the environment deterministic", {
  expect_identical(Sys.timezone(), "UTC")
  expect_identical(getOption("OutDec"), ".")
  expect_null(getOption("plot2.colour"))
})

test_that("source_file() and eval_chunks() read and run R Markdown chunks", {
  f <- tempfile(fileext = ".Rmd")
  writeLines(c("text", "```{r}", "x <- 1", "stop('boom')", "```",
               "```{r, eval = FALSE}", "warning('careful')", "```",
               "```{r}", "x + 1", "```"), f)
  res <- eval_chunks(f)
  expect_identical(nrow(res), 4L)
  expect_identical(res$error[2], "boom")
  expect_identical(res$warnings[3], "careful")
  expect_identical(nrow(eval_chunks(f, include_unevaluated = FALSE)), 3L)
  # exclude skips the matching expression only, not the rest of its chunk
  expect_identical(eval_chunks(f, exclude = "boom")$code, c("x <- 1", "warning(\"careful\")", "x + 1"))
  expect_true(file.exists(source_file("DESCRIPTION")))
})

test_that("shiny_app(), shiny_init() and shiny_change() drive the app's server", {
  app <- shiny_app()
  expect_s3_class(app, "shiny.appobj")
  expect_true(is.environment(shiny_app_env(app)))
  shiny::testServer(app, {
    shiny_init(session)
    expect_identical(input$dataset, "iris")
    shiny_change(session, title = "a")
    expect_match(call_string(), "title = \"a\"", fixed = TRUE)
  })
  f <- tempfile(fileext = ".csv")
  writeLines("a", f)
  expect_identical(shiny_file(f)$datapath, f)
})

test_that("axis_guide_angle() reads the angle of an axis guide", {
  expect_identical(axis_guide_angle(gg_bar + guides(x = guide_axis(angle = 45))), 45)
  expect_null(axis_guide_angle(gg_bar))
})
