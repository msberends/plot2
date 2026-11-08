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

# Test helpers for plot2 (see review_and_overhaul/ideas/I001).
#
# All accessors take either a ggplot object or an already built plot
# (ggplot_build() output), so a test can build once and query many times:
#
#   p <- plot2(df, x, y)
#   b <- built(p)
#   expect_equal(axis_breaks(b, "y"), c(0, 5, 10))
#
# Accessors only use the public ggplot2 API where it exists, so tests keep
# working across ggplot2 releases (finding F037).
#
# Naming: accessors are nouns (geoms(), axis_breaks()), expectations start
# with expect_ and return their input invisibly so they can be piped.

`%||%` <- function(x, y) if (is.null(x)) y else x

# Building ------------------------------------------------------------------

is_built <- function(x) {
  inherits(x, c("ggplot_built", "ggplot2::ggplot_built"))
}

#' Build a plot (or return an already built plot), muffling nothing.
built <- function(x) {
  if (is_built(x)) x else ggplot2::ggplot_build(x)
}

#' Build and render a plot to a gtable without drawing it. Rendering runs the
#' code that only executes at print time: label and break functions, geom
#' drawing (e.g. warnings about removed rows), and theme element rendering.
rendered <- function(x) {
  ggplot2::ggplot_gtable(built(x))
}

# the plot object, from either a plot or a built plot
plot_of <- function(x) {
  if (is_built(x)) x$plot else x
}

# Layers ----------------------------------------------------------------------

#' Geom class names per layer, in drawing order, e.g. c("GeomLabel", "GeomBar").
geoms <- function(x) {
  vapply(plot_of(x)$layers, function(l) class(l$geom)[1], character(1), USE.NAMES = FALSE)
}

#' Stat class names per layer.
layer_stats <- function(x) {
  vapply(plot_of(x)$layers, function(l) class(l$stat)[1], character(1), USE.NAMES = FALSE)
}

#' Position class names per layer, e.g. "PositionDodge2", "PositionStack".
positions <- function(x) {
  vapply(plot_of(x)$layers, function(l) class(l$position)[1], character(1), USE.NAMES = FALSE)
}

#' Index of the first layer with the given geom (e.g. "GeomBar" or "bar").
layer_index <- function(x, geom) {
  geom <- if (startsWith(geom, "Geom")) geom else paste0("Geom", toupper(substr(geom, 1, 1)), substring(geom, 2))
  i <- which(geoms(x) == geom)
  if (length(i) == 0) stop("No layer with ", geom, "; layers are: ", toString(geoms(x)), call. = FALSE)
  i[1]
}

#' Computed data of a layer (after stats and positions), by index or geom name.
layer_df <- function(x, i = 1) {
  if (is.character(i)) i <- layer_index(x, i)
  built(x)$data[[i]]
}

#' Fixed (non-mapped) parameters of a layer, e.g. colour = "#FF0000".
layer_params <- function(x, i = 1) {
  if (is.character(i)) i <- layer_index(x, i)
  l <- plot_of(x)$layers[[i]]
  c(l$aes_params, l$geom_params, l$stat_params)
}

#' Labels of the datalabel text layer (the last GeomText layer), as character.
datalabels_of <- function(x) {
  i <- which(geoms(x) %in% c("GeomText", "GeomSfText"))
  if (length(i) == 0) return(character(0))
  as.character(layer_df(x, max(i))$label)
}

#' Number of rows in the plot's own data (p$data).
plot_data <- function(x) {
  as.data.frame(plot_of(x)$data)
}

# Labels and titles ----------------------------------------------------------

#' Resolved labels (title, subtitle, caption, tag, x, y, colour, fill, ...).
labels_of <- function(x) {
  ggplot2::get_labs(plot_of(x))
}

#' Plain-text version of a label: expressions are deparsed, NULL becomes NA.
label_text <- function(x, which) {
  lab <- labels_of(x)[[which]]
  if (is.null(lab)) return(NA_character_)
  if (is.expression(lab) || is.call(lab)) return(paste(deparse(lab[[1]]), collapse = ""))
  as.character(lab)
}

# Axes (position scales) --------------------------------------------------------
# `axis` refers to the axis as drawn: with coord_flip(), "x" is the bottom axis.

panel_params <- function(x, panel = 1) {
  built(x)$layout$panel_params[[panel]]
}

#' Breaks within the visible range, as drawn.
axis_breaks <- function(x, axis = c("x", "y"), panel = 1) {
  axis <- match.arg(axis)
  br <- panel_params(x, panel)[[axis]]$get_breaks()
  as.vector(br[!is.na(br)])
}

#' Labels of the breaks within the visible range. Expressions are deparsed.
axis_labels <- function(x, axis = c("x", "y"), panel = 1) {
  axis <- match.arg(axis)
  pp <- panel_params(x, panel)[[axis]]
  br <- pp$get_breaks()
  lb <- pp$get_labels()
  lb <- lb[!is.na(br)]
  if (is.list(lb) || is.expression(lb)) {
    lb <- vapply(lb, function(l) if (is.character(l)) l else paste(deparse(l), collapse = ""), character(1))
  }
  as.character(lb)
}

#' Visible continuous range of an axis, as drawn (including expansion).
axis_range <- function(x, axis = c("x", "y"), panel = 1) {
  axis <- match.arg(axis)
  panel_params(x, panel)[[axis]]$continuous_range
}

#' Kind of the data scale behind an axis: "discrete", "continuous", "date" or "datetime".
#' Uses the unflipped data aesthetic ("x" is the aesthetic, regardless of coord_flip()).
axis_class <- function(x, aesthetic = c("x", "y")) {
  aesthetic <- match.arg(aesthetic)
  sc <- scale_of(x, aesthetic)
  if (inherits(sc, "ScaleDiscrete")) return("discrete")
  if (inherits(sc, "ScaleContinuousDatetime")) return("datetime")
  if (inherits(sc, "ScaleContinuousDate")) return("date")
  "continuous"
}

# Other scales -----------------------------------------------------------------

#' Trained scale for an aesthetic ("x", "colour", "fill", "size", ...), or NULL.
scale_of <- function(x, aesthetic) {
  b <- built(x)
  if (aesthetic %in% c("x", "y")) {
    return(b$layout[[paste0("panel_scales_", aesthetic)]][[1]])
  }
  b$plot$scales$get_scales(aesthetic)
}

scale_breaks <- function(x, aesthetic) {
  br <- scale_of(x, aesthetic)$get_breaks()
  as.vector(br[!is.na(br)])
}

scale_labels <- function(x, aesthetic) {
  sc <- scale_of(x, aesthetic)
  br <- sc$get_breaks()
  lb <- sc$get_labels(br)
  lb <- lb[!is.na(br)]
  if (is.list(lb) || is.expression(lb)) {
    lb <- vapply(lb, function(l) if (is.character(l)) l else paste(deparse(l), collapse = ""), character(1))
  }
  as.character(lb)
}

scale_limits <- function(x, aesthetic) {
  scale_of(x, aesthetic)$get_limits()
}

#' Values an aesthetic scale maps its breaks to, e.g. hex colours per category,
#' named by break.
scale_values <- function(x, aesthetic) {
  sc <- scale_of(x, aesthetic)
  br <- sc$get_breaks()
  br <- br[!is.na(br)]
  stats::setNames(as.vector(sc$map(br)), as.character(br))
}

# Theme, coordinates, facets ---------------------------------------------------

#' The completed theme of a plot (defaults filled in).
theme_of <- function(x) {
  ggplot2::complete_theme(plot_of(x)$theme)
}

#' A resolved theme element (inheritance applied), e.g. theme_el(p, "axis.text.x")$angle.
theme_el <- function(x, element) {
  ggplot2::calc_element(element, theme_of(x))
}

legend_position <- function(x) {
  theme_of(x)$legend.position
}

coord_class <- function(x) {
  class(plot_of(x)$coordinates)[1]
}

#' Facet summary: type ("FacetNull", "FacetWrap", "FacetGrid"), free scales,
#' number of rows and columns of the panel layout, number of panels.
facet_info <- function(x) {
  p <- plot_of(x)
  lay <- built(x)$layout$layout
  free <- p$facet$params$free
  list(type = class(p$facet)[1],
       free_x = isTRUE(free$x),
       free_y = isTRUE(free$y),
       nrow = length(unique(lay$ROW)),
       ncol = length(unique(lay$COL)),
       n_panels = nrow(lay))
}

# plot2 diagnostics ------------------------------------------------------------

#' Evaluate an expression and capture plot2's diagnostics.
#'
#' Returns a list with `value`, `messages` (data frame with `type` and `text`)
#' and `warnings` (R warnings, character). `type` is "info", "caution" or
#' "warning" for plot2's own output, derived from the cli symbol.
#'
#' plot2 currently queues its diagnostics and prints them only in interactive
#' sessions or during pkgdown builds (finding F014), so IN_PKGDOWN is set to
#' make them visible. Once F014 is fixed, plot2 emits real conditions, which
#' are captured by the same handlers.
plot2_diagnostics <- function(expr, build = TRUE) {
  withr::local_envvar(IN_PKGDOWN = "true")
  msgs <- character(0)
  warns <- character(0)
  value <- withCallingHandlers(
    {
      v <- expr
      if (isTRUE(build) && (ggplot2::is_ggplot(v))) rendered(v)
      v
    },
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  msgs <- trimws(cli::ansi_strip(msgs))
  msgs <- msgs[nzchar(msgs)]
  type <- ifelse(startsWith(msgs, "ℹ") | startsWith(msgs, "i "), "info",
                 ifelse(startsWith(msgs, "!"), "caution",
                        ifelse(startsWith(msgs, "✖") | startsWith(msgs, "x "), "warning", "other")))
  text <- trimws(sub("^(ℹ|!|✖|i|x) ", "", msgs))
  list(value = value,
       messages = data.frame(type = type, text = text, stringsAsFactors = FALSE),
       warnings = trimws(cli::ansi_strip(warns)))
}

# Colours ----------------------------------------------------------------------

#' Normalise colours (names or hex, with or without alpha) to upper-case
#' "#RRGGBB" for comparison; alpha is ignored. Names are kept.
col_hex <- function(x) {
  m <- grDevices::col2rgb(x)
  stats::setNames(toupper(grDevices::rgb(m[1, ], m[2, ], m[3, ], maxColorValue = 255)), names(x))
}

# Expectations -----------------------------------------------------------------

# Every expectation reports exactly one result (one call to testthat::expect()
# or testthat::fail()), as testthat 3 requires for custom expectations.

#' Expect that an expression returns a plot that builds and renders (to a
#' gtable, see rendered()) without R warnings or errors. Returns the plot
#' invisibly.
#'
#' plot2's own cautions and warnings are not R warnings yet (F014) and are
#' therefore not checked here; use expect_plot2_message() for them.
expect_plot2 <- function(object, allow_warnings = FALSE) {
  q <- rlang::enquo(object)
  label <- rlang::as_label(q)
  warns <- character(0)
  err <- NULL
  p <- tryCatch(
    withCallingHandlers({
      p <- rlang::eval_tidy(q)
      if (ggplot2::is_ggplot(p)) rendered(p)
      p
    }, warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }, message = function(m) invokeRestart("muffleMessage")),
    error = function(e) {
      err <<- conditionMessage(e)
      NULL
    })
  problem <- if (!is.null(err)) {
    sprintf("`%s` threw an error:\n%s", label, err)
  } else if (!(ggplot2::is_ggplot(p) || inherits(p, "patchwork"))) {
    sprintf("`%s` is not a ggplot or patchwork object but %s.", label, toString(class(p)))
  } else if (!isTRUE(allow_warnings) && length(warns) > 0) {
    sprintf("`%s` produced %d R warning(s):\n%s", label, length(warns), paste("-", warns, collapse = "\n"))
  } else {
    NULL
  }
  testthat::expect(is.null(problem), problem %||% "")
  invisible(p)
}

#' Expect the layers to contain these geoms (all of them, in any order), or to
#' be exactly these (in drawing order) with exact = TRUE.
expect_geoms <- function(x, expected, exact = FALSE) {
  expected <- ifelse(startsWith(expected, "Geom"), expected,
                     paste0("Geom", toupper(substr(expected, 1, 1)), substring(expected, 2)))
  actual <- geoms(x)
  ok <- if (exact) identical(actual, expected) else all(expected %in% actual)
  testthat::expect(ok, sprintf("Layers are [%s], expected %s[%s].",
                               toString(actual), if (exact) "exactly " else "to include ", toString(expected)))
  invisible(x)
}

#' Expect resolved labels, e.g. expect_labels(p, x = "Species", title = "Iris").
#' `NULL` means "no label". Expressions are compared on their deparsed text
#' (see label_text()). The plot argument is called `.plot` so that it cannot
#' clash with a label called `x`.
expect_labels <- function(.plot, ...) {
  expected <- list(...)
  problems <- character(0)
  for (nm in names(expected)) {
    act <- label_text(.plot, nm)
    exp <- if (is.null(expected[[nm]])) NA_character_ else as.character(expected[[nm]])
    if (!identical(act, exp)) {
      problems <- c(problems, sprintf("Label `%s` is %s, expected %s.", nm,
                                      encodeString(act, quote = "\""), encodeString(exp, quote = "\"")))
    }
  }
  testthat::expect(length(problems) == 0, paste(problems, collapse = "\n"))
  invisible(.plot)
}

#' Expect axis breaks (as drawn) to equal `expected`.
expect_axis_breaks <- function(x, axis, expected, tolerance = 1e-8) {
  act <- axis_breaks(x, axis)
  ok <- length(act) == length(expected) && isTRUE(all.equal(as.numeric(act), as.numeric(expected), tolerance = tolerance))
  testthat::expect(ok, sprintf("Breaks of the %s axis are [%s], expected [%s].", axis, toString(act), toString(expected)))
  invisible(x)
}

#' Expect axis labels (as drawn) to equal `expected`.
expect_axis_labels <- function(x, axis, expected) {
  act <- axis_labels(x, axis)
  testthat::expect(identical(act, as.character(expected)),
                   sprintf("Labels of the %s axis are [%s], expected [%s].", axis, toString(act), toString(expected)))
  invisible(x)
}

#' Expect the values a scale maps its breaks to, e.g. colours per category.
#' Hex colours are compared case-insensitively; names are compared if given.
expect_scale_values <- function(x, aesthetic, expected) {
  act <- scale_values(x, aesthetic)
  norm <- function(v) if (is.character(v)) toupper(v) else v
  ok <- identical(unname(norm(act)), unname(norm(as.vector(expected))))
  if (ok && !is.null(names(expected))) ok <- identical(names(act), names(expected))
  testthat::expect(ok, sprintf("Scale `%s` maps to [%s], expected [%s].", aesthetic,
                               paste(names(act), act, sep = "=", collapse = ", "),
                               paste(names(expected), expected, sep = if (is.null(names(expected))) "" else "=", collapse = ", ")))
  invisible(x)
}

#' Expect facet properties; only the arguments given are checked.
expect_facets <- function(x, n_panels = NULL, free_x = NULL, free_y = NULL, nrow = NULL, type = NULL) {
  info <- facet_info(x)
  checks <- list(n_panels = n_panels, free_x = free_x, free_y = free_y, nrow = nrow, type = type)
  problems <- character(0)
  for (nm in names(checks)) {
    if (is.null(checks[[nm]])) next
    if (!identical(info[[nm]], checks[[nm]])) {
      problems <- c(problems, sprintf("Facet `%s` is %s, expected %s.", nm, format(info[[nm]]), format(checks[[nm]])))
    }
  }
  testthat::expect(length(problems) == 0, paste(problems, collapse = "\n"))
  invisible(x)
}

#' Expect plot2 to report a diagnostic matching `regexp` (of a given type, or any).
#' Returns the evaluated value invisibly.
expect_plot2_message <- function(object, regexp, type = c("any", "info", "caution", "warning"), fixed = FALSE) {
  type <- match.arg(type)
  label <- rlang::as_label(rlang::enquo(object))
  d <- plot2_diagnostics(object)
  m <- d$messages
  if (type != "any") m <- m[m$type == type, , drop = FALSE]
  ok <- any(grepl(regexp, m$text, fixed = fixed, perl = !fixed))
  testthat::expect(ok, sprintf("`%s` did not report a %s matching '%s'. Reported:\n%s", label,
                               if (type == "any") "diagnostic" else type, regexp,
                               if (nrow(d$messages)) paste0("- [", d$messages$type, "] ", d$messages$text, collapse = "\n") else "(nothing)"))
  invisible(d$value)
}

#' Expect plot2 to report no diagnostic (matching `regexp`, if given).
expect_no_plot2_message <- function(object, regexp = NULL, type = c("any", "info", "caution", "warning")) {
  type <- match.arg(type)
  label <- rlang::as_label(rlang::enquo(object))
  d <- plot2_diagnostics(object)
  m <- d$messages
  if (type != "any") m <- m[m$type == type, , drop = FALSE]
  hits <- if (is.null(regexp)) m$text else m$text[grepl(regexp, m$text, perl = TRUE)]
  testthat::expect(length(hits) == 0, sprintf("`%s` reported:\n%s", label, paste("-", hits, collapse = "\n")))
  invisible(d$value)
}

#' Expect that evaluating (and building) leaves no trace in the session:
#' plot2's internal state holds only registered colours, the random-number
#' stream is untouched, and LC_TIME is unchanged (findings F001, F002, F003).
#' Errors in `object` are ignored on purpose: state must also be clean after
#' a failed call. `check` selects the aspects, so that a test of the internal
#' state is not hidden behind a known RNG problem (F002) and vice versa.
expect_clean_state <- function(object, check = c("state", "rng", "locale")) {
  check <- match.arg(check, several.ok = TRUE)
  label <- rlang::as_label(rlang::enquo(object))
  if (!exists(".Random.seed", envir = globalenv())) set.seed(NULL)
  seed_before <- get(".Random.seed", envir = globalenv())
  locale_before <- Sys.getlocale("LC_TIME")
  value <- tryCatch(suppressMessages({
    v <- object
    if (ggplot2::is_ggplot(v)) rendered(v)
    v
  }), error = function(e) NULL)
  problems <- character(0)
  leftover <- setdiff(ls(plot2:::plot2_env, all.names = TRUE), "reg_cols")
  if ("state" %in% check && length(leftover) > 0) {
    problems <- c(problems, sprintf("`%s` left internal state behind: %s.", label, toString(leftover)))
  }
  if ("rng" %in% check && !identical(seed_before, get(".Random.seed", envir = globalenv()))) {
    problems <- c(problems, sprintf("`%s` changed the random-number stream (.Random.seed).", label))
  }
  if ("locale" %in% check && !identical(locale_before, Sys.getlocale("LC_TIME"))) {
    problems <- c(problems, sprintf("`%s` changed LC_TIME from %s to %s.", label, locale_before, Sys.getlocale("LC_TIME")))
  }
  # clean up so that one failing test does not contaminate the next (F001)
  plot2:::clear_plot2_env()
  testthat::expect(length(problems) == 0, paste(problems, collapse = "\n"))
  invisible(value)
}

# Snapshots --------------------------------------------------------------------

#' Visual snapshot test via vdiffr; skipped on CRAN and without vdiffr.
expect_plot2_snapshot <- function(title, p) {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(title, p)
}

# Skips for open findings ------------------------------------------------------

#' Silence soft deprecations (lifecycle) caused by plot2 itself, for the rest
#' of the calling test, so that tests of other behaviour stay clean. Always name
#' the finding; remove the call when the finding is fixed. The finding's own
#' test must not use this.
local_known_deprecation <- function(id, .env = parent.frame()) {
  stopifnot(is.character(id), length(id) == 1)
  withr::local_options(lifecycle_verbosity = "quiet", .local_envir = .env)
}

#' Skip a test that describes the desired behaviour for an open finding.
#' Remove the call when the finding is fixed (see review_and_overhaul/README.md).
skip_open_finding <- function(id) {
  testthat::skip(paste0("open finding ", id, " (review_and_overhaul/findings)"))
}

# Package sources (R05) -------------------------------------------------------

#' Path to a file in the package source tree (README.Rmd, vignettes/, man/,
#' R/). Skips the test when the source tree is not available, e.g. during
#' R CMD check on an installed package.
source_file <- function(...) {
  p <- file.path(testthat::test_path(), "..", "..", ...)
  if (!file.exists(p)) testthat::skip(paste("source file not available:", file.path(...)))
  normalizePath(p)
}

#' Evaluate the R code chunks of an R Markdown file, expression by expression,
#' in a fresh environment. Chunks marked eval = FALSE are evaluated too unless
#' `include_unevaluated = FALSE`; chunks whose code matches `exclude` are left
#' out. ggplot results are built, so build errors and warnings are caught.
#' Returns a data frame with one row per expression: line of the chunk, code,
#' error message (NA if none) and warnings (collapsed, NA if none).
eval_chunks <- function(file, exclude = NULL, include_unevaluated = TRUE) {
  lines <- readLines(file, warn = FALSE)
  starts <- grep("^```\\{r", lines)
  ends <- grep("^```\\s*$", lines)
  env <- new.env(parent = globalenv())
  out <- list()
  for (s in starts) {
    header <- lines[s]
    if (!include_unevaluated && grepl("eval\\s*=\\s*FALSE", header)) next
    e <- min(ends[ends > s])
    code <- lines[seq_len(e - s - 1) + s]
    if (!is.null(exclude) && any(grepl(exclude, code))) next
    for (x in parse(text = code, keep.source = FALSE)) {
      w <- character(0)
      err <- tryCatch({
        withCallingHandlers({
          v <- eval(x, env)
          if (ggplot2::is_ggplot(v)) invisible(ggplot2::ggplot_build(v))
        },
        warning = function(c) { w <<- c(w, conditionMessage(c)); invokeRestart("muffleWarning") },
        message = function(c) invokeRestart("muffleMessage"))
        NA_character_
      }, error = function(e) conditionMessage(e))
      out[[length(out) + 1]] <- data.frame(line = s,
                                           code = paste(deparse(x), collapse = " "),
                                           error = err,
                                           warnings = if (length(w)) paste(unique(w), collapse = "; ") else NA_character_,
                                           stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, out)
}

# Shiny app (R07) -------------------------------------------------------------

#' The app object of create_interactively() for shiny::testServer(). Skips
#' when a package of the app is missing. Outside an interactive session
#' create_interactively() returns the app instead of running it. The plot
#' that the app stores in plot2_env is cleared when the calling test ends.
shiny_app <- function(..., .env = parent.frame()) {
  for (p in c("shiny", "shinyjs", "bslib", "DT", "clipr", "pillar", "rio", "fansi")) {
    testthat::skip_if_not_installed(p)
  }
  withr::defer({
    plot2_env$last_shiny_plot <- NULL
    if ("plot2res" %in% names(shiny::resourcePaths())) shiny::removeResourcePath("plot2res")
  }, envir = .env)
  create_interactively(...)
}

#' The environment in which the app was built (holds `ui`, `data_sets`, ...).
shiny_app_env <- function(app) {
  environment(app$serverFuncSource())
}

#' Input values a browser sends when the app starts.
shiny_init <- function(session, dataset = "iris") {
  session$setInputs(dataset = dataset, type = "auto", y_calc = "None",
                    export_width_cm = 15, export_height_cm = 10, export_dpi = 300)
}

#' Change inputs as a user would. The app records an input as changed only
#' from the second value it receives (its observer ignores the first, F123),
#' so every input first receives NA.
shiny_change <- function(session, ...) {
  vals <- list(...)
  do.call(session$setInputs, lapply(vals, function(v) NA))
  do.call(session$setInputs, vals)
}

#' A file in the shape of shiny::fileInput()'s value.
shiny_file <- function(path) {
  list(name = basename(path), datapath = path, size = file.size(path), type = "")
}
