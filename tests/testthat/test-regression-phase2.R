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

# Regression tests for Phase 2 issues (state and messaging).
# Each test_that() starts with the GitHub issue number.

test_that("#13 a failing back-to-back plot leaves no state for the next call", {
  try(suppressWarnings(plot2(data.frame(a = c("x", "y"), v = 1:2, f = c("m", "m")), a, v, facet = f, type = "b2b")), silent = TRUE)
  expect_identical(setdiff(ls(plot2:::plot2_env), "reg_cols"), character(0))
  expect_identical(length(plot2:::plot2_session$call_stack), 0L)
  p <- plot2(data.frame(g = c("a", "b", "c"), v = 1:3), g, v)
  expect_labels(p, x = "g")
})

test_that("#13 a nested call restores the state of the outer call", {
  withr::defer(plot2:::clear_plot2_env())
  env <- plot2:::plot2_env
  plot2:::call_state_enter()
  env$mapping_x <- "outer"
  plot2:::plot2_message("from the outer call")
  inner <- plot2(data.frame(g = c("a", "b", "c"), v = 1:3)) # gives messages about the guessed x and y
  expect_identical(env$mapping_x, "outer")
  # the inner call's diagnostics were handed to the outer call
  expect_true(length(env$infos) >= 2)
  expect_message(plot2:::call_state_exit(), "from the outer call", class = "plot2_message")
  expect_identical(length(plot2:::plot2_session$call_stack), 0L)
})

test_that("#13 a back-to-back plot keeps its labels and leaves a clean state", {
  d <- data.frame(age = rep(c("0-9", "10-19"), 2), n = c(5, 7, 6, 8), sex = rep(c("F", "M"), each = 2))
  expect_clean_state(plot2(d, age, n, facet = sex, type = "back-to-back"))
})

test_that("#13 diagnostics at render time are emitted at once, not with the next call", {
  p <- plot2(data.frame(x = letters[1:3], y = c(1e7, 2e7, 3e7)), x, y)
  suppressMessages(invisible(rendered(p)))
  expect_null(plot2:::plot2_env$infos)
})

test_that("#26 messages are conditions of class plot2_message and can be muffled", {
  df <- data.frame(g = c("a", "b", "c"), v = 1:3)
  expect_message(plot2(df), class = "plot2_message")
  withr::local_envvar(IN_PKGDOWN = "true") # prints messages, as in an interactive session
  expect_silent(suppressMessages(plot2(df)))
})

test_that("#26 plot2.silent = TRUE prints no messages but keeps them catchable", {
  withr::local_envvar(IN_PKGDOWN = "true")
  withr::local_options(plot2.silent = TRUE)
  df <- data.frame(g = c("a", "b", "c"), v = 1:3)
  out <- capture.output(type = "message", invisible(plot2(df)))
  expect_length(out, 0)
  expect_message(plot2(df), class = "plot2_message")
})

test_that("#26 plot2 warnings are R warnings of class plot2_warning", {
  expect_warning(plot2(data.frame(g = character(0), v = numeric(0)), g, v), "No observations", class = "plot2_warning")
  expect_warning(withr::with_options(list(plot2.silent = TRUE), plot2(data.frame(g = character(0), v = numeric(0)), g, v)),
                 class = "plot2_warning")
})

test_that("#26 diagnostics come grouped at the end of the call, in order of occurrence", {
  types <- character(0)
  texts <- character(0)
  withCallingHandlers(
    invisible(plot2(data.frame(x = c("b", "a", "c"), y = c(1, NA, 3)), x, y, colour = "#12:4;6", type = "step")),
    message = function(m) {
      if (inherits(m, "plot2_message")) {
        types <<- c(types, if (inherits(m, "plot2_caution")) "caution" else "info")
        texts <<- c(texts, conditionMessage(m))
      }
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      if (inherits(w, "plot2_warning")) types <<- c(types, "warning")
      invokeRestart("muffleWarning")
    })
  # infos, then cautions, then warnings
  expect_identical(types, types[order(match(types, c("info", "caution", "warning")))])
  expect_true(all(c("caution", "warning") %in% types))
  expect_false(anyDuplicated(texts) > 0)
})

test_that("#26 a long plotmath title does not warn about line wrapping", {
  # the source of a $...$ expression is not its drawn width; this warning was invisible before #26
  expect_plot2(plot2(mtcars, mpg, hp,
                     title = "$f[X](x)==frac(1, sigma*sqrt(2*pi))*plain(e)^{frac(-(x-mu)^2, 2*sigma^2)}$"))
})
