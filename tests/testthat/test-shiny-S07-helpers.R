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

# S07: helpers of the app

# the helpers build Shiny and bslib inputs
skip_if_not_installed("shiny")
skip_if_not_installed("bslib")

spec_row <- function(arg) {
  spec <- shiny_input_spec()
  spec[spec$arg == arg, , drop = FALSE]
}

test_that("S07.01 logical arguments get a switch, sizes a slider, sorting a selector", {
  inp <- make_input(spec_row("y.zoom"), FALSE, FALSE)
  expect_match(as.character(inp$tag), "bslib-input-switch", fixed = TRUE)
  expect_false(inp$initial)
  inp <- make_input(spec_row("datalabels.angle"), 0, FALSE)
  expect_match(as.character(inp$tag), "js-range-slider", fixed = TRUE)
  expect_identical(inp$initial, 0)
  inp <- make_input(spec_row("x.sort"), NULL, FALSE)
  expect_match(as.character(inp$tag), "freq-desc", fixed = TRUE)
  expect_identical(inp$initial, "NULL")
  # a default that depends on other arguments is shown as empty, not as a guess (#131)
  inp <- make_input(spec_row("datalabels.size"), quote(3 * text_factor), FALSE)
  expect_identical(inp$initial, NA_real_)
})

test_that("S07.02 pretty labels come from the input specification", {
  lab <- as.character(create_field(shiny::div(), "x.max_items", spec_row("x.max_items")$label, TRUE))
  expect_match(lab, ">Max items<", fixed = TRUE)
  lab <- as.character(create_field(shiny::div(), "na.rm", spec_row("na.rm")$label, TRUE))
  expect_match(lab, ">Remove missing<", fixed = TRUE)
  lab <- as.character(create_field(shiny::div(), "na.rm", spec_row("na.rm")$label, FALSE))
  expect_match(lab, "<code>na.rm</code>", fixed = TRUE)
})

test_that("S07.03 data set labels carry the dimensions", {
  app <- shiny_app()
  sets <- shiny_app_env(app)$data_sets
  expect_identical(names(sets$`Base R`)[sets$`Base R` == "iris"], "iris (150 x 5)")
  expect_identical(names(sets$`ggplot2 package`)[sets$`ggplot2 package` == "ggplot2::mpg"], "mpg (234 x 11)")
})

test_that("S07.04 install_shiny_deps() lists every package the app calls", {
  src <- readLines(source_file("R", "shiny.R"))
  used <- unique(unlist(regmatches(src, gregexpr("[A-Za-z0-9.]+(?=::)", src, perl = TRUE))))
  base <- c("stats", "utils", "tools", "grDevices", "rlang", "ggplot2", "plot2", "dplyr", "cli")
  dep <- deparse(body(install_shiny_deps))
  listed <- gsub('"', "", unlist(regmatches(dep, gregexpr('"[^"]+"', dep))))
  expect_identical(setdiff(setdiff(used, base), listed), character(0))
})

test_that("S07.05 every argument of plot2() has exactly one row in the input specification (#136)", {
  spec <- shiny_input_spec()
  expect_identical(sort(spec$arg), sort(setdiff(names(formals(plot2)), "...")))
  expect_false(anyDuplicated(spec$arg) > 0)
  # every set of choices exists, and every starting value is one of its choices
  for (i in which(spec$widget %in% c("select", "auto", "function", "multi"))) {
    inp <- make_input(spec[i, , drop = FALSE], formals(plot2)[[spec$arg[i]]], FALSE)
    expect_true(all(inp$initial %in% shiny_choice_set(spec$opts[i])), label = spec$arg[i])
  }
})

test_that("S07.06 typed values are read as constants, never run", {
  expect_identical(shiny_literal("c(0, NA)")$value, c(0, NA))
  expect_identical(shiny_literal("-1")$value, -1)
  expect_identical(shiny_literal('c("a", "b")')$value, c("a", "b"))
  expect_false(shiny_literal('system("x")')$ok)
  expect_false(shiny_literal("c(1, f(2))")$ok)
  expect_true(shiny_is_safe_expr(quote(log(Sepal.Length) * 2)))
  expect_false(shiny_is_safe_expr(quote(system("x"))))
  expect_false(shiny_is_safe_expr(quote({Species})))
  expect_false(shiny_is_safe_expr(quote(base::log(x))))
  expect_identical(shiny_direction(c("a", "b"), c("a", "b"))$expr, quote(c(a, b)))
  expect_identical(shiny_direction("my col", "my col")$expr, as.name("my col"))
})
