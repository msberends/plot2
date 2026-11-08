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

# R05 sheet A07: help pages of the exported helpers, options and data
# review_and_overhaul/reviews/R05_api/A07_helper-help-pages.md

test_that("A07.01 md_to_expression() converts every element, as its return value says", {
  res <- md_to_expression(c("*a*", "**b**"))
  expect_type(res, "list")
  expect_length(res, 2)
})

test_that("A07.02 big_mark() is a full stop if dec_mark() is a comma, a space otherwise", {
  withr::local_options(dec_mark = ",", big_mark = NULL)
  expect_identical(big_mark(), ".")
  withr::local_options(dec_mark = ".", big_mark = NULL)
  expect_identical(big_mark(), " ")
  withr::local_options(big_mark = "")
  expect_identical(big_mark(), "")
})

test_that("A07.03 add_line() with a single y gives a horizontal line, with a single x a vertical line", {
  p <- plot2(iris, Sepal.Length, Sepal.Width, zoom = TRUE)
  expect_geoms(add_line(p, y = mean(Sepal.Width)), "hline")
  expect_geoms(add_line(p, x = mean(Sepal.Length)), "vline")
})

test_that("A07.04 add_*(move = -1), as in the examples, puts the new layer below the top layer", {
  p <- plot2(iris, Sepal.Length, Sepal.Width, zoom = TRUE)
  p2 <- add_point(p, size = 5, colour = "lightgrey", move = -1)
  expect_identical(length(p2$layers), length(p$layers) + 1L)
  expect_identical(unname(as.character(layer_df(p2, 1)$colour[1])), "#D3D3D3")
})

test_that("A07.05 move_layer() moves the top layer one down by default", {
  p <- plot2(iris, Sepal.Length, Sepal.Width, zoom = TRUE) |> add_line(y = 3)
  expect_identical(geoms(move_layer(p)), rev(geoms(p)))
})

test_that("A07.06 every example of the helper help pages runs without error", {
  skip_on_cran()
  withr::local_pdf(NULL)
  withr::defer(unregister_colour("^(red[0-9]|red_group|navy_blue|burnt_orange|forest_green|goldenrod_yellow|slate_grey|plum_purple|my_organisation)"))
  withr::local_options(plot2.colour = NULL)
  for (rd in c("add_mapping", "colour", "coord_spider", "dec_mark", "get_plot_title", "md_to_expression", "theme_minimal2")) {
    ex <- tempfile(fileext = ".R")
    tools::Rd2ex(source_file("man", paste0(rd, ".Rd")), ex, commentDontrun = TRUE)
    rmd <- tempfile(fileext = ".Rmd")
    writeLines(c("```{r}", readLines(ex), "```"), rmd)
    res <- eval_chunks(rmd)
    expect_identical(res$code[!is.na(res$error)], character(0), label = rd)
  }
})

test_that("A07.07 the bundled data sets match their documentation", {
  expect_identical(dim(admitted_patients), c(250L, 7L))
  expect_setequal(as.character(unique(admitted_patients$hospital)), LETTERS[1:4])
  expect_setequal(unique(admitted_patients$ward), c("ICU", "Non-ICU"))
  expect_identical(nrow(netherlands), 12L)
  expect_true(all(c("province", "area_km2", "geometry") %in% names(netherlands)))
})

test_that("A07.08 the options page describes each option correctly", {
  skip_open_finding("F099")
  rd <- paste(readLines(source_file("man", "plot2-options.Rd")), collapse = " ")
  expect_false(grepl("axis.title.x} and \\\\code\\{axis.title.x", rd))
})
