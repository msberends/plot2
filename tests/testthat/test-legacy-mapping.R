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

# Legacy smoke tests from the former test_plot2.R (#50), split by topic. They
# mostly check the class and are skipped on CRAN; the content tests are in the
# test-plot2_exec-*, test-modules-*, test-api-* and test-validate-* files.

test_that("general mapping works", {
  skip_on_cran()
  expect_equal(plotdata |> plot2() |> get_mapping() |> names(),
               c("y", "x", "colour", "fill"))
  expect_equal(plotdata |> plot2(category.type = "color") |> get_mapping() |> names(),
               c("y", "x", "colour", "fill"))
  expect_equal(plotdata |> plot2(category.type = "fill") |> get_mapping() |> names(),
               c("y", "x", "colour", "fill"))
  expect_equal(plotdata |> plot2(category = n, category.type = "size") |> get_mapping() |> names(),
               c("y", "x", "size"))
  # remove X-axis
  expect_s3_class(admitted_patients |> plot2(x = NULL, y = age), "gg")
  expect_s3_class(admitted_patients |> plot2(x = 1:250, y = age), "gg")
  
  # set category.labels = md_to_expression automatically
  expect_s3_class(iris |> plot2(category = paste0("*", Species, "*")), "gg")
})

test_that("adding mapping works", {
  skip_on_cran()
  p <- iris |> plot2(Sepal.Length, Sepal.Width)
  expect_length(p$mapping, 3)
  p2 <- p |> add_mapping(shape = Species)
  expect_length(p2$mapping, 4)
  expect_s3_class(p2, "gg")
})

test_that("adding types works", {
  skip_on_cran()
  
  expect_length(mtcars |> plot2(mpg, hp, cyl) |> get_layers(), 1)
  expect_length(mtcars |> plot2(mpg, hp, cyl) |> add_line() |> get_layers(), 2)
  expect_length(mtcars |> plot2(mpg, hp, cyl) |> add_point(shape = 4, size = 5) |> get_layers(), 2)
  expect_length(mtcars |> plot2(mpg, hp, cyl) |> add_col() |> get_layers(), 2)
  expect_error(mtcars |> plot2(mpg, hp, cyl) |> add_type(type = NULL))
  
  p <- data.frame(x = 1:100,
                  y = rnorm(100, 100, 25)) |> 
    plot2()
  expect_length(p |> get_layers(), 1)
  expect_length(p |> 
                  add_line(mean(y)) |>
                  get_layers(), 2)
  expect_length(p |>
                  add_line(mean(y)) |>
                  add_col(y / 5,
                          colour = "black",
                          colour_fill = "yellow",
                          width = 0.25) |>
                  get_layers(), 3)
})

test_that("moving layer works", {
  skip_on_cran()
  expect_s3_class((mtcars |>
                     plot2(mpg, hp, cyl) +
                     geom_line(colour = "grey75")) |>
                    move_layer(-1), "gg")
})
