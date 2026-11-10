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

test_that("titles work", {
  skip_on_cran()
  expect_true(all(c(caption = "caption",
                    tag = "tag",
                    subtitle = "subtitle",
                    title = "title",
                    y = "y.title",
                    x = "x.title")
                  %in% (mtcars |>
                          plot2(caption = "caption",
                                tag = "tag",
                                subtitle = "subtitle",
                                title = "title",
                                y.title = "y.title",
                                x.title = "x.title") |>
                          get_labels())))
})

test_that("get title works", {
  skip_on_cran()
  p <- plot2(mtcars, title = "Plotting **mpg** vs. **cyl**!")
  expect_equal(get_plot_title(p), "plotting_mpg_vs_cyl")
  expect_equal(get_plot_title(p, valid_filename = FALSE), "Plotting mpg vs. cyl!")
  expect_equal(get_plot_title(plot2(mtcars)), "cyl_per_mpg")
  expect_equal(get_plot_title(plot2(mtcars, title = NULL), default = "test"), "test")
})

test_that("md to expression works", {
  skip_on_cran()
  expr1 <- md_to_expression("test1 *test2* **test3** ***test4*** _test5_ test6 **_test7_** _**test8**_ test_{9} test<sub>10</sub> test^{11} test<sup>12</sup>")
  expect_true(is.expression(expr1))
  expect_identical(as.character(expr1),
                   as.character(parse(text = "paste(\"test1 \", italic(\"test2\"), \" \", bold(\"test3\"), \" \", bolditalic(\"test4\"), \" _test5_ test6 \", bold(\"_test7_\"), \" _\", bold(\"test8\"), \"_ \", test[\"9\"], \" \", test[\"10\"], \" test\"^\"11\", \" \", test^\"12\")")))
  
  expr2 <- md_to_expression("test $alpha$")
  expect_true(is.expression(expr2))
  
  expr3 <- md_to_expression("$f[X](x)==frac(1, sigma*sqrt(2*pi))*plain(e)^{frac(-(x-mu)^2, 2*sigma^2)}$")
  expect_true(is.expression(expr3))
  
  expect_error(md_to_expression("test $**$"))
})

test_that("messaging works", {
  skip_on_cran()
  # outside a plot2() call, diagnostics are signalled at once as conditions (#26)
  expect_message(plot2_message("test"), "test", class = "plot2_message")
  expect_message(plot2_caution("test"), "test", class = "plot2_caution")
  expect_warning(plot2_warning("test"), "test", class = "plot2_warning")
  expect_null(plot2_env$infos)
})

test_that("manual fonts work", {
  skip_if_not_installed("showtext")
  skip_if_not_installed("showtextdb")
  skip_if_not_installed("sysfonts")
  skip_on_cran()
  expect_s3_class(mtcars |> plot2(mpg, hp, font = "Rock Salt"), "gg")
  expect_s3_class(mtcars |> plot2(mpg, hp, font = "Rock Salt"), "gg") # already downloaded
  expect_s3_class(suppressWarnings(mtcars |> plot2(mpg, hp, font = "Courier")), "gg")
  expect_s3_class(suppressWarnings(mtcars |> plot2(mpg, hp, font = "Courier")), "gg") # already downloaded
})

test_that("Plotly works", {
  skip_on_cran()
  skip_if_not_installed("plotly")
  expect_error(as_plotly(mtcars))
  expect_s3_class(mtcars |> plot2(mpg, hp) |> as_plotly(), "plotly")
  expect_s3_class(mtcars |>
                    plot2(mpg, hp) |> 
                    as_plotly(dragmode = "pan") |>
                    plotly_style(marker.line.color = "red",
                                 hoverinfo = "y"),
                  "plotly")
  expect_s3_class(mtcars |> plot2(mpg, hp) |> plotly_style(hoverinfo = "y"), "plotly")
})
