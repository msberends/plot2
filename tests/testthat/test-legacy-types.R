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

# Legacy smoke tests from the former test_plot2.R (#50), split by topic. They
# mostly check the class and are skipped on CRAN; the content tests are in the
# test-plot2_exec-*, test-modules-*, test-api-* and test-validate-* files.

test_that("general types work", {
  skip_on_cran()
  expect_s3_class(plot2(rnorm(10, 10)), "gg")
  expect_s3_class(plot2(rnorm(10, 10), type = "l"), "gg")
  expect_s3_class(plot2(mtcars, mpg^2, hp^2), "gg")
  expect_s3_class(plot2(mtcars, mpg^2, hp^2, smooth = TRUE), "gg")
  expect_s3_class(iris |> plot2(Species), "gg")
  expect_s3_class(iris |> plot2(Species, type = "violin"), "gg")
  expect_s3_class(iris |> plot2(Species, type = "violin"), "gg")
  expect_s3_class(iris |> plot2(Species, type = "blank"), "gg")
  expect_s3_class(iris |> plot2(Species, type = "area"), "gg")
  expect_plot2_message(iris |> plot2(Species, type = "dotplot"), "loosely supported", type = "caution")
  # difftime coercion to double:
  expect_s3_class(data.frame(x = letters[1:10],
                             y = difftime(Sys.time(), Sys.time() - seq_len(10))) |>
                    plot2(),
                  "gg")
  expect_s3_class(admitted_patients |> plot2(hospital, n(), where(is.character)), "gg")
  expect_s3_class(admitted_patients |> plot2(hospital, n(), c(gender, ward)), "gg")
  
  expect_s3_class(plotdata |> plot2(y.transform = "log2"), "gg")
  # dumbbell:
  expect_s3_class(admitted_patients |> plot2(age_group, n_distinct(patient_id), ward, type = "d"), "gg")
  expect_s3_class(admitted_patients |> plot2(age_group, n_distinct(patient_id), ward, gender, type = "d"), "gg")
  # sankey:
  skip_if_not_installed("ggforce")
  expect_s3_class(admitted_patients |>
                    plot2(x = c(hospital, ward),
                          y = n_distinct(patient_id),
                          category = age_group,
                          facet = gender,
                          type = "sankey"),
                  "gg")
})

test_that("na.rm works", {
  skip_on_cran()
  # as characters
  df <- data.frame(hp = mtcars$hp,
                   letters = letters[seq_len(nrow(mtcars))],
                   stringsAsFactors = FALSE)
  expect_lt(df |> plot2(na.rm = TRUE) |> get_data() |> nrow(), nrow(df))
  expect_lt(df |> plot2(na.rm = FALSE) |> get_data() |> nrow(), nrow(df))
  # as factors
  df <- data.frame(hp = mtcars$hp,
                   letters = letters[seq_len(nrow(mtcars))],
                   stringsAsFactors = TRUE)
  expect_lt(df |> plot2(na.rm = TRUE) |> get_data() |> nrow(), nrow(df))
  expect_lt(df |> plot2(na.rm = FALSE) |> get_data() |> nrow(), nrow(df))
})

test_that("infinite values are removed", {
  skip_on_cran()
  expect_equal(data.frame(x = letters[1:10],
                          y123 = c(1:9, Inf)) |>
                 plot2() |>
                 get_data() |> 
                 nrow(),
               9)
})

test_that("S3 implementations work", {
  skip_on_cran()
  # lm
  expect_s3_class(lm(mpg ~ hp, mtcars) |> plot2(), "gg")
  # freq
  skip_if_not_installed("cleaner")
  freq <- cleaner::freq(admitted_patients$hospital)
  expect_s3_class(freq |> plot2(), "gg")
  # sf
  skip_if_not_installed("sf")
  expect_s3_class(netherlands |> plot2(), "gg")
  expect_s3_class(netherlands |> plot2(crs = 28992, theme = theme_minimal2()), "gg")
  # bug_drug_combinations
  skip_if_not_installed("AMR")
  expect_s3_class(AMR::example_isolates |>
                    dplyr::select(mo, CIP, AMC) |>
                    AMR::bug_drug_combinations(FUN = AMR::mo_gramstain) |>
                    plot2(),
                  "gg")
  # sir_df
  expect_s3_class(AMR::example_isolates |>
                    dplyr::select(date, NIT, CIP, IPM, MEM) |>
                    dplyr::group_by(yr = format(date, "%Y")) |>
                    AMR::sir_df() |>
                    dplyr::filter(yr >= 2015) |>
                    plot2(),
                  "gg")
  # type should become boxplot here
  expect_s3_class(admitted_patients |> plot2(x = hospital, y = (age - mean(age)) / sd(age)), "gg")
  # this uses the format2_scientific function for the Y-axis
  expect_s3_class(admitted_patients |>
                    plot2(format(date, "%Y"),
                          (age - mean(age)) / sd(age),
                          hospital,
                          y.scientific = TRUE),
                  "gg")
  expect_s3_class(netherlands |> plot2(markdown = TRUE), "gg")
})

test_that("blank plot works", {
  skip_on_cran()
  expect_warning(p <- plotdata |> subset(n < 0) |> plot2(), "No observations", class = "plot2_warning")
  expect_s3_class(p, "gg")
  expect_warning(p <- data.frame() |> plot2(type = "blank",
                                            x.title = "test",
                                            y.title = "test",
                                            title = "test",
                                            subtitle = "test",
                                            tag = "test",
                                            caption = "test"), "No observations", class = "plot2_warning")
  expect_s3_class(p, "gg")
})

test_that("matrices works", {
  skip_on_cran()
  expect_s3_class(mtcars |>
                    stats::cor() |>
                    plot2(), "gg")
  expect_s3_class(mtcars |>
                    stats::cor() |>
                    plot2(colour = c("blue4", "white", "red4"),
                          category.limits = c(-1, 1)), "gg")
})

test_that("errorbars work", {
  skip_on_cran()
  expect_s3_class(plotdata |>
                    dplyr::mutate(error1 = n * 0.9, error2 = n * 1.1) |> 
                    plot2(type = "c", colour = "pink") |>
                    add_errorbar(error1, error2), "gg")
})

test_that("aes works", {
  skip_on_cran()
  expect_identical(
    aes(x = some_var, y = `as.character(some_var)`),
    setup_aes(x = "some_var", y = "as.character(some_var)"))
})

test_that("completing data works", {
  skip_on_cran()
  expect_s3_class(data.frame(x = c(1, 5, 6, 9),
                             y = 10) |>
                    plot2(type = "l", x.complete = 5),
                  "gg")
})

test_that("type validation works", {
  skip_on_cran()
  library(dplyr, warn.conflicts = FALSE)
  df <- tibble(x = letters[1:10],
               y = 1:10,
               z = LETTERS[1:10],
               `_var_x` = x,
               `_var_y` = y,
               `_var_category` = z)
  expect_equal(validate_type(NULL, df), "geom_col")
  expect_equal(validate_type(NULL, df |> select(-x, -`_var_x`)), "geom_boxplot")
  expect_equal(validate_type("a", df), "geom_area")
  expect_equal(validate_type("b", df), "geom_boxplot")
  expect_equal(validate_type("c", df), "geom_col")
  expect_equal(validate_type("h", df), "geom_histogram")
  expect_equal(validate_type("j", df), "geom_jitter")
  expect_equal(validate_type("l", df), "geom_line")
  expect_equal(validate_type("p", df), "geom_point")
  expect_equal(validate_type("r", df), "geom_ribbon")
  expect_equal(validate_type("v", df), "geom_violin")
})
