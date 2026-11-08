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

# R07 sheet S06: several sessions and server deployment
# review_and_overhaul/reviews/R07_shiny/S06_sessions-and-deployment.md

test_that("S06.01 a session never sees the plot of another session", {
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    shiny_change(session, x = "Species", y = "Sepal.Width")
    invisible(output$plot)
  })
  shiny::testServer(app, {
    expect_null(plot2_env$last_shiny_plot)
  })
})

test_that("S06.02 outside an interactive session, global data frames are only offered when passed", {
  assign("private_records", data.frame(a = 1:3), envir = globalenv())
  withr::defer(rm("private_records", envir = globalenv()))
  app <- shiny_app()
  expect_false("private_records" %in% unlist(shiny_app_env(app)$data_sets))
})

test_that("S06.03 errors in the server code do not end the session", {
  assign("my data", data.frame(a = 1:3), envir = globalenv())
  withr::defer(rm("my data", envir = globalenv()))
  withr::local_options(rlang_interactive = TRUE)
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session, dataset = "my data")
    expect_false(session$isClosed())
  })
})

test_that("S06.04 an app instance leaves the global state of plot2 alone", {
  clear_plot2_env()
  app <- shiny_app()
  expect_identical(setdiff(ls(plot2_env), "reg_cols"), character(0))
})
