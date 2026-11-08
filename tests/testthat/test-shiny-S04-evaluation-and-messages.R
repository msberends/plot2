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

# R07 sheet S04: running the code, messages and errors
# review_and_overhaul/reviews/R07_shiny/S04_evaluation-and-messages.md

test_that("S04.01 the plot is rendered", {
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    shiny_change(session, x = "Species", y = "Sepal.Width")
    out <- output$plot
    expect_match(out$src, "^data:image/png")
    expect_identical(output$error_msg, "")
  })
})

test_that("S04.02 an error in the code is shown with its own message", {
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    shiny_change(session, x = "does_not_exist")
    invisible(output$plot)
    expect_match(output$error_msg, "does_not_exist")
  })
})

test_that("S04.03 plot2's messages are shown when the app runs on a server", {
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    invisible(output$plot)
    expect_match(output$plot2_msgs$html, "Using")
  })
})

test_that("S04.04 messages are shown as text, not as HTML", {
  s04_html_names <- data.frame(a = c("x", "y", "x"), b = c(1, 2, 3))
  names(s04_html_names)[1] <- "<b>bold</b>"
  app <- shiny_app(data = s04_html_names)
  shiny::testServer(app, {
    shiny_init(session, dataset = "Provided")
    invisible(output$plot)
    expect_match(output$plot2_msgs$html, "&lt;b&gt;bold", fixed = TRUE)
  })
})

injection_test <- function(...) {
  marker <- tempfile()
  vals <- lapply(list(...), function(v) sprintf(v, marker))
  app <- shiny_app(.env = parent.frame())
  shiny::testServer(app, {
    shiny_init(session)
    shiny_change(session, x = "Species")
    try(do.call(shiny_change, c(list(session), vals)), silent = TRUE)
    try(invisible(output$plot), silent = TRUE)
  })
  file.exists(marker)
}

test_that("S04.05 text typed in a direction selector is not run as R code", {
  expect_false(injection_test(x = '{writeLines("x", "%s"); Species}'))
})

test_that("S04.06 text typed as a title is not run as R code", {
  expect_false(injection_test(title = 'a", subtitle = {writeLines("x", "%s"); "b"}, caption = "c'))
})

test_that("S04.07 text typed as summarise_function is not run as R code", {
  expect_false(injection_test(summarise_function = '{writeLines("x", "%s"); mean}'))
})

test_that("S04.08 a calculation typed in a direction selector is used; other functions are refused", {
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    shiny_change(session, x = "Species", y = "Sepal.Length * 2")
    expect_match(call_string(), "y = Sepal.Length * 2", fixed = TRUE)
    invisible(output$plot)
    expect_identical(output$error_msg, "")
    shiny_change(session, y = 'Sys.getenv("HOME")')
    expect_false(grepl("Sys.getenv", call_string(), fixed = TRUE))
    expect_match(output$error_msg, "not a column or a supported calculation", fixed = TRUE)
  })
})

test_that("S04.09 the call cannot reach objects in the global environment", {
  assign("s04_private", data.frame(secret = 1:3), envir = globalenv())
  withr::defer(rm("s04_private", envir = globalenv()))
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    shiny_change(session, x = "s04_private")
    invisible(output$plot)
    expect_match(output$error_msg, "s04_private")
    expect_null(plot_result()$plot)
  })
})
