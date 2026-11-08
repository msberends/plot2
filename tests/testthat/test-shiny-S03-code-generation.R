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

# R07 sheet S03: generating the plot2() code
# review_and_overhaul/reviews/R07_shiny/S03_code-generation.md

test_that("S03.01 without changes the code is a bare plot2() call on the data set", {
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    expect_identical(call_string(), "iris |>\n  plot2()")
  })
})

test_that("S03.02 directions come first in a fixed order, several values as c()", {
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    shiny_change(session, title = "T", category = "Species", x = c("Sepal.Length", "Petal.Length"), y = "Sepal.Width")
    code <- call_string()
    expect_match(code, "x = c(Sepal.Length, Petal.Length),\n    y = Sepal.Width,\n    category = Species,\n    title = \"T\"", fixed = TRUE)
  })
})

test_that("S03.03 the y transformation wraps y", {
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    session$setInputs(y_calc = "mean()")
    shiny_change(session, x = "Species", y = "Sepal.Width")
    expect_match(call_string(), "y = mean(Sepal.Width)", fixed = TRUE)
  })
})

test_that("S03.04 a setting changed back to its default is left out", {
  skip_open_finding("F112")
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    shiny_change(session, x.zoom = TRUE)
    expect_match(call_string(), "x.zoom = TRUE", fixed = TRUE)
    session$setInputs(x.zoom = FALSE)
    expect_false(grepl("x.zoom", call_string(), fixed = TRUE))
  })
})

test_that("S03.05 choosing Automatic again after another type gives valid code", {
  skip_open_finding("F116")
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    shiny_change(session, x = "Species", type = "col")
    session$setInputs(type = "auto")
    expect_identical(call_string(), "iris |>\n  plot2(x = Species)")
  })
})

test_that("S03.06 a title with a double quote is shown and used literally", {
  skip_open_finding("F006")
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    shiny_change(session, title = 'He said "hi"')
    invisible(output$plot)
    expect_identical(output$error_msg, "")
    expect_identical(label_text(plot2_env$last_shiny_plot, "title"), 'He said "hi"')
  })
})

test_that("S03.07 a vector setting such as y.limits can be entered", {
  skip_open_finding("F118")
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    shiny_change(session, x = "Species", y = "Sepal.Width", y.limits = "c(0, 10)")
    expect_match(call_string(), "y.limits = c(0, 10)", fixed = TRUE)
  })
})

test_that("S03.08 text that only looks like a number stays text", {
  skip_open_finding("F118")
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    expect_no_warning(shiny_change(session, sep = "."))
    expect_match(call_string(), 'sep = "."', fixed = TRUE)
  })
})

test_that("S03.09 a number typed in a text input becomes a number", {
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    shiny_change(session, datalabels.round = "2")
    expect_match(call_string(), "datalabels.round = 2", fixed = TRUE)
  })
})

test_that("S03.10 the generated code reproduces the plot in the app", {
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    shiny_change(session, x = "Species", y = "Sepal.Width", type = "boxplot", title = "Iris")
    invisible(output$plot)
    in_app <- plot2_env$last_shiny_plot # read first: every plot2() call clears plot2_env (F112)
    p <- eval(parse(text = call_string()), envir = new.env(parent = asNamespace("plot2")))
    expect_identical(layer_df(p), layer_df(in_app))
    expect_identical(geoms(p), geoms(in_app))
  })
})
