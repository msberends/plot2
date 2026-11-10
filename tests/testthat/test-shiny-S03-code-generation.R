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

# S03: generating the plot2() code

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
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    shiny_change(session, y.zoom = TRUE)
    expect_match(call_string(), "y.zoom = TRUE", fixed = TRUE)
    session$setInputs(y.zoom = FALSE)
    expect_false(grepl("y.zoom", call_string(), fixed = TRUE))
  })
})

test_that("S03.05 choosing Automatic again after another type gives valid code", {
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    shiny_change(session, x = "Species", type = "col")
    session$setInputs(type = "auto")
    expect_identical(call_string(), "iris |>\n  plot2(x = Species)")
  })
})

test_that("S03.06 a title with a double quote is shown and used literally", {
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    shiny_change(session, title = 'He said "hi"')
    invisible(output$plot)
    expect_identical(output$error_msg, "")
    expect_identical(label_text(plot_result()$plot, "title"), 'He said "hi"')
  })
})

test_that("S03.07 a vector setting such as y.limits can be entered", {
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    shiny_change(session, x = "Species", y = "Sepal.Width", y.limits = "c(0, 10)")
    expect_match(call_string(), "y.limits = c(0, 10)", fixed = TRUE)
  })
})

test_that("S03.08 text that only looks like a number stays text", {
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
    in_app <- plot_result()$plot
    p <- eval(parse(text = call_string()), envir = new.env(parent = asNamespace("plot2")))
    expect_identical(layer_df(p), layer_df(in_app))
    expect_identical(geoms(p), geoms(in_app))
  })
})

test_that("S03.11 the first value of an input is in the code (#135)", {
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    session$setInputs(title = "first")
    expect_match(call_string(), 'title = "first"', fixed = TRUE)
  })
})

test_that("S03.12 the code follows the inputs also after another data set is chosen (#135)", {
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    session$setInputs(title = "T")
    session$setInputs(dataset = "mtcars")
    expect_identical(call_string(), "mtcars |>\n  plot2(title = \"T\")")
  })
})

test_that("S03.13 values, labels and formulas are typed by their argument (#130)", {
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    shiny_change(session, x = "Species", y = "Sepal.Width", category.focus = "setosa",
                 x.labels = "toupper", smooth.formula = "y ~ poly(x, 2)", datalabels.format = "2024",
                 legend.position = '"bottom"', category.type = c("colour", "shape"))
    code <- call_string()
    expect_match(code, 'category.focus = "setosa"', fixed = TRUE)
    expect_match(code, "x.labels = toupper", fixed = TRUE)
    expect_match(code, "smooth.formula = y ~ poly(x, 2)", fixed = TRUE)
    expect_match(code, 'datalabels.format = "2024"', fixed = TRUE)
    expect_match(code, 'legend.position = "bottom"', fixed = TRUE)
    expect_match(code, 'category.type = c("colour", "shape")', fixed = TRUE)
  })
})

test_that("S03.14 values that are not valid for their input are left out and reported", {
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    shiny_change(session, x = "Species", y.limits = 'c(0, system("x"))', smooth.formula = 'y ~ system("x")',
                 theme = '"theme_bw"; system("x")', facet.size = 1e6, x.max_items = -1)
    code <- call_string()
    for (arg in c("y.limits", "smooth.formula", "theme", "facet.size", "x.max_items")) {
      expect_false(grepl(arg, code, fixed = TRUE), label = arg)
      expect_match(paste(plot_call()$problems, collapse = "\n"), arg, fixed = TRUE)
    }
  })
})

test_that("S03.15 the secondary axis can be set (#131)", {
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session, dataset = "mtcars")
    shiny_change(session, x = "cyl", y = "mpg", y_secondary = "hp", y_secondary.type = '"line"')
    expect_match(call_string(), "y_secondary = hp,\n    y_secondary.type = \"line\"", fixed = TRUE)
    invisible(output$plot)
    expect_identical(output$error_msg, "")
  })
})
