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

# R07 sheet S05: export and the copy button
# review_and_overhaul/reviews/R07_shiny/S05_export-and-copy.md

test_that("S05.01 PNG and JPG export have the size and resolution asked for", {
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    shiny_change(session, x = "Species", y = "Sepal.Width")
    invisible(output$plot)
    session$setInputs(export_width_cm = 2.54, export_height_cm = 5.08, export_dpi = 100)
    png <- output$export_png
    expect_true(file.exists(png))
    skip_if_not_installed("png")
    expect_identical(dim(png::readPNG(png))[1:2], c(200L, 100L))
    expect_gt(file.size(output$export_jpg), 0)
  })
})

test_that("S05.02 PDF and SVG export write files", {
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    shiny_change(session, x = "Species", y = "Sepal.Width")
    invisible(output$plot)
    expect_gt(file.size(output$export_pdf), 0)
    skip_if_not_installed("svglite")
    expect_gt(file.size(output$export_svg), 0)
  })
})

test_that("S05.03 the pixel size of raster exports is shown", {
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    expect_match(output$export_px_preview$html, "1,771 × 1,181 px", fixed = TRUE)
  })
})

test_that("S05.04 an export size beyond what the server should render is refused", {
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    shiny_change(session, x = "Species", y = "Sepal.Width")
    invisible(output$plot)
    # the maxima of the app's own inputs: 47,244 x 47,244 px, several GB of memory
    session$setInputs(export_width_cm = 100, export_height_cm = 100, export_dpi = 1200)
    expect_error(output$export_png, "megapixels")
    expect_match(output$export_px_preview$html, "megapixels")
    # beyond the limits of the inputs, which a client can bypass
    session$setInputs(export_width_cm = 1e6, export_dpi = 300)
    expect_error(output$export_pdf, "between 1 and 100 cm")
  })
})

test_that("S05.05 the copy button works when the app runs on a server", {
  app <- shiny_app()
  shiny::testServer(app, {
    shiny_init(session)
    session$setInputs(copy_btn = 1)
    expect_false(session$isClosed())
    # the browser copies the code and reports the result
    session$setInputs(copy_result = "ok")
    expect_false(session$isClosed())
  })
})

test_that("S05.06 the copy button copies in the browser, not on the server", {
  app <- shiny_app()
  html <- as.character(shiny_app_env(app)$ui)
  expect_match(html, "navigator.clipboard", fixed = TRUE)
  expect_false(any(grepl("clipr", deparse(shiny_app_env(app)$server), fixed = TRUE)))
})
