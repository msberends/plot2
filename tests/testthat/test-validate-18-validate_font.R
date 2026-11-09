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

# 18: validate_font() (#157)

skip_without_font <- function(family) {
  skip_if_not_installed("showtext")
  skip_if_not_installed("showtextdb")
  skip_if_not_installed("sysfonts")
  skip_if_not(family %in% sysfonts::font_files()$family, paste("font", family, "not installed"))
}

test_that("18.01 without a font, the family is empty everywhere", {
  p <- plot2(fx_counts, group, n)
  expect_identical(theme_el(p, "text")$family, "")
  expect_false(inherits(p, "plot2_font"))
})

test_that("18.02 an installed system font is used without a download", {
  skip_without_font("Nimbus Sans")
  local_mocked_bindings(font_from_google = function(...) stop("no download expected"))
  p <- plot2(fx_counts, group, n, font = "Nimbus Sans")
  expect_identical(theme_el(p, "text")$family, "Nimbus Sans")
  expect_s3_class(p, "plot2_font")
})

test_that("18.03 an unknown font that cannot be found or downloaded warns and uses the default (#23)", {
  skip_if_not_installed("showtext")
  skip_if_not_installed("showtextdb")
  skip_if_not_installed("sysfonts")
  local_mocked_bindings(font_from_cache = function(...) FALSE, font_from_google = function(...) FALSE)
  expect_warning(p <- plot2(fx_counts, group, n, font = "No Such Font Family"), "Ignoring unknown font family", class = "plot2_warning")
  expect_identical(theme_el(p, "text")$family, "")
})

test_that("18.04 in knitr without fig.showtext, a custom font gives an informative error", {
  withr::local_options(knitr.in.progress = TRUE)
  expect_error(plot2(fx_counts, group, n, font = "Nimbus Sans"), "requires `fig.showtext`")
})

test_that("18.05 showtext is not left switched on after drawing (#23)", {
  skip_without_font("Nimbus Sans")
  p <- plot2(fx_counts, group, n, font = "Nimbus Sans")
  hooks <- getHook("grid.newpage")
  f <- withr::local_tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  print(p)
  grDevices::dev.off()
  expect_identical(getHook("grid.newpage"), hooks)
})

test_that("18.06 the font family reaches the datalabel layers", {
  skip_without_font("Nimbus Sans")
  p <- plot2(fx_counts, group, n, font = "Nimbus Sans")
  fams <- vapply(p$layers[c(layer_index(p, "GeomLabel"), layer_index(p, "GeomText"))], function(l) l$aes_params$family, character(1))
  expect_identical(unname(fams), c("Nimbus Sans", "Nimbus Sans"))
})
