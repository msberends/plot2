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

#' Label Currencies
#' 
#' Format numbers as currency, rounding values to dollars or cents using a convenient heuristic.
#' @details The euro sign of [euros()] cannot be encoded by the default [pdf()][grDevices::pdf()] device. To save such plots as PDF, use the Cairo device, e.g. `ggplot2::ggsave("plot.pdf", device = grDevices::cairo_pdf)`, or in R Markdown the chunk option `dev = "cairo_pdf"`.
#' @param x Values.
#' @inheritParams plot2
#' @name labellers
#' @rdname labellers
#' @export
#' @examples
#' profit <- data.frame(group = LETTERS[1:4],
#'                      profit = runif(4, 10000, 25000))
#' 
#' profit |>
#'   plot2(y.labels = euros,
#'         datalabels = FALSE)
#'         
#' profit |>
#'   plot2(y.labels = euros,
#'         datalabels.format = euros)
#' 
#' dollars(c(1500, 2499.5))
euros <- function(x,
                  big.mark = big_mark(),
                  decimal.mark = dec_mark(),
                  ...) {
  if (all(is.finite(x) & (x == round(x)), na.rm = TRUE)) {
    # no cents
    trimws(paste0("\u20ac ", trimws(format(round(x, 0), decimal.mark = decimal.mark, big.mark = big.mark, scientific = FALSE))))
  } else {
    trimws(paste0("\u20ac ", trimws(format(round(x, 2), decimal.mark = decimal.mark, big.mark = big.mark, nsmall = 2, scientific = FALSE))))
  }
}

#' @rdname labellers
#' @export
dollars <- function(x,
                    big.mark = big_mark(),
                    decimal.mark = dec_mark(),
                    ...) {
  if (all(is.finite(x) & (x == round(x)), na.rm = TRUE)) {
    # no cents
    trimws(paste0("$", trimws(format(round(x, 0), decimal.mark = decimal.mark, big.mark = big.mark, scientific = FALSE))))
  } else {
    trimws(paste0("$", trimws(format(round(x, 2), decimal.mark = decimal.mark, big.mark = big.mark, nsmall = 2, scientific = FALSE))))
  }
}

#' Label Taxonomic Names in Italics
#' 
#' Format the names of microorganisms in axis or legend labels in italics, as is the convention in microbiology. Family and genus names in the [microorganisms][AMR::microorganisms] data set of the `AMR` package are italicised, and so are species and subspecies names that follow a genus or an abbreviated genus (such as *S. aureus*); other words are kept as they are.
#' @param x Values, such as the breaks of an axis.
#' @param ... Not used, allows this function to be used as a labeller.
#' @details Use this function for the labels of any plotting direction, e.g. `x.labels = label_taxonomy` or `category.labels = label_taxonomy`. It requires the `AMR` package.
#' @return An [expression] vector if any taxonomic name was found, otherwise `x` as character.
#' @export
#' @examples
#' if (requireNamespace("AMR", quietly = TRUE)) {
#'   label_taxonomy(c("Escherichia coli", "Other", "S. aureus"))
#'   
#'   bacteria <- data.frame(name = c("Escherichia coli", "Staphylococcus aureus", "Other"),
#'                          n = c(25, 18, 6))
#'   bacteria |>
#'     plot2(name, n, x.labels = label_taxonomy)
#' }
label_taxonomy <- function(x, ...) {
  rlang::check_installed("AMR", reason = "for `label_taxonomy()`.")
  if (is.null(plot2_session$taxonomic_names)) {
    # cached for the session, the data set has about 78,000 rows (#70)
    plot2_session$taxonomic_names <- list(
      higher = unique(c(AMR::microorganisms$family, AMR::microorganisms$genus)),
      epithets = unique(c(AMR::microorganisms$species, AMR::microorganisms$subspecies)))
  }
  nms <- plot2_session$taxonomic_names
  x <- as.character(x)
  marked <- vapply(FUN.VALUE = character(1),
                   X = strsplit(x, " "),
                   FUN = function(words) {
                     if (length(words) == 0 || all(is.na(words))) {
                       return(if (length(words) == 0) "" else NA_character_)
                     }
                     # families and genera anywhere; species and subspecies only after a genus,
                     # an abbreviated genus such as "S.", or a species, so that ordinary words that
                     # are also species names (such as "coli" or "group") stay upright (#70)
                     italic <- words %in% nms$higher
                     abbreviated <- grepl("^[A-Z][.]$", words)
                     for (i in seq_along(words)[-1]) {
                       if (words[i] %in% nms$epithets && (italic[i - 1] || abbreviated[i - 1])) {
                         italic[i] <- TRUE
                       }
                     }
                     words[italic] <- paste0("*", words[italic], "*")
                     out <- paste0(words, collapse = " ")
                     # abbreviated genus followed by a species, e.g. "S. aureus"
                     gsub("(.*)([A-Z][.]) [*]([a-z]+)[*](.*)", "\\1*\\2 \\3*\\4", out, perl = TRUE)
                   },
                   USE.NAMES = FALSE)
  marked <- gsub("* *", " ", marked, fixed = TRUE)
  if (!any(marked %like% "[*].+[*]", na.rm = TRUE)) {
    return(x)
  }
  do.call(expression, lapply(marked, function(m) {
    if (is.na(m)) NA_character_ else md_to_expression(m)[[1]]
  }))
}
