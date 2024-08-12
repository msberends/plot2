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

#' Convert Markdown to Plotmath Expression
#' 
#' This function converts common markdown language to an \R [plotmath][grDevices::plotmath] expression. [plot2()] uses this function internally to convert plot titles and axis titles.
#' @param x text to convert, only the first value will be evaluated
#' @details This function only supports common markdown (italic, bold, bold-italic, subscript, superscript), but also supports some additional functionalities for more advanced expressions using \R [plotmath][grDevices::plotmath]. Please see *Examples*.
#' 
#' In [plot2()], this function can be also set to argument `category.labels` to print the data values as expressions:
#' - `plot2(..., category.labels = md_to_expression)`
#' @export
#' @return An [expression] if `x` is length 1, or a [list] of expressions otherwise
#' @examples
#' # use '*' for italics, not '_', to prevent conflicts with variable naming
#' md_to_expression("this is *italic* text, this is _not italic_ text")
#' 
#' md_to_expression("this is **bold** text")
#' 
#' md_to_expression("this is ***bold and italic*** text")
#' 
#' # subscript and superscript can be done in HTML or markdown with curly brackets:
#' md_to_expression("this is some<sub>subscripted text</sub>, this is also_{subscripted} text")
#' md_to_expression("this is some<sup>superscripted text</sup>, this is also^{superscripted} text")
#' 
#' # use $...$ to use any plotmath expression as-is (see ?plotmath):
#' md_to_expression("text $omega$ text, $a[x]$")
#' 
#' mtcars |>
#'   plot2(mpg, hp,
#'         title = "*These are* the **Greek** lower $omega$ and upper $Omega$",
#'         x.title = "x_{mpg}",
#'         y.title = "y_{hp}")
#'         
#' mtcars |> 
#'   plot2(mpg, hp,
#'         title = "$f[X](x)==frac(1, sigma*sqrt(2*pi))*plain(e)^{frac(-(x-mu)^2, 2*sigma^2)}$",
#'         subtitle = "Some insane $widehat(plotmath)$ title")
md_to_expression <- function(x) {
  x <- as.character(x)
  
  if (length(x) > 1) {
    return(lapply(x, md_to_expression))
  }
  
  if (x %like% "^[$].+[$]$") {
    # a full plotmath expression
    return(parse(text = gsub("^[$](.+)[$]$", "\\1", x, perl = TRUE)))
  }
  x <- paste0("'", x, "'")
  
  # remove backticks
  x <- gsub("`", "", x, fixed = TRUE)
  
  # translate ***bold-italic***
  while (x %like% "[*]{3}.+[*]{3}") {
    x <- gsub("[*]{3}(.+?)[*]{3}", "', bolditalic('\\1'), '", x, perl = TRUE)
  }
  
  # translate **bold**
  while (x %like% "[*]{2}.+[*]{2}") {
    x <- gsub("[*]{2}(.+?)[*]{2}", "', bold('\\1'), '", x, perl = TRUE)
  }
  
  # translate *italic*
  while (x %like% "[*].+[*]") {
    x <- gsub("[*](.+?)[*]", "', italic('\\1'), '", x, perl = TRUE)
  }
  
  # translate sub<sub>script</sub>
  while (grepl("\\S+<sub>.+</sub>", x, ignore.case = FALSE)) {
    x <- gsub("(\\S+?)<sub>(.+?)</sub>", "', \\1['\\2'], '", x, perl = TRUE)
  }
  
  # translate super<sup>script</sup>
  while (grepl("\\S+<sup>.+</sup>", x, ignore.case = FALSE)) {
    x <- gsub("(\\S+?)<sup>(.+?)</sup>", "', \\1^'\\2', '", x, perl = TRUE)
  }
  
  # translate sub_{script}
  while (grepl("\\S+_[{].+[}]", x, ignore.case = FALSE)) {
    x <- gsub("(\\S+?)_[{](.+?)[}]", "', \\1['\\2'], '", x, perl = TRUE)
  }
  
  # translate super^{script}
  x <- gsub("\\^([a-zA-Z0-9,._-]+)", "^{\\1}", x)
  while (grepl("\\S+\\^[{].+[}]", x, ignore.case = FALSE)) {
    x <- gsub("(\\S+?)\\^[{](.+?)[}]+?", "\\1'^'\\2', '", x, perl = TRUE)
  }
  
  # translate $plotmath$, such as $omega$
  while (x %like% "[$].+[$]") {
    x <- gsub("[$](.+?)[$]", "', \\1, '", x, perl = TRUE)
  }
  
  # clean up
  x <- gsub("^', '?'?", "", x)
  x <- gsub("^'', ", "", x)
  x <- gsub(", ''$", "", x)
  x <- gsub(", '$", "", x)
  x <- gsub("''", "'", x, fixed = TRUE)
  x <- gsub("), ', '^", ")^", x, fixed = TRUE)
  x <- gsub(", '^", "^", x, fixed = TRUE)
  x <- gsub("^, ", "", x)
  
  tryCatch(parse(text = paste0("paste(", x, ")")),
           error = function(e) {
             stop("This cannot be parsed by md_to_expression(): \"", x,
                  "\"\n\nFor more complex expressions, start and end with '$' to write in plotmath, or use parse(text = \"...\").",
                  call. = FALSE)
           })
}
