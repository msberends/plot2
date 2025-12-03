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

#' Get Plot Title
#' 
#' Get the title of the plot, or a default value. If the title is not set in a plot, this function tries to generate one from the plot mapping.
#' @param plot A `ggplot2` plot.
#' @param valid_filename A [logical] to indicate whether the returned value should be a valid filename, defaults to `TRUE`.
#' @param default The default value, if a plot title is absent.
#' @importFrom ggplot2 is.ggplot
#' @export
#' @examples
#' without_title <- plot2(mtcars)
#' with_title <- plot2(mtcars, title = "Plotting **mpg** vs. **cyl**!")
#' 
#' # default is a guess:
#' get_plot_title(without_title)
#' get_plot_title(without_title, valid_filename = FALSE)
#' get_plot_title(with_title)
#' get_plot_title(with_title, valid_filename = FALSE)
#' 
#' # unless 'default' is set (only affects plots without title):
#' get_plot_title(without_title, default = "title")
#' get_plot_title(with_title, default = "title")
get_plot_title <- function(plot,
                           valid_filename = TRUE,
                           default = NULL) {
  
  if (!is.ggplot(plot)) {
    stop("`plot` must be a ggplot2 object.", call. = FALSE)
  }
  
  current_title <- plot$labels$title
  default_title <- get_default_title(plot = plot, default = NULL)
  
  if (identical(current_title, default_title)) {
    if (!is.null(default) && !is.na(default)) {
      title <- default
    } else {
      title <- current_title
    }
  } else {
    title <- gsub("\"", "***", as.character(current_title)) |>
      strsplit("***", fixed = TRUE) |>
      unlist()
    title <- title[which(title != "" & title != "paste(" & 
                           title != ", italic(" & title != ", bold(" & title != ", bolditalic(" &
                           title != ")" & title != "), ")]
    title <- gsub("_+", " ", paste0(title, collapse = "", sep = "")) |> 
      trimws()
    if (title == "") {
      title <- get_default_title(plot = plot, default = default)
    }
  }
  
  if (!is.null(title) && !is.na(title) && isTRUE(valid_filename)) {
    title <- gsub("[ .]+", "_", 
                  gsub("[?!|<>|:/\\*]", "", title)) |> 
      tolower()
  }
  
  caption <- plot$labels$caption
  if (!is.null(caption) && caption %like% "^[0-9a-f]+$") {
    if (is.na(title)) {
      title <- NULL
    }
    title <- trimws(paste(title, "-", caption))
  }
  
  title
}

get_default_title <- function(plot, default) {
  if (!is.null(default)) {
    return(default)
  }
  
  get_mapping <- function(plot) {
    c(gsub("~", "", sapply(plot$mapping, deparse)),
      gsub("~", "", sapply(plot$facet$params$facets, deparse)))
  }
  
  
  mapp <- get_mapping(plot)
  # no nonsense argument names
  mapp <- mapp[!mapp %in% c("x", "y")]
  val <- unname(mapp[names(mapp) == "y"])
  
  # generate txt of Y-axis
  is_dutch <- Sys.getlocale() %like% "nl|dutch|nederlands"
  txt_per <- ifelse(is_dutch, "per", "per")
  txt_sep <- ifelse(is_dutch, "en", "and")
  
  val[tolower(val) %in% c("`n()`", "n", "count", "freq")] <- ifelse(is_dutch, "aantal", "count")
  
  val <- gsub(", ?na[.]rm ?= ?(T(RUE)?|F(ALSE)?)", "", val)
  val <- gsub("^`(n_distinct|length\\(unique)\\(+(.*?)\\)+`$",
              paste(ifelse(is_dutch, "unieke", "unique"), "\\2"), tolower(val))
  
  val <- gsub("^`median\\(+(.*?)\\)+`$",
              paste(ifelse(is_dutch, "mediane", "median"), "\\1"), tolower(val))
  
  val <- gsub("^`mean\\(+(.*?)\\)+`$",
              paste(ifelse(is_dutch, "gemiddelde", "mean"), "\\1"), tolower(val))
  
  val <- gsub("^`min\\(+(.*?)\\)+`$",
              paste(ifelse(is_dutch, "minimale", "minimum"), "\\1"), tolower(val))
  
  val <- gsub("^`max\\(+(.*?)\\)+`$",
              paste(ifelse(is_dutch, "maximale", "maximum"), "\\1"), tolower(val))
  
  val <- gsub("[_.]", " ", val)
  
  if (length(val) > 0) {
    val <- tolower(paste0(val, " ", txt_per, " "))
    substr(val, 1, 1) <- toupper(substr(val, 1, 1))
  } else {
    val <- ""
  }
  
  mapp <- tolower(unique(unname(mapp[!names(mapp) %in% c("y", "group")])))
  mapp <- gsub("(^`|`$)", "", mapp)
  mapp <- gsub("[_.]", " ", mapp)
  mapp <- gsub("c\\((.*?)\\)", "\\1", mapp)
  mapp <- unlist(lapply(mapp, strsplit, ", ?"), use.names = FALSE)
  
  # don't create title if it contains nonsense such as functions
  if (any(mapp %like% "[()]")) {
    mapp <- character(0)
  }
  
  if (length(mapp) >= 1 && length(val) > 0 && val != "") {
    # transform to form: "x, y and z"
    if (length(mapp) > 1) {
      mapp <- paste(paste(mapp[seq_len(length(mapp) - 1)], collapse = ", "),
                    txt_sep, mapp[length(mapp)])
    }
    paste0(val, mapp)
  } else {
    default
  }
}
