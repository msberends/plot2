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

format2 <- function(x, ...) {
  UseMethod("format2")
}

#' @method format default
format2.default <- function(x, ...) {
  if (isTRUE(list(...)$percent)) {
    format2(as.percentage(x), ...)
  } else {
    # all below have to be wrapped in tryCatch to also work for e.g. functions and calls
    if (tryCatch(all(x %like% "^[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}$", na.rm = TRUE), error = function(e) FALSE)) {
      format2(as.Date(x), ...)
    } else if (tryCatch(all(x %like% "^([0-9]{4}\\-[0-9]{2}\\-[0-9]{2} [0-9]{2}:[0-9]{2}(:[0-9]{2})?|[0-9]+T[0-9]+)$", na.rm = TRUE), error = function(e) FALSE)) {
      format2(as.POSIXct(gsub("([0-9])T([0-9])", "\\1 \\2", x)), ...)
    } else if (tryCatch(all(is.double(x)), error = function(e) FALSE)) {
      format2(as.double(x), ...)
    } else {
      # fall back to base R format(), e.g. for character
      format(x, ...)
    }
  }
}

#' @importFrom cleaner as.percentage
#' @method format numeric
format2.numeric <- function(x,
                            round = ifelse(percent, 1, 2),
                            force_decimals = ifelse(percent, TRUE, FALSE),
                            decimal.mark = dec_mark(),
                            big.mark = big_mark(),
                            min_length = 0,
                            percent = FALSE,
                            ...) {
  
  if (percent == TRUE) {
    format2(x = as.percentage(x),
            round = round,
            force_decimals = force_decimals,
            decimal.mark = decimal.mark,
            big.mark = big.mark)
  } else {
    if (length(x) == 0) {
      return(character())
    }
    
    if (min_length > 0) {
      if (force_decimals == TRUE) {
        plot2_warning("`force_decimals = TRUE` will be overwritten by `min_length = ", min_length, "`")
      }
      x <- formatC(as.integer(x),
                   width = min_length,
                   flag = "0")
    } else {
      if (force_decimals == TRUE) {
        x <- formatC(
          round(as.double(x), digits = round),
          digits = round, # for latest R? ifelse(identical(round, 0), 1, round),
          big.mark = big.mark,
          decimal.mark = decimal.mark,
          format = "f"
        )
      } else {
        x <- format(
          round(as.double(x), digits = round),
          scientific = FALSE,
          big.mark = big.mark,
          decimal.mark = decimal.mark
        )
      }
    }
    x <- gsub("NA", NA, x, fixed = TRUE)
    x <- gsub(" NA", NA, x, fixed = TRUE)
    x <- trimws(x)
    x
  }
}

#' @importFrom cleaner percentage
format2.percentage <- function(x,
                               round = 1,
                               force_decimals = TRUE,
                               decimal.mark = dec_mark(),
                               big.mark = big_mark(),
                               ...) {
  
  if (length(x) == 0) {
    return(character())
  }
  # this will call cleaner:::format.percentage
  out <- trimws(format(x,
                       digits = round,
                       decimal.mark = decimal.mark,
                       big.mark = big.mark))
  out[out == "NA%"] <- NA_character_
  out
}

#' @method format2 Date
format2.Date <- function(x,
                         format = "d mmmm yyyy",
                         locale = "nl",
                         ...) {
  coerce_datetime(x = x, format = format, locale = locale, ...)
}

#' @method format2 POSIXt
format2.POSIXt <- function(x,
                           format = "yyyy-mm-dd HH:MM:SS",
                           locale = "nl",
                           ...) {
  coerce_datetime(x = x, format = format, locale = locale, ...)
}

#' @method format2 hms
format2.hms <- function(x,
                        format = "HH:MM:SS",
                        ...) {
  format2(as.POSIXct(x), format = format, ...)
}

#' @method format2 difftime
format2.difftime <- function(x,
                             round = 2,
                             force_decimals = FALSE,
                             decimal.mark = dec_mark(),
                             big.mark = big_mark(),
                             ...) {
  format2.numeric(x,
                  round = round,
                  force_decimals = force_decimals,
                  percent = FALSE,
                  decimal.mark = decimal.mark,
                  ...)
}

#' @importFrom rlang check_installed
coerce_datetime <- function(x, format, locale, ...) {
  
  format <- posix_date_format(format)
  
  if (!is.null(locale)) {
    if (locale %like% "^[a-z]{2}$") {
      locale <- paste0(tolower(locale), "_", toupper(locale))
    }
    if (Sys.getlocale("LC_TIME") %like% ".UTF-8" && locale %unlike% ".UTF-8") {
      locale <- paste0(locale, ".UTF-8")
    }
    # exception for Dutch on Windows
    if (locale %like% "nl_NL" && tolower(.Platform$OS.type) == "windows") {
      locale <- "Dutch_Netherlands.1252"
    }
  } else {
    locale <- Sys.getlocale("LC_TIME")
  }
  
  if (format == "%B") {
    # same as format = "mmmm"
    check_installed("lubridate")
    return(lubridate::month(x, label = TRUE, abbr = FALSE, locale = locale))
  } else if (format == "%b") {
    # same as format = "mmm"
    check_installed("lubridate")
    return(lubridate::month(x, label = TRUE, abbr = TRUE, locale = locale))
  }
  
  if (Sys.getlocale("LC_TIME") %unlike% locale) {
    old_option <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", locale = locale)
  } else {
    old_option <- NULL
  }
  
  if (inherits(x, c("hms", "difftime", "POSIXlt"))) {
    if (all(x %like% "^[0-9]+:[0-9]+")) {
      x <- paste("1970-01-01", x)
    }
    df <- data.frame(dat = as.POSIXct(x), form = format(as.POSIXct(x), format), stringsAsFactors = FALSE)
  } else {
    df <- data.frame(dat = as.POSIXct(x), form = format(as.POSIXct(x), format), stringsAsFactors = FALSE)
  }
  
  # remove extra spaces
  df$form <- gsub(" +", " ", trimws(df$form))
  
  # replace quarters
  if (any(df$form %like% "(q|qq)")) {
    check_installed("lubridate")
    df$quarter <- lubridate::quarter(df$dat)
    df$quarter[df$form %like% "qq"] <- paste0("Q", df$quarter[df$form %like% "qq"])
    df$form <- unlist(Map(gsub,
                          df$quarter,
                          df$form,
                          MoreArgs = list(pattern = "(q|qq)+"),
                          USE.NAMES = FALSE))
  }
  
  if (!is.null(old_option)) {
    tryCatch(Sys.setlocale("LC_TIME", locale = old_option),
             error = function(e) {
               plot2_warning("Unable to reset original language when running: ",
                             'Sys.setlocale("LC_TIME", locale = "', old_option, '")')
             })
  }
  
  if (format == "unix") {
    as.double(df$form)
  } else {
    df$form
  }
}

format2_scientific <- function(x,
                               round = 2,
                               decimal.mark = dec_mark(),
                               ...) {
  
  # turn into character string in scientific notation
  txt <- format(as.double(x), scientific = TRUE)
  out <- rep(NA_character_, length(x))
  
  out[!is.na(x)] <- paste0(gsub(".", decimal.mark, round(as.double(gsub("^(.*?)e.*", "\\1", txt[!is.na(x)])), digits = round), fixed = TRUE), 
                           " x 10^", 
                           as.double(gsub("^.*?e(.*)", "\\1", txt[!is.na(x)])))
  # remove leading zeroes
  out[!is.na(x) & out == "0 x 10^0"] <- "0"
  # and ones
  out[!is.na(x)] <- gsub("1 x", "", out[!is.na(x)], fixed = TRUE)
  out
}

posix_date_format <- function(format) {
  if (!any(grepl("%", format, fixed = TRUE))) {
    format <- gsub("mmmm", "%B1", format, fixed = TRUE)
    format <- gsub("mmm", "%b", format, fixed = TRUE)
    format <- gsub("mm", "%m", format, fixed = TRUE)
    format <- gsub("MM", "%M1", format, fixed = TRUE)
    format <- gsub("%m1", "%M", gsub("%b1", "%B", tolower(format), 
                                     fixed = TRUE), fixed = TRUE)
    format <- gsub("dddd", "%A", format, fixed = TRUE)
    format <- gsub("ddd", "%a", format, fixed = TRUE)
    format <- gsub("dd", "%!", format, fixed = TRUE)
    format <- gsub("d", "%e", format, fixed = TRUE)
    format <- gsub("%!", "%d", format, fixed = TRUE)
    format <- gsub("ww", "%V", format, fixed = TRUE)
    format <- gsub("w", "%V", format, fixed = TRUE)
    format <- gsub("yyyy_iso", "%G", format, fixed = TRUE)
    format <- gsub("yyyy", "%Y", format, fixed = TRUE)
    format <- gsub("yy_iso", "%g", format, fixed = TRUE)
    format <- gsub("yy", "%y", format, fixed = TRUE)
    format <- gsub("hh", "%H", format, fixed = TRUE)
    format <- gsub("h", "%k", format, fixed = TRUE)
    format <- gsub("ss", "%S", format, fixed = TRUE)
    format <- gsub("unix", "%s", format, fixed = TRUE)
    format <- gsub("epoc%k", "%s", format, fixed = TRUE)
    format <- gsub("iso", "%F", format, fixed = TRUE)
  }
  format
}
