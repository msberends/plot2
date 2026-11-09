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

#' @method format2 default
#' @exportS3Method
format2.default <- function(x, ...) {
  if (isTRUE(list(...)$percent)) {
    format2(as_percentage(x), ...)
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

#' @method format2 numeric
#' @exportS3Method
format2.numeric <- function(x,
                            round = ifelse(percent, 1, 2),
                            force_decimals = ifelse(percent, TRUE, FALSE),
                            decimal.mark = dec_mark(),
                            big.mark = big_mark(),
                            min_length = 0,
                            percent = FALSE,
                            ...) {
  
  if (percent == TRUE) {
    format2(x = as_percentage(x),
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

#' @exportS3Method
format2.percentage <- function(x,
                               round = 1,
                               force_decimals = TRUE,
                               decimal.mark = dec_mark(),
                               big.mark = big_mark(),
                               ...) {
  
  if (length(x) == 0) {
    return(character())
  }
  # formats like cleaner's percentage class did, without depending on cleaner (#120)
  out <- trimws(format_percentage(x,
                                  digits = round,
                                  decimal.mark = decimal.mark,
                                  big.mark = big.mark))
  out[out == "NA%"] <- NA_character_
  out
}

#' @method format2 Date
#' @exportS3Method
format2.Date <- function(x,
                         format = "d mmmm yyyy",
                         locale = NULL,
                         ...) {
  coerce_datetime(x = x, format = format, locale = locale, ...)
}

#' @method format2 POSIXt
#' @exportS3Method
format2.POSIXt <- function(x,
                           format = "yyyy-mm-dd HH:MM:SS",
                           locale = NULL,
                           ...) {
  coerce_datetime(x = x, format = format, locale = locale, ...)
}

#' @method format2 hms
#' @exportS3Method
format2.hms <- function(x,
                        format = "HH:MM:SS",
                        ...) {
  format2(as.POSIXct(x), format = format, ...)
}

#' @method format2 difftime
#' @exportS3Method
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

coerce_datetime <- function(x, format, locale, ...) {
  format <- posix_date_format(format)
  
  # format in the requested language; any change of LC_TIME is restored on exit (#15)
  local_time_locale(locale)
  
  if (inherits(x, c("hms", "difftime", "POSIXlt"))) {
    if (all(x %like% "^[0-9]+:[0-9]+")) {
      x <- paste("1970-01-01", x)
    }
  }
  # keep the time zone of date-times (#41)
  tz <- if (inherits(x, "POSIXt")) attr(as.POSIXct(x), "tzone") else NULL
  dat <- if (is.null(tz)) as.POSIXct(x) else as.POSIXct(x, tz = tz[1])
  
  # quarters (q = 1, qq = Q1) are not known to strftime(); posix_date_format() marks them with \001
  form <- format(dat, format)
  if (any(grepl("\001", form, fixed = TRUE), na.rm = TRUE)) {
    quarter <- as.POSIXlt(dat)$mon %/% 3 + 1
    form <- unlist(Map(function(f, q) gsub("\001", q, f, fixed = TRUE), form, quarter), use.names = FALSE)
  }
  
  # remove extra spaces
  form <- gsub(" +", " ", trimws(form))
  form[is.na(dat)] <- NA_character_
  
  if (identical(format, "%s")) {
    as.double(form)
  } else {
    # always character, also for month names (#41)
    form
  }
}

# Set LC_TIME for the calling function and restore it when that function exits.
# `locale` is a locale name or a two-letter language code; NULL uses the
# plot2.language option if set, otherwise the system locale.
local_time_locale <- function(locale = NULL, .env = parent.frame()) {
  if (is.null(locale)) {
    lang <- plot2_language()
    if (lang == "en" && is.null(getOption("plot2.language"))) {
      # no language set: keep the system locale
      return(invisible(NULL))
    }
    locale <- lang
  }
  candidates <- locale
  if (locale %like% "^[a-z]{2}$") {
    # common names of the locale on Linux, macOS and Windows
    base <- paste0(tolower(locale), "_", switch(tolower(locale), en = "US", toupper(locale)))
    candidates <- c(paste0(base, c(".UTF-8", ".utf8", "")),
                    switch(tolower(locale), nl = "Dutch_Netherlands.1252", en = "English_United States.1252", NULL))
  }
  old <- Sys.getlocale("LC_TIME")
  if (old %in% candidates) {
    return(invisible(NULL))
  }
  new <- ""
  for (cand in candidates) {
    new <- suppressWarnings(Sys.setlocale("LC_TIME", locale = cand))
    if (!identical(new, "")) break
  }
  if (identical(new, "")) {
    plot2_caution("The locale \"", candidates[1], "\" is not available on this system, so dates use the system locale")
    return(invisible(NULL))
  }
  do.call(on.exit, list(substitute(suppressWarnings(Sys.setlocale("LC_TIME", locale = old)), list(old = old)), add = TRUE),
          envir = .env)
  invisible(NULL)
}

format2_scientific <- function(x,
                               round = 2,
                               decimal.mark = dec_mark(),
                               ...) {
  out <- vector("list", length(x))
  
  for (i in seq_along(x)) {
    v <- x[i]
    
    # Handle NA cleanly
    if (is.na(v)) {
      out[[i]] <- NA
      next
    }
    
    # Special case zero
    if (v == 0) {
      out[[i]] <- "0"
      next
    }
    
    # Get scientific components
    txt <- format(v, scientific = TRUE)
    mant <- as.double(sub("e.*", "", txt))
    expn <- as.integer(sub(".*e", "", txt))
    
    # Round mantissa to two digits
    mant <- round(mant, round)
    mant <- trimws(format(mant, decimal.mark = decimal.mark))
    
    # Build expression
    # plotmath %*% gives a multiplication sign on every device and console (#119)
    out[[i]] <- bquote(.(mant) %*% 10^.(expn))
  }
  out
}

# Translate plot2's date format tokens (e.g. "d mmmm yyyy") to strftime() codes (#41).
# A run of letters is converted only if it consists entirely of tokens, so that
# words such as "Week" or "Month" stay as they are; text in single quotes is
# always kept literally. Tokens are case-insensitive, except "MM" (minutes)
# versus "mm" (month). Formats that already contain "%" are used as-is.
posix_date_format <- function(format) {
  if (is.null(format) || is.na(format) || any(grepl("%", format, fixed = TRUE))) {
    return(format)
  }
  tokens <- c(yyyy_iso = "%G", yy_iso = "%g", epoch = "%s", unix = "%s", iso = "%F",
              mmmm = "%B", mmm = "%b", mm = "%m",
              dddd = "%A", ddd = "%a", dd = "%d", d = "%e",
              ww = "%V", w = "%V", yyyy = "%Y", yy = "%y",
              hh = "%H", h = "%k", ss = "%S", qq = "{QQ}", q = "{Q}")
  convert_run <- function(run) {
    # split a run of letters entirely into tokens (longest first), or return NULL
    if (run == "") return("")
    if (startsWith(run, "MM") && !startsWith(run, "MMM")) {
      rest <- convert_run(substr(run, 3, nchar(run)))
      return(if (is.null(rest)) NULL else paste0("%M", rest))
    }
    for (tok in names(tokens)[order(-nchar(names(tokens)))]) {
      if (startsWith(tolower(run), tok)) {
        rest <- convert_run(substr(run, nchar(tok) + 1, nchar(run)))
        if (!is.null(rest)) return(paste0(tokens[[tok]], rest))
      }
    }
    NULL
  }
  # split into quoted literals, letter runs, and other characters
  parts <- regmatches(format, gregexpr("'[^']*'|[A-Za-z_]+|[^A-Za-z_']+", format))[[1]]
  out <- vapply(parts, function(p) {
    if (startsWith(p, "'")) {
      gsub("'", "", p, fixed = TRUE)
    } else if (grepl("^[A-Za-z_]+$", p)) {
      conv <- convert_run(p)
      if (is.null(conv)) p else conv
    } else {
      p
    }
  }, character(1), USE.NAMES = FALSE)
  out <- paste(out, collapse = "")
  # a single q means the quarter number, qq "Q" plus the number
  out <- gsub("{Q}", "\001", out, fixed = TRUE)
  out <- gsub("{QQ}", "Q\001", out, fixed = TRUE)
  out
}

# percentages, formerly from the cleaner package (#120); the class name "percentage"
# is kept, so that format2() also formats percentages made with cleaner
as_percentage <- function(x) {
  if (inherits(x, "percentage")) {
    return(x)
  }
  structure(as.double(x), class = c("percentage", "numeric"))
}

format_percentage <- function(x, digits = 1, ...) {
  x <- as.double(x) * 100
  # round half away from zero
  x <- trunc(abs(x) * 10 ^ digits + 0.5 + sqrt(.Machine$double.eps)) / 10 ^ digits * sign(x)
  out <- format(x, scientific = FALSE, ...)
  out[!is.na(out)] <- paste0(out[!is.na(out)], "%")
  out
}
