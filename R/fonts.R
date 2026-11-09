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

# Fonts (#23) ------------------------------------------------------------------
#
# Custom fonts are rendered with 'showtext'. Fonts that are not installed are
# downloaded from Google Fonts into the user cache, tools::R_user_dir("plot2",
# "cache"), as the CRAN policy allows; fonts unused for `font_cache_days` days
# are removed from that cache. showtext is switched on only while a plot that
# uses a custom font is drawn: such plots get the extra class "plot2_font",
# whose print() and grid.draw() methods enable showtext for that drawing only.

font_cache_days <- 90

font_cache_dir <- function() {
  file.path(tools::R_user_dir("plot2", which = "cache"), "fonts")
}

# remove cached fonts that were not used for `font_cache_days` days
prune_font_cache <- function(dir = font_cache_dir(), days = font_cache_days) {
  if (!dir.exists(dir)) {
    return(invisible(character(0)))
  }
  families <- list.dirs(dir, recursive = FALSE)
  last_used <- vapply(families, function(f) {
    marker <- file.path(f, ".last_used")
    as.double(file.mtime(if (file.exists(marker)) marker else f))
  }, double(1))
  old <- families[!is.na(last_used) & last_used < as.double(Sys.time()) - days * 24 * 3600]
  unlink(old, recursive = TRUE)
  invisible(basename(old))
}

# register a cached font with sysfonts; returns TRUE if it was found in the cache
font_from_cache <- function(family, dir = font_cache_dir()) {
  path <- file.path(dir, gsub("[^A-Za-z0-9]+", "_", tolower(family)))
  files <- list.files(path, pattern = "^(regular|bold|italic|bolditalic)[.]", full.names = TRUE)
  if (length(files) == 0) {
    return(FALSE)
  }
  face <- function(nm) {
    f <- files[startsWith(basename(files), paste0(nm, "."))]
    if (length(f) == 0) NULL else f[1]
  }
  if (is.null(face("regular"))) {
    return(FALSE)
  }
  sysfonts::font_add(family = family,
                     regular = face("regular"),
                     bold = face("bold"),
                     italic = face("italic"),
                     bolditalic = face("bolditalic"))
  marker <- file.path(path, ".last_used")
  if (!file.exists(marker)) file.create(marker)
  Sys.setFileTime(marker, Sys.time())
  TRUE
}

# download a Google Font into the cache and register it; returns TRUE on success
font_from_google <- function(family, dir = font_cache_dir()) {
  urls <- tryCatch(showtextdb::google_fonts(family), error = function(e) NULL)
  urls <- urls[grepl("_url$", names(urls))]
  if (length(urls) == 0) {
    return(FALSE)
  }
  path <- file.path(dir, gsub("[^A-Za-z0-9]+", "_", tolower(family)))
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  ok <- vapply(names(urls), function(nm) {
    url <- urls[[nm]]
    dest <- file.path(path, paste0(sub("_url$", "", nm), ".", tools::file_ext(url)))
    !inherits(tryCatch(utils::download.file(url, dest, mode = "wb", quiet = TRUE),
                       error = function(e) e, warning = function(w) w), "condition")
  }, logical(1))
  if (!isTRUE(ok[["regular_url"]])) {
    unlink(path, recursive = TRUE)
    return(FALSE)
  }
  file.create(file.path(path, ".last_used"))
  prune_font_cache(dir)
  font_from_cache(family, dir)
}

# draw with showtext switched on, as showtext_auto() would, but only for this drawing
with_showtext <- function(expr) {
  if (!requireNamespace("showtext", quietly = TRUE)) {
    return(force(expr))
  }
  hooks <- getHook("grid.newpage")
  setHook("grid.newpage", function() showtext::showtext_begin(), "append")
  on.exit({
    setHook("grid.newpage", hooks, "replace")
    try(showtext::showtext_end(), silent = TRUE)
  }, add = TRUE)
  if (grDevices::dev.cur() > 1) {
    showtext::showtext_begin()
  }
  force(expr)
}

as_plot2_font <- function(p) {
  if (!inherits(p, "plot2_font")) {
    class(p) <- c("plot2_font", class(p))
  }
  p
}

#' @method print plot2_font
#' @noRd
#' @export
print.plot2_font <- function(x, ...) {
  with_showtext(NextMethod())
}

#' @noRd
#' @exportS3Method grid::grid.draw
grid.draw.plot2_font <- function(x, recording = TRUE) {
  with_showtext(NextMethod())
}
