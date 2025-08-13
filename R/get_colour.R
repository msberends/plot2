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

viridisLite_colours <- c("viridis", "magma", "inferno", "plasma", "cividis", "rocket", "mako", "turbo")

#' Colours from \R, Viridis and More
#'
#' Colours from \R, viridis and more. The output prints in the console with the actual colours.
#' @param x colour or colour palette name. Input can be:
#' * One of the colourblind-safe `viridisLite` palettes: `r paste0('\n  - \u0060"', viridisLite_colours, '"\u0060', collapse = "")`
#' * One of the built-in palettes in \R (these are from \R `r paste(R.version$major, R.version$minor, sep = ".")`): `r paste0('\n  - \u0060"', sort(c(grDevices::palette.pals(), "topo", "heatmap", "rainbow", "terrain", "greyscale", "grayscale")), '"\u0060', collapse = "")`
#' * One of the `r length(colours())` built-in [colours()] in \R (even case-insensitive), such as `r paste0('\u0060"', sort(sample(colours()[colours() %unlike% "^grey|gray"], 5)), '"\u0060', collapse = ", ")`
#' * One of the pre-registered colours using [register_colour()]
#' @param length size of the vector to be returned
#' @param opacity amount of opacity (0 = solid, 1 = transparent)
#' @details A palette from \R will be expanded where needed, so even `get_colour("R4", length = 20)` will work, despite "R4" only supporting a maximum of eight colours.
#' @return [character] vector in HTML format (i.e., `"#AABBCC"`) with new class `colour`
#' @rdname colour
#' @importFrom grDevices rainbow heat.colors terrain.colors topo.colors col2rgb colours grey.colors rgb
#' @importFrom viridisLite viridis
#' @export
#' @examples
#' get_colour(c("red", "tan1", "#ffa", "FFAA00"))
#' 
#' par(mar = c(0.5, 2.5, 1.5, 0)) # set plot margins for below plots
#' 
#' # all colourblind-safe colour palettes from the famous viridisLite package
#' barplot(1:7,
#'         col = get_colour("viridis", 7))
#' barplot(1:7,
#'         col = get_colour("magma", 7))
#'
#' barplot(8:1,
#'         col = get_colour("R4", 8),
#'         main = "Some palettes have only 8 colours...")
#' barplot(20:1,
#'         col = get_colour("R4", 20),
#'         main = "Not anymore!")
get_colour <- function(x, length = 1, opacity = 0) {
  
  if (is.null(x)) {
    # transparent white
    return(structure(rep("#FFFFFF00", length),
                     class = c("colour", "character")))
  }
  
  opacity <- as.double(opacity)
  length <- as.double(length)
  if (all(is.na(length))) {
    stop("length must be a numeric value")
  }
  
  if ((length(x) > 1 && length != 1) | (length > 1 && length(x) != 1)) {
    stop("Either the length of `x`, or `length` must be 1")
  }
  
  x_names <- names(x) # required for named colour vectors such as in ggplot2 and plot2
  x <- unname(x)
  
  # NA: should become transparent white
  x_na <- is.na(x)
  x[x_na] <- "#FFFFFF"
  
  if (length(x) == 1) {
    
    if (x %in% names(plot2_env$reg_cols)) {
      x <- plot2_env$reg_cols[names(plot2_env$reg_cols) == x][[1]]
      if (length(x) < length) {
        x <- grDevices::colorRampPalette(x)(length)
        # plot2_caution("Colour palette expanded using grDevices::colorRampPalette()")
      }
      x <- x[seq_len(length)]
      # some support names, so return the object
      return(structure(x, class = c("colour", "character")))
      
    } else if (x %in% viridisLite_colours) {
      x <- viridis(length, option = x)
      
    } else if (tolower(x) %in% tolower(c("R", grDevices::palette.pals()))) {
      if (toupper(x) == "R") {
        x <- "R4"
      }
      new_cols <- tryCatch(grDevices::palette.colors(length, palette = x),
                           error = function(e) NULL,
                           warning = function(w) NULL)
      if (is.null(new_cols)) {
        # failed or returned a warning, so now try to expand the colour palette with an 8-sized basis
        new_cols <- grDevices::colorRampPalette(grDevices::palette.colors(8, palette = x))(length)
        # plot2_caution("Colour palette expanded using grDevices::colorRampPalette()")
      }
      # some support names, so return the object
      return(structure(new_cols, class = c("colour", "character")))
      
    } else if (x == "topo") {
      x <- topo.colors(length)
    } else if (x == "heatmap") {
      x <- heat.colors(length)
    } else if (x == "rainbow") {
      x <- rainbow(length)
    } else if (x == "terrain") {
      x <- terrain.colors(length)
    } else if (x %in% c("greyscale", "grayscale")) {
      x <- grey.colors(length)
    }
    
    if (length > 1 & length(x) == 1) {
      x <- rep(x, length = length)
    }
    
  }
  
  # replace R colour names with HTML code
  if (any(tolower(x) %in% colours())) {
    x[tolower(x) %in% colours()] <- sapply(as.list(as.data.frame(col2rgb(tolower(x[tolower(x) %in% colours()])))),
                                           function(rgb) rgb(red = rgb[1], green = rgb[2], blue = rgb[3], maxColorValue = 255))
  }
  # replace pre-registered colour names with HTML code
  if (any(x %in% names(plot2_env$reg_cols))) {
    x[x %in% names(plot2_env$reg_cols)] <- unlist(plot2_env$reg_cols[match(x[x %in% names(plot2_env$reg_cols)], names(plot2_env$reg_cols))], use.names = FALSE)
  }
  
  # support #ffa -> #ffffaa
  shorts <- x %like% "^#?[0-F]{3}$"
  if (any(shorts)) {
    x[shorts] <- paste0(sapply(strsplit(x[shorts], "")[[1]],
                               function(x) ifelse(x == "#", "", paste0(x, x))),
                        collapse = "")
  }
  
  # support fffaa -> #fffaa
  missing_hastag <- x %like% "^[0-F]{6}$"
  if (any(missing_hastag)) {
    x[missing_hastag] <- paste0("#", x[missing_hastag])
  }
  
  # some colours add FF as opacity to HTML colour - remove them
  x[which(nchar(x) > 7 & x %like% "FF$")] <- substr(x[which(nchar(x) > 7 & x %like% "FF$")], 1, 7)
  
  invalid <- x %unlike% "^#[0-F]{6,8}$"
  if (any(invalid)) {
    inv <- unique(x[invalid])
    plot2_warning("Invalid colour", ifelse(length(inv) != 1, "s", ""), ", replacing with a grey: ", paste0(font_magenta(paste0('"', inv, '"'), collapse = NULL), collapse = ", "))
    x[invalid] <- sapply(seq_len(length(invalid)), function(i) {
      int <- sample(c(38:217), 1, replace = FALSE)
      rgb(int, int, int, maxColorValue = 255)
    })
  }
  
  if (length > length(x)) {
    # misses some colours, so fill with greys that are 70-95% white
    plot2_warning("Missing ", length - length(x), " colours, filling with random light greys")
    x <- c(x[1:length(x)],
           grey.colors(length - length(x), start = 0.7, end = 0.95))
  }
  
  # so everything is now hexadecimal; paste alpha (opacity) to it
  if (opacity > 0 & any(nchar(x) == 7)) {
    opacity <- toupper(as.hexmode(round((1 - opacity) * 255)))
    if (nchar(opacity) == 1) {
      opacity <- paste0("0", opacity)
    }
    x[nchar(x) == 7] <- paste0(x[nchar(x) == 7], opacity)
  }
  
  # support NA - make them white and transparent
  x[x_na] <- "#FFFFFF00"
  
  x <- toupper(unname(x))
  names(x) <- x_names
  structure(x, class = c("colour", "character"))
}

#' @rdname colour
#' @param ... named vectors with known, valid colours. They must be coercible with [get_colour()].
#' @details
#' 
#' ### Registering Colours In Another Package
#' 
#' To register/unregister colours in another package, add something like this to a package file `R/zzz.R`:
#' 
#' ```r
#' #' @importFrom plot2 register_colour
#' .onLoad <- function(...) {
#'   register_colour(
#'     my_colour1  = "#007A8A",
#'     my_colour2  = "#2E8540",
#'     my_colour3  = "#5B3FA8",
#'     my_colours  = c("my_colour1", "my_colour2", "my_colour3"))
#' 
#'   options(plot2.colour = "my_colours", plot2.colour_font_secondary = "my_colour1")
#' }
#'
#' #' @importFrom plot2 unregister_colour
#' .onUnload <- function(...) {
#'   register_colour("^my_colour") # this is a regular expression
#'   options(plot2.colour = NULL, plot2.colour_font_secondary = NULL)
#' }
#' ```
#' 
#' Do not forget to add `plot2` to `Imports:` in the `DESCRIPTION` file of your package.
#' 
#' @export
#' @examples
#' 
#' 
#' # Registering Colours --------------------------------------------------
#' 
#' # to register colours, use named input - the values will be evaluated
#' # with get_colour()
#' get_colour("red123")
#' register_colour(red123 = "red", red456 = "#ff0000", red789 = "f00")
#' get_colour("red123")
#' get_colour("red456")
#' get_colour("red789")
#' 
#' # you can also register a group name
#' register_colour(red_group = c("red123", "ff4400", "red3", "red4"))
#' get_colour("red_group")
#' get_colour("red_group", 3)
#' 
#' # Registering colours is ideal for your (organisational) style in plots.
#' # Let's say these are your style:
#' register_colour(navy_blue = "#1F3A93",
#'                 burnt_orange = "#D35400",
#'                 forest_green = "#2C6F47",
#'                 goldenrod_yellow = "#DAA520",
#'                 slate_grey = "#708090",
#'                 plum_purple = "#8E4585")
#' 
#' # Then register the whole colour list too:
#' register_colour(my_organisation = c("navy_blue", "burnt_orange",
#'                                     "forest_green", "goldenrod_yellow",
#'                                     "slate_grey", "plum_purple"))
#' # Check that it works:
#' get_colour("my_organisation", length = 6)
#' 
#' # Now use it in plots as you like:
#' iris |>
#'   plot2(x = Species, y = where(is.double), colour = "my_organisation")
#' 
#' # Or even set the option to use it in any future plot:
#' options(plot2.colour = "my_organisation")
#' 
#' iris |>
#'   plot2(x = Species, y = where(is.double))
#' 
#' # reset option again
#' options(plot2.colour = NULL)
register_colour <- function(...) {
  dots <- list(...)
  if (length(dots) == 1 && is.null(names(dots)) && !is.null(names(dots[[1]]))) {
    dots <- as.list(dots[[1]])
  }
  for (i in seq_len(length(dots))) {
    if (is.null(names(dots)[i])) {
      stop("Input must be named")
    }
    # try to coerce
    out <- as.character(suppressWarnings(suppressMessages(get_colour(dots[[i]]))))
    if (anyNA(out)) {
      stop("Input must not be NA, and must be known, valid colours")
    }
    out <- list(out)
    names(out) <- names(dots)[i]
    # if a previous identical names was registered, overwrite it
    plot2_env$reg_cols <- plot2_env$reg_cols[which(names(plot2_env$reg_cols) != names(out))]
    # save the colour
    plot2_env$reg_cols <- c(plot2_env$reg_cols, out)
  }
  message(length(dots), " colour", ifelse(length(dots) == 1, "", "s"), " registered.")
}

#' @rdname colour
#' @param regex a [regex] to unregister colours
#' @export
unregister_colour <- function(regex) {
  len <- length(plot2_env$reg_cols)
  plot2_env$reg_cols <- plot2_env$reg_cols[which(names(plot2_env$reg_cols) %unlike% regex)]
  changed <- len - length(plot2_env$reg_cols)
  message(changed, " colour", ifelse(changed == 1, "", "s"), " unregistered.")
}

get_registered_colour <- function(name) {
  name <- name[1L]
  if (!name %in% names(plot2_env$reg_cols)) {
    stop("colour not registered: ", name)
  }
  get_colour(plot2_env$reg_cols[[which(names(plot2_env$reg_cols) == name)]])
}

#' @method as.character colour
#' @rdname colour
#' @export
as.character.colour <- function(x, ...) {
  substr(unclass(x), 1, 9)
}

#' @method print colour
#' @importFrom crayon make_style
#' @rdname colour
#' @export
print.colour <- function(x, ...) {
  tryCatch({
    if (any(nchar(as.character(x)) == 9)) {
      # some have opacity as last 2 characters
      str_length <- 9
    } else {
      str_length <- 7
    }
    y <- x
    nms <- substr(names(x), 1, str_length + 3)
    nms <- format(c(nms, strrep(" ", str_length + 4)), justify = "right")[seq_len(length(x))]
    cols <- substr(y[!is.na(x)], 1, str_length)
    y[!is.na(x)] <- paste0('"', cols, '"',
                           sapply(cols, function(z) make_style(z, bg = TRUE)(" ")))
    max_print <- floor(options()$width / (str_length + 5)) + 1
    for (i in c(0:(length(y) / max_print))) {
      from <- i * max_print + 1
      to <- min(i * max_print + max_print, length(y))
      if (from <= length(y)) {
        ind <- seq(from = from, to = to, by = 1)
        formatted_index_nr <- formatC(from,
                                      width = ifelse(length(y) <= max_print, 1, nchar(length(y))))
        cat(
          # names, should they exist
          ifelse(!is.null(names(x)),
                 paste0(strrep(" ", nchar(formatted_index_nr) + 2), paste0(nms[ind], collapse = ""), "\n"),
                 ""),
          # index number
          paste0("[", formatted_index_nr, "]"),
          " ",
          # actual colours
          paste0(y[ind], " "), sep = "")
        cat("\n")
      }
    }
  }, error = function(e) print(unclass(x), ...))
  invisible(x)
}

#' @method rev colour
#' @noRd
#' @export
rev.colour <- function(x) {
  structure(stats::setNames(rev(as.character(x)), rev(names(x))),
            class = class(x))
}

#' @method rep colour
#' @noRd
#' @export
rep.colour <- function(x, ...) {
  structure(rep(as.character(x), ...),
            class = class(x))
}

#' @method unique colour
#' @noRd
#' @export
unique.colour <- function(x, ...) {
  if (is.null(names(x))) {
    return(get_colour(unique(as.character(x))))
  }
  out <- data.frame(vals = as.character(x),
                    nms = names(x),
                    stringsAsFactors = FALSE)
  out$uniq <- paste(out$vals, out$nms)
  out <- unique(out, fromLast = TRUE)
  get_colour(stats::setNames(out$vals, out$nms))
}

#' @method c colour
#' @noRd
#' @export
c.colour <- function(...) {
  get_colour(unlist(lapply(list(...), as.character)))
}

#' @rdname colour
#' @param white number between `[0, 1]` to add white to `x`
#' @export
#' @examples
#' 
#' 
#' # Use add_white() to add white to existing colours:
#' colours <- get_colour("R4", 6)
#' colours
#' add_white(colours, 0.25)
#' add_white(colours, 0.5)
#' add_white(colours, 0.75)
#' 
#' add_white("red", 1/128)
#' add_white("red", 1/64)
#' add_white("red", 1/32)
add_white <- function(x, white) {
  white <- min(1000, max(1, 1000 - (white * 1000)))
  out <- unname(vapply(FUN.VALUE = character(1),
                       x,
                       function(y) grDevices::colorRampPalette(c("white", y))(1000)[white]))
  get_colour(out)
}
