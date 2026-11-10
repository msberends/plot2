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

# Every @importFrom tag is used by the function it documents, and
# every function from a non-base package that a plot2 function calls
# without `pkg::` is declared in that function's own @importFrom tags.

# Parse the roxygen tags of a block; a tag may continue on the next lines
roxygen_tags <- function(block) {
  txt <- sub("^#' ?", "", block)
  out <- character(0)
  cur <- NULL
  for (l in txt) {
    if (grepl("^@", l)) {
      if (!is.null(cur)) out <- c(out, cur)
      cur <- l
    } else if (!is.null(cur)) {
      cur <- paste(cur, l)
    }
  }
  if (!is.null(cur)) out <- c(out, cur)
  out
}

# Audit all top-level function definitions in R/*.R against their own roxygen block
import_audit <- function(r_dir, desc_file) {
  files <- list.files(r_dir, pattern = "[.]R$", full.names = TRUE)
  base_pkgs <- c("base", "stats", "utils", "grDevices", "graphics", "methods")
  base_funs <- unique(c(unlist(lapply(base_pkgs, getNamespaceExports)), ls(baseenv(), all.names = TRUE)))
  desc <- read.dcf(desc_file)
  imports <- trimws(sub("\\(.*", "", strsplit(gsub("\n", " ", desc[1, "Imports"]), ",")[[1]]))
  imports <- imports[nzchar(imports)]

  pkg_defs <- character(0)
  units <- list()
  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    exprs <- parse(f, keep.source = TRUE)
    srcrefs <- attr(exprs, "srcref")
    for (i in seq_along(exprs)) {
      e <- exprs[[i]]
      if (!(is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && (is.symbol(e[[2]]) || is.character(e[[2]])))) next
      nm <- as.character(e[[2]])
      pkg_defs <- c(pkg_defs, nm)
      if (!(is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function")))) next
      first <- srcrefs[[i]][1]
      j <- first - 1
      while (j >= 1 && grepl("^#'", lines[j])) j <- j - 1
      block <- if (j + 1 <= first - 1) lines[(j + 1):(first - 1)] else character(0)
      units[[length(units) + 1]] <- list(file = basename(f), name = nm, line = first, block = block,
                                         text = paste(lines[first:srcrefs[[i]][3]], collapse = "\n"))
    }
  }
  pkg_defs <- unique(pkg_defs)

  res <- list()
  for (u in units) {
    tags <- roxygen_tags(u$block)
    imp <- tags[grepl("^@importFrom ", tags)]
    declared <- do.call(rbind, lapply(imp, function(t) {
      w <- strsplit(trimws(sub("^@importFrom ", "", t)), "\\s+")[[1]]
      if (length(w) < 2) return(NULL)
      data.frame(pkg = w[1], fun = gsub("`", "", w[-1]), stringsAsFactors = FALSE)
    }))
    if (is.null(declared)) declared <- data.frame(pkg = character(0), fun = character(0))

    pd <- utils::getParseData(parse(text = u$text, keep.source = TRUE))
    pd <- pd[pd$terminal, ]
    # pkg::fun and pkg:::fun need no import
    skip_ids <- pd$id[which(pd$token %in% c("NS_GET", "NS_GET_INT")) + 1]
    # list elements and slots (tags$div(), x@f()) are not functions to import
    skip_ids <- c(skip_ids, pd$id[which(pd$token %in% c("'$'", "'@'")) + 1])
    calls <- unique(gsub("`", "", pd$text[pd$token %in% c("SYMBOL_FUNCTION_CALL", "SPECIAL") & !pd$id %in% skip_ids]))
    # functions can also be used as values, e.g. lapply(x, rev) or labels = waiver
    syms <- unique(gsub("`", "", pd$text[pd$token == "SYMBOL" & !pd$id %in% skip_ids]))
    assigned <- pd$text[which(pd$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) - 1]
    locals <- gsub("`", "", unique(c(pd$text[pd$token %in% c("SYMBOL_FORMALS", "SYMBOL_SUB")], assigned, ".")))
    used <- unique(c(calls, syms))

    # an imported generic is used by an S3 method of it defined here (e.g. ggplot_add.CoordSpider)
    unused <- declared[!declared$fun %in% used & !startsWith(u$name, paste0(declared$fun, ".")), , drop = FALSE]
    missing <- setdiff(calls, c(base_funs, pkg_defs, locals, declared$fun))
    for (k in seq_len(nrow(unused))) {
      res[[length(res) + 1]] <- data.frame(issue = "unused", where = paste0(u$file, ":", u$line, " ", u$name, "()"),
                                           name = paste0(unused$pkg[k], "::", unused$fun[k]))
    }
    for (x in missing) {
      from <- imports[vapply(imports, function(p) x %in% getNamespaceExports(p), logical(1))]
      res[[length(res) + 1]] <- data.frame(issue = "missing", where = paste0(u$file, ":", u$line, " ", u$name, "()"),
                                           name = paste0(if (length(from)) paste(from, collapse = "/") else "?", "::", x))
    }
  }
  out <- do.call(rbind, res)
  if (is.null(out)) out <- data.frame(issue = character(0), where = character(0), name = character(0))
  out
}

audit_report <- function(a) paste0("\n", paste(a$where, a$name, sep = ": ", collapse = "\n"))

test_that("every @importFrom tag is used by the function it documents", {
  a <- import_audit(source_file("R"), source_file("DESCRIPTION"))
  a <- a[a$issue == "unused", ]
  expect(nrow(a) == 0, paste("Imported but not used:", audit_report(a)))
})

test_that("every non-base function called without pkg:: is imported by the calling function", {
  a <- import_audit(source_file("R"), source_file("DESCRIPTION"))
  a <- a[a$issue == "missing", ]
  expect(nrow(a) == 0, paste("Used but not declared in the function's @importFrom:", audit_report(a)))
})

test_that("NAMESPACE imports no name from two packages", {
  ns <- parseNamespaceFile(basename(dirname(source_file("NAMESPACE"))), dirname(dirname(source_file("NAMESPACE"))))
  imp <- do.call(rbind, lapply(Filter(is.list, ns$imports), function(i) {
    data.frame(pkg = i[[1]], fun = gsub("`", "", i[[2]]))
  }))
  dup <- imp[imp$fun %in% imp$fun[duplicated(imp$fun)], ]
  expect(nrow(dup) == 0, paste("Imported from more than one package:", paste(dup$pkg, dup$fun, sep = "::", collapse = ", ")))
})

test_that("the audit detects an unused and a missing import", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "R"))
  writeLines(c("Package: x", "Imports: dplyr"), file.path(d, "DESCRIPTION"))
  writeLines(c("#' @importFrom dplyr filter",
               "f <- function(df) {",
               "  df <- mutate(df, a = 1)",
               "  dplyr::select(df, a)",
               "}",
               "g <- function(x) lapply(x, rev)"),
             file.path(d, "R", "f.R"))
  a <- import_audit(file.path(d, "R"), file.path(d, "DESCRIPTION"))
  expect_identical(a$issue, c("unused", "missing"))
  expect_identical(a$name, c("dplyr::filter", "dplyr::mutate"))
})
