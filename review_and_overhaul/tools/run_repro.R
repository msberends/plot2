# Runs the Reproduction chunk of one or more findings, each in a fresh R process
# (so package state cannot leak between reproductions). Base R only.
#
# Usage (from the package root):
#   Rscript review_and_overhaul/tools/run_repro.R F004
#   Rscript review_and_overhaul/tools/run_repro.R F004 F016
#   Rscript review_and_overhaul/tools/run_repro.R all
#
# Each chunk ends with an "Expected after fix" comment; compare the output with it.

`%||%` <- function(x, y) if (is.null(x)) y else x

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("Give one or more finding IDs, or 'all'.", call. = FALSE)

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
root <- normalizePath(file.path(dirname(sub("^--file=", "", file_arg)), ".."))
pkg_root <- normalizePath(file.path(root, ".."))

files <- list.files(file.path(root, "findings"), pattern = "^F[0-9]+_.*\\.md$", full.names = TRUE)
if (!identical(args, "all")) {
  files <- files[sub("_.*$", "", basename(files)) %in% toupper(args)]
}
if (!length(files)) stop("No matching findings.", call. = FALSE)

extract_chunk <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  start_sec <- which(lines == "## Reproduction")
  if (!length(start_sec)) return(NULL)
  rest <- lines[(start_sec + 1):length(lines)]
  open <- which(rest == "```r")[1]
  if (is.na(open)) return(NULL)
  close <- which(rest == "```")
  close <- close[close > open][1]
  rest[(open + 1):(close - 1)]
}

for (f in files) {
  id <- sub("_.*$", "", basename(f))
  chunk <- extract_chunk(f)
  cat("\n========================================", id, "========================================\n")
  if (is.null(chunk)) {
    cat("(no reproduction chunk)\n")
    next
  }
  tmp <- tempfile(fileext = ".R")
  writeLines(c(sprintf("suppressMessages(devtools::load_all(%s, quiet = TRUE))", deparse(pkg_root)),
               "options(warn = 1)",
               chunk), tmp)
  out <- suppressWarnings(system2("Rscript", tmp, stdout = TRUE, stderr = TRUE))
  cat(out, sep = "\n")
  cat("[exit status:", attr(out, "status") %||% 0, "]\n")
}
