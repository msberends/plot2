# R07 sheet S07: which input widget the Shiny app builds for every plot2() argument.
# Run from the package root:
#   Rscript review_and_overhaul/repro/shiny_widgets_r07.R [output.csv]
# Rerun after implementing D001 and compare (F124).
suppressMessages(devtools::load_all(".", quiet = TRUE))
out <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(out)) out <- file.path(tempdir(), "shiny_widgets_r07.csv")
f <- formals(plot2::plot2)
f <- f[order(names(f))]
skip <- c(".data", "x", "y", "category", "facet", "type", "data", "...", "print")
args <- setdiff(names(f), skip)
widget <- vapply(args, function(nm) {
  tag <- as.character(make_input(nm, f[[nm]], FALSE))
  if (grepl("bslib-input-switch", tag)) "switch"
  else if (grepl("js-range-slider", tag)) "slider"
  else if (grepl("<select", tag)) "select"
  else if (grepl('type="number"', tag)) "number"
  else if (grepl('type="text"', tag)) "text"
  else "other"
}, character(1))
default <- vapply(args, function(nm) paste(deparse(f[[nm]]), collapse = ""), character(1))
d <- data.frame(argument = args, widget = unname(widget), default = unname(default))
print(table(d$widget))
utils::write.csv(d, out, row.names = FALSE)
cat("written to", out, "\n")
