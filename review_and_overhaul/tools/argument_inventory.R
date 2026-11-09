# Generates the R05 argument inventory of plot2().
#
# Run from the package root:
#   Rscript review_and_overhaul/tools/argument_inventory.R
#
# Writes review_and_overhaul/reviews/R05_api/A00_argument-inventory.md and a
# machine-readable copy A00_argument-inventory.csv next to it. Needs devtools
# and roxygen2 (both already used for package development).

suppressMessages(devtools::load_all(".", quiet = TRUE, export_all = TRUE))
ns <- asNamespace("plot2")

out_dir <- file.path("review_and_overhaul", "reviews", "R05_api")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

dep <- function(x) {
  if (missing(x) || identical(x, quote(expr = ))) return("(none)")
  s <- paste(deparse(x, width.cutoff = 500L), collapse = " ")
  s <- gsub("\\s+", " ", s)
  if (nchar(s) > 60) s <- paste0(substr(s, 1, 57), "...")
  s
}
md_escape <- function(x) gsub("|", "\\|", x, fixed = TRUE)

# --- formals of generic, methods and plot2_exec ------------------------------
generic <- formals(ns$plot2)
args <- names(generic)
methods <- c("plot2.default", "plot2.data.frame", "plot2.formula", "plot2.matrix",
             "plot2.sf", "plot2.freq", "plot2.lm")
methods <- methods[vapply(methods, exists, logical(1), envir = ns)]
exec <- formals(ns$plot2_exec)

method_diffs <- vapply(args, function(a) {
  d <- character(0)
  for (m in methods) {
    f <- formals(get(m, envir = ns))
    if (!a %in% names(f)) {
      d <- c(d, paste0(sub("^plot2\\.", "", m), ": absent"))
    } else if (!identical(f[[a]], generic[[a]])) {
      d <- c(d, paste0(sub("^plot2\\.", "", m), ": `", dep(f[[a]]), "`"))
    }
  }
  if (!a %in% names(exec)) d <- c(d, "plot2_exec: absent")
  else if (!identical(exec[[a]], generic[[a]]) && !identical(exec[[a]], quote(expr = ))) {
    d <- c(d, paste0("exec: `", dep(exec[[a]]), "`"))
  }
  paste(d, collapse = "; ")
}, character(1))
extra_method_args <- unlist(lapply(methods, function(m) {
  x <- setdiff(names(formals(get(m, envir = ns))), args)
  if (length(x)) paste0(m, "(", x, ")") else NULL
}))

# --- roxygen @param blocks of R/plot2.R --------------------------------------
blocks <- roxygen2::parse_file(file.path("R", "plot2.R"), env = NULL)
param_tags <- unlist(lapply(blocks, function(b) {
  Filter(function(t) t$tag == "param", b$tags)
}), recursive = FALSE)
doc_of <- list()
block_of <- list()
for (t in param_tags) {
  nms <- trimws(strsplit(t$val$name, ",")[[1]])
  for (n in nms) {
    doc_of[[n]] <- t$val$description
    block_of[[n]] <- t$val$name
  }
}
documented_default <- function(txt) {
  if (is.null(txt)) return("")
  txt <- gsub("\\s+", " ", txt)
  sent <- unlist(strsplit(txt, "(?<=[.;:])\\s+(?=[A-Z`(])", perl = TRUE))
  hit <- sent[grepl("default|defaults|if left blank|determined automatically", sent, ignore.case = TRUE)]
  if (!length(hit)) return("")
  s <- paste(hit, collapse = " ")
  if (nchar(s) > 140) s <- paste0(substr(s, 1, 137), "...")
  s
}

# --- use in plot2_exec and validation ----------------------------------------
exec_body <- body(ns$plot2_exec)
is_empty_arg <- function(x) identical(x, quote(expr = ))
walk_calls <- function(e, fn_pattern, acc = list()) {
  if (is.call(e)) {
    fn <- e[[1]]
    if (is.symbol(fn) && grepl(fn_pattern, as.character(fn))) {
      acc[[length(acc) + 1]] <- list(fn = as.character(fn), syms = all.names(e))
    }
    for (el in as.list(e)[-1]) if (!is_empty_arg(el) && !is.null(el)) acc <- walk_calls(el, fn_pattern, acc)
  }
  acc
}
val_calls <- walk_calls(exec_body, "^validate_")
validated_by <- vapply(args, function(a) {
  fns <- unique(vapply(Filter(function(v) a %in% v$syms, val_calls), `[[`, "", "fn"))
  paste(sort(fns), collapse = ", ")
}, character(1))
exec_names <- all.names(exec_body)
exec_refs <- vapply(args, function(a) sum(exec_names == a), integer(1))

# references from the generic and the methods (arguments that the methods
# consume themselves, e.g. sf or freq specific handling)
method_refs <- vapply(args, function(a) {
  n <- 0L
  for (m in c("plot2", methods)) n <- n + sum(all.names(body(get(m, envir = ns))) == a)
  n
}, integer(1))

# --- tests, vignettes, README ------------------------------------------------
read_all <- function(files) {
  stats::setNames(lapply(files, function(f) paste(readLines(f, warn = FALSE), collapse = "\n")), files)
}
tests <- read_all(list.files(file.path("tests", "testthat"), pattern = "\\.R$", full.names = TRUE))
tests <- tests[!grepl("helper-|setup\\.R$", names(tests))]
vign <- read_all(c(list.files("vignettes", pattern = "\\.Rmd$", full.names = TRUE), "README.Rmd"))
count_arg_use <- function(a, texts) {
  pat <- paste0("(?<![A-Za-z0-9_.])", gsub(".", "\\.", a, fixed = TRUE), "\\s*=(?!=)")
  sum(vapply(texts, function(t) length(gregexpr(pat, t, perl = TRUE)[[1]][gregexpr(pat, t, perl = TRUE)[[1]] > 0]), integer(1)))
}
test_uses <- vapply(args, count_arg_use, integer(1), texts = tests)
test_files <- vapply(args, function(a) sum(vapply(tests, function(t) count_arg_use(a, list(t)) > 0, logical(1))), integer(1))
vign_uses <- vapply(args, count_arg_use, integer(1), texts = vign)

# --- naming ------------------------------------------------------------------
group <- ifelse(grepl("\\.", args) & !args %in% c(".data", "...", "na.rm", "na.replace", "decimal.mark", "big.mark"),
                sub("\\..*$", "", args), "(core)")
suffix <- ifelse(group == "(core)", args, sub("^[^.]*\\.", "", args))
style <- ifelse(args == "...", "",
         ifelse(grepl("_", suffix) & grepl("\\.", suffix), "mixed . and _",
         ifelse(grepl("_", suffix), "snake", ifelse(grepl("\\.", suffix), "dot", "single word"))))

inv <- data.frame(
  n = seq_along(args),
  argument = args,
  group = group,
  suffix = suffix,
  style = style,
  default = vapply(args, function(a) dep(generic[[a]]), character(1)),
  method_differences = unname(method_diffs),
  roxygen_block = vapply(args, function(a) if (is.null(block_of[[a]])) "(undocumented)" else block_of[[a]], character(1)),
  documented_default = vapply(args, function(a) documented_default(doc_of[[a]]), character(1)),
  validated_by = unname(validated_by),
  exec_refs = unname(exec_refs),
  method_refs = unname(method_refs),
  test_uses = unname(test_uses),
  test_files = unname(test_files),
  vignette_readme_uses = unname(vign_uses),
  stringsAsFactors = FALSE
)
utils::write.csv(inv, file.path(out_dir, "A00_argument-inventory.csv"), row.names = FALSE)

# --- markdown ----------------------------------------------------------------
commit <- tryCatch(system("git rev-parse --short HEAD", intern = TRUE), error = function(e) "unknown")
lines <- c(
  "# A00 Argument inventory of plot2()",
  "",
  paste0("**Generated** by `review_and_overhaul/tools/argument_inventory.R` on ", Sys.Date(),
         " (commit `", commit, "`). Do not edit by hand; edit the sheets instead. A CSV copy is next to this file."),
  "",
  "Columns: *default* is the default in the generic `plot2()`; *method differences* lists S3 methods (and `plot2_exec()`) whose default differs from the generic or that lack the argument; *roxygen block* is the `@param` line in `R/plot2.R` that documents it; *documented default* quotes the sentences of that block that mention a default; *validated by* lists the `validate_*()` calls in `plot2_exec()` that receive the argument; *exec refs* and *method refs* count the symbol in the bodies of `plot2_exec()` and of the generic plus methods; *tests* counts `arg =` uses in `tests/testthat/` (uses / files); *vign.* counts `arg =` uses in the vignettes and README.Rmd.",
  "",
  "## Summary",
  "",
  paste0("- Arguments in the generic: ", length(args), "; groups: ",
         paste(sprintf("%s (%d)", names(table(group)), as.integer(table(group))), collapse = ", "), "."),
  paste0("- Arguments without a roxygen `@param`: ",
         if (any(inv$roxygen_block == "(undocumented)")) paste0("`", inv$argument[inv$roxygen_block == "(undocumented)"], "`", collapse = ", ") else "none", "."),
  paste0("- Arguments never referenced in `plot2_exec()` nor in the generic or methods: ",
         { z <- inv$argument[inv$exec_refs == 0 & inv$method_refs == 0 & inv$argument != "..."]; if (length(z)) paste0("`", z, "`", collapse = ", ") else "none" }, "."),
  paste0("- Arguments never used in any test: ", sum(inv$test_uses == 0), " of ", nrow(inv), "."),
  paste0("- Arguments never shown in vignettes or README: ", sum(inv$vignette_readme_uses == 0), " of ", nrow(inv), "."),
  paste0("- Extra arguments of methods, not in the generic: ",
         if (length(extra_method_args)) paste0("`", extra_method_args, "`", collapse = ", ") else "none", "."),
  paste0("- Naming styles of the part after the group prefix: ",
         paste(sprintf("%s (%d)", names(table(style[style != ""])), as.integer(table(style[style != ""]))), collapse = ", "), "."),
  "",
  "## Inventory",
  "",
  "| # | argument | group | default | method differences | roxygen block | documented default | validated by | exec refs | method refs | tests | vign. |",
  "|---|---|---|---|---|---|---|---|---|---|---|---|"
)
for (i in seq_len(nrow(inv))) {
  r <- inv[i, ]
  lines <- c(lines, paste0(
    "| ", r$n, " | `", r$argument, "` | ", r$group, " | `", md_escape(r$default), "` | ",
    md_escape(r$method_differences), " | ",
    if (r$roxygen_block == "(undocumented)") "**undocumented**" else paste0("`", r$roxygen_block, "`"), " | ",
    md_escape(r$documented_default), " | ", r$validated_by, " | ", r$exec_refs, " | ", r$method_refs, " | ",
    r$test_uses, " / ", r$test_files, " | ", r$vignette_readme_uses, " |"))
}
writeLines(lines, file.path(out_dir, "A00_argument-inventory.md"))
cat("Wrote", file.path(out_dir, "A00_argument-inventory.md"), "with", nrow(inv), "arguments\n")
