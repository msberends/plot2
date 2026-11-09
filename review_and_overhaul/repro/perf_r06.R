# R06 sheet T03: scaling benchmark of plot2() along several dimensions.
# Run from the package root (takes several minutes, start detached):
#   Rscript review_and_overhaul/repro/perf_r06.R [output.csv]
# For every scenario: median of 3 timed runs after one warm-up, split into the
# plot2() call and ggplot_build(); a run is abandoned after 120 s.
# Set PLOT2_LIB to a library with an installed build: devtools::load_all()
# does not byte-compile, which inflates the per-call overhead by about a quarter.
lib <- Sys.getenv("PLOT2_LIB")
if (nzchar(lib)) {
  suppressMessages(library(plot2, lib.loc = lib))
  cat("plot2", format(packageVersion("plot2", lib.loc = lib)), "from", lib, "(installed, byte-compiled)\n")
} else {
  suppressMessages(devtools::load_all(".", quiet = TRUE))
}
options(plot2.silent = TRUE, warn = -1)
out <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(out)) out <- file.path(tempdir(), "perf_r06.csv")

time_one <- function(expr_fun) {
  setTimeLimit(elapsed = 120, transient = TRUE)
  on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
  t1 <- system.time(p <- expr_fun())[["elapsed"]]
  t2 <- system.time(ggplot2::ggplot_build(p))[["elapsed"]]
  c(call = t1, build = t2)
}
bench <- function(scenario, size, expr_fun) {
  res <- tryCatch({
    invisible(time_one(expr_fun))                 # warm-up
    m <- vapply(1:3, function(i) time_one(expr_fun), numeric(2))
    apply(m, 1, stats::median)
  }, error = function(e) c(call = NA_real_, build = NA_real_))
  row <- data.frame(scenario = scenario, size = size, call_s = unname(res["call"]),
                    build_s = unname(res["build"]), stringsAsFactors = FALSE)
  cat(sprintf("%-34s %9s  call %7.3f s  build %7.3f s\n", scenario, format(size, big.mark = ","), row$call_s, row$build_s))
  flush.console()
  row
}

set.seed(2026)
rows <- list()
add <- function(r) rows[[length(rows) + 1]] <<- r

# 1. call overhead on tiny data, compared with plain ggplot2
tiny <- data.frame(g = letters[1:5], v = 1:5)
add(bench("tiny: plot2 col", 5, function() plot2(tiny, g, v)))
add(bench("tiny: ggplot2 col", 5, function() ggplot2::ggplot(tiny, ggplot2::aes(g, v)) + ggplot2::geom_col()))
add(bench("tiny: plot2 point", 5, function() plot2(iris, Sepal.Length, Sepal.Width)))
add(bench("tiny: ggplot2 point", 150, function() ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Sepal.Width)) + ggplot2::geom_point()))

# 2. rows, per type
for (n in c(1e3, 1e4, 1e5, 1e6)) {
  d <- data.frame(g = sample(letters[1:10], n, TRUE), v = runif(n), w = runif(n),
                  dt = as.Date("2020-01-01") + sample(0:1000, n, TRUE))
  add(bench("rows: col (summarised)", n, function() plot2(d, g, v, type = "col")))
  add(bench("rows: col, datalabels = FALSE", n, function() plot2(d, g, v, type = "col", datalabels = FALSE)))
  add(bench("rows: boxplot", n, function() plot2(d, g, v, type = "boxplot")))
  add(bench("rows: point", n, function() plot2(d, v, w)))
  add(bench("rows: histogram", n, function() plot2(d, v, type = "hist")))
  add(bench("rows: date x, n()", n, function() plot2(d, dt, n())))
}

# 3. number of x values (1e5 rows)
for (k in c(10, 100, 1000, 10000)) {
  d <- data.frame(g = sample(sprintf("x%05d", seq_len(k)), 1e5, TRUE), v = runif(1e5))
  add(bench("x values: col", k, function() plot2(d, g, v, type = "col")))
  add(bench("x values: boxplot (automatic)", k, function() plot2(d, g, v)))
}

# 4. number of category values (1e5 rows, 10 x values)
for (k in c(2, 10, 50, 200)) {
  d <- data.frame(g = sample(letters[1:10], 1e5, TRUE), cat = sample(sprintf("c%03d", seq_len(k)), 1e5, TRUE), v = runif(1e5))
  add(bench("category values: col", k, function() plot2(d, g, v, cat, type = "col")))
}

# 5. number of facets (1e5 rows)
for (k in c(2, 10, 50)) {
  d <- data.frame(g = sample(letters[1:10], 1e5, TRUE), f = sample(sprintf("f%03d", seq_len(k)), 1e5, TRUE), v = runif(1e5))
  add(bench("facets: col", k, function() plot2(d, g, v, facet = f, type = "col")))
}

# 6. special types (1e4 rows)
d <- data.frame(a = sample(c("A", "B", "C"), 1e4, TRUE), b = sample(c("X", "Y"), 1e4, TRUE),
                c = sample(c("P", "Q", "R"), 1e4, TRUE), s1 = rbinom(1e4, 1, 0.3), s2 = rbinom(1e4, 1, 0.5),
                s3 = rbinom(1e4, 1, 0.2), v = runif(1e4))
d$n <- 1L  # Sankey needs a numeric column (F106)
add(bench("type: sankey", 1e4, function() plot2(d, x = c(a, b, c), y = n, category = b, type = "sankey")))
add(bench("type: upset", 1e4, function() plot2(d, x = c(s1, s2, s3), type = "upset")))
add(bench("type: back-to-back", 1e4, function() plot2(d, a, n(), facet = b, type = "back-to-back")))
add(bench("type: spider", 1e4, function() plot2(d, a, mean(v), category = b, type = "spider")))

write.csv(do.call(rbind, rows), out, row.names = FALSE)
cat("written to", out, "\nDONE\n")
