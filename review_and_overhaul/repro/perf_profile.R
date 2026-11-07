# Benchmark and profile for F025. Run from the package root:
#   Rscript review_and_overhaul/repro/perf_profile.R
suppressMessages(devtools::load_all(".", quiet = TRUE))
set.seed(2026)
for (n in c(1e4, 1e5, 1e6)) {
  big <- data.frame(g = sample(letters, n, TRUE), v = runif(n))
  t_default <- system.time(plot2(big, g, v))[["elapsed"]]
  t_nolabels <- system.time(plot2(big, g, v, datalabels = FALSE))[["elapsed"]]
  cat(sprintf("n = %8.0f  default: %6.2f s  datalabels = FALSE: %6.2f s\n", n, t_default, t_nolabels))
}
big <- data.frame(g = sample(letters, 2e5, TRUE), v = runif(2e5))
prof <- tempfile()
Rprof(prof, interval = 0.01)
invisible(plot2(big, g, v))
Rprof(NULL)
s <- summaryRprof(prof)$by.total
print(utils::head(s[!grepl("^\"(<Anonymous>|eval|doTryCatch|tryCatch|withCallingHandlers|suppress)", rownames(s)), ], 25))
