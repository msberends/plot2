---
id: F025
title: Slow on large data; datalabels formatted row by row before summarising
severity: medium
type: performance
area: performance
status: verified
verification: verified
effort: M
api_change: no
locations: [R/validate.R:339, R/validate.R:2917, R/validate.R:2957, R/validate.R:1984, R/utils.R:271, R/validate.R:94, R/validate.R:224, R/plot2.R:602, R/plot2.R:1327, R/plotly.R:40, R/utils.R:682]
related: [F022]
origin: R01 §3.1
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F025 Performance on large data

## Summary

A boxplot of 1e6 rows takes about 7.4 s. At 2e5 rows, in a warm session, a call takes 1.5 to 2.9 s with default settings and 0.2 s with `datalabels = FALSE`; profiling attributes about 71% of the time to `format_datalabels()`: datalabels default to `TRUE`, are formatted per raw row with `format2()` and `mapply(gsub)`, and are discarded afterwards because there are more than 25 rows. Further costs: every title is evaluated via `as.data.frame()` + `mutate()` over the full data (about 8 times per call); `get_column_name()` compares every column with `identical()`; `utils::installed.packages()` is called in hot paths; `data_is_numeric()` runs a regex over whole vectors.

## Evidence

Profile script: `repro/perf_profile.R`.

## Reproduction

```r
big <- data.frame(g = sample(letters, 2e5, TRUE), v = runif(2e5))
invisible(plot2(big, g, v, datalabels = FALSE))  # warm-up, the first call includes one-off costs
cat("default:", system.time(plot2(big, g, v))[["elapsed"]], "s\n")
cat("datalabels = FALSE:", system.time(plot2(big, g, v, datalabels = FALSE))[["elapsed"]], "s\n")
# Expected after fix: well under 0.5 s for both on this machine.
```

## Impact

Sluggish on routine surveillance-sized data; poor experience in Shiny.

## Proposed fix

Format datalabels only after summarising and only when they will be drawn; evaluate titles only when they are expressions referring to data; track column names explicitly instead of recovering them by comparison; replace `installed.packages()` with `requireNamespace(quietly = TRUE)`; use `is.numeric()` instead of regex detection. Add a benchmark to `repro/` and track it.

## History

- 2026-11-07: filed in R01, profiled. Warm timings at 2e5 rows: default 1.5 to 2.9 s, `datalabels = FALSE` 0.2 s (a cold first call distorts the comparison).
- 2026-11-08: R06 (sheet T03, `repro/perf_r06.R`, installed build): scatter plots are affected as well. 1e6 rows take 9.5 s for `plot2(d, v, w)`, of which about 70% (profile at 2e5 rows) is `format_datalabels()`, although point plots never show datalabels. Columns: 8.2 s with default datalabels against 0.6 s with `datalabels = FALSE`.
- 2026-11-08: fixed: datalabels are formatted once, in generate_datalabels(), on the final rows and only when they are drawn; placeholders are filled vectorised instead of mapply(gsub); constant titles are not evaluated over the data; get_column_name() pulls the target once; installed.packages() replaced (is.mic by class, requireNamespace); data_is_numeric() on unique values. Installed build, 2e5 rows: default 0.17 s (was 1.5 to 2.9 s), scatter 0.39 s (was 2.0 s), 1e6 rows boxplot 0.70 s (was 7.4 s).
