---
id: F078
title: Box plots pass the fatten argument, soft-deprecated in ggplot2 4.0.0
severity: low
type: bug
area: dependencies
status: open
verification: verified
effort: S
api_change: no
locations: [R/validate.R:1666]
related: [F037, F075]
origin: R03 sheet E09
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F078 Deprecated fatten in geom_boxplot()

## Summary

`generate_geom()` passes `fatten` to `geom_boxplot()` (`R/validate.R:1666`) to draw a thicker median. ggplot2 4.0.0, the minimum version in DESCRIPTION, soft-deprecated `fatten` in favour of `median.linewidth`. End users do not see the warning; plot2's own tests, CRAN and reverse-dependency checks do. Box plots will break when ggplot2 removes the argument.

## Reproduction

```r
# soft deprecations warn only when the calling package is under test
Sys.setenv(TESTTHAT = "true", TESTTHAT_PKG = "plot2")
w <- character(0)
p <- withCallingHandlers(ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot2(iris, Species, Sepal.Length))),
  warning = function(cnd) { w <<- c(w, conditionMessage(cnd)); invokeRestart("muffleWarning") })
print(any(grepl("fatten", w)))
# Expected after fix: FALSE.
```

## Proposed fix

Replace `fatten` by `median.linewidth` (computed as the box line width times the current factor). Check `geom_crossbar()` and `geom_pointrange()` calls for the same argument.

## History

- 2026-11-07: found by an R03 end-to-end test (E09.04), reproduced; downgraded to low after confirming that end users do not see the warning.
