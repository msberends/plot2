---
id: F075
title: UpSet plots use a soft-deprecated tidyselect idiom (external vector in c_across())
severity: low
type: bug
area: dependencies
status: verified
verification: verified
effort: S
api_change: no
locations: [R/plot2.R:1137]
related: [F037]
origin: R03 sheet E05
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F075 UpSet uses a deprecated tidyselect idiom

## Summary

`mutate(count = sum(c_across(vars_x)))` (`R/plot2.R:1137`) selects with a bare external character vector, soft-deprecated since tidyselect 1.1.0. Soft deprecations are silent for end users; the warning ("Using an external vector in selections was deprecated") appears when plot2's own tests run, and in CRAN and reverse-dependency checks. When tidyselect escalates the deprecation, UpSet plots stop working.

## Reproduction

```r
up <- data.frame(a = c(TRUE, TRUE, FALSE), b = c(TRUE, FALSE, TRUE))
# soft deprecations warn only when the calling package is under test
Sys.setenv(TESTTHAT = "true", TESTTHAT_PKG = "plot2")
w <- character(0)
invisible(withCallingHandlers(plot2(up, x = c(a, b), type = "upset"),
  warning = function(cnd) { w <<- c(w, conditionMessage(cnd)); invokeRestart("muffleWarning") }))
print(any(grepl("external vector|warnings in `mutate", w)))
# Expected after fix: FALSE.
```

## Proposed fix

`c_across(all_of(vars_x))`. Search the package for other bare external vectors in selections (`select(vars)`, `across(vars)`).

## History

- 2026-11-07: found by an R03 end-to-end test (E05.01), reproduced; downgraded to low after confirming that end users do not see the warning.
- 2026-11-08: fixed and verified (Phase 1). 2026-11-08: fixed; c_across(all_of(vars_x)). No other bare external vectors in selections found in R/.
