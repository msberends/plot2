---
id: F012
title: Typo "POSIXTt" makes POSIXct columns eligible as automatic y
severity: high
type: bug
area: data-validation
status: open
verification: verified
effort: S
api_change: no
locations: [R/validate.R:133, R/validate.R:137]
related: []
origin: R01 §2.5
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F012 Typo "POSIXTt"

## Summary

`inherits(col, c("Date", "POSIXTt", "factor"))` should be `"POSIXt"`. POSIXct columns have mode numeric, so they are classified as numeric candidates for the automatically chosen y.

## Evidence

`R/validate.R:133`, `R/validate.R:137`.

## Reproduction

```r
d <- data.frame(t = as.POSIXct(c("2024-01-01", "2024-01-02")), grp = c("a", "b"), v = c(5, 6))
res <- tryCatch({ p <- plot2(d, grp); rlang::as_label(p$mapping$y) }, error = function(e) conditionMessage(e))
print(res)
# Currently: the POSIXct column is taken as y and plot2() crashes ("'%%' not defined for POSIXt objects").
# Expected after fix: "v"
```

## Impact

When y is not given and a date-time column precedes the numeric one, the date-time column is taken as y and `plot2()` crashes in the y scale.

## Proposed fix

Correct the class name; add a regression test.

## History

- 2026-11-07: filed in R01 (by reading); reproduced the same day via `tools/run_repro.R` (crash).
