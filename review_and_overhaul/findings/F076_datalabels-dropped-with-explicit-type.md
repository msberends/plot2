---
id: F076
title: datalabels = TRUE or a column is silently dropped for point and line plots when type is given
severity: medium
type: bug
area: data-validation
status: verified
verification: verified
effort: S
api_change: no
locations: [R/validate.R:291, R/plot2.R:1379]
related: [F007]
origin: R03 sheet E06
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F076 Explicit datalabels ignored for continuous types

## Summary

The documentation says `datalabels = TRUE` forces datalabels. `validate_data()` removes the datalabels column whenever `type` is given and is a continuous geom (point, line, area, boxplot, ...), without checking whether the user asked for datalabels (`R/validate.R:291`). `plot2_exec()` later has the correct rule, removing them only when `datalabels` was not given (`R/plot2.R:1379`), but by then the column is already gone. Result: `plot2(df, x, y, datalabels = TRUE)` shows labels, `plot2(df, x, y, type = "point", datalabels = TRUE)` does not, and no message is given.

## Reproduction

```r
df <- data.frame(x = c(1, 2, 3), y = c(2, 4, 3))
g <- function(p) vapply(p$layers, function(l) class(l$geom)[1], "")
print(g(plot2(df, x, y, datalabels = TRUE)))
print(g(plot2(df, x, y, type = "point", datalabels = TRUE)))
print(g(plot2(df, x, y, type = "line", datalabels = y)))
# Expected after fix: label layers in all three.
```

## Proposed fix

Remove the type-based branch in `validate_data()` and keep the single rule in `plot2_exec()` (drop for continuous geoms only when `datalabels` is missing).

## History

- 2026-11-07: found by an R03 end-to-end test (E06.04), reproduced.
- 2026-11-08: fixed and verified (Phase 1). 2026-11-08: fixed; the type-based removal in validate_data() is gone, leaving the single rule in plot2_exec() (remove for continuous types only when datalabels was not given).
