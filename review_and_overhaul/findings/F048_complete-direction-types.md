---
id: F048
title: x.complete fails for character values and completes date-times per second
severity: high
type: bug
area: data-validation
status: verified
verification: verified
effort: S
api_change: no
locations: [R/validate.R:2601]
related: []
origin: R02 sheet 19
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F048 complete_direction() and value types

## Summary

`complete_direction()` always uses `tidyr::full_seq()`. For character and factor values this errors; for POSIXct the period is 1, which means one second, so half a day becomes 43,201 rows.

## Reproduction

```r
print(tryCatch({ plot2(data.frame(g = c("a", "c"), v = 1:2, k = c("u", "w")), g, v, k, x.complete = TRUE); "OK" },
               error = function(e) conditionMessage(e)))
p <- plot2(data.frame(t = as.POSIXct(c("2024-01-01 00:00", "2024-01-01 12:00"), tz = "UTC"), v = 1:2), t, v, x.complete = TRUE)
print(nrow(p$data))
# Currently: error, and 43201 rows.
# Expected after fix: character x completed over the observed values (crossed with category); date-times completed at a sensible period (e.g. the smallest observed gap) or an argument for the period.
```

## Impact

Completing missing categories, the main use case for categorical data, is not possible; date-time data can exhaust memory.

## Proposed fix

Branch per class: `tidyr::complete(df, _var_x, _var_category)` (crossing observed values) for character and factor; for date-times use the greatest common divisor of observed gaps or an explicit `*.complete_period`.

## History

- 2026-11-07: filed in R02, reproduced.
- 2026-11-08: fixed and verified (Phase 1). 2026-11-08: fixed; complete_direction() branches by class (factor levels, observed character values, Date per day, POSIXct per greatest common divisor of the observed gaps, numeric as before) and crosses the completed direction with the other directions present. Regression test F048.
