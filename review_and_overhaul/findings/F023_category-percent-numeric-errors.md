---
id: F023
title: category.percent = TRUE (or a function for category.limits) errors for numeric categories
severity: high
type: bug
area: scales
status: verified
verification: verified
effort: S
api_change: no
locations: [R/validate.R:1408, R/validate.R:1480]
related: []
origin: R01 §2.16
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F023 category.percent with numeric category

## Summary

`limits_fn()` can return a function (for `category.percent = TRUE`, or when the user supplies one), but the result is wrapped in `as.numeric()`.

## Evidence

`R/validate.R:1409` returns a function; `R/validate.R:1480` applies `as.numeric()`.

## Reproduction

```r
d <- data.frame(x = 1:3, y = 1:3, c = c(0.1, 0.5, 0.9))
print(tryCatch({ plot2(d, x, y, c, category.percent = TRUE, type = "point"); "OK" }, error = function(e) conditionMessage(e)))
# Expected after fix: "OK"
```

## Impact

A documented argument cannot be used.

## Proposed fix

Only coerce when the result is not a function.

## History

- 2026-11-07: filed in R01, reproduced.
- 2026-11-08: fixed and verified (Phase 1). Fixed by the change in the F132 commit: limits returned as a function are no longer coerced with `as.numeric()`; regression test 07.06 unparked.
