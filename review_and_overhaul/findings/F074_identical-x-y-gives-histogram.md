---
id: F074
title: Numeric x and y with identical values silently give a histogram that ignores y
severity: high
type: bug
area: types
status: verified
verification: verified
effort: S
api_change: no
locations: [R/validate.R:26]
related: [F007, F013]
origin: R03 sheet E04
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F074 Identical x and y values select a histogram

## Summary

Without `type`, `validate_type()` chooses a histogram when `identical(get_x(df), get_y(df))` (`R/validate.R:26`). The intention is the case where only one numeric variable exists (x and y refer to the same column), but the test compares values, not arguments. Any data where two different columns happen to hold the same values (e.g. `x = 1:4, y = 1:4`, a cumulative count equal to its index, a calibration line) give a histogram of x, with the info message "Ignoring y". Same family as F007 (data tested instead of arguments).

## Reproduction

```r
df <- data.frame(x = 1:4, y = 1:4)
p <- plot2(df, x, y)
print(vapply(p$layers, function(l) class(l$geom)[1], ""))
# Expected after fix: "GeomPoint" (both axes numeric), as for data.frame(x = 1:4, y = c(1, 2, 3, 5)).
```

## Proposed fix

Decide on the arguments: histogram only when `y` was not given (or refers to the same column as `x`), never by comparing values.

## History

- 2026-11-07: found by an R03 end-to-end test (E04.09), reproduced.
- 2026-11-08: fixed and verified (Phase 1). 2026-11-08: fixed; validate_type() chooses a histogram when x and y refer to the same column (names compared), not when their values are identical. Regression test F074.
