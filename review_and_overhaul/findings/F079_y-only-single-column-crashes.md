---
id: F079
title: plot2(df, y = col) crashes when df has no other column to use as x
severity: medium
type: bug
area: data-validation
status: verified
verification: verified
effort: S
api_change: no
locations: [R/validate.R:238, R/validate.R:241, R/validate.R:247]
related: [F007, F074]
origin: R03 sheet E10
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F079 Guessing x fails for a single-column data frame

## Summary

When `x` is missing, `validate_data()` guesses it if `ncol(df) > 1` (`R/validate.R:238`). Because `_var_y` has already been added, this condition is always true. It then skips the first column if its values are identical to y (`R/validate.R:241`, values compared instead of arguments, as in F007 and F074) and takes the second eligible column, which does not exist: `pull(df, NA)` errors with "Subscript must be a location, not a character NA". A box plot of a single variable (`plot2(df, y = value)`) is a common case. The branch for "no x" in `validate_type()` (box plot without x-axis) is therefore unreachable from the data frame method.

## Reproduction

```r
print(tryCatch(class(plot2(data.frame(v = c(1, 2, 3, 4, 10)), y = v))[1], error = function(e) conditionMessage(e)))
# Expected after fix: "ggplot2::ggplot", a box plot without x-axis labels.
```

## Proposed fix

Count only the user's columns (not `_var_*`), compare column names rather than values to exclude the y column, and leave x empty when no eligible column remains.

## History

- 2026-11-07: found by an R03 end-to-end test (E10.06), reproduced.
- 2026-11-08: fixed and verified (Phase 1). 2026-11-08: fixed; a missing x is guessed from the user's columns only, excluding the y, category and facet columns by name; x stays empty (box plot without x-axis) when none is left. Regression test F079.
