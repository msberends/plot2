---
id: F072
title: Character or factor vector input crashes when its expression is longer than 30 characters
severity: high
type: bug
area: methods
status: open
verification: verified
effort: S
api_change: no
locations: [R/plot2-methods.R:190, R/plot2-methods.R:200]
related: [F030]
origin: R03 sheet E01
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F072 Long vector expressions crash plot2.default()

## Summary

For an atomic vector, `plot2.default()` uses the deparsed expression as the x column name, but replaces it by `"y"` when longer than 30 characters (`R/plot2-methods.R:191`). For character and factor vectors, the count column is also called `"y"` (`R/plot2-methods.R:200`), so both columns are named `y` and dplyr fails with "Can't transform a data frame with duplicate names". Typical calls such as `plot2(my_data$hospital_location)` or an inline `c(...)` of six values are affected. Numeric vectors are not affected.

## Reproduction

```r
v <- c("a", "b", "b")
print(class(plot2(v))[1])
print(tryCatch(class(plot2(c("a", "b", "b", "c", "c", "c")))[1], error = function(e) conditionMessage(e)))
long_variable_name_for_testing_purposes <- c("a", "b", "b")
print(tryCatch(class(plot2(long_variable_name_for_testing_purposes))[1], error = function(e) conditionMessage(e)))
# Expected after fix: "ggplot2::ggplot" three times.
```

## Proposed fix

Use `"x"` as the fallback name for the x column (and a truncated expression as the x title), never the name of the y column.

## History

- 2026-11-07: found by an R03 end-to-end test (E01.07), reproduced.
