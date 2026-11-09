---
id: F065
title: Numeric y values are interpreted as column positions; back-to-back plots crash for small counts
severity: high
type: bug
area: data-validation
status: verified
verification: verified
effort: M
api_change: no
locations: [R/back2back_plot.R:203, R/back2back_plot.R:338, R/plot2.R:947]
related: [F064]
origin: R03 sheets E04, E08
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F065 Numeric y vectors are taken as column positions

## Summary

`plot2_exec()` first tries `select({{ y }})` to detect multiple y columns. A numeric vector such as `c(1, 2)` is a valid tidyselect specification (column positions), so it is treated as "several y columns". `back2back_plot()` passes the already computed y values as a numeric vector (`y = !!get_y(df_left)`), so back-to-back plots with small counts (1, 2, 3, ...) crash, and with a category they fail with "if 'y' contains more than one variable, 'category' must not be set".

## Reproduction

```r
pop2 <- data.frame(age = rep(c("0-9", "10-19"), 2), sex = rep(c("M", "F"), each = 2), grp = c("a", "b", "b", "b"), n = c(1, 2, 3, 4))
print(tryCatch({ plot2(pop2, age, n, grp, facet = sex, type = "b2b"); "OK" }, error = function(e) conditionMessage(e)))
print(tryCatch({ p <- plot2(data.frame(x = c("a", "b"), v = c(5, 6), w = c(7, 8)), x, y = c(2, 3)); toString(names(p$mapping)) }, error = function(e) conditionMessage(e)))
# Expected after fix: "OK"; y = c(2, 3) treated as values (or a clear error), never as columns 2 and 3.
```

## Impact

Back-to-back plots fail for typical small-count data (e.g. pyramids of a few patients per age group).

## Proposed fix

Only treat `y` as a column selection when it is a symbol, a `c()` of symbols, or a tidyselect helper call (`where()`, `starts_with()`, ...); never for evaluated numeric vectors. In `back2back_plot()`, pass the y column by name rather than by value.

## History

- 2026-11-07: filed in R03, reproduced.
- 2026-11-08: fixed and verified (Phase 1). New helper `is_column_selection()`: only names, `c()` of names, ranges and tidyselect helpers can select several y columns; evaluated values never; regression tests in E04 and E08; full suite passes.
