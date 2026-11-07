---
id: F007
title: Logical checks on data instead of arguments crash on NA and misread 0/1 values
severity: critical
type: bug
area: data-validation
status: open
verification: verified
effort: M
api_change: no
locations: [R/validate.R:283, R/validate.R:287, R/validate.R:291, R/validate.R:340]
related: []
origin: R01 §1.7
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F007 Data tested instead of arguments

## Summary

To support `category = FALSE` and `datalabels = TRUE/FALSE`, `validate_data()` checks the materialised column with `all(x == FALSE)` or `all(x == TRUE)`. With real data this goes wrong in two ways: `NA` values make the condition `NA` and crash, and genuine numeric data of all 0s or all 1s is mistaken for `FALSE`/`TRUE`.

## Evidence

`R/validate.R:283`, `287`, `292`, `340`.

## Reproduction

```r
f <- function(expr) tryCatch({ expr; "OK" }, error = function(e) conditionMessage(e))
print(f(plot2(data.frame(x = letters[1:3], y = 1:3, c = NA), x, y, c)))
print(f(plot2(data.frame(x = letters[1:3], y = 1:3, d = c(1, NA, 1)), x, y, datalabels = d)))
p <- plot2(data.frame(x = letters[1:3], y = 4:6, d = 1), x, y, datalabels = d)
b <- ggplot2::ggplot_build(p)
print(b$data[[length(p$layers)]]$label)  # shows y values 4,5,6 instead of d
p <- plot2(data.frame(x = letters[1:3], y = 1:3, c = 0), x, y, c)
print(names(p$mapping))                  # category silently dropped
# Expected after fix: no errors; labels "1"; category kept (constant colour).
```

## Impact

Crashes on common real-world data (missing values) and silently wrong plots for constant 0/1 columns.

## Proposed fix

Inspect the quosure before turning it into a column: if the evaluated argument is a single logical (`isTRUE()`/`isFALSE()` on a length-1 logical), handle it as a switch; otherwise treat it as data. Never use `==` on data in `if()` conditions.

## History

- 2026-11-07: filed in R01, reproduced.
