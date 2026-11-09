---
id: F081
title: A formula piped after the data (df |> plot2(y ~ x)) fails with a cryptic across() error
severity: medium
type: bug
area: methods
status: verified
verification: verified
effort: S
api_change: no
locations: [R/plot2.R:608, R/plot2-methods.R:1622]
related: [F019, F020]
origin: R04 sheet M02
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F081 A formula piped after the data (df |> plot2(y ~ x)) fails with a cryptic across() error

## Summary

Dispatch is on the first argument, so `mtcars |> plot2(mpg ~ wt)` goes to `plot2.data.frame()` with the formula as `x`. The formula is then used as a tidy selection and fails deep in dplyr with "Must supply a column selection". The pipe-first form is the natural one in modern R code and in plot2's own examples, while the formula method only works as `plot2(mpg ~ wt, data = mtcars)`.

## Reproduction

```r
print(class(plot2(mpg ~ wt, data = mtcars))[1])
print(tryCatch(class(mtcars |> plot2(mpg ~ wt))[1], error = function(e) conditionMessage(e)))
# Expected after fix: "ggplot2::ggplot" twice (or a clear error naming the formula form).
```

## Proposed fix

In `plot2.data.frame()`, detect a formula in `x` (`rlang::quo_is_call(enquo(x), "~")` or an evaluated formula) and hand over to the formula method with `data = .data`.

## History

- 2026-11-08: filed in R04, reproduced.
- 2026-11-08: fixed with F019: plot2.data.frame() detects a formula in x and hands over to plot2.formula() with data = .data and all other arguments. Tests: M02.05 unparked, test-regression-phase4.R (F081).
