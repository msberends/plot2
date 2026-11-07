---
id: F019
title: Formula method parses the formula as text and fails on function terms
severity: high
type: bug
area: methods
status: open
verification: verified
effort: M
api_change: possible
locations: [R/plot2-methods.R:552, R/plot2-methods.R:556, R/plot2-methods.R:572]
related: [F020]
origin: R01 §2.12
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F019 Formula method parsing

## Summary

`plot2.formula()` splits `as.character(formula)` on `" + "` and `" - "`, then keeps only columns via `select(any_of())`. Function terms containing `+`/`-` are split apart, computed terms are dropped, `|` (conditioning, natural for facets) is not supported, and the error for missing `data` lacks `call. = FALSE`.

## Evidence

`R/plot2-methods.R:552-581`.

## Reproduction

```r
f <- function(expr) tryCatch({ expr; "OK" }, error = function(e) conditionMessage(e))
print(f(plot2(log(mpg) ~ hp, data = mtcars)))
print(f(plot2(mpg ~ I(hp + 1), data = mtcars)))
# Expected after fix: "OK" twice.
```

## Impact

The formula interface only works for the simplest formulas.

## Proposed fix

Use `rlang::f_lhs()`/`f_rhs()` and walk the right-hand side as a call tree (split only on top-level `+`, treat `|` as facet), then pass the expressions as quosures with the formula's environment. Do not subset columns with `select()`.

## History

- 2026-11-07: filed in R01, reproduced.
