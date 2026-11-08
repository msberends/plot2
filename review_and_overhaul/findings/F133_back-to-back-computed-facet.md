---
id: F133
title: Back-to-back plots fail when the facet is an expression that changes its type
severity: low
type: bug
area: types
status: duplicate
verification: verified
effort: M
api_change: no
locations: [R/back2back_plot.R]
related: [F021, F064, F065]
origin: fix of F021
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F133 Back-to-back with a computed facet

## Summary

After F021, the two-values check of back-to-back plots works for any facet expression. Expressions that change the type of the facet still fail later, inside the nested `plot2()` calls of `back2back_plot()`: `facet = s == 1` ends in "Can't select columns that don't exist. Column `s == 1` doesn't exist" (the deparsed expression is used as a column name), and `facet = s * 2` in "Can't combine `s * 2` <double> and `a` <factor>". Expressions that keep the values character (`tolower(sex)`) work.

## Reproduction

```r
d <- data.frame(a = rep(c("x", "y"), 2), v = 1:4, s = c(1, 1, 2, 2))
print(tryCatch({ ggplot2::ggplot_build(plot2(d, a, v, facet = s == 1, type = "b2b")); "ok" }, error = conditionMessage))
# Expected after fix: "ok"
```

## Impact

A computed split, such as `facet = age >= 65`, cannot be used for back-to-back plots.

## Proposed fix

Pass the computed `_var_facet` column (as character) to the nested calls instead of re-evaluating the user's expression; best done with F064 (composite types from one prepared data set).

## History

- 2026-11-08: found while fixing F021.
- 2026-11-08: closed as a duplicate of F065: the failures come from the small counts in the reproduction (1 to 4); with counts such as 10 to 200, `s == 1`, `s * 2` and `toupper(s)` all work.
