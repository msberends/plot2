---
id: F089
title: plot2() on a plain matrix plots only the first column and loses the axis titles
severity: low
type: bug
area: methods
status: verified
verification: verified
effort: S
api_change: no
locations: [R/plot2-methods.R:1985, R/plot2-methods.R:1987]
related: [F035]
origin: R04 sheet M03
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F089 plot2() on a plain matrix plots only the first column and loses the axis titles

## Summary

For a numeric matrix that is not a correlation matrix, `plot2.matrix()` adds the row names as column `x` and passes on; the generic guessing then takes the first column as y ("Using y = A") and ignores the others without a caution. Axis titles are lost (`get_labs()` gives NULL for x and y) because the labels are taken from `deparse(substitute(x))` of the method's own defaults.

## Reproduction

```r
m <- matrix(1:6, nrow = 3, dimnames = list(c("r1", "r2", "r3"), c("A", "B")))
p <- plot2(m)
print(ggplot2::get_labs(p)[c("x", "y")])
print(colnames(p$data))
# Expected after fix: titles "x"/"value" (or similar), and either all columns plotted (as y = where(is.numeric)) or a caution that only A is used.
```

## Proposed fix

Decide the intended semantics of a plain matrix (all columns as categories is the natural one, like `y = where(is.numeric)`), and pass explicit `_label.*` values.

## History

- 2026-11-08: filed in R04, reproduced.
- 2026-11-08: fixed: a plain matrix is pivoted to long format, rows (names or numbers) on x and every column as a category, so all columns are plotted. Axis titles: the matrix method deliberately defaults to x.title = FALSE and y.title = FALSE (documented), so they are not restored in general; when the dimensions are named, those names are used as the x title (if x.title is not given) and the category title. Labels no longer come from deparse(substitute()) of the method's defaults. Tests: M03.04 unparked and adapted to the documented title defaults.
