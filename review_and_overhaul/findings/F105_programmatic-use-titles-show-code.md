---
id: F105
title: In functions, axis and legend titles show the code ({{ col }}, .data[[v]], !!sym(v)) instead of the column name
severity: medium
type: bug
area: architecture
status: verified
verification: verified
effort: M
api_change: no
locations: [R/plot2-methods.R:386, R/plot2-methods.R:751, R/plot2-methods.R:755, R/plot2-methods.R:1087, R/plot2-methods.R:1457]
related: [F030, F035, F036]
origin: R06 sheet T04
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F105 Titles in programmatic use

## Summary

Every method derives the default titles with `deparse(substitute(x))` (`R/plot2-methods.R:386-391` and the same block in each method). This captures the code as written, not what it refers to. As soon as `plot2()` is called from a function, a loop or another package, the titles show the tidy-evaluation code. The data are plotted correctly; only the titles are wrong.

## Evidence

| call | x title |
|---|---|
| `f <- function(df, col) plot2(df, {{ col }}, n()); f(iris, Species)` | `{ { col } }` |
| `f <- function(df, v) plot2(df, .data[[v]], n()); f(iris, "Species")` | `.data[[v]]` |
| `plot2(iris, !!rlang::sym("Species"), n())` | `!!rlang::sym(v)` (inside a function) |
| `lapply(c("Sepal.Length", "Sepal.Width"), function(v) plot2(iris, Species, .data[[v]]))` | y title `.data[[v]]` twice |

Also, `plot2.default` passes `` `_label.y_secondary` `` twice (`R/plot2-methods.R:755-756`).

## Reproduction

```r
f <- function(df, col) plot2(df, {{ col }}, n())
g <- function(df, v) plot2(df, .data[[v]], n())
print(c(f(iris, Species)$labels$x, g(iris, "Species")$labels$x))
# Expected after fix: "Species" "Species"
```

## Impact

Anyone who writes a wrapper function or loops over variables, typical for reports with many similar plots, gets code in the titles and must set every title by hand. Packages that build on plot2 are affected in the same way.

## Proposed fix

Capture the arguments as quosures once (in the single argument definition of F035) and derive titles with `rlang::as_label()` after resolving `.data[[...]]` and injected symbols, e.g. with `rlang::quo_squash()` and a small helper that maps `.data[["a"]]` and `.data$a` to `a`. Add tests for embracing, `.data`, `!!sym()` and loops.

## History

- 2026-11-08: filed in R06 (sheet T04); test T04.01 parked.
- 2026-11-08: fixed with F035: default titles come from the captured quosures, with .data[[v]], .data$v, {{ }} and !! resolved (plot2_label()).
