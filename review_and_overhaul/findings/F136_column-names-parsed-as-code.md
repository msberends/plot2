---
id: F136
title: Column names are parsed as R code in back-to-back and UpSet plots and in setup_aes()
severity: critical
type: security
area: core
status: verified
verification: verified
effort: S
api_change: no
locations: [R/back2back_plot.R:206, R/back2back_plot.R:208, R/back2back_plot.R:339, R/back2back_plot.R:341, R/plot2.R:1073, R/plot2.R:1163, R/utils.R:675]
related: [F006, F111]
origin: Phase 6 (found while fixing F006)
commit: c6352e6
created: 2026-11-08
updated: 2026-11-08
---

# F136 Column names parsed as code

## Summary

Several internal steps turn a column name back into an expression with `str2lang()`: the back-to-back plot (`x` and `category` of both halves), the UpSet plot (selecting the `x` columns and the `category` of the upper panel), and `setup_aes()` for names given as text. A column name is data, not code: a column named `system("...")` (for instance in an uploaded CSV in `create_interactively()`) is run when a back-to-back plot is made of it. In `setup_aes()` a name that starts with a backtick is parsed as it is, so `` `a`+system("...") `` runs too. Non-syntactic names such as `my col` make the back-to-back plot fail with a parse error.

## Evidence

- Probe: a data set whose `x` column is named `writeLines("x", "<file>")` writes the file during `plot2(..., type = "back-to-back")`; a column `my col` gives "unexpected symbol".

## Reproduction

```r
marker <- tempfile()
d <- data.frame(a = c("p", "q", "p", "q"), g = c("m", "m", "f", "f"), n = 1:4)
names(d)[1] <- sprintf('writeLines("x", "%s")', marker)
p <- plot2(d, x = !!rlang::sym(names(d)[1]), y = n, facet = g, type = "back-to-back")
print(file.exists(marker))
# Expected after fix: FALSE
```

## Impact

Code execution through data when plot2 runs on data from others (the Shiny app on a server); failures for non-syntactic column names.

## Proposed fix

Use `rlang::sym()` for column names, and in `setup_aes()` accept backticked text only when it parses to a single name.

## History

- 2026-11-08: filed in Phase 6; test in test-regression-phase6.R.
- 2026-11-08: fixed: rlang::sym() for column names, column_selection_expr() for the UpSet selection label, setup_aes() parses backticked text only into a name; non-syntactic names now work in back-to-back plots.
