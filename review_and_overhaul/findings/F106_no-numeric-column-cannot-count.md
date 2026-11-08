---
id: F106
title: Without y and without a numeric column, plot2() stops instead of counting rows
severity: medium
type: bug
area: data-validation
status: verified
verification: verified
effort: S
api_change: no
locations: [R/validate.R:151]
related: [F097, F079, F012]
origin: R06 sheet T03
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F106 No numeric column: no counting

## Summary

`plot2(df, h)` on a data frame with only categorical columns stops with "no numeric column found to use for y", for every type (default, `"col"`, `"barpercent"`, `"sankey"`), although counting rows is the obvious meaning and `plot2.default()` already counts for a character vector. The message does not hint at `y = n()`. Sankey diagrams on raw (unaggregated) data are affected in the same way: they need an artificial numeric column.

## Reproduction

```r
d <- data.frame(h = c("a", "b", "b"), k = c("x", "y", "y"))
print(tryCatch(plot2(d, h), error = conditionMessage))
print(tryCatch(plot2(d, x = c(h, k), type = "sankey"), error = conditionMessage))
# Expected after fix: bars of 1 and 2; a Sankey diagram of the counts.
```

## Impact

One of the most natural first calls (a frequency chart of a categorical column) fails. Together with F097, missing-y handling chooses badly in both directions: an ID column when one exists, an error when none exists.

## Proposed fix

When `y` is missing and no suitable numeric column exists, use `n()`, with an info message. Consider doing the same for character or factor x in column types generally, and exclude ID-like columns (F097).

## History

- 2026-11-08: filed in R06 (sheet T03, found while benchmarking a Sankey diagram); test T03.04 parked.
- 2026-11-08: fixed and verified (Phase 1). 2026-11-08: fixed; when y is missing and the data have no numeric column, or the type is barpercent or sankey, plot2_exec() uses y = n() with a message. This replaces the 'Sankey plots require y' error added for F071 (regression test updated). Not done: a default category for Sankey plots (first x variable) would change the documented error of E06.06 and E15.03; raised to the maintainer. T03.05 now passes category explicitly.
