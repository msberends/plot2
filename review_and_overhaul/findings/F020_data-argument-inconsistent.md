---
id: F020
title: data argument documented on the generic but missing from most methods
severity: high
type: bug
area: methods
status: open
verification: verified
effort: S
api_change: possible
locations: [R/plot2.R:549, R/plot2-methods.R:1471]
related: [F019, F035]
origin: R01 §2.13
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F020 data argument inconsistent

## Summary

The generic `plot2()` has `data = NULL`, as do the formula and sf methods; the data.frame, default, matrix and freq methods do not. Passing `data =` to them triggers partial matching against the many `datalabels.*` arguments.

## Evidence

Formals comparison during R01; `plot2.data.frame` lacks `data`.

## Reproduction

```r
print(tryCatch({ plot2(iris, Species, Sepal.Length, data = mtcars); "OK" }, error = function(e) conditionMessage(e)))
# Expected after fix: a clear message (either "data is only used with formulas" or the argument is accepted).
```

## Impact

Cryptic error "argument 4 matches multiple formal arguments".

## Proposed fix

Add `data = NULL` to all methods and warn when it is supplied but unused, or remove it from the generic and accept it only in `plot2.formula()`.

**Decision needed:** where `data` belongs in the API.

## History

- 2026-11-07: filed in R01, reproduced.
- 2026-11-08: maintainer decided (AskUserQuestion): only in plot2.formula(): `data` removed from the generic; other methods give a clear error when `data =` is supplied.
