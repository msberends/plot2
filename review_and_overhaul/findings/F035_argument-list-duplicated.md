---
id: F035
title: About 150 arguments are duplicated across eight functions (about 2,000 lines)
severity: medium
type: maintainability
area: architecture
status: verified
verification: verified
effort: XL
api_change: no
locations: [R/plot2.R:398, R/plot2.R:615, R/plot2-methods.R:23, R/plot2-methods.R:399, R/plot2-methods.R:765, R/plot2-methods.R:1100, R/plot2-methods.R:1471, R/plot2-methods.R:1811]
related: [F020, F034, F036]
origin: R01 §4.1
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F035 Duplicated argument lists

## Summary

The generic, `plot2_exec()` and six methods each repeat the full argument list and forward every argument by name. This has already drifted: `_label.y_secondary` is passed twice in several methods, defaults differ (F034), and `data` is inconsistent (F020). Any new argument must be added in at least eight places plus the Shiny app.

## Reproduction

```r
fns <- list(plot2, plot2_exec, plot2.default, plot2.data.frame, plot2.matrix, plot2.sf, plot2.freq, plot2.formula)
print(vapply(fns, function(f) length(formals(f)), 1))
# Expected after fix: the argument list defined once.
```

## Proposed fix

Methods only adapt `.data` (and method-specific defaults), then forward with `match.call()` / `rlang::call_modify()` to a single implementation; or define the formals once and assign them with `formals<-` at build time. Method-specific defaults become a small named list per method.

## History

- 2026-11-07: filed in R01.
- 2026-11-08: fixed: the arguments are defined once, in the generic; R/zzz.R generates the formals of the methods and plot2_exec() and builds the forwarding call (plot2_exec_call()), so each method only states its own defaults and preparation.
