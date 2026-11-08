---
id: F030
title: plot2.default looks up character scalars with get() and can leave new_df undefined
severity: medium
type: bug
area: methods
status: verified
verification: verified
effort: S
api_change: possible
locations: [R/plot2-methods.R:180, R/plot2-methods.R:200]
related: []
origin: R01 §3.6
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F030 plot2.default lookup and undefined variable

## Summary

`plot2("a")` runs `get("a")` through dynamic scoping (package namespace, imports, search path) and plots any data frame named `a` it finds, instead of the single value "a". The comment mentions `ellmer::tool()` support. In the atomic-vector branch, a numeric `.data` with `y` supplied leaves `new_df` undefined.

## Reproduction

```r
a <- data.frame(q = c("z", "w"), v = 1:2)
p <- plot2("a")
print(names(p$data))
# Expected after fix: a plot of the character value "a" (or an explicit, documented lookup in the caller's environment only).
```

## Proposed fix

Look up only in `parent.frame()` and only when explicitly requested (for example a separate helper for tool calling), or drop the feature. Add an explicit error for the uncovered branch.

## History

- 2026-11-07: filed in R01 (by reading).
- 2026-11-08: maintainer decided (AskUserQuestion): keep the lookup of a character scalar, but only in the caller's environment (not the search path or package namespace) and only for data frames; document it.
- 2026-11-08: fixed as decided: a character scalar is looked up only in the caller's environment (get(inherits = FALSE) on parent.frame()), only data frames are used, and this is documented under .data. The uncovered branch is now handled: a numeric vector with y given is used as x; a vector with x given errors clearly. Tests: M01.09 unparked, M01.10 rewritten to the decided behaviour.
