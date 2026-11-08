---
id: F080
title: plot2() on sf data crashes when x or y is given, instead of the intended caution
severity: medium
type: bug
area: methods
status: verified
verification: verified
effort: S
api_change: no
locations: [R/plot2-methods.R:1274, R/plot2-methods.R:1277]
related: [F036]
origin: R04 sheet M04
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F080 plot2() on sf data crashes when x or y is given, instead of the intended caution

## Summary

`plot2.sf()` wants to warn that `x` and `y` are ignored for maps, but tests `is.null(x)` and `is.null(y)` (`R/plot2-methods.R:1274`, `1277`). This forces the argument, which is a column name meant for tidy evaluation, so `plot2(netherlands, x = province)` fails with "object 'province' not found" and the helpful caution is never shown.

## Reproduction

```r
print(tryCatch({ plot2(netherlands, x = province); "OK" }, error = function(e) conditionMessage(e)))
# Expected after fix: "OK", with the caution that x is ignored (did you mean category?).
```

## Proposed fix

Use `!missing(x)` (or `!rlang::quo_is_null(rlang::enquo(x))`) instead of `is.null(x)`; the same pattern for `y`.

## History

- 2026-11-08: filed in R04, reproduced.
- 2026-11-08: fixed and verified (Phase 1). 2026-11-08: fixed; plot2.sf() uses missing(x) and missing(y), so the caution is shown instead of an evaluation error.
