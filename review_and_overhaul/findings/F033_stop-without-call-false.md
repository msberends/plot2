---
id: F033
title: Eight stop() calls lack call. = FALSE
severity: low
type: maintainability
area: housekeeping
status: verified
verification: reading
effort: S
api_change: no
locations: [R/plot2.R:822, R/plot2-methods.R:553, R/get_colour.R:62, R/get_colour.R:66, R/get_colour.R:287, R/get_colour.R:292, R/add_type.R:242, R/shiny.R:52]
related: [F014]
origin: R01 §3.9
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F033 stop() without call. = FALSE

## Summary

Project convention (CLAUDE.md) requires `call. = FALSE`; eight calls do not follow it.

## Reproduction

```r
print(tryCatch(get_colour("red", length = NA), error = function(e) deparse(conditionCall(e))))
# Expected after fix: "NULL"
```

## Proposed fix

Add `call. = FALSE`, or switch to `cli::cli_abort()` as part of F014.

## History

- 2026-11-07: filed in R01 (by reading).
- 2026-11-08: fixed and verified (Phase 1). Added `call. = FALSE` to the remaining calls; regression test "F033" walks all of `R/`.
