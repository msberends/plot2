---
id: F095
title: options(plot2.default_type) skips the type aliases, so "line" or "l" silently give geom_abline
severity: medium
type: bug
area: types
status: verified
verification: verified
effort: S
api_change: no
locations: [R/validate.R:48, R/plot2-methods.R:209, R/plot2.R:71, R/options.R:37]
related: [F031, F093]
origin: R05 sheet A02
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F095 plot2.default_type bypasses the type aliases

## Summary

`type = "line"` and `type = "l"` work, but the same values set as `options(plot2.default_type = )` give `geom_abline`. The option is read inside `validate_type()` (`R/validate.R:48`) and in `plot2.default()` (`R/plot2-methods.R:209`), after the alias handling for `type` has already run, and then go through the fuzzy geom lookup of F031. `?plot2` says the option "defaults to `"col"`", which suggests that short names are valid.

## Reproduction

```r
d <- data.frame(x = c("a", "b"), y = 1:2)
geom <- function(p) class(p$layers[[1]]$geom)[1]
print(geom(plot2(d, x, y, type = "line")))                       # GeomLine
print(withr::with_options(list(plot2.default_type = "line"), geom(plot2(d, x, y)))) # GeomAbline
# Expected after fix: GeomLine in both cases.
```

## Impact

A user who sets a house default gets a wrong plot type without any message.

## Proposed fix

Normalise the option with the same function that normalises `type` (one alias table), and validate it once.

## History

- 2026-11-08: filed in R05; test A02.04 parked.
- 2026-11-08: fixed and verified (Phase 1). 2026-11-08: fixed; the plot2.default_type option is normalised by validate_type() like the type argument.
