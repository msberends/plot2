---
id: F031
title: Unknown types are fuzzy-matched to the first alphabetical geom without notice
severity: medium
type: design
area: types
status: open
verification: verified
effort: S
api_change: no
locations: [R/validate.R:82, R/validate.R:89, R/validate.R:98]
related: [F009]
origin: R01 §3.7
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F031 Fuzzy type matching

## Summary

After normalisation (trailing "s" removed, `column` mapped to `col`), an unknown type is matched with `valid_geoms %like% type` and the first hit is used silently. The user's text is used as a regex. Because punctuation is stripped first, a type consisting of whitespace or punctuation (`" "`, `"()"`) reduces to `"geom_"`, which matches every geom, so `geom_abline` is chosen.

## Reproduction

```r
print(validate_type("geom_te"))
print(validate_type("seg"))
print(validate_type(" "))   # currently geom_abline
print(validate_type("()"))  # currently geom_abline
# Expected after fix: an error listing close matches, or at least a message naming the chosen geom.
```

## Proposed fix

Use exact matching against a documented alias table; for near misses, error with suggestions (`agrep()`).

## History

- 2026-11-07: filed in R01 (by reading).
- 2026-11-07: R02 added the whitespace and punctuation cases; reproduced.
