---
id: F103
title: Help pages change at every documentation run because roxygen calls sample() and embeds the R version
severity: low
type: maintainability
area: housekeeping
status: verified
verification: verified
effort: S
api_change: no
locations: [R/get_colour.R:22, R/get_colour.R:23]
related: [F002]
origin: R05 sheet A07
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F103 Non-reproducible help pages

## Summary

The `@param x` text of `get_colour()` uses inline R code with `sample(colours(), 5)` and `R.version`. Every `devtools::document()` run therefore rewrites `man/colour.Rd` with five different colour names (visible in the last three commits that touched the file) and with the R version of whoever documented. It also consumes the random-number stream during documentation.

## Reproduction

```r
system("git log -p -n 3 -- man/colour.Rd | grep '^[-+].*such as'")
# Expected after fix: no diff in man/colour.Rd between documentation runs.
```

## Impact

Noise in diffs and reviews; the help page shows a version that need not match the user's R.

## Proposed fix

Use a fixed set of example colours and drop the R version (or say "in the R version you use").

## History

- 2026-11-08: filed in R05; test A01.06 parked.
- 2026-11-08: fixed and verified (Phase 1). 2026-11-08: fixed; the get_colour() documentation is static (no sample(), no R version, no generated palette list); two consecutive document() runs leave man/ unchanged.
