---
id: F102
title: Vignettes use suggested packages unconditionally and fail to build without them
severity: low
type: dependency
area: dependencies
status: open
verification: verified
effort: S
api_change: no
locations: [vignettes/supported_types.Rmd:22, vignettes/plot2.Rmd:22]
related: [F008]
origin: R05 sheet A09
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F102 Vignettes and suggested packages

## Summary

`supported_types.Rmd` reads `ggplot2movies::movies` in its set-up chunk and uses `type = "beeswarm"` (ggbeeswarm), `sf` data and `ggforce` (Sankey), all in Suggests; `plot2.Rmd` loads `patchwork` (Suggests). On this server, where `ggplot2movies` is not installed, rendering stops at the first chunk ("there is no package called 'ggplot2movies'"). Writing R Extensions (section 1.1.3.1) asks that suggested packages be used conditionally in vignettes, so that `R CMD check` with `_R_CHECK_FORCE_SUGGESTS_=false` and CRAN's no-Suggests checks pass.

## Reproduction

```r
src <- readLines("vignettes/supported_types.Rmd")
print(grep("ggplot2movies|requireNamespace", src, value = TRUE))
# Expected after fix: a requireNamespace() guard, e.g. a set-up chunk option
# eval = requireNamespace("ggplot2movies", quietly = TRUE) for the UpSet section.
```

## Impact

Vignette build fails on systems without the suggested packages; CRAN checks without Suggests would flag it.

## Proposed fix

Guard per section with `eval = requireNamespace(...)`, or replace `movies` by a small bundled or base data set for the UpSet example.

## History

- 2026-11-08: filed in R05; reproduced by rendering on this server; test A09.02 parked.
