---
id: F094
title: Eighteen arguments behave differently when their shown default is passed explicitly
severity: medium
type: design
area: architecture
status: open
verification: verified
effort: M
api_change: possible
locations: [R/plot2-methods.R:373, R/plot2.R:783, R/plot2.R:1379]
related: [F035, F036, F093, I004]
origin: R05 sheet A01
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F094 Visible defaults that act as sentinels

## Summary

Every method passes `missing()` flags for 18 arguments to `plot2_exec()` as hidden dot-arguments (`_misses.datalabels`, `_misses.x.title`, `_misses.x.max_items`, `_misses.y.percent_break`, ...; `R/plot2-methods.R:373` and the same block in every method). The behaviour then depends on whether the user typed the argument, not on its value. The usage, however, shows a concrete value such as `datalabels = TRUE`, `x.max_items = Inf` or `y.percent_break = 0.1`, so typing the default changes the plot. Arguments affected: `x`, `y`, `category`, `facet`, `title`, `subtitle`, `caption`, `tag`, `x.title`, `y.title`, `colour_fill`, `datalabels`, `summarise_function`, `x.max_items`, `x.zoom`, `facet.fixed_x`, `y.percent`, `y.percent_break`.

## Evidence

`datalabels`: with 30 x values the default shows no labels, `datalabels = TRUE` shows 30 (`R/plot2.R:1379` only removes or limits labels when `misses_datalabels`). The same mechanism explains F093 point 1 (`x.expand`, via `NULL` there) and the `barpercent` special-casing of `x.max_items`.

## Reproduction

```r
d <- data.frame(x = paste0("v", 1:30), y = 1:30)
n_labels <- function(p) { b <- ggplot2::ggplot_build(p); sum(vapply(b$data, function(l) if ("label" %in% names(l)) nrow(l) else 0L, 0L)) }
print(c(default = n_labels(plot2(d, x, y)), explicit = n_labels(plot2(d, x, y, datalabels = TRUE))))
# Expected after fix: equal counts (a NULL default for "automatic", TRUE meaning "always").
```

## Impact

Function wrappers, the Shiny app (which writes out arguments explicitly) and users who copy the usage get different plots from the same apparent call. It also blocks a single argument definition (F035), since `missing()` does not survive being passed on.

## Proposed fix

Use `NULL` (or a documented `"auto"`) as the default wherever plot2 decides automatically, and let explicit values mean what they say. Part of the consolidated API proposal I004.

## History

- 2026-11-08: filed in R05; test A01.05 parked.
- 2026-11-08: changing these defaults is not a breaking change for anyone before the first CRAN release (no public users); fix before that release.
- 2026-11-08: maintainer decided: `NULL` is the default wherever plot2 decides automatically, and explicit values mean what they say (e.g. `datalabels = TRUE` always shows labels). API change consented. Stays in Phase 5 with F035 and I004 unless pulled forward.
- 2026-11-08: maintainer decided for `summarise_function` (AskUserQuestion): default `NULL` means automatic (sum, or no summarising for several y columns and UpSet plots); `summarise_function = FALSE` means do not summarise; a function always applies. Breaking: `NULL` no longer means "do not summarise".
