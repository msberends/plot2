---
id: F139
title: An empty facet panel (facet.drop = FALSE) gives R warnings when rendering
severity: low
type: bug
area: facets
status: open
verification: verified
effort: S
api_change: no
locations: [R/validate.R:1643]
related: []
origin: I002 sheet 16 (case 16.10)
commit: 55edcca
created: 2026-11-08
updated: 2026-11-08
---

# F139 Empty facet panel warnings

## Summary

With a factor facet that has an unused level and `facet.drop = FALSE`, the empty panel makes rendering emit "no non-missing arguments to max; returning -Inf" (from `position_dodge2()` computing positions on an empty panel; plain ggplot2 does the same) and "Position guide is perpendicular to the intended axis" (source not yet traced; probably plot2's guide settings on a panel without data).

## Reproduction

```r
d <- data.frame(x = rep(c("a", "b", "c"), 2), y = c(1, 2, 3, 100, 200, 300),
                facet = factor(rep(c("f1", "f2"), each = 3), levels = c("f1", "f2", "f3")))
p <- plot2(d, x, y, facet = facet, facet.drop = FALSE)
invisible(ggplot2::ggplotGrob(p)) # two warnings
```

## Proposed fix

Trace the guide warning; for the dodge warning, consider not using a dodge position when there is no category (there is nothing to dodge), which also avoids the upstream warning.

## History

- 2026-11-08: found while coding I002 sheet 16 (case 16.10); test 16.10b is parked on it.
