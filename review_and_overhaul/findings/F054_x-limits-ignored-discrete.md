---
id: F054
title: x.limits is silently ignored for discrete x
severity: low
type: bug
area: scales
status: verified
verification: verified
effort: S
api_change: no
locations: [R/validate.R:869]
related: []
origin: R02 sheet 05
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F054 x.limits ignored on discrete x

## Summary

For character or factor x, `validate_x_scale()` builds `scale_x_discrete()` without `limits`, so `x.limits = c("a", "b")` has no effect and no message.

## Reproduction

```r
p <- plot2(data.frame(g = c("a", "b", "c"), v = 1:3), g, v, x.limits = c("a", "b"))
print(ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$get_labels())
# Currently: a b c
# Expected after fix: a b (or a warning pointing to x.sort / filtering).
```

## History

- 2026-11-07: filed in R02, reproduced.
- 2026-11-08: fixed and verified (Phase 1). 2026-11-08: fixed; character or factor x.limits on a discrete x select and order the items (scale limits), drop the other rows before plotting, warn about values not in x, and are kept out of coord_flip(). Test 05.05 now uses numeric limits to trigger the class warning.
