---
id: F087
title: theme_minimal2(...) replaces whole elements, losing plot2's settings for them
severity: low
type: bug
area: theme
status: verified
verification: verified
effort: S
api_change: no
locations: [R/theme_minimal2.R:98]
related: [F032]
origin: R04 sheet M08
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F087 theme_minimal2(...) replaces whole elements, losing plot2's settings for them

## Summary

Arguments in `...` are applied with `%+replace%` (`R/theme_minimal2.R:98`), so `theme_minimal2(plot.title = element_text(face = "italic"))` resets the title's size (13 to 11), alignment margin and the `plot2.colour_font_primary` colour. Users following the documentation ("Arguments passed on to theme()") expect the behaviour of `+ theme(...)`, which only changes the given properties.

## Reproduction

```r
library(ggplot2)
e0 <- calc_element("plot.title", complete_theme(theme_minimal2()))
e1 <- calc_element("plot.title", complete_theme(theme_minimal2(plot.title = element_text(face = "italic"))))
print(c(default = e0@size, with_dots = e1@size))
# Expected after fix: both 13.
```

## Proposed fix

Use `t + theme(...)` (merge) instead of `%+replace%`.

## History

- 2026-11-08: filed in R04, reproduced.
- 2026-11-08: fixed and verified (Phase 1). 2026-11-08: fixed; theme_minimal2(...) merges with + theme(...) instead of %+replace%. Regression test F087.
