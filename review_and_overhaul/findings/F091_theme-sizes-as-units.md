---
id: F091
title: theme_minimal2() passes grid units as element_text() sizes
severity: low
type: maintainability
area: theme
status: verified
verification: verified
effort: S
api_change: no
locations: [R/theme_minimal2.R:63, R/theme_minimal2.R:65, R/theme_minimal2.R:80]
related: [F037, F057]
origin: R04 sheet M08
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F091 Theme text sizes are grid units instead of numbers

## Summary

`theme_minimal2()` sets text sizes as `unit(9, "pt")`, `unit(13, "pt")` etc. `element_text(size = )` expects a number in points; a `unit` is accepted only because it is a numeric vector with attributes, so computed elements carry a `unit` attribute (`calc_element("plot.title", ...)@size` is a unit, not 13). Arithmetic with `rel()` and the `plot2.text_factor` scaling happens to work, but any stricter type check in ggplot2's S7 properties would break every plot2 theme.

## Reproduction

```r
s <- ggplot2::calc_element("plot.title", ggplot2::complete_theme(theme_minimal2()))@size
print(class(s))
# Expected after fix: "numeric".
```

## Proposed fix

Use plain numbers (`size = 13`).

## History

- 2026-11-08: filed in R04, reproduced.
- 2026-11-08: fixed and verified (Phase 1). 2026-11-08: fixed; theme_minimal2() uses plain numbers for element_text() sizes (legend.key.size stays a unit). Regression test F091.
