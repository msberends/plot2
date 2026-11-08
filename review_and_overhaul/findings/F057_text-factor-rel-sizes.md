---
id: F057
title: text_factor turns rel() sizes into absolute sizes relative to the base size, not the parent element
severity: low
type: bug
area: theme
status: verified
verification: verified
effort: S
api_change: no
locations: [R/validate.R:2195]
related: [F032]
origin: R02 sheet 15
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F057 text_factor and rel() sizes

## Summary

With `text_factor != 1`, every `element_text` with a `rel()` size gets `base_size * text_factor * rel`. `rel()` is relative to the parent element, not the base size; for nested elements (e.g. `strip.text` under `text` vs. a parent that is itself relative) the result differs from the intended size.

## Reproduction

```r
p1 <- plot2(iris, Species, theme = "theme_bw", text_factor = 1)
p2 <- plot2(iris, Species, theme = "theme_bw", text_factor = 2)
s <- function(p, el) ggplot2::calc_element(el, ggplot2::complete_theme(p$theme))$size
print(c(s(p1, "axis.text.x"), s(p2, "axis.text.x")))
# Expected after fix: the second value exactly twice the first, for every text element.
```

## Proposed fix

Multiply only `theme$text$size` (and absolute sizes) by `text_factor` and leave `rel()` sizes untouched; ggplot2 inheritance then scales everything consistently.

## History

- 2026-11-07: filed in R02 (by reading).
- 2026-11-07: closed as not reproducible. With `theme_minimal2`, `theme_bw` and `theme_grey`, all 21 text elements are exactly doubled by `text_factor = 2` (checked with `calc_element()` on the completed theme). The concern from reading does not occur in practice; test case 15.10 keeps guarding it.
- 2026-11-08: fixed with F032: text_factor multiplies only absolute text sizes; rel() sizes stay relative to their parent, so every text element scales by exactly text_factor (checked for theme_minimal2 and theme_bw). Tests: test-regression-phase4.R (F057).
