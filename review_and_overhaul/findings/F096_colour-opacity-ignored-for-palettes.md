---
id: F096
title: colour_opacity is ignored for the default colours and every built-in R palette
severity: medium
type: bug
area: colours
status: verified
verification: verified
effort: S
api_change: no
locations: [R/get_colour.R:96, R/get_colour.R:117]
related: [F067, F088]
origin: R05 sheet A04
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F096 colour_opacity ignored for palettes

## Summary

`get_colour()` returns early for palette names from `grDevices::palette.pals()` (including the default `"ggplot2"` and `"R4"`, `"Okabe-Ito"`, ...) at `R/get_colour.R:117`, before the opacity is appended further down. `plot2(..., colour_opacity = 0.5)` therefore has no effect unless the user also gives explicit colours or a viridis palette, although `colour_opacity` is documented as the opacity "for `colour`/`colour_fill`". `get_colour("ggplot2", 2, opacity = 0.5)` returns opaque colours too.

## Reproduction

```r
print(as.character(get_colour("ggplot2", 2, opacity = 0.5)))   # "#F8766D" "#00BA38"
print(as.character(get_colour("red", opacity = 0.5)))          # "#FF000080"
p <- plot2(data.frame(x = c("a", "b"), y = 1:2), x, y, colour_opacity = 0.5)
print(unique(ggplot2::layer_data(p, 2)$fill))                  # opaque
# Expected after fix: 8-digit hex codes ending in 80 in all three cases.
```

## Impact

The argument appears broken in the most common situation (default colours). Same pattern as F067 (`category.focus` ignored with default colours).

## Proposed fix

Apply the opacity in one place at the end of `get_colour()` for every branch, rather than per branch.

## History

- 2026-11-08: filed in R05; test A04.05 parked.
- 2026-11-08: fixed and verified (Phase 1). 2026-11-08: fixed; get_colour() no longer returns early for registered colours and palettes, so opacity and NA handling apply to every branch; plot2() sets the colours itself (original_colours = FALSE) when colour_opacity > 0. Side note for the maintainer: plot2's 'ggplot2' palette is R's fixed palette (second colour #00BA38), whereas ggplot2's default hue spacing depends on the number of groups (#00BFC4 for two), so setting an opacity changes the second default colour.
- 2026-11-08: follow-up at the maintainer's request ("ggplot2" stands for not setting a colour): get_colour("ggplot2", n) now returns ggplot2's own hue spacing (scales::hue_pal()), so an opacity no longer changes the default colours. Regression test added.
