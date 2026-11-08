---
id: F092
title: x.zoom, y.zoom and zoom do not set the expansion to 0 as documented, and zoom differs from x.zoom plus y.zoom
severity: medium
type: bug
area: scales
status: open
verification: verified
effort: S
api_change: no
locations: [R/plot2.R:117, R/plot2.R:179, R/validate.R:754, R/validate.R:979, R/plot2.R:1364]
related: [F024, F093]
origin: R05 sheet A03
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F092 Zoom arguments do not do what is documented

## Summary

The documentation says `x.zoom = TRUE` sets `x.limits = c(NA, NA)` and `x.expand = 0` (likewise for y), and that `zoom = TRUE` "will set `x.zoom = TRUE` and `y.zoom = TRUE`". In the code, `x.zoom` sets `x.expand <- 0.5` (`R/validate.R:754`) and `y.zoom` sets `y.expand <- c(0.25, 0.25)` (`R/validate.R:979`), so the axes are widened by 50% and 25% of the data range on both sides. `zoom = TRUE` gives yet another result than `x.zoom = TRUE, y.zoom = TRUE`, and with explicit `y.limits` it expands below them, where `y.zoom` does not.

## Evidence

`iris`, `plot2(Sepal.Length, Sepal.Width)` (data ranges 4.3 to 7.9 and 2.0 to 4.4):

| call | x range | y range |
|---|---|---|
| default | -0.2 to 9.48 | 0 to 5.5 |
| `x.zoom = TRUE` | 2.5 to 9.7 | |
| `y.zoom = TRUE` | | 1.4 to 5.0 |
| `zoom = TRUE` | 3.76 to 8.44 | 1.64 to 4.76 |
| `y.limits = c(1, NA), y.zoom = TRUE` | | 1.0 to 4.4 |
| `y.limits = c(1, NA), zoom = TRUE` | | -0.66 to 5.06 |

## Reproduction

```r
b <- function(...) ggplot2::ggplot_build(plot2(iris, Sepal.Length, Sepal.Width, ...))$layout$panel_params[[1]]
print(b(x.zoom = TRUE)$x.range)                  # 2.5 9.7, documented: 4.3 7.9
print(b(zoom = TRUE)$x.range)                    # 3.76 8.44, differs from x.zoom = TRUE
print(b(y.limits = c(1, NA), zoom = TRUE)$y.range) # -0.66 5.06, below the limit
# Expected after fix: 4.3 7.9; the same as x.zoom = TRUE, y.zoom = TRUE; starting at 1.
```

## Impact

Users who want a tight view get a loose one, and `zoom = TRUE` with explicit limits ignores the lower limit. The README and vignette suggest zooming as the way to remove the zero baseline, so this is a commonly used path.

## Proposed fix

Decide the intended expansion for zoomed axes (the code comment says "set default value to 0.5", so a small expansion may be intended) and make the documentation and the code agree. Route `zoom` through exactly the same code as `x.zoom` and `y.zoom`, after `y.limits` is known.

## History

- 2026-11-08: filed in R05 (sheet A03); tests A03.01 to A03.03 parked.
- 2026-11-08: maintainer decided: zoomed axes cover the data range with ggplot2's usual small expansion (5% on continuous axes); `zoom = TRUE` equals `x.zoom = TRUE, y.zoom = TRUE` and respects `y.limits`; documentation to match.
