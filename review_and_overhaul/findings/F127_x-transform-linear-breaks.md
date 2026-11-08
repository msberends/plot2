---
id: F127
title: x.transform gives linear breaks on a transformed x axis
severity: medium
type: bug
area: scales
status: open
verification: verified
effort: S
api_change: no
locations: [R/validate.R:893]
related: [F043]
origin: I002 (R02 sheet tests)
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F127 Linear breaks on a transformed x axis

## Summary

`validate_x_scale()` always passes its own `breaks_fn()` (`R/validate.R:893`), which returns `scales::breaks_pretty()` in data units and ignores the transformation. With `x.transform = "log10"` and x from 1 to 1000, the breaks are 1,000, 2,000 and 3,000: the lower three decades have no break at all. `validate_y_scale()` handles the same case correctly (1, 10, 100, 1,000).

## Reproduction

```r
p <- plot2(data.frame(x = c(1, 10, 100, 1000), y = 1:4), x, y, x.transform = "log10")
b <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x
print(b$get_labels()[!is.na(b$get_breaks())])
# Currently: "1 000" "2 000" "3 000". Expected after fix: "1" "10" "100" "1 000"
```

## Impact

Logarithmic x axes are unreadable.

## Proposed fix

Use the transformation's own breaks (`waiver()`) when `x.transform` is not identity and no `x.breaks` are given, as the y scale does.

## History

- 2026-11-08: filed while coding the I002 tests (case 05.10).
