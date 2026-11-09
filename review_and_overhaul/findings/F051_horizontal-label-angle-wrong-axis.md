---
id: F051
title: With horizontal = TRUE, x.lbl_angle rotates the value axis instead of the category labels
severity: medium
type: bug
area: theme
status: verified
verification: verified
effort: S
api_change: no
locations: [R/validate.R:2146]
related: [F032]
origin: R02 sheet 15
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F051 x.lbl_angle on the wrong axis when horizontal

## Summary

`validate_theme()` swaps several x/y theme settings for horizontal plots (italic, removal), but not the label angle and alignment: `theme$axis.text.x$angle` is set, which after `coord_flip()` is the value axis at the bottom.

## Reproduction

```r
p <- plot2(data.frame(g = c("a", "b"), v = 1:2), g, v, horizontal = TRUE, x.lbl_angle = 45)
cat("axis.text.x angle:", p$theme$axis.text.x$angle, " axis.text.y angle:", if (is.null(p$theme$axis.text.y$angle)) "NULL" else p$theme$axis.text.y$angle, "\n")
# Expected after fix: the angle applied to axis.text.y (the category labels).
```

## History

- 2026-11-07: filed in R02, reproduced.
- 2026-11-08: fixed and verified (Phase 1). 2026-11-08: fixed; validate_theme() sets angle and hjust on axis.text.y when horizontal = TRUE. Regression test F051 in test-regression-phase1.R.
