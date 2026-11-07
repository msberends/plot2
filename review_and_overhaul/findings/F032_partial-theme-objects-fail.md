---
id: F032
title: Partial theme objects fail because elements are modified via $ on missing entries
severity: medium
type: bug
area: theme
status: open
verification: verified
effort: M
api_change: no
locations: [R/validate.R:2047, R/validate.R:2118, R/validate.R:2147, R/validate.R:2190, R/theme_minimal2.R:41]
related: [F005, F037]
origin: R01 §3.8
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F032 Partial theme objects

## Summary

`validate_theme()` edits theme elements with `theme$axis.text.x$angle <- ...`. If the element is absent (partial theme), this creates a plain list, which ggplot2 cannot merge. Sizes are stored as `unit()` objects in text elements (`strip.text`, `legend.text`), which ggplot2 does not expect.

## Reproduction

```r
df <- data.frame(g = c("a", "b", "c"), v = 1:3)
print(tryCatch({ ggplot2::ggplot_build(plot2(df, g, v, theme = ggplot2::theme(panel.background = ggplot2::element_blank()))); "OK" },
               error = function(e) conditionMessage(e)))
# Expected after fix: "OK" (partial theme added on top of the default), or a clear error.
```

## Proposed fix

Complete the theme first (`theme_minimal2() + user_theme` or `ggplot2::complete_theme()`), then add modifications as a `ggplot2::theme(...)` object instead of editing elements in place. Use numeric sizes.

## History

- 2026-11-07: filed in R01, reproduced.
