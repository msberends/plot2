---
id: F005
title: theme argument given as a function shadows ggplot2::theme() and crashes
severity: critical
type: bug
area: theme
status: open
verification: verified
effort: S
api_change: no
locations: [R/back2back_plot.R:331, R/back2back_plot.R:464, R/back2back_plot.R:500, R/plot2.R:1190, R/plot2.R:1230, R/plot2.R:1265, R/plot2.R:1298, R/plot2.R:2110, R/plot2.R:2248]
related: [F032]
origin: R01 §1.5
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F005 theme argument as a function crashes

## Summary

Inside `plot2_exec()` the argument is called `theme`. Calls such as `theme(legend.position = ...)` resolve to that argument whenever it is a function (R skips non-function objects when looking up a function, so the default character value does not trigger it). The documented form `theme = theme_bw` therefore fails.

## Evidence

`R/plot2.R:2110`: `p <- p + theme(legend.position = legend.position)`; same pattern at lines 1190, 1230, 1265, 1298 and 2248.

## Reproduction

```r
df <- data.frame(g = c("a", "b", "c"), v = 1:3)
res <- tryCatch({ plot2(df, g, v, theme = ggplot2::theme_bw); "OK" }, error = function(e) conditionMessage(e))
print(res)
# Expected after fix: "OK"
```

## Impact

Documented usage fails with a confusing "unused argument" error.

## Proposed fix

Use `ggplot2::theme(...)` explicitly in all internal calls (cheap, robust). Alternatively normalise the user's argument into a local `theme_obj` at the top of `plot2_exec()`.

## History

- 2026-11-07: filed in R01, reproduced.
- 2026-11-07: R03 found the same pattern in `back2back_plot()` (`theme(...)` calls after the halves and for the patchwork), reproduced with `type = "b2b", theme = ggplot2::theme_bw`.
