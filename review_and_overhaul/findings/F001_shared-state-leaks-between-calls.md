---
id: F001
title: Shared plot2_env state leaks between calls and corrupts axis labels
severity: critical
type: bug
area: architecture
status: open
verification: verified
effort: L
api_change: no
locations: [R/utils.R:14, R/utils.R:635, R/utils.R:658, R/back2back_plot.R:505, R/plot2.R:918, R/plot2.R:1200, R/plot2.R:1244, R/validate.R:1145]
related: [F014, F036]
origin: R01 §1.1
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F001 Shared state leaks between calls and corrupts axis labels

## Summary

All per-call information (mapping names, limits, queued messages) is stored in the package-level environment `plot2_env`. It is only cleared by an `on.exit()` that is not registered on every path, is registered too late in the back-to-back path, and is triggered prematurely by nested `plot2()` calls. Because `set_plot2_env()` only writes a mapping name when none is stored yet, a leftover value from an earlier call wins over the current one.

## Evidence

- `set_plot2_env()` writes `mapping_x` etc. only `if (is.null(plot2_env$mapping_x))` (`R/utils.R:641` onwards).
- `back2back_plot()` registers its cleanup at the very end (`R/back2back_plot.R:505`); `plot2_exec()` deliberately skips its own cleanup for back-to-back (`R/plot2.R:918`).
- `plot2_env$y_lim` is set for Sankey plots before the cleanup is registered (`R/plot2.R:875`).
- UpSet plots call `plot2()` recursively (`R/plot2.R:1200`, `R/plot2.R:1244`); the inner call's `on.exit()` clears the outer call's state.
- Label functions that are evaluated at render time call `plot2_message()` after the state was cleared (`R/validate.R:1145`); the message then appears with the next, unrelated call.

## Reproduction

```r
df <- data.frame(g = c("a", "b", "c"), v = 1:3)
try(plot2(data.frame(a = c("x", "y"), v = 1:2, f = c("m", "m")), a, v, facet = f, type = "b2b"), silent = TRUE)
cat("left in plot2_env:", ls(plot2_env), "\n")
p <- plot2(df, g, v)
cat("x label of unrelated plot:", ggplot2::get_labs(p)$x, "\n")
clear_plot2_env()
p <- plot2(data.frame(x = letters[1:3], y = c(1e7, 2e7, 3e7)), x, y)
invisible(ggplot2::ggplot_build(p))
cat("messages queued after rendering:", plot2_env$infos, "\n")
# Expected after fix: nothing left in plot2_env except reg_cols; x label "g"; no queued messages.
```

## Impact

Silent wrong labels on plots after any failed call in the same session; messages attributed to the wrong plot. Particularly harmful in long interactive sessions and Shiny apps.

## Proposed fix

Replace the module-level state with a per-call context: create an environment in `plot2_exec()` (`ctx <- new.env()`), pass it explicitly to helpers (or store it in a dynamically scoped variable looked up via a small accessor), and drop `clear_plot2_env()` except for the registered colours. Short-term mitigation: register cleanup as the first statement of `plot2_exec()` for every path, and save and restore (not clear) the previous state so nested calls are safe. Render-time functions must not emit messages; compute such decisions eagerly.

## History

- 2026-11-07: filed in R01, reproduced.
