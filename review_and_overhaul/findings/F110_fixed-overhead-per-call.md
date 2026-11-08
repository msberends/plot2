---
id: F110
title: Every plot2() call costs about 0.1 s regardless of data size, a third of it rebuilding the theme
severity: low
type: performance
area: performance
status: open
verification: verified
effort: M
api_change: no
locations: [R/validate.R:2047]
related: [F025, F032]
origin: R06 sheet T03
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F110 Fixed overhead per call

## Summary

On five rows, `plot2()` takes 0.11 s before building, against 0.003 s for the equivalent `ggplot() + geom_col()` (installed, byte-compiled build; `repro/perf_r06.R`). Profiling 30 calls attributes 38% of the time to `validate_theme()` (`R/validate.R:2047`), which builds `theme_bw()`, `theme_grey()` and many `theme()` elements as S7 objects on every call, about 21% to `validate_data()`, and 13% to `validate_type()`. The overhead does not depend on the data.

## Reproduction

```r
tiny <- data.frame(g = letters[1:5], v = 1:5)
invisible(plot2(tiny, g, v))
print(system.time(for (i in 1:20) plot2(tiny, g, v))[["elapsed"]] / 20)
print(system.time(for (i in 1:20) ggplot2::ggplot(tiny, ggplot2::aes(g, v)) + ggplot2::geom_col())[["elapsed"]] / 20)
# Expected after fix: well below 0.05 s per plot2() call.
```

## Impact

Unnoticeable for one plot; noticeable in Shiny (every input change), in reports looping over hundreds of variables (100 plots: 11 s before rendering) and in the test suite.

## Proposed fix

Cache the completed default theme per combination of the options it depends on (font, text factor, colours) and only add the per-call modifications; avoid repeated `%+replace%` on full themes.

## History

- 2026-11-08: filed in R06 (sheet T03).
