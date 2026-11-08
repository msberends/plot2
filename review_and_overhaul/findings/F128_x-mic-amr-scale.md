---
id: F128
title: x.mic = TRUE errors with the current AMR package: unused argument drop
severity: medium
type: bug
area: dependencies
status: open
verification: verified
effort: S
api_change: possible
locations: [R/plot2.R:1940]
related: [D001]
origin: I002 (R02 sheet tests)
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F128 x.mic and AMR::scale_x_mic()

## Summary

`plot2()` calls `AMR::scale_x_mic(drop = x.drop, mic_range = x.limits)` (`R/plot2.R:1940`). Current AMR (3.1.0.9011 here) builds `scale_x_mic()` on a continuous ggplot2 scale, which has no `drop` argument, so every `x.mic = TRUE` plot fails with "unused argument (drop = FALSE)". D001.3 moves `x.mic` to a helper; the call must be fixed either way.

## Reproduction

```r
d <- data.frame(mic = AMR::as.mic(c("0.25", "1", "4", "1")), n = c(2, 5, 3, 1))
print(tryCatch({ p <- plot2(d, mic, n, x.mic = TRUE); "ok" }, error = conditionMessage))
# Expected after fix: "ok"
```

## Impact

The MIC feature does not work at all with the current AMR version.

## Proposed fix

Stop passing `drop`; pass only arguments that `AMR::scale_x_mic()` documents, and add a test that runs whenever AMR is installed (case 05.16).

## History

- 2026-11-08: filed while coding the I002 tests (case 05.16). The incompatibility may have been introduced on the AMR side; both packages are maintained by the maintainer.
