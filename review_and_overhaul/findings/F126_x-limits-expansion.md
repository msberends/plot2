---
id: F126
title: x.limits announces x.expand = 0 but keeps the default expansion
severity: medium
type: bug
area: scales
status: open
verification: verified
effort: S
api_change: no
locations: [R/validate.R:763, R/validate.R:769, R/validate.R:906]
related: [F024, F092]
origin: I002 (R02 sheet tests)
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F126 x.limits keeps the default expansion

## Summary

With `x.limits` on a numeric x, plot2 says "Assuming x.expand = 0 since x.limits is set" and sets `x.expand <- 0` (`R/validate.R:769`), but `set_x.expand` was computed before that (`:763`), so the continuous branch (`:906`) still applies the default expansion of 2.5% and 20%. `x.limits = c(0, 10)` gives an axis from -0.25 to 12 with a break at 12. The y-axis does not have this problem.

## Reproduction

```r
p <- plot2(data.frame(x = c(1, 2, 3, 4, 5), y = c(2, 4, 3, 8, 6)), x, y, x.limits = c(0, 10))
print(ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x.range)
# Currently: -0.25 12. Expected after fix: 0 10
```

## Impact

The axis does not end where the user asked, and the message says the opposite of what happens.

## Proposed fix

Compute `set_x.expand` after the defaults, or set it to `TRUE` together with `x.expand <- 0`.

## History

- 2026-11-08: filed while coding the I002 tests (case 05.03).
