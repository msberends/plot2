---
id: F082
title: add_errorbar() fails on any plot2 plot without a category
severity: high
type: bug
area: methods
status: open
verification: verified
effort: S
api_change: no
locations: [R/add_type.R:515, R/add_type.R:521, R/add_type.R:531]
related: [F040]
origin: R04 sheet M05
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F082 add_errorbar() fails on any plot2 plot without a category

## Summary

`add_errorbar()` builds new data with only `ymin`, `ymax` and `x`. Without a category it sets `inherit.aes = TRUE` (`R/add_type.R:531`), so the layer inherits the plot's `group` mapping, which plot2 sets to the original x column (e.g. `group = g`). That column does not exist in the new data, so building fails with "object 'g' not found". Only plots with a category work, which is what the single documented example uses.

## Reproduction

```r
d <- data.frame(g = c("a", "b"), n = c(3, 4))
p <- add_errorbar(plot2(d, g, n), min = n - 1, max = n + 1)
print(tryCatch(ggplot2::layer_data(p, length(p$layers))$ymin, error = function(e) conditionMessage(e)))
# Expected after fix: 2 3
```

## Proposed fix

Never inherit the plot mapping here: always use `inherit.aes = FALSE` with an explicit mapping (x, ymin, ymax, plus colour/group for a category), and carry the facet column into the new data.

## History

- 2026-11-08: filed in R04, reproduced.
