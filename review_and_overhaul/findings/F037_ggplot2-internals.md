---
id: F037
title: Heavy reliance on ggplot2 internals that are only kept for compatibility in ggplot2 4.x
severity: medium
type: maintainability
area: architecture
status: open
verification: reading
effort: L
api_change: no
locations: [R/utils.R:579, R/validate.R:2497, R/plot2.R:1802, R/plot2.R:1842, R/plot2.R:2187, R/coord_spider.R:112, R/move_layer.R:23, R/get_plot_title.R:45]
related: [F032]
origin: R01 §4.3
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F037 ggplot2 internals

## Summary

The package edits `p$layers`, `p$mapping`, `p$facet$params`, reads `p$labels` and `layer$stat_params`, and implements `CoordSpider` as a ggproto subclass of `Coord`. In ggplot2 4.x (S7 classes) the `$` accessors are a compatibility shim, and labels are resolved at build time, so `p$labels$title` may not reflect the final label.

## Reproduction

```r
# Structural finding. Track with the reverse-dependency checks of each ggplot2 release.
# Expected after fix: public API only (get_labs(), layer_data(), get_layer_data(), ggplot_add methods).
```

## Proposed fix

Inventory all internal accesses; replace with public helpers where they exist; isolate the remainder in one file with tests against ggplot2 development versions (the existing `check_dev_versions.yaml` workflow is the right place).

## History

- 2026-11-07: filed in R01.
