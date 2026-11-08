---
id: F119
title: Inputs derived from defaults: secondary-axis arguments missing, sliders without range, misleading starting values
severity: low
type: bug
area: shiny
status: open
verification: verified
effort: S
api_change: no
locations: [R/shiny.R:173, R/shiny.R:182, R/shiny.R:1143, R/shiny.R:1189]
related: [F118, F124]
origin: R07 sheet S01
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F119 Gaps in the generated inputs

## Summary

- The eight `y_secondary*` arguments are excluded from every tab (`R/shiny.R:182`, and the title filter at `:173`), so the secondary axis cannot be used in the app. (`legend.title` is also excluded; that is consistent with D001.12, since `category.title` is offered.)
- Numeric defaults without a specific rule get a slider from 0 to three times the default (`:1189`). For a default of 0 that is a slider from 0 to 0: `colour_opacity` and `category.expand` cannot be changed.
- Sliders whose default is a call fall back to a fixed starting value that differs from plot2's real default (`datalabels.size` starts at 1, plot2 uses 3), so the first touch jumps.
- `is.numeric(default) && is.null(default)` (`:1143`) can never be true: dead branch.

## Evidence

- Probe over all 143 arguments; tests S01.02 and S01.03 (parked).

## Reproduction

```r
app <- create_interactively()
html <- as.character(get("ui", environment(app$serverFuncSource())))
print(grepl('id="y_secondary"', html))
print(regmatches(html, regexpr('<input class="js-range-slider" id="colour_opacity"[^>]*>', html)))
# Expected after fix: TRUE, and a slider with data-max greater than data-min
```

## Impact

Features missing or unusable in the app.

## Proposed fix

Part of the explicit input specification proposed in F124.

## History

- 2026-11-08: filed in R07 (verified).
