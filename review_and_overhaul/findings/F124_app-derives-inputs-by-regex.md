---
id: F124
title: The app derives tabs, widgets and labels from argument names and defaults by regular expressions, so D001 will change it silently
severity: medium
type: maintainability
area: shiny
status: verified
verification: verified
effort: M
api_change: no
locations: [R/shiny.R:146, R/shiny.R:1140, R/shiny.R:1199]
related: [D001, I004, F118, F119]
origin: R07 sheet S07
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F124 Inputs derived by pattern

## Summary

Which tab an argument appears in, which widget it gets and how its pretty label reads are all derived from `formals(plot2)` with regular expressions on names and on the class of the default. The implementation of D001 will therefore change the app without anyone touching `R/shiny.R`, and in several places for the worse:

- D001.2 (automatic defaults shown as `NULL`): `make_input()` gives a switch only when the default is a logical or the name matches a fixed list (`R/shiny.R:1140`); arguments whose default becomes `NULL` turn into text inputs.
- D001.7 and D001.8 (`x.lbl_*` to `x.label_*`, `facet.repeat_lbls_*` to `facet.repeat_labels_*`): the switch list names `repeat_lbls_x|repeat_lbls_y`, and the pretty labels replace `lbl|txt` (`:1202`).
- D001.9 (`sankey.*` to `sankey_*`): the pretty-label rule `"sankey"` and the grouping by prefix.
- D001.1 (`colour_opacity` to `colour_transparency`), D001.14 (`facet.fill` to `facet.colour_fill`), D001.10 (new `legend`), D001.3 (`x.mic`, `x.lbl_taxonomy` leave plot2()): tabs and widgets follow the names by accident, not by design.

## Evidence

- `R/shiny.R:146` to `:182` (grouping), `:1127` to `:1192` (widgets), `:1199` to `:1213` (labels). Probe: of 143 arguments, 57 become text inputs; see F118 and F119 for the current effects. Not testable before D001 is implemented; noted on sheet S07.

## Reproduction

```r
# By reading. After D001 is implemented, compare the widget table of the probe in
# review_and_overhaul/repro/shiny_widgets_r07.R before and after.
```

## Impact

Unnoticed regressions in the app whenever plot2's arguments change, which D001 will do on a large scale before the first CRAN release.

## Proposed fix

One explicit specification of the app's inputs (argument, tab, widget, kind of value, label, range), checked by a test against `formals(plot2)` so that a new or renamed argument fails the test until it is placed. Implement it together with D001.

## History

- 2026-11-08: filed in R07 (by reading).
- 2026-11-08: fixed: shiny_input_spec(), one row per argument (tab, widget, label, range or choices), checked against formals(plot2) by test S07.05.
