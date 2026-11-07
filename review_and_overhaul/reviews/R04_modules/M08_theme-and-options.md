# M08 theme_minimal2() and package options

`R/theme_minimal2.R`, `R/options.R`

## Role

plot2's default theme, built on `theme_bw()` with fixed sizes and colours from options (`plot2.colour_font_primary`, `_secondary`, `_axis`, `plot2.colour_background`). All options used in the code are documented in `plot2-options` (checked in R04: complete).

## Findings

F087 (new: `...` replaces whole elements), F091 (new: sizes given as grid units), F032 (partial themes in `plot2()`).

## End-to-end test cases

- [x] M08.01 Complete theme with plot2's sizes (title 13, subtitle 11).
- [x] M08.02 Font colour options are honoured.
- [x] M08.03 `plot2.colour_background` sets panel and plot background.
- [x] M08.04 `+ theme(...)` merges with plot2's settings.
- [x] M08.05 (skipped: F087) `theme_minimal2(...)` merges as well.
- [x] M08.06 Every `plot2.*` option read in the code is documented in `R/options.R`.
