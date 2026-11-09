# 09 validate_colour()

`R/validate.R:1760-1888`

## Role

Resolves `colour`, `colour_fill` and `colour_opacity` into the concrete colour vectors for the geom and the scales, for numeric and discrete categories.

## Called from

`R/plot2.R:1464`.

## Contract

- In: `df`, `type`, `type_backup`, `colour`, `colour_fill`, `colour_opacity`, `misses_colour_fill`, `horizontal`.
- Out: `list(colour, colour_fill)` of class `colour`.
- Side effects: reads registered colours.

## Branches

1. Numeric or date category: palettes of 4 (registered sets) or 5 (viridis) colours; fill defaults to colour.
2. No category: first colour; continuous geoms get a 75% lighter fill.
3. Discrete category: one colour per category; lighter fill for continuous geoms; reversed when horizontal; named colours kept.
4. "Expansion" of colours for repeated groups (marked TODO, hack since ggplot2 3.4.0).
5. sf without category: single fill.

## Findings

F047 (fewer colours than categories errors), F061 (hack and dead code), F016 (via `get_colour()`). By reading: `category.focus = 3` (in `plot2.R`) refers to the alphabetical order, not the displayed order.

## End-to-end test cases

- [x] 09.01 Default colours without category: geom colour equals `get_colour("ggplot2")`.
- [x] 09.02 `colour = "red"` on columns: fill and colour red.
- [x] 09.03 boxplot without category: white fill and a darker outline (corrected 2026-11-08: the sheet expected a lighter version of the colour; the current white fill is intended and tested).
- [x] 09.04 Discrete category, palette name: one distinct colour per category.
- [x] 09.05 Named colour vector maps names to categories regardless of order.
- [x] 09.06 Two colours for three categories: recycled or clear warning (F047). (parked on F047 until it was fixed)
- [x] 09.07 `colour_opacity = 0.5`: alpha in the hex codes. (parked on F096 until it was fixed)
- [x] 09.08 `horizontal = TRUE`: colours reversed so the legend order matches the bars.
- [x] 09.09 Registered colour set with a numeric category: four-colour gradient.
- [x] 09.10 `category.focus = "b"`: b coloured, others grey85; numeric focus uses the displayed order. (parked on F067 until it was fixed)
- [x] 09.11 Invalid colour name: caution and fallback to ggplot2 colours.
