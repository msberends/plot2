# 02 validate_legend.position()

`R/validate.R:108-122`

## Role

Normalises `legend.position` to one of top, right, bottom, left, none.

## Called from

`R/plot2.R:2109`, `R/back2back_plot.R:199`, `R/validate.R:1338` (category scale).

## Contract

- In: `legend.position` (character, NULL or NA).
- Out: one of the five strings. NULL or NA become "none".
- Side effects: none.

## Branches

1. NULL/NA to "none". 2. One-letter abbreviations t, r, b, l. 3. Case and whitespace normalised. 4. Anything else: error.

## Findings

F050 (no "inside" or numeric positions; `FALSE` errors).

## End-to-end test cases

- [x] 02.01 Default: "top" for discrete category, "right" for numeric category.
- [x] 02.02 Each of "t", "R ", "bottom", "Left", "none" sets `theme$legend.position` accordingly.
- [x] 02.03 `legend.position = NA`: no legend shown; `NULL` is automatic since F094 (updated 2026-11-08).
- [x] 02.04 Invalid value errors with the allowed values listed.
- [x] 02.05 `legend.position = FALSE` hides the legend (desired, F050).
- [x] 02.06 `legend.position = "inside"` with coordinates (desired, F050; depends on decision).
- [x] 02.07 Back-to-back plot honours the setting.
