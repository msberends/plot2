# 16 validate_facet()

`R/validate.R:2251-2330`

## Role

Creates `facet_wrap()` (or `facet_grid()` for `facet.relative = TRUE`) with free or fixed scales derived from `facet.repeat_lbls_x`, `facet.repeat_lbls_y` and `facet.fixed_y`.

## Called from

`R/plot2.R:2147`.

## Contract

- In: `df`, `type`, facet arguments, `horizontal`.
- Out: a facet object.
- Side effects: message when assuming `facet.repeat_lbls_y = FALSE`.

## Branches

1. Scales: free, free_y, free_x, fixed (swapped when horizontal); sf always fixed.
2. NA in facet: `drop = TRUE` forced.
3. Relative: grid in columns (`facet.nrow` 1 or NULL) or rows (warning).
4. Wrap with `strip.position` and `nrow`.

## Findings

F052 (free x and y scales by default; fixed y only by heuristic). By reading: `facet.repeat_lbls_x` controls scale freedom, not only label repetition, so the name does not describe the effect; `facet.position` other than top or bottom is not honoured with `facet.relative = TRUE`; the local variable `switch` shadows `base::switch`.

## End-to-end test cases

- [ ] 16.01 Default facet: scales per decision F052 (currently free x and free y).
- [ ] 16.02 Facets with similar maxima: fixed y, no repeated y labels, message.
- [ ] 16.03 `facet.fixed_y = TRUE`: identical y ranges across panels.
- [ ] 16.04 `facet.repeat_lbls_x = FALSE`: shared x scale.
- [ ] 16.05 `facet.nrow = 2`: two rows.
- [ ] 16.06 `facet.position = "bottom"`.
- [ ] 16.07 `facet.relative = TRUE`: panel widths proportional to the number of x values; with `facet.nrow = 2`: warning, rows.
- [ ] 16.08 NA in facet: plot builds, NA panel present (with `na.replace`).
- [ ] 16.09 Horizontal plot: scale freedom swapped.
- [ ] 16.10 `facet.drop = TRUE` drops empty factor levels.
- [ ] 16.11 sf plot with facet: fixed scales.
