# E05 UpSet plots

`R/plot2.R:1102-1323`

## Role

Builds an UpSet plot from several logical x columns: intersection bar chart (top right, via a nested `plot2()`), set sizes (bottom left, nested `plot2()`), dot matrix with connecting segments (bottom right), y title (top left), combined with patchwork.

## Contract

- Requires at least two x columns; values coerced to logical.
- y defaults to 1 per row (counts), summed with `summarise_function`.
- `x.sort` defaults to "freq-desc".

## Findings

F001 (nested `plot2()` calls clear the outer state), F005 (`theme()` calls), F010 (`print = TRUE` falls through), F071 (`x.sort` vector), F075 (new: tidyselect deprecation warning from `c_across(vars_x)`), F002 (RNG consumed by the queued messages).

## End-to-end test cases

- [x] E05.01 Result is a patchwork of four plots.
- [x] E05.02 Intersection sizes equal the counts of each combination, sorted descending.
- [x] E05.03 Set sizes in the bottom-left panel equal the column sums (drawn as negative bars).
- [x] E05.04 Default y title "Intersection size"; `y.title` replaces it.
- [x] E05.05 `category` stacks the intersection bars.
- [x] E05.06 `y` given: intersection heights are sums of y.
- [x] E05.07 `x.sort = "freq-asc"` sorts ascending.
- [x] E05.08 A single x column errors.
- [x] E05.09 (skipped: F071) `x.sort` as a vector.
- [x] E05.10 (skipped: F010) `print = TRUE`.
- [x] E05.11 No internal state left behind (state and locale).
- [x] E05.11b (skipped: F002) Random-number stream untouched.
- [x] E05.12 (skipped: F075) No R warnings.
