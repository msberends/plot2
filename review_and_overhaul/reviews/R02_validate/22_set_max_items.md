# 22 set_max_items()

`R/validate.R:2775-2870`

## Role

Implements `x.max_items`, `category.max_items`, `facet.max_items`: keeps the first n - 1 levels and merges the rest into one level named by `*.max_txt` (with `%n` for the number merged and `%p` for their share of rows).

## Called from

`validate_data()` (`R/validate.R:649`).

## Contract

- In: `df`, column names, limits and texts, `horizontal`, summarising and formatting arguments.
- Out: `df` with merged levels (and summarised again in some cases).
- Side effects: warning when values are not a factor.

## Findings

F062 (re-summarising condition reads inverted; `%p` is the share of rows, not of y). By reading: `n_max` items include the "rest" item, so `x.max_items = 10` shows 9 values plus the rest; this should be stated in the documentation.

## End-to-end test cases

- [x] 22.01 Five values, `x.max_items = 3`: two values plus "(rest, x3)".
- [x] 22.02 The rest item's y equals the summed y of the merged values.
- [x] 22.03 `x.max_txt = "Other (%n, %p)"`: count and percentage correct.
- [x] 22.04 `horizontal = TRUE`: rest item at the bottom.
- [x] 22.05 `category.max_items` and `facet.max_items`.
- [x] 22.06 Numeric x with `x.max_items`: warning.
- [x] 22.07 `type = "barpercent"` defaults to 10 items.
