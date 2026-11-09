# 21 sort_data()

`R/validate.R:2662-2771`

## Role

Applies the sort method to x, category or facet values and returns a factor with the right level order.

## Called from

`validate_data()` (x, category, facet, Sankey axes), `back2back_plot()` (facets).

## Contract

- In: values, original values (for "inorder"), sort method, data points and `summarise_function` (for frequency sorting), `horizontal`, `drop`, argument name (for messages).
- Out: factor (ordered for manual sorting) or the input unchanged (NULL method, numeric, dates, factor with `TRUE`).
- Side effects: message when frequency sorting summarises.

## Branches

Manual vector; factor asc/desc on levels; alphabetical with natural number ordering (stringr); in order of appearance; frequency ascending/descending via `fct_reorder()`; keeping unused levels when `drop = FALSE`.

## Findings

F008 (stringr only in Suggests), F013 (numeric x made character first). By reading: manual sorting returns an *ordered* factor, which changes model and scale behaviour if users reuse `p$data`; values not listed in a manual sort are appended in data order.

## End-to-end test cases

- [x] 21.01 Character x default: natural alphabetical ("A2" before "A10").
- [x] 21.02 Factor x default: level order kept.
- [x] 21.03 `x.sort = c("c", "a")`: c, a, then remaining in data order.
- [x] 21.04 `x.sort = "freq-desc"` with duplicates: ordered by summed y, message.
- [x] 21.05 `x.sort = FALSE`: order of appearance.
- [x] 21.06 `category.sort` and `facet.sort` equivalents.
- [x] 21.07 `x.drop = FALSE` keeps unused factor levels on the axis.
- [x] 21.08 Without stringr installed: still sorts (F008; run in a separate library or mock).
