# A04 plot2(): category, legend, colours and facets

`R/plot2.R:39-49, 86-95, 118-136, 173-174`; behaviour in `validate_colour()`, `validate_category_scale()`, `validate_facet()`, `get_colour()`.

## Checked against behaviour

| Claim | Result |
|---|---|
| `legend.position` defaults: `"right"` for numeric category and sf, `"top"` otherwise | holds |
| `legend.position` `NA` hides; abbreviations accepted | holds |
| `legend.position` `NULL` hides | `NULL` is the default and means automatic (F093) |
| `colour_opacity` "for colour/colour_fill" | ignored for default colours and every R palette (F096, new) |
| `category.focus` by value or position | holds with explicit colours; not with defaults (F067) |
| `x.sort` values list | `"alpha-desc"` used in examples is not listed (F098) |
| Named `colour` vector matches category | holds |
| `facet.fixed_y` CV rule (25%) | as documented; open decision F052 |
| `facet.nrow` | holds |
| `reverse` versus `legend.reverse` | both work; two arguments for related concepts (I004) |
| `facet.fixed_x` "acts like the inverse of `x.drop`" | not further tested |

## Findings

F096 (new); F052, F067, F093, F098 (existing or new elsewhere).

## End-to-end test cases

- [x] A04.01 `legend.position` defaults for discrete, numeric and sf.
- [x] A04.02 `NA` hides the legend; `"b"` gives bottom.
- [x] A04.03 `legend.position = NULL` gives the automatic default.
- [x] A04.04 `colour_opacity` with an explicit colour.
- [x] A04.05 (skipped: F096) `colour_opacity` with default colours.
- [x] A04.06 `category.focus` by value and by position.
- [x] A04.07 `category.sort = "alpha-desc"`.
- [x] A04.08 Named colour vector.
- [x] A04.09 `facet.nrow`.
- [x] A04.10 `reverse` changes the stacking order.
