# 23 summarise_data()

`R/validate.R:2873-2914`

## Role

Summarises y per combination of x, category and facet with `summarise_function` (default `sum`) and recomputes datalabels.

## Called from

`R/plot2.R:1353` (discrete geoms with group sizes > 1), `set_max_items()`.

## Contract

- In: `df`, `summarise_function`, formatting arguments.
- Out: summarised `df` with `_var_*` columns restored.

## Findings

F049 (a temporary column named `n` overwrites a user column `n`; crash), F053 (non-scalar or NA results give dplyr errors). By reading: NA results are dropped silently, removing the group.

## End-to-end test cases

- [x] 23.01 Duplicate x values summed by default.
- [x] 23.02 `summarise_function = mean`, `median`, `max`.
- [ ] 23.03 x, category or facet column named `n`, `y` or `count` (F049). (test written, parts parked: F049)
- [ ] 23.04 `summarise_function = range`: clear error that a single value is required (F053). (test written, parts parked: F053)
- [x] 23.05 Datalabels show the summarised values.
- [x] 23.06 Facets and categories kept after summarising.
