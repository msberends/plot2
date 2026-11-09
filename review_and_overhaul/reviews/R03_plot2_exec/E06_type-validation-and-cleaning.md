# E06 Taxonomy, type validation and cleaning steps

`R/plot2.R:1326-1417`

## Role

Applies taxonomic italics, validates the type (automatic choice), summarises groups for discrete types, applies `zoom`, decides markdown, removes datalabels for continuous types, ignores y for density and histogram, removes x for sf, and checks the requirements of Sankey and back-to-back plots; resolves `stacked` versus `stacked_fill`.

## Findings

F071 (requirement checks too late), F013 (heuristics), F014 (diagnostics), F076 (new: with an explicit continuous type, `validate_data()` drops the datalabels even when asked for; the correct rule at `R/plot2.R:1379` never sees them).

## End-to-end test cases

- [x] E06.01 Discrete type with duplicate x: values summed.
- [x] E06.02 barpercent: summarising message names the function.
- [x] E06.03 `zoom = TRUE` on points: the y range follows the data, not 0.
- [x] E06.04 Point plots have no datalabels by default; `datalabels = TRUE` adds them (automatic type).
- [x] E06.04b (skipped: F076) `datalabels = TRUE` with `type = "point"`, and a column with `type = "line"`.
- [x] E06.05 Density ignores y with a message.
- [x] E06.06 Sankey without category errors with the friendly message (skip without ggforce).
- [x] E06.07 Back-to-back with a facet of three values errors.
- [x] E06.08 `stacked` and `stacked_fill` both TRUE: warning, fill wins.
- [x] E06.09 `size` on a line plot becomes `linewidth`, with a warning.
