# E14 Restoring the mapping and returning the plot

`R/plot2.R:2251-2270`, `restore_mapping()` and `setup_aes()` in `R/utils.R`

## Role

Replaces the anonymous `_var_*` names in all mappings and the facet by the original names, removes the anonymous columns, converts plain data frames to tibbles, prints or returns.

## Findings

F069 (internal columns remain), F001 (state).

## End-to-end test cases

- [x] E14.01 No mapping refers to `_var_*` direction names (the datalabel layers still map `_var_datalabels`, F069).
- [x] E14.02 `p$data` has no `_var_x`, `_var_y`, `_var_category`, `_var_facet`.
- [x] E14.03 (skipped: F069) No internal columns at all in `p$data`, and no internal names in any mapping.
- [x] E14.04 `p$data` is a tibble.
- [x] E14.05 Users can add layers that refer to the original columns.
- [x] E14.06 Facet parameters refer to the original facet name.
- [x] E14.07 `print = TRUE` returns the plot invisibly.
- [x] E14.08 Two identical calls give identical layer data (no state carried over).
- [x] E14.09 Several types leave no internal state behind.
