# M10 plot2.freq(), plot2.data.frame() wrapper, re-exports and data

`R/plot2-methods.R:765-1099`, `R/plot2-methods.R:1471-1810`, `R/reexports.R`, `R/data.R`, `R/aa_pkg.R`

## Role

`plot2.freq()` plots the first two columns of a `cleaner::freq()` table (item, count) in the table's order. `plot2.data.frame()` ungroups and forwards. Re-exports: `%>%`, `n`, `n_distinct`, `first`, `last`, `all_of`, `any_of` (dplyr) and `everything`, `starts_with`, `ends_with`, `matches`, `where` (tidyselect). Data: `admitted_patients` (250 rows, 7 columns), `netherlands` (sf, EPSG:4326).

## Findings

None new. Note for F084: the bundled `netherlands` data are in EPSG:4326.

## End-to-end test cases

- [x] M10.01 `freq` object: columns in frequency order, titles "Item" and "Count".
- [x] M10.02 Grouped data frames are ungrouped.
- [x] M10.03 All re-exports are exported and identical to their source.
- [x] M10.04 Bundled data have the documented structure.
