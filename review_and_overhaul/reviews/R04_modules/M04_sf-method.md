# M04 plot2.sf()

`R/plot2-methods.R:1100-1470`

## Role

Maps of simple-features data: converts non-sf input with `st_as_sf()`, optionally transforms the CRS, warns that x and y are ignored, adds dummy x and y and calls `plot2_exec()` with `type = "sf"`.

## Findings

F080 (new: x or y given crashes), F008 (sf in Suggests), F014.

All cases skip without sf.

## End-to-end test cases

- [x] M04.01 `plot2(netherlands)`: sf layer plus labels.
- [x] M04.02 `category` maps the fill (continuous for a numeric column).
- [x] M04.03 `datalabels = FALSE` removes the labels.
- [x] M04.04 `crs` transforms the data.
- [x] M04.05 `colour` without `colour_fill`: caution that colour sets the borders.
- [x] M04.06 (skipped: F080) `x = column` gives a caution, not an error.
