# 10 validate_size()

`R/validate.R:1890-1903`

## Role

Default `size`: 5 for dumbbell, 4 for UpSet, 2 for point, jitter and beeswarm, 0.75 otherwise.

## Called from

`R/plot2.R:1105`, `R/plot2.R:1420`, `R/add_type.R:434`.

## Contract

Pure function. No validation of user values (negative or non-numeric sizes pass through).

## Findings

None specific. By reading: in line and area plots, `size` is converted to `linewidth` in `plot2.R:1340`; the default 0.75 for "other" geoms is also used as boxplot outlier size basis (`* 3`).

## End-to-end test cases

- [x] 10.01 Point default size 2 in layer data.
- [x] 10.02 Dumbbell default 5; UpSet dots 4.
- [x] 10.03 `size = 4` on points honoured.
- [x] 10.04 `size` on a line plot: warning and used as linewidth.
- [x] 10.05 Negative or character `size`: clear error (desired).
