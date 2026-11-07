# 12 validate_linewidth()

`R/validate.R:1916-1931`

## Role

Default `linewidth`: 0.1 for sf, 0.5 for boxplot and violin, 1 for dumbbell, UpSet and spider, 0.25 for other continuous geoms with fill, 0.5 otherwise.

## Called from

`R/plot2.R:1107`, `R/plot2.R:1422`, `R/add_type.R:389`, `R/add_type.R:608`.

## Contract

Pure function; no validation.

## Findings

None specific. By reading: boxplot median thickness is derived from `linewidth` through the `fatten` argument (`R/validate.R:1666`), which ggplot2 4.x supersedes with `median.linewidth`.

## End-to-end test cases

- [ ] 12.01 Line default 0.5; area 0.25; sf 0.1.
- [ ] 12.02 Dumbbell segment and spider polygon 1.
- [ ] 12.03 `linewidth = 2` honoured.
- [ ] 12.04 Boxplot median thicker than the box outline.
