# 11 validate_width()

`R/validate.R:1905-1914`

## Role

Default `width`: 0.66 for boxplot, violin, jitter, beeswarm; 0.5 otherwise.

## Called from

`R/plot2.R:1106`, `R/plot2.R:1421`.

## Contract

Pure function; no validation of user input.

## Findings

None specific.

## End-to-end test cases

- [x] 11.01 Column width 0.5 (xmax - xmin in layer data).
- [x] 11.02 Boxplot width 0.66.
- [x] 11.03 `width = 0.9` honoured; whisker width follows.
- [x] 11.04 Datalabels dodge width equals the bar width with categories.
