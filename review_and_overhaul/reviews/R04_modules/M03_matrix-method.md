# M03 plot2.matrix()

`R/plot2-methods.R:1811-2167`

## Role

A correlation matrix (identical row and column names) becomes a tile plot in long format with the coefficients as category and datalabels. Other matrices get their row names as x.

## Findings

F089 (new: plain matrix uses only the first column, titles lost).

## End-to-end test cases

- [x] M03.01 Correlation matrix: tiles plus datalabels, one tile per cell, message.
- [x] M03.02 Correlation matrix: the diagonal has value 1.
- [x] M03.03 Plain matrix with row names: row names on the x-axis.
- [x] M03.04 (skipped: F089) Plain matrix: all columns plotted and axis titles present.
