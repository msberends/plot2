# E08 Back-to-back plots

`R/plot2.R:1473-1626` and `R/back2back_plot.R`

## Role

Splits the data by the two facet values and builds two horizontal column plots via nested `plot2()` calls (left with a reversed scale), combined with patchwork, with shared titles and a collected legend.

## Findings

F064 (independent scales), F065 (y passed as values, crashes for small counts), F066 (colours differ between halves), F005 (`theme()` with a function), F021 (facet expressions), F001 (state cleared only at the end; nested calls).

## End-to-end test cases

- [x] E08.01 Patchwork of two halves; the left half has a reversed value axis.
- [x] E08.02 Title and subtitle are on the combined plot.
- [x] E08.03 (skipped: F064) Both halves share the same value range.
- [x] E08.04 (skipped: F065) Small counts (1, 2, 3, 4) with a category.
- [x] E08.05 (skipped: F066) Category colours equal on both sides.
- [x] E08.06 (skipped: F005) `theme = ggplot2::theme_bw`.
- [x] E08.07 (skipped: F021) Facet given as an expression.
- [x] E08.08 Facet with three values errors.
- [x] E08.09 No internal state left behind after a successful call.
