# E12 Facets, datalabels and coordinates

`R/plot2.R:2143-2204`

## Role

Adds facets (`validate_facet()`), datalabels (`generate_datalabels()`), and the coordinate system: `coord_flip()` for horizontal plots, or `coord_cartesian(clip = "off")` when datalabels could fall outside the panel and a title or facets are present.

## Findings

F052 (facet scales), F055.

## End-to-end test cases

- [x] E12.01 Facets: one panel per facet value; facet variable keeps its original name.
- [x] E12.02 Columns get two datalabel layers (label and text) with the values.
- [x] E12.03 Horizontal plots use `coord_flip()`; clipping off when a title is present.
- [x] E12.04 Vertical plot with datalabels and a title: `coord_cartesian()` with clipping off.
- [x] E12.05 Vertical plot without title or facets: default coordinates.
- [x] E12.06 `type = "blank"`: no datalabels.
