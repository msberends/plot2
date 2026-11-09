# E13 Additional layers for linedot, dumbbell and spider

`R/plot2.R:2206-2249`

## Role

linedot: two point layers (outer in the background colour, inner in the series colour). Dumbbell: grey segments from minimum to maximum per x (and facet), moved below the points; requires exactly two categories. Spider: axis titles removed through the theme (`axis.title = element_blank()`), not through the labels.

## Findings

None new.

## End-to-end test cases

- [x] E13.01 linedot: outer points in the background colour, five times the size; inner points twice the size.
- [x] E13.02 linedot: custom `background` colour used for the outer points.
- [x] E13.03 Dumbbell: one grey segment per x from minimum to maximum, drawn below the points.
- [x] E13.04 Dumbbell with facets: segments per facet.
- [x] E13.05 Dumbbell with three categories errors and names the count.
- [x] E13.06 Spider: no axis titles.
