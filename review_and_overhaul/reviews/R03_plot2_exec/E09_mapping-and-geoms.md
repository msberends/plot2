# E09 Mapping and geom generation

`R/plot2.R:1628-1833`

## Role

Builds the aesthetic mapping on the anonymous columns (x, y, group, category types), the ggplot object, boxplot whiskers, the main geom (`generate_geom()`), Sankey layers, the secondary-axis layer, the spider coordinate system, and smoothing (density for histograms, `add_smooth()` otherwise).

## Findings

F068 (secondary type overlaps), F005, F078 (new: `fatten` deprecated in ggplot2 4.0.0).

## End-to-end test cases

- [x] E09.01 Mapping without category: x, y, group.
- [x] E09.02 Mapping with category: colour, fill and group map the category.
- [x] E09.03 `category.type = "shape"` maps shape, not colour.
- [x] E09.04 Boxplot: an errorbar (whisker) layer is drawn before the boxplot, in the set colour.
- [x] E09.04b (skipped: F078) Boxplots raise no deprecation warning.
- [x] E09.05 Secondary y: an extra layer for the secondary series.
- [x] E09.06 (skipped: F068) Secondary series drawn as a line by default for column plots.
- [x] E09.07 Spider: extra arguments for `coord_spider()` are passed on, with a message.
- [x] E09.08 Spider: facets and fewer than three x values error.
- [x] E09.09 `smooth = TRUE`: smooth layer drawn before (below) the points; `smooth.colour` honoured.
- [x] E09.10 Histogram: density curve added by default, scaled to counts.
