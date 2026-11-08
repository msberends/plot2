---
id: F061
title: validate_colour() contains a self-declared hack and dead code for repeated groups
severity: low
type: maintainability
area: colours
status: open
verification: reading
effort: M
api_change: no
locations: [R/validate.R:1853, R/validate.R:1877]
related: [F047]
origin: R02 sheet 09
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F061 validate_colour() hack and dead code

## Summary

The block marked `# TODO this very hacky... since ggplot2 3.4.0 manual values in scale_*_manual work differently` extends colour vectors based on group sizes. `colour_fill <- colour_fill[grp_sizes != 0]` is a no-op because zero sizes were removed two lines earlier. The logic decides colour counts from the number of x-category-facet groups, which is unrelated to the number of categories.

## Reproduction

```r
# Structural finding; behaviour covered by sheet 09 test cases 09.04 to 09.06.
```

## Proposed fix

Always produce exactly one colour per category level, named by level, and pass `values = named vector` to the manual scale; remove the expansion block.

## History

- 2026-11-07: filed in R02 (by reading).
- 2026-11-08: attempted fix reverted (c329cb2, reverted in 4ff67bf). The block is not dead: when colours reach the geom as fixed parameters instead of through a scale (e.g. a boxplot with a category and named `colour` plus a single `colour_fill`, or a registered colour set with `y = where(is.double)`), ggplot2 needs one value per drawn group, and the block supplies that by repeating the colours up to the number of x-category-facet groups. A comparison of 16 plots that only used scales showed no difference; the API tests A06.01 and A08.01 caught it. A proper fix maps these colours through the scale instead of geom parameters, so that one colour per category level suffices.
- 2026-11-08: partial cleanup: the no-op `colour_fill <- colour_fill[grp_sizes != 0]` is removed and the TODO comment is replaced by an explanation of why the block is needed. Left open for the structural fix (colours of these layers through the scale), which touches the layer construction in plot2_exec() and is larger than the finding's severity justifies now.
