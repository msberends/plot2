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
updated: 2026-11-07
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
