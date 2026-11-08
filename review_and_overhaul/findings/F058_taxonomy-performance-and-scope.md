---
id: F058
title: x.lbl_taxonomy rebuilds the name list per call and only applies to x
severity: low
type: performance
area: markdown
status: verified
verification: verified
effort: S
api_change: possible
locations: [R/validate.R:679]
related: [F018]
origin: R02 sheet 04
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F058 Taxonomy labelling: performance and scope

## Summary

`validate_taxonomy()` builds `unique(c(family, genus, species, subspecies))` from `AMR::microorganisms` (about 78,000 rows) on every call (about 0.6 s per plot on the review machine), and splits every label on spaces. Only x labels are supported: categories (legends) and facets with taxonomic names stay upright. Generic words that are also species epithets may be italicised.

## Reproduction

```r
# skip without AMR
if (requireNamespace("AMR", quietly = TRUE)) {
  d <- data.frame(m = c("Escherichia coli", "Other group"), v = 1:2)
  print(system.time(for (i in 1:5) plot2(d, m, v, x.lbl_taxonomy = TRUE))[["elapsed"]] / 5)
  print(ggplot2::ggplot_build(plot2(d, m, v, x.lbl_taxonomy = TRUE))$layout$panel_params[[1]]$x$get_labels())
}
# Expected after fix: name list cached; only genuine taxonomic names italic; option for category and facet.
```

## Proposed fix

Cache the name set in the package environment (outside the per-call state); match only genus-led binomials (`Genus species`) and abbreviations; add `category.lbl_taxonomy` and `facet.lbl_taxonomy` or a general `lbl_taxonomy`.

## History

- 2026-11-07: filed in R02 (by reading).
- 2026-11-07: reproduced; 0.6 s per call with `x.lbl_taxonomy = TRUE`.
- 2026-11-08: fixed with D001.3: x.lbl_taxonomy is replaced by label_taxonomy(), a labeller for any direction (x.labels, category.labels); the name lists are cached per session; species and subspecies names are only italicised after a genus or an abbreviated genus, so ordinary words such as 'coli' or 'group' stay upright. Facet strips have no labels argument in plot2, so facets are not covered.
