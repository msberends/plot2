---
id: I003
title: Allow facets in spider plots
area: types
status: verified
effort: S
api_change: no
related: []
origin: R04 sheet M07
created: 2026-11-08
updated: 2026-11-08
---

# I003 Allow facets in spider plots

## Idea

`plot2()` stops with "Spider plots do not support facets." (`R/plot2.R:1778`), but `coord_spider()` itself works with `facet_wrap()` in plain ggplot2 (verified in R04). Lift the restriction after checking the datalabels and theme parts of spider plots with facets.

## Motivation

Comparing profiles per group (e.g. per hospital or per year) is the main use of spider plots; facets are the natural way in plot2.

## History

- 2026-11-08: filed during R04.
- 2026-11-08: implemented: the facet restriction is removed; validate_facet() forces fixed scales for spider plots (as for sf, since coord_spider() does not support free scales). Found while testing: with datalabels and a title or facets, coord_cartesian() replaced CoordSpider (also without facets); that block now skips spider plots. Tests: E09.08 rewritten.
