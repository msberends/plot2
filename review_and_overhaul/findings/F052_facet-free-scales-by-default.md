---
id: F052
title: Facets use free x and y scales by default, fixed only when a 25% heuristic says so
severity: medium
type: design
area: scales
status: verified
verification: verified
effort: M
api_change: possible
locations: [R/validate.R:2262, R/validate.R:782, R/validate.R:1016]
related: [F013]
origin: R02 sheets 05, 06, 16
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F052 Facet scales free by default

## Summary

With facets, `facet.repeat_lbls_x = TRUE` and `facet.repeat_lbls_y = NULL` lead to `scales = "free"`. y is fixed only when the coefficient of variation of the facet maxima is below 25% (likewise for x). Whether panels are comparable therefore depends on the data, and changes silently when the data change (for example next month's surveillance data).

## Reproduction

```r
p <- plot2(admitted_patients, hospital, n(), facet = ward)
print(p$facet$params$free)
# Currently: x TRUE, y TRUE (data-dependent).
# Expected after decision: a documented, data-independent default.
```

## Impact

Readers compare bar heights across panels; free y scales by default invite misreading, which matters for epidemiological reporting.

## Proposed fix

Make fixed y the default (ggplot2's default) and keep free scales opt-in, or keep the heuristic but report it as a visible message. Rename `facet.repeat_lbls_x/y` or document that they control scale freedom.

**Decision needed:** default scale behaviour for facets.

## History

- 2026-11-07: filed in R02, reproduced.
- 2026-11-08: maintainer decided (AskUserQuestion): keep the 25% heuristic, but report the chosen facet scales in a message and document it.
- 2026-11-08: fixed as decided: the 25% heuristic is kept; when it leaves the scales free, a message now says so and names facet.fixed_y (or facet.fixed_x) = TRUE; the fixed outcome already had a message. facet.fixed_y, facet.fixed_x and facet.repeat_lbls_x/y document the rule and its data dependence. Tests: test-regression-phase4.R (F052).
