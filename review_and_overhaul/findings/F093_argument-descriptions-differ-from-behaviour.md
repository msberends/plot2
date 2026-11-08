---
id: F093
title: Several argument descriptions state defaults or effects that differ from the behaviour
severity: medium
type: docs
area: architecture
status: open
verification: verified
effort: S
api_change: no
locations: [R/plot2.R:71, R/plot2.R:109, R/plot2.R:114, R/plot2.R:126, R/plot2.R:139, R/plot2.R:173, R/options.R:38, R/validate.R:758, R/validate.R:1086]
related: [F034, F092, F094, F050]
origin: R05 sheets A02 to A05
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F093 Argument descriptions that differ from the behaviour

## Summary

Checked one by one against the code (sheets A02 to A05), these descriptions in `?plot2` and `?plot2-options` are wrong or incomplete in a way that changes what users get:

1. `x.expand`: "defaults to 0.5". Internally the value 0.5 is a sentinel that becomes 2.5% below and 20% above for a scatter plot; passing `x.expand = 0.5` explicitly gives 50% on both sides (x range -3.95 to 11.85 instead of -0.2 to 9.48 for `iris`).
2. `x.sort`: "defaults to `TRUE`"; the usage shows `x.sort = NULL`, and `NULL` is documented in the same list as "do not sort/transform at all". `category.sort` and `facet.sort` do default to `TRUE`.
3. `legend.position`: lists `NULL` next to `NA` as a way to remove the legend, but `NULL` is the default and means automatic (`"top"` or `"right"`).
4. `datalabels`: documents the default as "Left blank" with a 25-label limit, but the usage shows `datalabels = TRUE`, and passing `TRUE` removes the limit (see F094).
5. `plot2.default_type`: `?plot2` says the option "defaults to `"col"`"; the code default is `"geom_col"`, and short values do not work (F095). `?plot2-options` gives no defaults for any option.
6. `y.percent_break`: "A value on which the Y-axis should have breaks". An explicit value is silently replaced when it would give three or fewer breaks (`R/validate.R:1086`, only an info message).
7. `type` left blank: "the length of unique values per X-axis item is at least 3" is right, but the case "no X-axis" crashes when the data has only the y column (F079).

## Reproduction

```r
rx <- function(...) ggplot2::ggplot_build(plot2(iris, Sepal.Length, Sepal.Width, ...))$layout$panel_params[[1]]$x.range
print(rx()); print(rx(x.expand = 0.5))            # differ although 0.5 is documented as the default
print(formals(plot2)$x.sort)                        # NULL, documented default TRUE
p <- plot2(admitted_patients, hospital, n(), gender, legend.position = NULL)
print(ggplot2::ggplot_build(p)$plot$theme$legend.position)  # "top", documented as a way to hide it
d <- data.frame(x = c("a", "b"), y = c(0.2, 0.55))
b <- ggplot2::ggplot_build(plot2(d, x, y, y.percent = TRUE, y.percent_break = 0.25))
print(diff(b$layout$panel_params[[1]]$y$breaks)[1]) # 0.1, not the requested 0.25
# Expected after fix: documentation and behaviour agree for all seven points.
```

## Impact

Users who copy a documented default get a different plot (1, 4), or cannot find out what the default does (2, 3, 5). Point 6 overrides an explicit request.

## Proposed fix

Correct the texts; for 1 and 4 the cleaner fix is to stop using visible values as sentinels (F094). Document option defaults in `?plot2-options`. For 6, either honour the explicit value or document the rule.

## History

- 2026-11-08: filed in R05; tests A03.05 and A03.08b parked; A04.03 records the actual `legend.position = NULL` behaviour.
- 2026-11-08: points 3 (legend.position) and 5 (option defaults) corrected; point 7 fixed with F079. Points 1, 2, 4 and 6 describe defaults that depend on missing() and wait for F094.
