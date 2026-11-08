---
id: F101
title: Main vignette text contradicts its own code in several places
severity: low
type: docs
area: housekeeping
status: open
verification: verified
effort: S
api_change: no
locations: [vignettes/plot2.Rmd:201, vignettes/plot2.Rmd:213, vignettes/plot2.Rmd:261, vignettes/plot2.Rmd:331, vignettes/plot2.Rmd:559, vignettes/plot2.Rmd:333]
related: [F100, F011, F013]
origin: R05 sheet A08
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F101 Vignette prose versus code

## Summary

All chunks of `vignettes/plot2.Rmd` run, but the text describes something else in these places:

- Line 201 to 209: "count the number of patients", then `n_distinct(patient_id)`, then "number of admissions per hospital" (admissions are rows, `n()`).
- Line 213: "the number of unique age groups within each hospital"; the code categorises by period (before or since 2010).
- Line 261: "X-axis to reflect the hospital name and use facets for the year of admission"; the code has gender on x, year as category and hospital as facet.
- Line 331: `x.max_items = 5` "limits the plot to the top 5 most frequent"; without `x.sort` it keeps the first values (1, 2, 3, 4 and a rest group). Only the next example, with `x.sort = "freq-desc"`, does what is described.
- Line 559: "the required DPI will be set for you, even in R Markdown outputs"; plot2 stops in R Markdown unless the chunk sets `fig.showtext = TRUE` (as the next chunk does).
- Line 333: recommends `top_n()`, superseded in dplyr since 1.0.0 (`slice_max()`).
- The carburettor examples (`carb`, values 1 to 8) depend on the undocumented conversion of 1 to 12 to character (F013).

## Reproduction

```r
p <- plot2(mtcars, carb, y = n(), x.max_items = 5, type = "col")
print(ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$get_labels())
print(names(sort(table(mtcars$carb), decreasing = TRUE))[1:4])
# Expected after fix: the text describes what the code shows.
```

## Impact

Readers learn wrong semantics (`x.max_items`, `n_distinct`).

## Proposed fix

Edit the prose; consider showing `x.max_items` only together with `x.sort`.

## History

- 2026-11-08: filed in R05.
