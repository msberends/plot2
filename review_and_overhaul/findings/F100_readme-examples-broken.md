---
id: F100
title: README examples that are not evaluated are broken or misleading
severity: medium
type: docs
area: housekeeping
status: open
verification: verified
effort: S
api_change: no
locations: [README.Rmd:136, README.Rmd:165, README.Rmd:61]
related: [F101]
origin: R05 sheet A10
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F100 README examples

## Summary

12 of the 19 code chunks in `README.Rmd` have `eval = FALSE`, so nothing checks them. Run one by one:

- The dumbbell example (`mpg`, `category = drv`) stops: "Dumbbell plots can only be used if the category (drv) is set with two distinct values - it now has three."
- The `colour_fill = c(setosa = ..., versicolor = ..., virginica = ...)` example has no category, so the names cannot match; ggplot2 warns "No shared levels found between `names(values)` of the manual scale and the data's fill values" and the bars are not coloured per species, contrary to the text above it.
- "Both produce the same chart" for the opening comparison: axis titles (`class`/`n()` versus "Vehicle class"/"Count"), theme and expansion differ.

- "Install from CRAN or r-universe" with `install.packages("plot2", repos = c("https://cran.r-project.org", ...))`: the package is not on CRAN yet (maintainer, 2026-11-08), so the text is wrong until the first release; the command works only through r-universe.

The other chunks run without error or warning.

## Reproduction

```r
library(ggplot2)
print(tryCatch(plot2(mpg, x = manufacturer, y = mean(hwy), category = drv, type = "dumbbell"), error = conditionMessage))
p <- plot2(iris, x = Species, y = mean(Sepal.Length), colour = "black",
           colour_fill = c(setosa = "#3F681C", versicolor = "#375E97", virginica = "#FFBB00"))
print(tryCatch({ ggplot_build(p); "no warning" }, warning = conditionMessage))
# Expected after fix: both examples run without error or warning and show what the text says.
```

## Impact

The README is the first thing users see on GitHub, CRAN and r-universe; a failing example costs trust.

## Proposed fix

Evaluate all README chunks (render with `eval = TRUE`, or keep the test A10.02 that runs them); filter `mpg` to two `drv` values or pick another example; use `category = Species` in the colour example, or show `colour_fill` per x value as supported behaviour.

## History

- 2026-11-08: filed in R05; test A10.02 parked.
- 2026-11-08: added the CRAN installation claim (the package is not on CRAN).
