---
id: F013
title: Numeric x of 2000-2050 or exactly 1:12 silently converted to character, also for scatter plots
severity: high
type: design
area: data-validation
status: verified
verification: verified
effort: S
api_change: possible
locations: [R/validate.R:375, R/validate.R:409]
related: [F014]
origin: R01 §2.6
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F013 Numeric x silently made discrete

## Summary

`validate_data()` assumes numeric x (and category) values between 2000 and 2050 are years and values exactly 1 to 12 are months, and converts them to character, regardless of the geom. A scatter plot of weights in grams, or of a score from 1 to 12, becomes a discrete axis.

## Evidence

`R/validate.R:375-402` (x), `R/validate.R:409-439` (category).

## Reproduction

```r
p <- plot2(data.frame(w = c(2001, 2010, 2049), v = 1:3), w, v, type = "point")
print(class(p$data$w))
p <- plot2(data.frame(m = 1:12, v = rnorm(12)), m, v, type = "point")
print(class(p$data$m))
# Expected after fix: numeric for continuous geoms.
```

## Impact

Distorted axes (equal spacing, no scale) in scientific figures; the only signal is a message that is hidden in non-interactive output (F014).

## Proposed fix

Apply the heuristics only for discrete geoms and integer-valued data, or only when `x.character` is not set and the type is chosen automatically. Consider dropping the year heuristic entirely.

**Decision needed:** how much guessing plot2 should do.

## History

- 2026-11-07: filed in R01, reproduced.
- 2026-11-08: maintainer decided (AskUserQuestion): keep the year (2000-2050) and month (1:12) heuristics only for discrete types and integer values; continuous geoms keep numeric x; the conversion is reported in a message.
- 2026-11-08: fixed as decided (with the follow-up answer: the guess also applies when type is not set): looks_like_years() and looks_like_months() require whole numbers; for x the guess is skipped for continuous-x types (point, line, area, ribbon, path, step, smooth, histogram, density, polygon, rect, sf); the messages say how to prevent it; x.character and category.character documented. Category keeps the guess regardless of type (whole numbers only). Tests: 03.14 unparked, test-regression-phase4.R (F013).
