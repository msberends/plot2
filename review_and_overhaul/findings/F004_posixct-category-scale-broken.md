---
id: F004
title: Date-time (POSIXct) category produces thousands of nonsense legend breaks
severity: critical
type: bug
area: scales
status: verified
verification: verified
effort: S
api_change: no
locations: [R/validate.R:1353, R/validate.R:1384]
related: [F003]
origin: R01 §1.4
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F004 POSIXct category scale broken

## Summary

`validate_category_scale()` treats any date-like category as `Date`: breaks are generated with `seq.Date()` over values that are seconds since the epoch, and labels with `as.Date(as.numeric(x))`.

## Evidence

- `R/validate.R:1384`: `seq.Date(from = min(values), to = max(values), by = breaks)`.
- `R/validate.R:1353`: `format2(as.Date(as.numeric(x), origin = "1970-01-01"), ...)`.

## Reproduction

```r
d <- data.frame(x = 1:3, y = 1:3,
                c = as.POSIXct(c("2024-01-01", "2024-02-01", "2024-03-01"), tz = "UTC"))
p <- plot2(d, x, y, c, type = "point")
b <- ggplot2::ggplot_build(p)
lbls <- b$plot$scales$get_scales("colour")$get_labels()
cat("number of legend labels:", length(lbls), "\n")
# Expected after fix: a handful of sensible date-time labels.
```

## Impact

Any plot coloured by a date-time variable has an unusable legend.

## Proposed fix

Branch on `inherits(values, "POSIXt")`: use `seq.POSIXt()` for breaks and `as.POSIXct(x, origin = "1970-01-01", tz = ...)` for labels, keeping the time zone of the data.

## History

- 2026-11-07: filed in R01, reproduced.
- 2026-11-08: R06 (sheet T02): also for `Date` categories. A category with two dates (2024-01-01 and 2024-02-01) gives a legend with 32 daily entries ("1 jan" to "1 feb"); the date labels are Dutch in every locale (F003).
- 2026-11-08: fixed and verified (Phase 1). POSIXct categories use `seq.POSIXt()` breaks and date-time labels in the data's time zone; up to three distinct dates get one legend entry each; regression tests 07.07 and T02.03.
