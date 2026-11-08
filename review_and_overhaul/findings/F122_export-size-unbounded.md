---
id: F122
title: Export width, height and DPI are not checked on the server; the app's own maxima ask for a 47,244 x 47,244 pixel image
severity: critical
type: security
area: shiny
status: verified
verification: verified
effort: S
api_change: no
locations: [R/shiny.R:566, R/shiny.R:572, R/shiny.R:1058]
related: [F112]
origin: R07 sheet S05
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F122 Export size unbounded

## Summary

The export inputs are `numericInput()`s with `min`/`max` in the browser only; the server passes the values straight to `ggsave()`. Even within the app's own maxima (100 x 100 cm at 1,200 DPI) a PNG or JPG is 47,244 x 47,244 pixels, several gigabytes of memory for one request. Values beyond the maxima can be sent by any client. ragg refuses dimensions above 50,000 pixels, which is the only limit today.

## Evidence

- `R/shiny.R:566`, `:572` (inputs), `:1058` to `:1076` (exports without checks). Probe: `ggsave(..., width = 100, height = 100, units = "cm", dpi = 1e5)` stops at ragg's limit; at 1,200 DPI it is accepted (not executed in this review because of its memory use).
- Test S05.04 (parked; it must not be unparked without the fix).

## Reproduction

```r
# Not executed: would allocate several GB.
# app <- create_interactively(); in a session set export_width_cm = 100,
# export_height_cm = 100, export_dpi = 1200 and request output$export_png.
# Expected after fix: refused with a notification above a pixel budget (for example 40 megapixels)
```

## Impact

One visitor can exhaust the memory of the R process (and of other sessions in it).

## Proposed fix

Validate on the server with `shiny::validate()`/`req()`: positive finite numbers, width and height within the stated range, and a total pixel budget for raster formats.

## History

- 2026-11-08: filed in R07 (by reading, with a probe of ragg's limit).
- 2026-11-08: severity set to critical by the maintainer (all security findings are critical).
- 2026-11-08: fixed: shiny_export_size() checks width and height (1 to 100 cm) and DPI (1 to 1200) on the server, and raster exports against a budget of 40 megapixels; refusals give a notification, and the size preview states the problem. Test S05.04 unparked and extended.
