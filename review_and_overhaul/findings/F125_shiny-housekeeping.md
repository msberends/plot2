---
id: F125
title: Shiny app housekeeping: dead code, discarded checks, no statement on deployment
severity: low
type: maintainability
area: shiny
status: open
verification: reading
effort: S
api_change: no
locations: [R/shiny.R:46, R/shiny.R:63, R/shiny.R:844, R/shiny.R:1101, R/shiny.R:1109]
related: [F006, F113]
origin: R07 sheet S01
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F125 Shiny app housekeeping

## Summary

- `R/shiny.R:46`: the logo `tryCatch` computes a validity check and discards it; only existence is checked afterwards, and the error message names file types that are not checked.
- `R/shiny.R:63` to `:71`: the RStudio viewer and Windows `wmic` code is dead (`rstudio_viewer <- FALSE`); `wmic` is also removed from current Windows versions.
- `R/shiny.R:844`: CSS colour `'transparant'` (invalid; "transparent").
- `R/shiny.R:1101`: `data_visible` is never used; the comment above the Legend tab says "Datalabels".
- `R/shiny.R:1109` to `:1123`: in an interactive session the function runs the app and returns nothing, outside one it returns the app; there is no documented way to get the app object for deployment, and the documentation says nothing on whether server deployment is supported, even though `install_shiny_deps()` addresses administrators. `@return` is missing.
- The roxygen text of `upload_tab` mentions a modal dialogue that no longer exists (also F120).

## Evidence

By reading.

## Reproduction

```r
# By reading; see the line references.
```

## Impact

Maintenance burden and unclear support for deployment.

## Proposed fix

Remove dead code, fix the typos, add `@return`, and decide (and document) whether deployment is supported; if so, add a function or argument that returns the app object.

## History

- 2026-11-08: filed in R07 (by reading).
