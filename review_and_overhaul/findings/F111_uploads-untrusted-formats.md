---
id: F111
title: Uploads accept R serialisation files, and uploading cannot be switched off
severity: critical
type: security
area: shiny
status: open
verification: verified
effort: S
api_change: possible
locations: [R/shiny.R:206, R/shiny.R:316, R/shiny.R:611, R/shiny.R:719]
related: [F006, F115, F120]
origin: R07 sheet S02
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F111 Uploads accept R serialisation files, and uploading cannot be switched off

## Summary

The upload tab offers `.rds`, `.rda` and `.rdata` and reads them with `rio::import()` without the `trust` argument. Deserialising an R object from an unknown source can run code (rio itself says so). `upload_tab = FALSE` only hides the tab with CSS: the "Upload data set..." item in the data set selector still reveals it, and the server accepts a file and the import button in any case, so an administrator has no way to start the app without uploads.

## Evidence

- `R/shiny.R:206` and `:207` list `rds` and `rda`; `R/shiny.R:719` calls `rio::import()` without `trust`. rio 1.2.4 warns "Missing `trust` will be set to FALSE by default for RDS in 2.0.0"; with `trust = FALSE` it refuses: "RDS files may execute arbitary code. Only load RDS files that you personally generated or can trust the origin." From rio 2.0.0 these formats will stop working in the app altogether.
- `R/shiny.R:316` renders the upload tab whatever `upload_tab` is; `R/shiny.R:611` to `:621` reveal it on demand; the upload observers (`:625` onwards) have no `upload_tab` condition.
- Tests S02.08 and S02.09 (parked).

## Reproduction

```r
f <- tempfile(fileext = ".rds"); saveRDS(data.frame(a = 1:2), f)
app <- create_interactively(upload_tab = FALSE)
shiny::testServer(app, {
  session$setInputs(dataset = "iris", type = "auto", y_calc = "None")
  session$setInputs(upload_file = list(name = basename(f), datapath = f, size = 1, type = ""), upload_format = "rds")
  session$setInputs(upload_import_btn = 1)
  print(dim(imported_data()))
})
# Expected after fix: NULL; no upload with upload_tab = FALSE, and no R serialisation formats unless explicitly allowed
```

## Impact

On a server, any visitor can hand the R process an object to deserialise. Also a forward-compatibility break with rio 2.0.0.

## Proposed fix

Remove `rds`/`rda` from the default formats (or allow them only through an explicit argument, passing `trust = TRUE` knowingly). Make `upload_tab = FALSE` mean no upload at all: no selector item, no tab, and server observers that return early.

## History

- 2026-11-08: filed in R07 (verified with `shiny::testServer()`).
- 2026-11-08: severity set to critical by the maintainer (all security findings are critical).
