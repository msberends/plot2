---
id: F113
title: The whole folder of logo_path is served over HTTP
severity: critical
type: security
area: shiny
status: open
verification: verified
effort: S
api_change: no
locations: [R/shiny.R:75]
related: [F006]
origin: R07 sheet S01
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F113 The whole folder of the logo is served

## Summary

`shiny::addResourcePath("plot2res", dirname(logo_path))` publishes the folder that contains the logo, not the logo. By default that is the root of the installed plot2 package. With a custom logo in, say, a home or project folder, every file in that folder is downloadable by anyone who can reach the app, at `/plot2res/<file name>`.

## Evidence

- `R/shiny.R:75`.
- Probe and test S01.04 (parked): with a logo next to `secret.txt`, `shiny::resourcePaths()[["plot2res"]]` is that folder, so `secret.txt` is served.

## Reproduction

```r
dir <- tempfile("logo"); dir.create(dir)
file.copy(system.file("logo.svg", package = "plot2"), file.path(dir, "logo.svg"))
writeLines("secret", file.path(dir, "secret.txt"))
app <- create_interactively(logo_path = file.path(dir, "logo.svg"))
print(file.exists(file.path(shiny::resourcePaths()[["plot2res"]], "secret.txt")))
# Expected after fix: FALSE; only the logo is reachable
```

## Impact

Disclosure of arbitrary files next to the logo when the app is deployed.

## Proposed fix

Embed the logo as a data URI (`base64enc`/`xfun::base64_uri()`, or read the SVG inline), or copy it alone into a fresh temporary folder and serve that. Also validate the extension properly: the `tryCatch` at `R/shiny.R:46` computes the check and discards its result.

## History

- 2026-11-08: filed in R07 (verified).
- 2026-11-08: severity set to critical by the maintainer (all security findings are critical).
