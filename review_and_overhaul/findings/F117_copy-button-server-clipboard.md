---
id: F117
title: The copy button writes to the clipboard of the R process, not of the user
severity: medium
type: bug
area: shiny
status: verified
verification: verified
effort: S
api_change: no
locations: [R/shiny.R:1026, R/shiny.R:56]
related: [F115, F108]
origin: R07 sheet S05
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F117 Copy button uses the server's clipboard

## Summary

`clipr::write_clip()` runs in the R process. That is the user's own clipboard only when R runs on the user's own desktop. On a server it fails (clipr refuses in non-interactive sessions and needs xclip/xsel or a display), and the unhandled error ends the session (F115). It is also the only reason clipr is a requirement of the app (`R/shiny.R:56`).

## Evidence

- On this headless server: `clipr::clipr_available()` is `FALSE`; `write_clip()` errors "To run write_clip() in non-interactive mode, either call write_clip() with allow_non_interactive = TRUE, or set the environment variable CLIPR_ALLOW=TRUE".
- Test S05.05 (parked): the session is closed after pressing the button.

## Reproduction

```r
app <- create_interactively()
shiny::testServer(app, {
  session$setInputs(dataset = "iris", type = "auto", y_calc = "None")
  try(session$setInputs(copy_btn = 1))
  print(session$isClosed())
})
# Expected after fix: FALSE; copying happens in the browser
```

## Impact

The button fails wherever the browser and R are not on the same desktop session, including RStudio Server and Posit Workbench.

## Proposed fix

Copy in the browser with `navigator.clipboard.writeText()` (a few lines of JavaScript, e.g. through `shinyjs::runjs()` or an `onclick` on the button reading the text of `#code`), and drop clipr from the app's requirements.

## History

- 2026-11-08: filed in R07 (verified).
- 2026-11-08: fixed: the copy button copies in the browser (navigator.clipboard, with a fallback for non-secure origins) and reports the result to the server for a notification; clipr (and the no longer used fansi) removed from the app and from Suggests. Tests S05.05 (unparked), S05.06; verified in headless Chromium (clipboard holds the code).
