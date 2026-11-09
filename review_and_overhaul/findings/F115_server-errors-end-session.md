---
id: F115
title: Errors in the app's server code end the session instead of showing a message
severity: medium
type: bug
area: shiny
status: verified
verification: verified
effort: S
api_change: no
locations: [R/shiny.R:611, R/shiny.R:681, R/shiny.R:811, R/shiny.R:1026]
related: [F114, F116, F117]
origin: R07 sheet S06
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F115 Errors in server code end the session

## Summary

Only the `rio::import()` call and the plot evaluation are wrapped in `tryCatch()`. Any other error in an observer ends the Shiny session: the browser greys out and the user loses everything. Found triggers:

- a missing reader package: `rlang::check_installed()` inside `do_preview()` (`R/shiny.R:681`) errors when not interactive (and on a local session asks to install packages from inside a Shiny callback);
- a data set name that does not parse (`:811`, F114);
- the copy button on a server (`:1026`, F117);
- `input$dataset` being `NULL` while the app starts, at `:611` (`if (NULL == "import")`, "argument is of length zero"), seen in tests.

## Evidence

- Probes and tests S02.10 and S06.03 (parked): `session$isClosed()` is `TRUE` after selecting `my data`; `do_preview()` raises `rlib_error_package_not_found` for `.ods` without readODS.

## Reproduction

```r
assign("my data", data.frame(a = 1:3), envir = globalenv())
app <- create_interactively()
shiny::testServer(app, {
  session$setInputs(dataset = "my data", type = "auto", y_calc = "None")
  print(session$isClosed())
})
# Expected after fix: FALSE, with a notification about the problem
```

## Impact

Lost work and a disconnected app instead of a message.

## Proposed fix

Check optional packages when the app is built (or disable formats whose reader is missing) instead of in the callback; wrap observer bodies that touch user input in `tryCatch()` with `shiny::showNotification()`; use `isTRUE(input$dataset == "import")` or `shiny::req()`.

## History

- 2026-11-08: filed in R07 (verified with `shiny::testServer()`).
- 2026-11-08: partly fixed with F111: a missing reader package, a failing read and an unknown file type give a notification and keep the session (test S02.10 unparked; skipped here because readODS is installed).
- 2026-11-08: fixed: column selector updates and reading errors are caught and shown as notifications, plot errors in the error panel; input$dataset is checked before use; the copy button follows in F117. Test S06.03 unparked.
