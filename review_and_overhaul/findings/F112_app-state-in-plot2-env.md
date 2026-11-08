---
id: F112
title: The app keeps its state in plot2_env, shared by all sessions and wiped by every plot2() call
severity: critical
type: security
area: shiny
status: open
verification: verified
effort: M
api_change: no
locations: [R/shiny.R:139, R/shiny.R:951, R/shiny.R:1011, R/shiny.R:1034, R/shiny.R:1197, R/utils.R:658]
related: [F001, F006]
origin: R07 sheet S06
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F112 App state in plot2_env

## Summary

The app stores two things in the package environment `plot2_env`: the defaults of all inputs (`shiny_defaults`, written while the UI is built) and the last rendered plot (`last_shiny_plot`, used by every export button). Both are shared by all sessions of the R process, and both are deleted by `clear_plot2_env()`, which every `plot2()` call runs.

Consequences:

- After the first plot is rendered, `shiny_defaults` is empty, so "skip if unchanged" (`R/shiny.R:951`) never applies again: a setting changed and then set back to its default stays in the generated code (`x.zoom = FALSE`).
- On a server, an export button writes whichever plot was rendered last in the process, possibly another visitor's plot (a data leak), or fails when another `plot2()` call has just cleared it.
- Creating an app changes the state of plot2 for the rest of the R session.

## Evidence

- `R/shiny.R:139` (`plot2_env$shiny_defaults <- list()`), `:1197` (written per input), `:951` (read), `:1011` (`last_shiny_plot` written), `:1034`, `:1045`, `:1059`, `:1071` (read by the exports); `R/utils.R:658` (`clear_plot2_env()` removes everything except `reg_cols`).
- Probe: the length of `plot2_env$shiny_defaults` is 102 after building the app and 0 once the server has rendered a plot. A second test session sees the plot of the first before it has rendered anything.
- Tests S03.04, S06.01 and S06.04 (parked).

## Reproduction

```r
app <- create_interactively()
shiny::testServer(app, {
  session$setInputs(dataset = "iris", type = "auto", y_calc = "None")
  session$setInputs(x = NA); session$setInputs(x = "Species")
  invisible(output$plot)
})
shiny::testServer(app, print(class(plot2_env$last_shiny_plot)))
# Expected after fix: "NULL"; each session keeps its own plot (and its own defaults) in its own reactive values
```

## Impact

Functional for every user (generated code carries defaults); on a shared server, one visitor can download another visitor's plot.

## Proposed fix

Keep the defaults in a local list in `create_interactively()` (returned by the input builders rather than written into `plot2_env`) and the last plot in a `shiny::reactiveVal()` inside `server`. Nothing of the app belongs in `plot2_env`.

## History

- 2026-11-08: filed in R07 (verified with `shiny::testServer()`).
- 2026-11-08: severity set to critical by the maintainer (all security findings are critical).
