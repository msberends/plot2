---
id: F006
title: create_interactively() builds code from unescaped inputs and evaluates it
severity: critical
type: security
area: shiny
status: open
verification: reading
effort: M
api_change: no
locations: [R/shiny.R:913, R/shiny.R:960, R/shiny.R:811, R/shiny.R:1000, R/shiny.R:1105, R/shiny.R:1230]
related: []
origin: R01 §1.6
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F006 Code injection in create_interactively()

## Summary

The Shiny app assembles a `plot2()` call as a string, pasting free-text inputs with `sprintf('%s = "%s"', nm, val)` without escaping, and then runs it with `eval(parse(text = code))`. The x/y/category selectors accept free text (`create = TRUE`) and are pasted unquoted. `input$dataset` is also evaluated with `eval(parse())`.

## Evidence

- `R/shiny.R:960`: unescaped string interpolation.
- `R/shiny.R:1000`: `eval(parse(text = code))`.
- `R/shiny.R:811`, `R/shiny.R:1105`, `R/shiny.R:1230`: `eval(parse(text = input$dataset))` and similar.
- `install_shiny_deps()` documentation addresses administrators, which suggests multi-user server deployment.

## Reproduction

```r
# Manual: run create_interactively(), type the following as the plot title:
#   x"); message("injected"); ("
# Expected after fix: the title is shown literally; nothing is executed.
```

## Impact

Functional: any `"` in a title breaks the app. Security: on a shared Shiny Server, any visitor can run arbitrary R code with the server's permissions.

## Proposed fix

Build the call as a language object (`rlang::call2("plot2", !!!args)` with symbols for columns validated against `names(data)`), evaluate that object, and produce the displayed code with `deparse()`/`rlang::expr_text()`. Resolve datasets through a lookup list rather than `eval(parse())`. State clearly in the documentation whether server deployment is supported.

## History

- 2026-11-07: filed in R01 (by reading).
