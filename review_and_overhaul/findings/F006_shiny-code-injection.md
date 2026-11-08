---
id: F006
title: create_interactively() builds code from unescaped inputs and evaluates it
severity: critical
type: security
area: shiny
status: open
verification: verified
effort: M
api_change: no
locations: [R/shiny.R:913, R/shiny.R:926, R/shiny.R:939, R/shiny.R:961, R/shiny.R:811, R/shiny.R:1000, R/shiny.R:1105, R/shiny.R:1230]
related: [F111, F112, F113, F114, F118, F121]
origin: R01 §1.6
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F006 Code injection in create_interactively()

## Summary

The Shiny app assembles a `plot2()` call as a string, pasting free-text inputs with `sprintf('%s = "%s"', nm, val)` without escaping, and then runs it with `eval(parse(text = code))`. The x/y/category selectors accept free text (`create = TRUE`) and are pasted unquoted. `input$dataset` is also evaluated with `eval(parse())`.

## Evidence

- `R/shiny.R:960`: unescaped string interpolation.
- `R/shiny.R:1000`: `eval(parse(text = code))`.
- `R/shiny.R:811`, `R/shiny.R:1105`, `R/shiny.R:1230`: `eval(parse(text = input$dataset))` and similar.
- `install_shiny_deps()` documentation addresses administrators, which suggests multi-user server deployment.
- R07 (verified with `shiny::testServer()`): four independent routes run arbitrary code, each confirmed by writing a marker file:
  1. a value of the x/y/category/facet selectors (free text allowed, pasted unquoted at `R/shiny.R:926`);
  2. a title or any other text input (`"` closes the string at `R/shiny.R:961`);
  3. `summarise_function`, evaluated directly with `eval(str2lang(val))` at `R/shiny.R:939` while building the code;
  4. a value of the data set selector (`R/shiny.R:811`); Shiny does not check selectize values against the choices on the server.
- Tests S02.03, S03.06, S04.05, S04.06 and S04.07 (parked).

## Reproduction

```r
marker <- tempfile()
app <- create_interactively()
shiny::testServer(app, {
  session$setInputs(dataset = "iris", type = "auto", y_calc = "None")
  session$setInputs(dataset = sprintf('{writeLines("x", "%s"); iris}', marker))
})
print(file.exists(marker))
# Expected after fix: FALSE; nothing typed or sent by the browser is executed
```

## Impact

Functional: any `"` in a title breaks the app. Security: on a shared Shiny Server, any visitor can run arbitrary R code with the server's permissions.

## Proposed fix

Build the call as a language object (`rlang::call2("plot2", !!!args)` with symbols for columns validated against `names(data)`), evaluate that object, and produce the displayed code with `deparse()`/`rlang::expr_text()`. Resolve datasets through a lookup list rather than `eval(parse())`. State clearly in the documentation whether server deployment is supported.

## History

- 2026-11-07: filed in R01 (by reading).
- 2026-11-08: R07 verified all four routes with `shiny::testServer()` and replaced the manual reproduction; related findings F111 to F121 cover further server-deployment risks.
