---
id: F123
title: Change tracking creates new observers for all inputs whenever the set of inputs changes, and ignores the first value of each
severity: low
type: maintainability
area: shiny
status: verified
verification: verified
effort: S
api_change: no
locations: [R/shiny.R:597, R/shiny.R:825]
related: [F112]
origin: R07 sheet S03
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F123 Change tracking

## Summary

The observer at `R/shiny.R:597` depends on `names(input)`. Every time an input appears (upload sheet selector, file input, DataTables state inputs), it runs again and creates a fresh `observeEvent()` for every plot2 argument, without destroying the previous ones, so observers accumulate during a session. Each new observer uses `ignoreInit = TRUE`, so the first value an input sends after its observer is created is not recorded as a change. In the browser the static inputs exist from the start, which hides this; it shows up with dynamic inputs and in tests (the helpers set each input twice). When the data set changes, the list of changed inputs is reset (`:825`), but the inputs keep their values, so the visible settings and the code disagree.

## Evidence

- Probe: the first value given to `x` after start was not recorded; the second was. `R/shiny.R:597` to `:608`.

## Reproduction

```r
app <- create_interactively()
shiny::testServer(app, {
  session$setInputs(dataset = "iris", type = "auto", y_calc = "None")
  session$setInputs(title = "first")
  print(changed_inputs$names)
})
# Expected after fix: "title"
```

## Impact

Small leak; code that does not match the visible settings after a data set change.

## Proposed fix

Create the observers once for the known argument inputs (they are known when the UI is built), or derive "changed" by comparing each input with its default at the time the code is built, which needs no observers at all.

## History

- 2026-11-08: filed in R07 (verified with `shiny::testServer()`).
- 2026-11-08: fixed: no observers; an argument is in the code when its input differs from its starting value, so code and inputs always agree. Tests S03.11, S03.12 added.
