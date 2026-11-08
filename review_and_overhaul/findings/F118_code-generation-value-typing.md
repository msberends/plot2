---
id: F118
title: Generated code types input values by guesswork: vectors and expressions cannot be entered, and text that looks numeric changes type
severity: medium
type: bug
area: shiny
status: open
verification: verified
effort: M
api_change: no
locations: [R/shiny.R:921, R/shiny.R:942, R/shiny.R:945, R/shiny.R:961]
related: [F006, F119, F124]
origin: R07 sheet S03
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F118 Value typing in the generated code

## Summary

`call_string()` decides the type of each input value from its content, not from the argument:

- any text matching `^([0-9]|[.])+$` becomes a number: `sep = "."` becomes `sep = NA` with a coercion warning, `"1.2.3"` becomes `NA`, and a title "2024" becomes the number 2024;
- the strings `"TRUE"`, `"FALSE"`, `"NULL"` become constants, so a title "TRUE" cannot be set;
- everything else is quoted, so arguments that take vectors or expressions (`x.limits`, `y.breaks`, `x.expand`, `category.focus`, `colour` vectors, `smooth.formula`, `x.labels` functions, 57 text inputs in all) can only receive one string: `y.limits = "c(0, 10)"`;
- quoting does not escape, so a `"` in a text breaks the code (and is the injection route of F006);
- non-syntactic column names are pasted bare (`x = my column`).

## Evidence

- Probes and tests S03.07 and S03.08 (parked). Widget table from the probe: 57 text inputs, 31 sliders, 43 switches, 13 selectors.

## Reproduction

```r
app <- create_interactively()
shiny::testServer(app, {
  session$setInputs(dataset = "iris", type = "auto", y_calc = "None")
  session$setInputs(sep = NA); session$setInputs(sep = ".")
  session$setInputs(y.limits = NA); session$setInputs(y.limits = "c(0, 10)")
  cat(call_string(), "\n")
})
# Expected after fix: sep = ".", y.limits = c(0, 10) (parsed and validated as a numeric vector, not evaluated)
```

## Impact

A large part of plot2's arguments cannot be used from the app; some inputs silently produce different code.

## Proposed fix

Give every argument an explicit kind (string, number, logical, numeric vector, column, function name from a whitelist) in one specification table (see F124), convert per kind, and build the call as a language object as proposed in F006; let `deparse()` produce the code.

## History

- 2026-11-08: filed in R07 (verified with `shiny::testServer()`).
