---
id: F116
title: Choosing 'Automatic' as plot type after another type breaks the generated code
severity: medium
type: bug
area: shiny
status: open
verification: verified
effort: S
api_change: no
locations: [R/shiny.R:940, R/shiny.R:945]
related: [F115]
origin: R07 sheet S03
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F116 Plot type back to Automatic

## Summary

In `call_string()`, `type = "auto"` is turned into `val <- NULL` (`R/shiny.R:940`), and the next statement is `if (val %in% c("TRUE", "FALSE", "NULL"))` (`:945`), which fails with "argument is of length zero". Once a user has chosen any type, going back to "Automatic" breaks the code output and the plot until the app is restarted.

## Evidence

- Probe and test S03.05 (parked).

## Reproduction

```r
app <- create_interactively()
shiny::testServer(app, {
  session$setInputs(dataset = "iris", type = "auto", y_calc = "None")
  session$setInputs(x = NA); session$setInputs(x = "Species")
  session$setInputs(type = "col"); session$setInputs(type = "auto")
  print(tryCatch(call_string(), error = conditionMessage))
})
# Expected after fix: "iris |>\n  plot2(x = Species)"
```

## Impact

A basic interaction breaks the app.

## Proposed fix

Return `""` right away for `type == "auto"`, and test with `isTRUE(...)`/`length()` before `if`. Covered anyway by the rewrite proposed in F006 and F118.

## History

- 2026-11-08: filed in R07 (verified with `shiny::testServer()`).
