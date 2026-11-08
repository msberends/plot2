---
id: F114
title: Data sets are looked up by name with get() and eval(parse()), which picks up functions, fails on non-syntactic names and exposes the global environment
severity: critical
type: bug
area: shiny
status: open
verification: verified
effort: M
api_change: possible
locations: [R/shiny.R:81, R/shiny.R:94, R/shiny.R:439, R/shiny.R:811, R/shiny.R:1105, R/shiny.R:1230]
related: [F006, F115]
origin: R07 sheet S02
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F114 Data set lookup by name

## Summary

The app keeps data sets as names and turns names back into objects in several ways, each with its own problem:

1. `create_interactively(data = x)` deparses the argument and keeps the name when `get(name)` finds *anything* from the function's frame (`R/shiny.R:94` to `:98`). `get()` also finds functions on the search path, so `data = df` refers to `stats::df` (and `data = data`, `c`, `t`, ...): the passed data are silently unusable.
2. Names are evaluated with `eval(parse(text = name))` (`:811`, `:1105`, `:1230`). A global data frame with a non-syntactic name such as `my data` is offered but selecting it raises a parse error in an observer, which ends the session (F115).
3. All data frames in the global environment are offered (`:81` to `:85`). Locally that is the point of the app; on a server it exposes whatever the hosting script has loaded.
4. `selected = ifelse(is.null(data), "iris", names(globalenv_labels)[1])` (`:439`) passes a label ("mtcars (32 x 11)") where a value is needed; it works only because selectize falls back to the first choice.

## Evidence

- Probe: inside a function, `df <- data.frame(...); create_interactively(data = df)` lists `df` and evaluates it to `function (x, df1, df2, ncp, log = FALSE)`.
- Probe: `assign("my data", ...)` in the global environment and selecting it: "unexpected symbol" in the observer at `:808`, session closed.
- Tests S02.02, S02.11 and S06.02 (parked).

## Reproduction

```r
f <- function() { df <- data.frame(a = c("x", "y"), b = 1:2); create_interactively(data = df) }
app <- f()
print(get("data_sets", environment(app$serverFuncSource()))[[1]])
shiny::testServer(app, {
  session$setInputs(dataset = "df", type = "auto", y_calc = "None")
  invisible(output$plot)
  print(nrow(plot2_env$last_shiny_plot$data))
})
# Expected after fix: the passed data frame is used (2 rows); the selector lists it as 'df' or 'Provided'
```

## Impact

Wrong or missing data for common variable names; crashes for non-syntactic names; exposure of global objects when deployed.

## Proposed fix

Collect data sets into a named list of objects once (`list(Provided = data, iris = datasets::iris, ...)`, and the global ones with `mget()`), and look them up with `[[` by key. For the displayed code, use the original name with backticks where needed. Add an argument (or rule) to restrict the offered data sets, e.g. global data frames only when interactive. Use the value, not the label, for `selected`.

## History

- 2026-11-08: filed in R07 (verified with `shiny::testServer()`).
- 2026-11-08: severity set to critical by the maintainer (security aspect: global data frames of the host offered to visitors).
