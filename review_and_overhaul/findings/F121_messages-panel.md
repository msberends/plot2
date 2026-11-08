---
id: F121
title: The messages panel is empty on a server, shows messages as unescaped HTML, and errors are reduced to a generic sentence
severity: critical
type: bug
area: shiny
status: open
verification: verified
effort: S
api_change: no
locations: [R/shiny.R:1002, R/shiny.R:1005, R/shiny.R:1016, R/utils.R:155]
related: [F006]
origin: R07 sheet S04
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F121 Messages and errors in the app

## Summary

- plot2 prints its messages only when `interactive()` (or under pkgdown) (`R/utils.R:155`). A deployed app is not interactive, so the panel that should show plot2's explanations (`R/shiny.R:1005` onwards) stays empty.
- Where messages are shown, they are converted with `fansi::sgr_to_html()` and inserted with `shiny::HTML()` (`:1016`). fansi does not escape HTML, and the messages contain column names (`Using x = <name>`), so a column name with markup is rendered as markup: a column called `<img src=x onerror=...>` runs script in the page.
- Any error becomes "This code contains an error." (`:1002`); the actual message, which plot2 takes care to make helpful, is dropped. Warnings are not shown at all.

## Evidence

- Probe: `fansi::sgr_to_html("<b>x</b>")` returns `<b>x</b>`; with messages forced on, a data set with a column named `<img src=x onerror=alert(1)>` yields the message `Using x = <img src=x onerror=alert(1)>` unchanged.
- Tests S04.02, S04.03 and S04.04 (parked).

## Reproduction

```r
app <- create_interactively()
shiny::testServer(app, {
  session$setInputs(dataset = "iris", type = "auto", y_calc = "None")
  session$setInputs(x = NA); session$setInputs(x = "does_not_exist")
  invisible(output$plot)
  print(output$error_msg)
  print(output$plot2_msgs)
})
# Expected after fix: the error names does_not_exist, and plot2's messages are listed (HTML-escaped) also when not interactive
```

## Impact

The app's teaching function (showing what plot2 did) is lost on servers; HTML injection through data; unhelpful errors.

## Proposed fix

Collect plot2's messages independently of `interactive()` (e.g. an option or argument that makes `plot2_message_out()` signal them), escape with `htmltools::htmlEscape()` before the SGR conversion, show `conditionMessage(e)` and warnings.

## History

- 2026-11-08: filed in R07 (verified).
- 2026-11-08: severity set to critical by the maintainer (security aspect: HTML injection through column names).
