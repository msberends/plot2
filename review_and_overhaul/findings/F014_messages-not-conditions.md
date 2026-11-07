---
id: F014
title: Messages and warnings are not R conditions; plot2.silent has no effect
severity: high
type: design
area: messaging
status: open
verification: verified
effort: M
api_change: possible
locations: [R/utils.R:144, R/utils.R:154, R/utils.R:169, R/utils.R:195]
related: [F001, F002, F013]
origin: R01 §2.7
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F014 Messages and warnings are not R conditions

## Summary

`plot2_message()`, `plot2_caution()` and `plot2_warning()` only append text to `plot2_env`; `throw_messages()` prints them via cli, and only in interactive sessions or during pkgdown builds. Consequences:

- `options(plot2.silent = TRUE)` is ignored, because `throw_messages()` passes `print` explicitly and the default argument of `plot2_message_out()` that checks the option is never used.
- Warnings ("No observations", values removed, invalid colours) never reach Rscript, knitr or Quarto output, and cannot be caught with `tryCatch()`/`withCallingHandlers()` or silenced with `suppressWarnings()`.
- Messages are sorted alphabetically, not in the order they occurred.

## Evidence

`R/utils.R:154-201`.

## Reproduction

```r
df <- data.frame(g = c("a", "b", "c"), v = 1:3)
Sys.setenv(IN_PKGDOWN = "1")   # makes plot2 print its messages, as in an interactive session
options(plot2.silent = TRUE)
out <- capture.output(type = "message", invisible(plot2(df)))
cat("message lines with plot2.silent = TRUE:", length(out), "\n")
options(plot2.silent = NULL)
Sys.unsetenv("IN_PKGDOWN")
w <- tryCatch(plot2(df[0, ], g, v), warning = function(w) "caught")
print(if (identical(w, "caught")) "warning catchable" else "warning NOT catchable")
# Expected after fix: 0 lines; "warning catchable".
```

## Impact

Users cannot see, control or test plot2's diagnostics; important warnings disappear in reports.

## Proposed fix

Emit real conditions at the point of origin: `cli::cli_inform()` with class `plot2_message`, `cli::cli_warn()` with class `plot2_warning`. Deduplicate per call via the per-call context (F001), keep chronological order, respect `plot2.silent` by muffling messages only. Keep warnings visible always.

## History

- 2026-11-07: filed in R01. The first reproduction used `rlang::local_interactive()` at top level, which printed withr notes instead; corrected the same day (simulating interactivity via `IN_PKGDOWN`), and the defect was confirmed: 2 message lines still printed with `plot2.silent = TRUE`.
