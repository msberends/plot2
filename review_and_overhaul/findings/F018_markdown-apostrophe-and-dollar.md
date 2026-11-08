---
id: F018
title: Markdown conversion breaks on apostrophes and turns any $ into plotmath
severity: high
type: bug
area: markdown
status: verified
verification: verified
effort: M
api_change: no
locations: [R/md_to_expression.R:61, R/validate.R:2018, R/validate.R:1890, R/validate.R:1963]
related: []
origin: R01 §2.11
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F018 Markdown: apostrophes and dollar signs

## Summary

`md_to_expression()` wraps text in single quotes and builds R code with string substitution, so any apostrophe produces unparsable code and `plot2()` errors. Markdown detection treats any `$` as plotmath, turning titles such as "Costs in $" into expressions (which also disables line wrapping). `validate_markdown()` scans the concatenation of all titles, so patterns can match across titles.

## Evidence

- `R/md_to_expression.R:61`: `x <- paste0("'", x, "'")` without escaping.
- `R/validate.R:2025` and `R/plot2.R:1896`: `%like% "[$]"`.

## Reproduction

```r
df <- data.frame(g = c("a", "b", "c"), v = 1:3)
print(tryCatch({ plot2(df, g, v, title = "Children's *E. coli* isolates"); "OK" }, error = function(e) conditionMessage(e)))
print(class(ggplot2::get_labs(plot2(df, g, v, title = "Costs in $ per patient"))$title))
# Expected after fix: "OK"; "character".
```

## Impact

Titles and category values containing apostrophes (common in English and in names) crash the plot when any markdown is present.

## Proposed fix

Escape quotes and backslashes before building the expression, or construct the expression with `bquote()`/`call()` instead of parsing text. Require paired `$...$` for plotmath. Consider `ggtext` as an optional backend.

## History

- 2026-11-07: filed in R01, reproduced.
- 2026-11-08: fixed: md_to_expression() protects apostrophes and backslashes outside $...$ parts and restores them as escapes; $...$ follows Pandoc's rule (md_plotmath_regex), so a single $ or currency amounts stay text; one has_markdown() helper replaces the duplicated detection in validate_title() and plot2_exec; validate_markdown() checks each title on its own; a title of several $...$ parts is no longer parsed as one expression. Tests: test-regression-phase4.R (F018).
