---
id: F036
title: Data flow relies on global state, super-assignment and hidden dot-arguments
severity: medium
type: maintainability
area: architecture
status: verified
verification: verified
effort: XL
api_change: no
locations: [R/plot2.R:945, R/plot2.R:963, R/plot2.R:996, R/plot2-methods.R:1782, R/utils.R:271]
related: [F001, F035]
origin: R01 §4.2
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F036 Data flow hard to follow

## Summary

Information travels through `plot2_env`, `<<-` from an anonymous function inside a pipe (`R/plot2.R:963-1015`), hidden `_misses.*`/`_label.*`/`_summarise_fn_name` arguments in `...`, and column names recovered after the fact by comparing column contents (`get_column_name()`). Roughly a dozen data-driven guesses (types, x as character, markdown, facets fixed, scientific notation) are spread over the code.

## Reproduction

```r
# Structural finding; no runtime reproduction.
# Expected after fix: a per-call context object carrying mappings, names, flags and diagnostics.
```

## Proposed fix

Introduce an explicit per-call context (see F001) holding: original quosures and labels, which arguments were missing, resolved column names, and diagnostics. Collect the heuristics into one documented "guessing" step so their behaviour is testable and can be switched off (`plot2.guess = FALSE`).

## History

- 2026-11-07: filed in R01.
- 2026-11-08: partly done in Phase 2: per-call context for state and diagnostics (F001). Maintainer's decision: the central guessing step (and any plot2.guess option) moves to Phase 5 with D001/I004; the hidden _misses.*/_label.* dot-arguments go with F035/F094.
- 2026-11-08: with F094 the hidden `_misses.*` dot-arguments are gone; the `_label.*` and `_summarise_fn_name` ones remain, now built in one place (plot2_exec_call(), F035).
- 2026-11-08: fixed: maintainer decided (AskUserQuestion) to document the guesses in one place instead of moving them into one function or adding a plot2.guess option. ?plot2 has a section 'Automatic choices' listing every rule, and test-api-A11-automatic-choices.R tests each rule with its message. With F001 (per-call context), F035 (one forwarding call) and F094 (no _misses.* arguments), the remaining parts of this finding are done. Found on the way: the caution about summarising said 'summarise_function = sum was set' when the user had set nothing; it now says that sum is the default.
