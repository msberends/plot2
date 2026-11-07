---
id: F015
title: dec_mark() returns the warning text when the option is invalid
severity: high
type: bug
area: i18n
status: open
verification: verified
effort: S
api_change: no
locations: [R/dec_mark.R:43]
related: [F014]
origin: R01 §2.8
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F015 dec_mark() with an invalid option

## Summary

When `getOption("dec_mark")` is invalid, `dec_mark()` calls `plot2_warning()` and returns its value (the vector of queued warnings), which then becomes the decimal mark. The warning itself is only shown at the next plot2 call.

## Evidence

`R/dec_mark.R:43-55`: no fallback after the warning.

## Reproduction

```r
options(dec_mark = ";")
print(dec_mark())
options(dec_mark = NULL)
# Expected after fix: "." (the OutDec value) plus a visible warning.
```

## Impact

Garbled numbers on every axis of every plot until the option is fixed.

## Proposed fix

Fall back to `getOption("OutDec", ".")` after warning; use a real `warning()` (see F014). Consider namespacing the options (`plot2.dec_mark`), since `dec_mark` may clash with other packages.

## History

- 2026-11-07: filed in R01, reproduced.
