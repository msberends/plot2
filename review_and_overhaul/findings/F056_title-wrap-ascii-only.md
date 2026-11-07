---
id: F056
title: Title wrapping counts only ASCII characters, so accented or non-Latin titles are not wrapped
severity: low
type: bug
area: i18n
status: open
verification: verified
effort: S
api_change: no
locations: [R/validate.R:2008, R/validate.R:2030]
related: []
origin: R02 sheet 14
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F056 Title wrapping counts only ASCII

## Summary

`out_plain <- gsub("[^a-zA-Z0-9,. .-]", "", out)` removes every other character before comparing `nchar(out_plain)` with `max_length`. Titles with accents, Greek, Cyrillic or CJK characters appear shorter than they are and are not wrapped.

## Reproduction

```r
print(validate_title("Résumé ééééé ééééé ééééé ééééé", markdown = FALSE, max_length = 20))
print(validate_title("Resume eeeee eeeee eeeee eeeee", markdown = FALSE, max_length = 20))
# Expected after fix: both wrapped after 20 characters.
```

## Proposed fix

Strip only markup characters (`*`, `_`, `^`, `{`, `}`, `$`, HTML tags) and count with `nchar(type = "width")`.

## History

- 2026-11-07: filed in R02, reproduced.
