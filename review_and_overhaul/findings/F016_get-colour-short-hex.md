---
id: F016
title: get_colour() expands only the first short hex code and uses a wrong character range
severity: high
type: bug
area: colours
status: open
verification: verified
effort: S
api_change: no
locations: [R/get_colour.R:150, R/get_colour.R:157, R/get_colour.R:167]
related: [F017]
origin: R01 §2.9
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F016 get_colour() short hex codes

## Summary

The "#ffa -> #ffffaa" conversion uses `strsplit(x[shorts], "")[[1]]`, so the first short code is assigned to all short codes. The regex `[0-F]` is a character range from "0" to "F" that includes `:;<=>?@`; combined with `like()` lowercasing (F017) it becomes `[0-f]`, which includes `[\]^_` and backtick.

## Evidence

`R/get_colour.R`, block "support #ffa -> #ffffaa" and the validity checks after it.

## Reproduction

```r
print(unclass(get_colour(c("#f00", "#0f0", "#00f"))))
# Expected after fix: "#FF0000" "#00FF00" "#0000FF"
```

## Impact

Wrong colours in plots that use short hex codes.

## Proposed fix

Vectorise: `vapply(strsplit(sub("^#", "", x[shorts]), ""), function(ch) paste0("#", paste0(ch, ch, collapse = "")), "")`. Use `[0-9A-Fa-f]` with `grepl()` directly.

## History

- 2026-11-07: filed in R01, reproduced.
