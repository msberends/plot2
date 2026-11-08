---
id: F120
title: Upload options produce invalid reader arguments: free-text encoding and .txt with the default delimiter
severity: medium
type: bug
area: shiny
status: open
verification: verified
effort: S
api_change: no
locations: [R/shiny.R:399, R/shiny.R:688, R/shiny.R:691]
related: [F111, F115]
origin: R07 sheet S02
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F120 Upload reader options

## Summary

- The CSV encoding is a free text field passed to `data.table::fread()` (through rio), which accepts only "unknown", "UTF-8" or "Latin-1". The common spellings "latin1" or "ISO-8859-1" fail with "Argument 'encoding' must be 'unknown', 'UTF-8' or 'Latin-1'", and the user only sees "Read failed".
- `.txt` with the default comma delimiter fails: rio reads `txt` with a comma as decimal mark, "sep == dec (',') is not allowed".
- The statistical-software encoding is free text as well.
- The documentation of `upload_tab` still describes a modal dialogue that no longer exists.

## Evidence

- Probes and tests S02.06 and S02.07 (parked). Comment character, skip and NA strings work (S02.05).

## Reproduction

```r
f <- tempfile(fileext = ".csv"); writeLines(c("g,v", "a,1"), f)
print(tryCatch(nrow(rio::import(f, format = "csv", encoding = "latin1")), error = conditionMessage))
print(tryCatch(nrow(rio::import(f, format = "txt", sep = ",")), error = conditionMessage))
# Expected after fix: the app offers a selector of valid encodings (or maps common spellings) and reads .txt with any delimiter
```

## Impact

Uploads fail for ordinary files with an unhelpful message.

## Proposed fix

A selector for encodings with the values the reader accepts; for `txt`, pass `dec = "."` or read it as CSV with the chosen delimiter; update the roxygen text.

## History

- 2026-11-08: filed in R07 (verified).
