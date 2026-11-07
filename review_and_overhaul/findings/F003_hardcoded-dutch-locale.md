---
id: F003
title: Dates formatted in Dutch by default and locale handling is global and inconsistent
severity: critical
type: i18n
area: i18n
status: needs-decision
verification: verified
effort: M
api_change: possible
locations: [R/format2.R:116, R/format2.R:125, R/format2.R:156, R/format2.R:185, R/format2.R:217, R/validate.R:1125, R/validate.R:1132, R/validate.R:1353, R/get_plot_title.R:96]
related: [F029]
origin: R01 §1.3
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F003 Dates formatted in Dutch by default; locale handling global and inconsistent

## Summary

`format2.Date()` and `format2.POSIXt()` default to `locale = "nl"`. These are used for date category legends, so every user sees Dutch month names there, while date x-axes use the system locale. The locale switch uses `Sys.setlocale()`, which changes the whole session and is restored without `on.exit()`. Other labels switch language based on the machine's locale.

## Evidence

- `R/format2.R:118`, `R/format2.R:127`: `locale = "nl"`.
- `R/format2.R:185-221`: `Sys.setlocale("LC_TIME", ...)` with manual restore at the end of the function; an error in between leaves the session in the other locale. On systems without `nl_NL` installed, `Sys.setlocale()` emits an OS warning.
- `R/validate.R:1125`, `R/validate.R:1132`: `y.24h` and `y.age` labels use "u"/"jr" or "h"/"yrs" depending on `LC_COLLATE`.
- `R/get_plot_title.R:96`: default titles in Dutch or English depending on `Sys.getlocale()`.

## Reproduction

```r
Sys.setlocale("LC_TIME", "en_GB.UTF-8")
cat(format2(as.Date("2024-10-03")), "\n")
print(format2(as.Date("2024-10-03"), format = "mmm"))
# Expected after fix: "3 October 2024" and "Oct" (character) in an English locale.
```

## Impact

International users (the large majority) get Dutch text in published figures; output differs between machines for the same code.

## Proposed fix

Default `locale = NULL` (system locale) everywhere; restore any locale change with `on.exit()`; prefer locale-independent formatting where possible. Decide whether plot2 should translate generated text at all, and if so via an explicit option such as `plot2.language`.

**Decision needed:** keep any Dutch-specific behaviour behind an option, or remove it.

## History

- 2026-11-07: filed in R01, reproduced.
