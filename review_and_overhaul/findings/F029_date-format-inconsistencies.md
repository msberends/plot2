---
id: F029
title: Date formatting returns inconsistent types and mangles literal text; time zone dropped
severity: medium
type: bug
area: i18n
status: verified
verification: verified
effort: M
api_change: possible
locations: [R/format2.R:176, R/format2.R:268, R/utils.R:507]
related: [F003, F008]
origin: R01 §3.5
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F029 Date formatting inconsistencies

## Summary

- `format2(date, format = "mmm")` returns an ordered factor (via `lubridate::month()`), other formats return character.
- `posix_date_format()` lowercases the whole format string and replaces letters inside words: "Week ww" becomes "%Veek %V", "Month" becomes "mont%k".
- `unify_years()` rebuilds POSIXct from formatted text, dropping the time zone.

## Reproduction

```r
print(class(format2(as.Date("2024-10-03"), format = "mmm")))
print(posix_date_format("Week ww"))
x <- as.POSIXct("2024-03-05 12:00", tz = "UTC")
print(attr(unify_years(x), "tzone"))
# Expected after fix: "character"; "Week %V"; "UTC".
```

## Proposed fix

Always return character; tokenise the format string (only recognised tokens, allow quoting of literals); keep `tzone` when unifying years.

## History

- 2026-11-07: filed in R01, reproduced.
- 2026-11-08: R06 (sheet T02): the `d` format code gives space-padded days (`%e`), so date labels start with a space (" 5 February 2024") in every locale.
- 2026-11-08: maintainer decided: format2() always returns character for dates (API change consented).
- 2026-11-08: fixed and verified (Phase 3, with F003). Format tokens are converted only when a whole run of letters consists of tokens (words such as "Week" stay), text in single quotes is literal; format2() returns character for all date formats; quarters without lubridate; unify_years() keeps the time zone. T02.08 unparked.
