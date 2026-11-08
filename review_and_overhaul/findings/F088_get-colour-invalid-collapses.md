---
id: F088
title: One invalid colour replaces the whole colour vector by a single grey; register_colour() accepts invalid colours
severity: medium
type: bug
area: colours
status: verified
verification: verified
effort: S
api_change: no
locations: [R/get_colour.R:165, R/get_colour.R:169, R/get_colour.R:291, R/get_colour.R:78]
related: [F016, F014, F047]
origin: R04 sheet M09
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F088 One invalid colour replaces the whole colour vector by a single grey; register_colour() accepts invalid colours

## Summary

When any element is not a valid colour, `get_colour()` queues a caution and returns `get_colour("ggplot2", length = length)` (`R/get_colour.R:169`), i.e. one grey (`#595959`) for the whole input: valid entries and names are discarded. `colour = c(a = "red", b = "nope", c = "blue")` therefore gives all categories the same grey, and the caution is shown only in interactive sessions (F014). Because invalid input returns grey and not `NA`, the validity check in `register_colour()` (`R/get_colour.R:291`) never triggers, so `register_colour(a = "notacolour")` succeeds. Registered names are matched case-sensitively while R colour names are not (`get_colour("MY_RED")` after registering `my_red` gives grey).

## Reproduction

```r
print(unclass(get_colour(c(a = "red", b = "nope", c = "blue"))))
print(tryCatch({ suppressMessages(register_colour(zz = "notacolour")); "registered" }, error = function(e) conditionMessage(e)))
# Expected after fix: "#FF0000", NA (or a default) for b, "#0000FF", names kept; an error for the registration.
```

## Proposed fix

Replace only the invalid elements (keep names and length), raise a real warning (F014), and let `register_colour()` validate with a strict check that errors on unknown colours. Match registered names case-insensitively, consistent with R colour names.

## History

- 2026-11-08: filed in R04, reproduced.
- 2026-11-08: fixed: get_colour() replaces only invalid elements with grey (#595959), keeping valid colours, names and length, with a real warning; a single unknown unnamed name (likely a mistyped palette) still falls back to the default palette, now with a warning; register_colour() errors on invalid colours; registered names are matched case-insensitively. Documented under x. Tests: M09.07 and M09.08 unparked, 09.11 and the F016 regression test now expect a warning, the F014 ordering test uses type = 'step' as its caution source, test-regression-phase4.R (F088).
