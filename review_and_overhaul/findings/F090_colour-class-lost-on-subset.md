---
id: F090
title: The colour class is lost when subsetting with [
severity: low
type: bug
area: colours
status: verified
verification: verified
effort: S
api_change: no
locations: [R/get_colour.R:365]
related: []
origin: R04 sheet M09
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F090 The colour class is lost when subsetting with [

## Summary

There is no `[.colour` method, so `get_colour(c("red", "blue"))[1]` returns a plain character vector: printing, `rev()`, `unique()` and the vctrs casts for colours no longer apply. Other methods (`c`, `rep`, `rev`, `unique`) keep the class.

## Reproduction

```r
x <- get_colour(c(a = "red", b = "blue"))
print(class(x[1]))
# Expected after fix: "colour" "character".
```

## Proposed fix

Add `` `[.colour` <- function(x, i) structure(unclass(x)[i], class = class(x)) `` (and `[[`), or implement the class with `vctrs::new_vctr()`.

## History

- 2026-11-08: filed in R04, reproduced.
- 2026-11-08: fixed and verified (Phase 1). 2026-11-08: fixed; [.colour and [[.colour keep the class (and names). Consequence: colours passed to ggplot2 keep the class in the built layer data; tests A04.04, A04.08, A07.04 and the F067 regression test now compare values with as.character(). Regression test F090.
