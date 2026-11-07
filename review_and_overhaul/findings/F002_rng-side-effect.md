---
id: F002
title: plot2() consumes the user's random-number stream
severity: critical
type: bug
area: messaging
status: open
verification: verified
effort: S
api_change: no
locations: [R/utils.R:195, R/utils.R:725, R/utils.R:747]
related: [F014]
origin: R01 §1.2
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F002 plot2() consumes the user's random-number stream

## Summary

`throw_messages()` draws `stats::runif(1)` to decide whether to show a tip about `options(plot2.silent = TRUE)`. This advances `.Random.seed` whenever a call produced messages, so any seeded analysis that calls `plot2()` in between becomes non-reproducible. The unused helper `msg_not_thrown_before()` also calls `sample()`.

## Evidence

- `R/utils.R:195`: `stats::runif(1) < 0.01`.
- `R/utils.R:747`: `sample()` inside `msg_not_thrown_before()`, which is not called anywhere (dead code).

## Reproduction

```r
df <- data.frame(g = c("a", "b", "c"), v = 1:3)
set.seed(1); a <- runif(1)
set.seed(1); invisible(plot2(df)); b <- runif(1)
cat("RNG stream unchanged by plot2():", identical(a, b), "\n")
# Expected after fix: TRUE
```

## Impact

Breaks reproducibility of scientific analyses, the core audience of the package. Very hard for users to diagnose.

## Proposed fix

Remove the random tip, or show it deterministically (for example once per session, tracked with a counter in the package environment). Delete `msg_not_thrown_before()`. Add a regression test that compares `.Random.seed` before and after `plot2()`.

## History

- 2026-11-07: filed in R01, reproduced.
