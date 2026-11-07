---
id: F010
title: UpSet plot with print = TRUE continues execution and errors
severity: high
type: bug
area: types
status: open
verification: verified
effort: S
api_change: no
locations: [R/plot2.R:1318]
related: [F001]
origin: R01 §2.3
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F010 UpSet with print = TRUE falls through

## Summary

In the UpSet branch, `if (isTRUE(print)) print(out) else return(out)` has no return after printing, so execution continues into the normal pipeline with `type = "upset"`.

## Evidence

`R/plot2.R:1318-1322`.

## Reproduction

```r
pdf(NULL)
res <- tryCatch({ plot2(data.frame(a = c(TRUE, FALSE, TRUE), b = c(TRUE, TRUE, FALSE)), x = c(a, b), type = "upset", print = TRUE); "OK" },
                error = function(e) conditionMessage(e))
dev.off()
print(res)
# Expected after fix: "OK"
```

## Impact

`print = TRUE` (used in loops and reports) is broken for UpSet plots.

## Proposed fix

`if (isTRUE(print)) { print(out); return(invisible(out)) }`. Harmonise all `print = TRUE` paths to return `invisible(p)` (the empty-data path returns `invisible(NULL)`).

## History

- 2026-11-07: filed in R01, reproduced.
