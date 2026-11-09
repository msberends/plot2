---
id: F099
title: Typos, contradictions and small factual errors in the help pages
severity: low
type: docs
area: housekeeping
status: verified
verification: verified
effort: S
api_change: no
locations: [R/plot2.R:93, R/plot2.R:98, R/plot2.R:102, R/plot2.R:105, R/plot2.R:111, R/plot2.R:166, R/plot2.R:206, R/md_to_expression.R:18, R/options.R:28, R/options.R:32, R/options.R:42, R/add_type.R:23, R/add_type.R:620, R/data.R:30, R/labellers.R:15]
related: [F098, F104]
origin: R05 sheets A06, A07
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F099 Small errors in the help pages

## Summary

Checked by reading and with aspell (en_GB) over `man/` and the vignettes:

- `?plot2`: "Get_colour(s) to set" (`colour`, `colour_fill`); "should in in *italics*" (`x.lbl_italic`); "continous"; "cq." (Dutch, `x.n_breaks`); `y.24h` starts lower case and lacks a full stop; the `x.complete` example `complete(full_seq(x, ...), fill = list(x = 0))` fills x instead of y; Details "currently has 152 arguments, all with a default value" (153 arguments, `.data` has none).
- `?md_to_expression`: `@param x` says "only the first value will be evaluated", `@return` (and the code) return a list for longer input.
- `?plot2-options`: `plot2.colour_font_axis` names `axis.title.x` twice; `plot2.colour_font_secondary` says subtitle only, `theme_minimal2()` also uses it for the caption; `plot2.silent` is described as "Default value to show console messages" (TRUE hides them).
- `?add_type`: `move` says "`move = 1` will place the newly added geom down 1 layer"; the example uses `move = -1` to move it down (and positive moves are broken, F083). `datalabels.nudge_y` "is `datalabels` is not `NULL`".
- `?netherlands`: "geometies".
- `?labellers`: titled "Label Euro currencies" but documents `dollars()` too.
- US spellings in prose of a British-spelled package: "customizability", "carburetor" (vignette), "color" (README, vignette), "center" (coord_spider).

## Reproduction

```r
rd <- paste(readLines("man/plot2.Rd"), collapse = " ")
print(regmatches(rd, regexpr("currently has [0-9]+ arguments", rd)))  # 152
print(length(formals(plot2)))                                          # 153
print(class(md_to_expression(c("*a*", "**b**"))))                     # list, despite "only the first value"
# Expected after fix: the numbers agree and the texts are corrected.
```

## Impact

Cosmetic, except the `x.complete`, `md_to_expression` and `move` texts, which describe wrong behaviour.

## Proposed fix

Correct in one documentation pass; let the argument count be computed with `length(formals(plot2))` and drop "all with a default value".

## History

- 2026-11-08: filed in R05; tests A06.02 and A07.08 parked.
- 2026-11-08: fixed and verified (Phase 1). All listed typos and errors corrected; the x.complete description now matches the per-class completion of F048; tests A06.02 and A07.08 unparked.
