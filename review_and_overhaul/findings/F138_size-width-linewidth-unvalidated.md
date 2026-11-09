---
id: F138
title: Negative or non-numeric size, width and linewidth are not validated
severity: low
type: bug
area: validation
status: verified
verification: verified
effort: S
api_change: no
locations: [R/validate.R:2195]
related: []
origin: I002 sheet 10 (case 10.05)
commit: 0608a53
created: 2026-11-08
updated: 2026-11-08
---

# F138 size, width and linewidth unvalidated

## Summary

`validate_size()`, `validate_width()` and `validate_linewidth()` only fill in defaults. `size = -1` is drawn without complaint, and `size = "a"` returns a plot that fails only when printed, with ggplot2's internal error "non-numeric argument to binary operator" in `pointsGrob()`.

## Reproduction

```r
p <- plot2(data.frame(x = 1:3, y = 1:3), x, y, size = "a") # no error
print(p) # Error in pointsize * .pt: non-numeric argument to binary operator
```

## Proposed fix

Check in the three validators that a set value is a non-negative number, with a clear error naming the argument.

## History

- 2026-11-08: found while coding I002 sheet 10 (case 10.05); fixed in the same session with `check_non_negative()`. Tests 10.05, 11.03 and 12.03.
