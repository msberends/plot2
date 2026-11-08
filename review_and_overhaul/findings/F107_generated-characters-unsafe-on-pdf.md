---
id: F107
title: Characters that plot2 generates (scientific-notation bullet, euro sign) fail on the default pdf device, and the bullet depends on the console
severity: medium
type: bug
area: i18n
status: open
verification: verified
effort: S
api_change: no
locations: [R/format2.R:235, R/format2.R:263, R/labellers.R:15]
related: [F063, F008]
origin: R06 sheet T07
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F107 Generated characters on pdf()

## Summary

1. Scientific axis labels are built as `mantissa • 10^exponent` with `cli::symbol$bullet` (`R/format2.R:235`). That symbol is a console glyph: with `options(cli.unicode = FALSE)` (non-UTF-8 consoles, some CI and server set-ups) it becomes `*`, so the content of the plot depends on the console. The conventional sign is ×, available device-independently in plotmath as `%*%`.
2. On `grDevices::pdf()`, the default device for `ggsave("x.pdf")` and for PDF output from R Markdown, the bullet cannot be encoded: "for '•' in 'mbcsToSbcs': . substituted for •", so the plot shows "1.5 . 10^7". This is the warning seen in the `?plot2` examples (secondary axis example) in R05.
3. `euros()` labels ("€ 1 500") fail on `pdf()` with "conversion failure on '€ 1 500' in 'mbcsToSbcs'" and the euro sign is replaced. `cairo_pdf()`, `png()`, `svg()` and `ragg` render both correctly.
4. Side observation: `euros()` formats large values in scientific notation ("€ 1e+07" for 1e7).

## Reproduction

```r
withr::with_options(list(cli.unicode = FALSE), print(plot2:::format2_scientific(3e8)))  # "3 * 10^8"
f <- tempfile(fileext = ".pdf")
w <- character(0)
withCallingHandlers({ pdf(f); print(plot2(data.frame(x = c("a", "b"), y = c(1e7, 3e7)), x, y, y.scientific = TRUE,
                                          datalabels.format = euros)); dev.off() },
                    warning = function(c) { w <<- c(w, conditionMessage(c)); invokeRestart("muffleWarning") })
print(unique(w))
# Expected after fix: no warnings; labels "1 × 10^7" on every device and in every console.
```

## Impact

Figures saved as PDF, the format most journals ask for, show substituted characters. Plot content varies with console settings.

## Proposed fix

Use plotmath `%*%` for scientific labels. For the euro sign, document that `cairo_pdf` is needed, or let `euros()` produce a plotmath expression; in examples use `ggsave(device = cairo_pdf)`.

## History

- 2026-11-08: filed in R06 (sheet T07); tests T07.01 and T07.02 parked.
