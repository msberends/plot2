---
id: F134
title: euros() labels cannot be rendered by the default pdf() device
severity: low
type: bug
area: i18n
status: wontfix
verification: verified
effort: S
api_change: no
locations: [R/labellers.R:37]
related: [F107]
origin: split from F107 (point 3)
commit: 8425756
created: 2026-11-08
updated: 2026-11-08
---

# F134 Euro sign on the default pdf() device

## Summary

`euros()` produces labels such as "€ 1 500". The default `grDevices::pdf()` device (used by `ggsave("x.pdf")` and PDF output of R Markdown) cannot encode the euro sign: "conversion failure on '€ 1 500' in 'mbcsToSbcs'", and the sign is replaced by dots. `cairo_pdf()`, `png()`, `svg()` and `ragg` render it correctly. Since F107, the `euros()` documentation says to use `cairo_pdf`.

## Reproduction

```r
p <- plot2(data.frame(x = c("a", "b"), y = c(1500, 2500)), x, y, datalabels.format = euros)
f <- tempfile(fileext = ".pdf")
grDevices::pdf(f); print(p); grDevices::dev.off()
# Expected after fix: no encoding warning
```

## Impact

Wrong currency sign in PDF output with default settings; only a warning signals it.

## Options (maintainer's decision)

1. Keep the text labels and rely on the documentation (status quo since F107).
2. Let `euros()` return plotmath expressions; the euro sign then still depends on the font encoding of `pdf()`, so this is not a reliable fix.
3. Use `pdf(encoding = "CP1252")` advice, or detect the device at print time and warn once with a plot2 message pointing to `cairo_pdf`.
4. Fall back to "EUR 1 500" on devices that cannot encode the sign (requires device detection).

## History

- 2026-11-08: split from F107; F107 points 1, 2 and 4 fixed, point 3 documented and filed here for a decision.
- 2026-11-08: maintainer decided: documentation only (status quo since F107, the euros() help page advises cairo_pdf); no device detection.
