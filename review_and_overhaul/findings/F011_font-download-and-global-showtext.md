---
id: F011
title: Fonts downloaded without consent and showtext enabled globally
severity: high
type: design
area: fonts
status: verified
verification: verified
effort: M
api_change: possible
locations: [R/validate.R:2506, R/validate.R:2544, R/validate.R:2563]
related: []
origin: R01 §2.4
commit: d09e04d
created: 2026-11-07
updated: 2026-11-08
---

# F011 Font download and global showtext

## Summary

When a requested font is not installed, `validate_font()` silently downloads it from Google Fonts; `showtextdb::font_install()` stores it inside the installed showtextdb package directory. `showtext_auto(enable = TRUE)` switches text rendering for all subsequent graphics in the session, including base R and plain ggplot2 plots.

## Evidence

- `R/validate.R:2563-2569`: download inside `tryCatch(..., error = function(e) invisible())`.
- `R/validate.R:2544`: `showtext_auto(enable = TRUE)`.

## Reproduction

```r
# Manual: plot2(iris, Species, font = "Rock Salt"), then check
#   showtext::showtext_auto() state and list.files(system.file("fonts", package = "showtextdb"))
# Expected after fix: no download unless explicitly allowed; showtext not left enabled globally.
```

## Impact

CRAN policy conflict (network access and writing outside the temporary directory without consent); fails on read-only libraries; unexpected rendering changes in unrelated plots.

## Proposed fix

Make downloading opt-in (option `plot2.download_fonts`, or an exported `install_font()` helper), cache under `tools::R_user_dir("plot2", "cache")`, and scope showtext to plot2 output (for example a print method that wraps `showtext_begin()`/`showtext_end()`).

**Decision needed:** keep automatic download (opt-in) or require an explicit helper.

## History

- 2026-11-07: filed in R01 (by reading).
- 2026-11-08: maintainer decided: automatic download, scoped, if CRAN allows it, otherwise opt-in. CRAN Repository Policy (revision 6985, read 2026-11-08) allows user cache files in tools::R_user_dir() for R >= 4.0 if sizes are kept small and contents are actively managed (removing outdated material), and requires Internet use to fail gracefully; writing into the installed showtextdb folder is not allowed. So: automatic download into R_user_dir("plot2", "cache") with automatic pruning, graceful fallback, showtext switched on only while plot2 output is drawn.
- 2026-11-08: fixed. New `R/fonts.R`: Google Fonts are downloaded once into `tools::R_user_dir("plot2", "cache")/fonts` (https, failures fall back silently with the existing font error), registered with `sysfonts::font_add()`, and fonts unused for 90 days are pruned at each download. `showtext_auto()` removed: plots with a custom font get the class `plot2_font`, whose `print()` and `grid.draw()` methods switch showtext on for that drawing only (pixel-identical output for print and ggsave). showtext, showtextdb and sysfonts moved to Suggests with an install hint (F108). Tests and vignettes redirect `R_USER_CACHE_DIR` to a temporary folder; the ?plot2 font example runs only interactively. Tests: test-regression-phase3.R (F011).
