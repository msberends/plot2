# A10 README, NEWS, DESCRIPTION and pkgdown

`README.Rmd`, `README.md`, `NEWS.md`, `DESCRIPTION`, `_pkgdown.yml`.

## Checked

- `README.md` is identical to a fresh render of `README.Rmd`.
- 12 of 19 README chunks have `eval = FALSE`. Run one by one: the dumbbell example fails, the `colour_fill` example warns and does not colour per species, and "Both produce the same chart" is not exact (F100).
- `pkgdown::check_pkgdown()`: no problems.
- `NEWS.md`: no entries for spider plots, `coord_spider()`, beeswarm (F104).
- `DESCRIPTION`: version 1.99.0.9031 (CLAUDE.md says 9028, F039); runtime use of Suggests packages already in F008.

## Findings

F100 (new); F008, F039, F104.

## End-to-end test cases

- [x] A10.01 The evaluated README chunks run without error or warning.
- [x] A10.02 (skipped: F100) The `eval = FALSE` README chunks run too.
- [x] A10.03 The pkgdown reference index lists every topic.
- [x] A10.04 (skipped: F104) NEWS mentions the features added since 1.0.0.
