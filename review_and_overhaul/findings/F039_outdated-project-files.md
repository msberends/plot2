---
id: F039
title: Project metadata outdated (CLAUDE.md, codecov.yml) and stray files not build-ignored
severity: low
type: docs
area: housekeeping
status: open
verification: verified
effort: S
api_change: no
locations: [CLAUDE.md, codecov.yml, .Rbuildignore]
related: []
origin: R01 §5.2
commit: d09e04d
created: 2026-11-07
updated: 2026-11-07
---

# F039 Outdated project files

## Summary

- `CLAUDE.md` mentions `plot2.lm` (no such method; `lm` objects go through `plot2.default` and `broom::augment()`), `R/universal.R` (does not exist) and version 1.99.0.9028 (DESCRIPTION: 1.99.0.9031).
- `codecov.yml` excludes the non-existent `R/universal.R`.
- `Rplots.pdf` in the repository root is neither git-ignored nor build-ignored; `.claude` is not in `.Rbuildignore`.

## Reproduction

```r
print(file.exists("R/universal.R"))
print(exists("plot2.lm"))
# Expected after fix: documentation matches the code.
```

## Proposed fix

Update CLAUDE.md and codecov.yml; add `^Rplots\.pdf$` and `^\.claude$` to `.Rbuildignore`.

## History

- 2026-11-07: filed in R01.
