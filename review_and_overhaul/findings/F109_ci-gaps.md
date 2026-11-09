---
id: F109
title: CI does not test the declared minimum R version or a set-up without suggested packages, and the dev-version job is unverified
severity: medium
type: test
area: tests
status: fixed
verification: verified
effort: S
api_change: no
locations: [.github/workflows/check.yaml:31, .github/workflows/check_dev_versions.yaml:23, .github/workflows/check_dev_versions.yaml:51, codecov.yml:1]
related: [F008, F038, F039, F102]
origin: R06 sheet T08
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F109 CI gaps

## Summary

- **Minimum R version untested.** DESCRIPTION declares `R (>= 4.1.0)`; the matrix stops at `oldrel-3` (R 4.2 with R 4.5 current). An R 4.1.3 container run in R06 showed that plot2 itself works there (only test-suite issues, F038), so the claim holds today but nothing guards it.
- **No job without suggested packages.** All jobs install every Suggests (`needs: check`), so runtime use of suggested packages (F008) and unguarded use in vignettes (F102) and tests (F038) cannot be detected.
- **Dev-version job not verified.** `check_dev_versions.yaml` adds r-universe repositories but never prints or asserts which versions were installed, so it may silently test CRAN versions. It runs on every push to every branch and daily, like `check.yaml`, doubling the runs. Its comment "see R/zzz.R" refers to a file that does not exist (also in `check.yaml`).
- **Coverage not collected.** `codecov.yml` exists, but no workflow uploads coverage (see also F039).
- **Hygiene.** No `permissions:` block in the two check workflows (default token rights); actions pinned to major tags; macOS installs `proj` and `gdal` via Homebrew on every run, with `brew link --overwrite gcc`.

## Reproduction

```r
wf <- readLines(".github/workflows/check.yaml")
print(grep("r: '", wf, value = TRUE))
print(any(grepl("hard|_R_CHECK_FORCE_SUGGESTS_", unlist(lapply(list.files(".github/workflows", full.names = TRUE), readLines)))))
print(file.exists("R/zzz.R"))
# Expected after fix: an R 4.1 entry (or oldrel-4), a hard-dependencies job, no reference to R/zzz.R.
```

## Impact

Regressions in the R 4.1 promise, in dependency declarations and against ggplot2 development versions can reach users unnoticed. The double daily runs cost CI minutes.

## Proposed fix

Add `{os: ubuntu-latest, r: '4.1'}`; add a job with `dependencies: '"hard"'` plus `extra-packages: any::rcmdcheck, any::testthat, any::knitr, any::rmarkdown` and `_R_CHECK_FORCE_SUGGESTS_: false`; in the dev job, print `sessioninfo::package_info()` and run it on a schedule and on pull requests only; add a coverage job or delete `codecov.yml`; add `permissions: read-all`.

## History

- 2026-11-08: filed in R06 (sheet T08); tests T08.01 to T08.03 parked.
- 2026-11-08: fixed and verified (Phase 1). 2026-11-08: fixed in the workflows (R 4.1 job, hard-dependencies job with _R_CHECK_FORCE_SUGGESTS_ = false, permissions: read-all, stale R/zzz.R comments removed, dev-versions job on schedule and pull requests only and printing the installed versions). Status fixed, not verified, until the first CI run. Coverage upload left to F039 (codecov.yml). The maintainer prefers the minimum-R test in CI over local containers.
