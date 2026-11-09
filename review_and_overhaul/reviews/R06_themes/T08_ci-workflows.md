# T08 CI workflows

`.github/workflows/check.yaml`, `check_dev_versions.yaml`, `website.yaml` (first review of these files).

## Checked

- `check.yaml`: 7 jobs (macOS, Windows release; Ubuntu devel, release, oldrel-1 to oldrel-3) on every push to every branch, every pull request and daily. All Suggests installed; no hard-dependencies job; R 4.1 not covered (F109).
- `check_dev_versions.yaml`: r-universe repositories added; no verification of installed versions; same triggers as `check.yaml`; stale reference to `R/zzz.R` in both files (F109). R06 ran the suite against ggplot2 development itself: passes (T01).
- `website.yaml`: pkgdown on pushes to main, deploys to `gh-pages`; `permissions: read-all` with `contents: write` for the job. Fine.
- `codecov.yml` without a coverage workflow (F109, F039).
- No `permissions:` block in the check workflows; macOS installs `gdal` via Homebrew in every run (F109).

## Findings

F109 (new); F039.

## End-to-end test cases

- [x] T08.01 (skipped: F109) Matrix includes the declared minimum R version.
- [x] T08.02 (skipped: F109) A job without suggested packages.
- [x] T08.03 (skipped: F109) No references to non-existent files.
