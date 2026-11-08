---
id: F108
title: Imports pull in heavy packages for one or two functions, crayon duplicates cli, and grDevices is imported but listed in Suggests
severity: low
type: dependency
area: dependencies
status: open
verification: verified
effort: M
api_change: no
locations: [DESCRIPTION:25, NAMESPACE:63, NAMESPACE:64, NAMESPACE:66, NAMESPACE:183]
related: [F008, F011, D001]
origin: R06 sheet T06
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# F108 Dependency footprint

## Summary

Counted per package (NAMESPACE imports and `pkg::` calls in `R/`, recursive hard dependencies from the installed library, excluding base packages; the reproduction counts them including base packages, hence 27 and 20):

| package | in | used for | recursive deps |
|---|---|---|---|
| broom | Imports | `augment()` once, as a fallback for model objects (`R/utils.R:784`) | 21 |
| cleaner | Imports | `percentage()`, `as.percentage()` | 14 |
| crayon | Imports | 8 style functions for messages, while `cli` (in Suggests) is called unconditionally for the same messages (F008) | 0 |
| forcats | Imports | `fct_inorder()`, `fct_reorder()`, `fct_relevel()` in sorting | 10 |
| showtext, showtextdb, sysfonts | Imports | custom fonts only (F011) | 3 |
| grDevices | Suggests | 8 functions imported in NAMESPACE | base |
| AMR | Suggests | `x.mic`, `x.lbl_taxonomy` (moving to helpers, D001.3) | |

16 of the 39 suggested packages are used only by the Shiny app (arrow, bslib, clipr, DT, fansi, haven, jsonlite, nanoparquet, pillar, readODS, readxl, rio, shiny, shinyjs, xml2, yaml), so every CI job and every `R CMD check` with Suggests installs them.

## Reproduction

```r
ns <- readLines("NAMESPACE")
print(grep("^importFrom\\((broom|cleaner|crayon|grDevices),", ns, value = TRUE))
ip <- installed.packages()
print(lengths(tools::package_dependencies(c("broom", "cleaner"), db = ip, recursive = TRUE)))
# Expected after fix: broom and cleaner in Suggests or not needed; one styling package; grDevices in Imports.
```

## Impact

Installation weight and exposure to upstream changes for small gains; `grDevices` in Suggests while imported is formally inconsistent.

## Proposed fix

Move `broom` to Suggests (use it only for model objects, with `check_installed()`); replace `cleaner::percentage()` by a small formatter (or `scales`, which is needed anyway, F008); drop `crayon` in favour of `cli`; move `grDevices` to Imports; consider moving the font packages to Suggests together with F011. Decide with F008 in one dependency change.

## History

- 2026-11-08: filed in R06 (sheet T06).
