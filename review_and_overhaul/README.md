# Review and overhaul of plot2

This folder is the working system for scrutinising and overhauling `plot2`. It is excluded from the package build (`.Rbuildignore`) and is meant to grow over many sessions: new review rounds, ideas, decisions and fixes all land here.

**Start here:** [`INDEX.md`](INDEX.md) (generated overview of everything), then [`JOURNAL.md`](JOURNAL.md) (what happened last).

## Structure

| Folder / file | Contains | ID scheme |
|---|---|---|
| `findings/` | One file per defect, risk or weakness found in the code | `F001`, `F002`, ... |
| `ideas/` | One file per idea or wish (new features, API changes, future scrutiny topics) | `I001`, `I002`, ... |
| `decisions/` | One file per design decision (architecture, API, behaviour), ADR style | `D001`, `D002`, ... |
| `reviews/` | One file per review round: scope, method, coverage, outcome. A round may have a subfolder with working sheets (e.g. `reviews/R02_validate/`, one sheet per function with its test specification); subfolders are not indexed | `R01`, `R02`, ... |
| `plans/` | Roadmap (`roadmap.md`), planned review rounds (`review_rounds.md`) and work packages | free names |
| `repro/` | Longer reproduction and benchmark scripts (short ones live inside the finding) | free names |
| `templates/` | Templates for each item type; copy, never edit in place | |
| `tools/` | Base-R helper scripts (no extra dependencies) | |
| `coverage.md` | Per source file: how deeply it has been reviewed, and when | |
| `JOURNAL.md` | Dated log of every session working in this folder, newest first | |
| `INDEX.md` | **Generated**, never edit by hand | |

## Single source of truth

Every item file starts with YAML-like frontmatter (`key: value`, lists as `[a, b]`). The index, counts and checks are derived from that frontmatter only. To change the status of something, edit its frontmatter and regenerate:

```sh
Rscript review_and_overhaul/tools/build_index.R
```

The script also validates the frontmatter (required fields, allowed values, unique IDs, dangling `related` references) and stops with a clear message when something is wrong.

## Field vocabulary

| Field | Allowed values | Applies to |
|---|---|---|
| `severity` | `critical`, `high`, `medium`, `low` | findings |
| `type` | `bug`, `design`, `performance`, `security`, `maintainability`, `test`, `docs`, `dependency`, `i18n` | findings |
| `status` | `open`, `in-progress`, `needs-decision`, `fixed`, `verified`, `wontfix`, `duplicate` | findings, ideas |
| `verification` | `verified` (reproduced in R), `reading` (found by reading code only) | findings |
| `effort` | `S` (< 1 h), `M` (half a day), `L` (days), `XL` (multi-session) | findings, ideas |
| `api_change` | `no`, `possible`, `yes` | findings, ideas |
| `area` | free, but reuse existing ones: `architecture`, `messaging`, `i18n`, `scales`, `theme`, `shiny`, `data-validation`, `dependencies`, `types`, `fonts`, `colours`, `markdown`, `methods`, `performance`, `tests`, `housekeeping` | all |
| `related` | list of other IDs, e.g. `[F001, I003]` | all |
| `origin` | the review round or source, e.g. `R01 §1.1`, `user`, `fix of F004` | all |

Status flow for findings: `open` -> `in-progress` -> `fixed` (code changed, reference commit) -> `verified` (reproduction no longer fails, regression test added). `needs-decision` means the fix depends on a choice by the maintainer; link the `D` file once it exists.

## Conventions

- British spelling; no em dashes (use commas or colons).
- Line references (`R/plot2.R:886`) are valid for the commit named in the item's `commit` field. When code moves, update the reference or note the new commit.
- Each finding has a **Reproduction** section with a self-contained R chunk, run from the package root after `devtools::load_all()`. The chunk ends with a comment stating the expected result after the fix. Run them with `Rscript review_and_overhaul/tools/run_repro.R F004` (or `all`).
- Every fix should add a regression test in `tests/testthat/` and mention it under **History** in the finding.
- Never delete items. Close them with a status (`wontfix`, `duplicate`) and a one-line reason under **History**.
- Every working session adds an entry to `JOURNAL.md`.

## Adding things

1. Copy the matching file from `templates/` to the right folder, named `<ID>_<short-slug>.md` (next free ID: see `INDEX.md`, or `tools/build_index.R` prints it).
2. Fill in the frontmatter and sections.
3. Run `tools/build_index.R`.
4. Add a line to `JOURNAL.md`.

A new review round (`R02`, ...) gets its own file in `reviews/`, updates `coverage.md`, and files each result as a finding with `origin: R02 §x`.
