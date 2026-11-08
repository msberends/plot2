---
id: I004
title: Consolidated proposal for argument names, defaults and conventions of plot2()
area: architecture
status: open
effort: L
api_change: yes
related: [D001, F094, F093, F035, F034, F020, F098, F050]
origin: R05 sheet A01
created: 2026-11-08
updated: 2026-11-08
---

# I004 Consolidated API proposal

## Idea

Collect every naming and default inconsistency found in R05 into one proposal with one deprecation path, instead of scattered renames. Basis: the generated inventory `reviews/R05_api/A00_argument-inventory.md` (153 arguments in 13 groups). This is a proposal for the maintainer; nothing here is decided (see D001).

## Motivation

The dot-prefix scheme (`x.`, `y.`, `category.`, `facet.`, ...) works well and is the package's main navigational aid. Where it is broken, users have to remember exceptions. The package is not yet on CRAN, is considered beta and has no known public users (maintainer, 2026-11-08). Breaking changes are therefore free until, and not later than, the first CRAN release; after that, every rename needs a deprecation path.

## Observations

**A. Naming rule is not applied uniformly.**

1. Geom-specific settings are grouped for `smooth.*` and `sankey.*`, but not for `jitter_seed` and `violin_scale` (core arguments with snake case).
2. Abbreviation `lbl` in `x.lbl_angle`, `x.lbl_align`, `x.lbl_italic`, `x.lbl_taxonomy`, `facet.repeat_lbls_x/_y`, while the same concept is spelled out in `x.labels`, `category.labels`.
3. Fill colours: `colour_fill`, `datalabels.colour_fill`, `y_secondary.colour_fill`, but `facet.fill`.
4. Two arguments for one concept: `category.title` and `legend.title`; `reverse` (category values) next to `legend.reverse` (legend order).
5. `colour_opacity` is documented as "0 = solid, 1 = transparent": that is transparency, the inverse of what "opacity" (and ggplot2's `alpha`, used by `smooth.alpha` and `sankey.alpha`) means. `get_colour(opacity = )` has the same inversion.
6. Three meanings of "type": `type` (geom), `y_secondary.type` (geom), `category.type` (aesthetic).
7. The only group prefix with snake case is `y_secondary`.
8. Options: `dec_mark` and `big_mark` lack the `plot2.` prefix all other options have.
9. `stacked` and `stacked_fill` are two logicals for one three-valued choice (dodge, stack, fill).

**B. Coverage per direction is uneven.** `x` has `date_breaks`, `date_labels`, `date_remove_years`, `character`, `drop`, `mic`, `zoom`, `lbl_*`; `category` has `date_*`, `character`, `percent`; `y` has `percent`, `scientific`, `age`, `24h`; `facet` has none of these. Titles: `title.colour` and `subtitle.colour`, but no `caption.colour`; `title.linelength` and `subtitle.linelength` only. Some asymmetry is natural (y is the value axis); a short table of "what exists for which direction" in the documentation would help either way.

**C. Defaults.** 18 arguments show a value in the usage but act on `missing()` (F094); `x.expand` documents an internal sentinel (F093); the generic and the methods disagree on `y_secondary.colour(_fill)` (F034); method-specific defaults are undocumented (F104).

**D. Domain-specific arguments in the core API.** `x.mic` and `x.lbl_taxonomy` serve microbiology (via the AMR package). They are useful, but they are the only domain-specific arguments among 153 general ones. A question for the maintainer, not a proposal to remove them.

## Possible approach

1. **Conventions** (written down once, in `?plot2` and CLAUDE.md): `<direction>.<property>`, property in snake case; geom-specific settings grouped as `<geom>.<property>`; spelled-out words (`labels`, not `lbl`); `colour_fill` for fills everywhere; ggplot2 vocabulary where it exists (`alpha`); base R names kept where users know them (`na.rm`, `decimal.mark`, `big.mark`); `NULL` means "automatic" and is shown as such.
2. **Candidate renames** (old name kept as alias during deprecation): `jitter_seed` to `jitter.seed`; `violin_scale` to `violin.scale`; `x.lbl_angle`/`_align`/`_italic`/`_taxonomy` to `x.labels_angle`/... (or `x.label_angle`); `facet.repeat_lbls_x/_y` to `facet.repeat_labels_x/_y`; `facet.fill` to `facet.colour_fill`; `colour_opacity` to `colour_alpha` with inverted meaning, or keep the name and invert the meaning (breaking either way); options `dec_mark`/`big_mark` to `plot2.decimal.mark`/`plot2.big.mark` (old options read as fallback); `stacked` plus `stacked_fill` to one argument (`stacked = FALSE | TRUE | "fill"`), keeping `stacked_fill` as alias.
3. **Merge duplicates**: `legend.title` as alias of `category.title` (or the reverse); keep `reverse` and `legend.reverse` but document the difference next to each other.
4. **Defaults**: `NULL` for every automatic default (F094); one table of method-specific defaults in `?plot2-methods`.
5. **No deprecation path before CRAN.** Since there are no public users, renames and default changes can be made directly, without aliases or deprecation messages, as long as they land before the first CRAN release. Only the package's own callers need updating in the same change: README, vignettes, examples, tests and the Shiny app (R07). F035 (single argument definition) is not a prerequisite for renaming, but doing F035 first means each rename touches one definition instead of eight. After the first CRAN release, any further rename would need the usual route (alias with a message, then a warning, then removal).

## Open questions

- Which changes must be in place before the first CRAN release (the hard deadline)?
- Which renames are worth the churn? Option 2 above lists all candidates; a minimal set would be F094 (sentinels), `colour_opacity` (semantics) and the options prefix.
- Keep `x.mic` and `x.lbl_taxonomy` in core, or move them to an extension or helper?

## History

- 2026-11-08: filed in R05 as the consolidated API proposal decided at the start of the round; decision in D001.
- 2026-11-08: maintainer: not on CRAN, beta, no known public users, so breaking changes are allowed until (not later than) the first CRAN release. Deprecation path replaced by direct renames.
- 2026-11-08: decided item by item in D001 (verified); D001's Consequences section is the binding list of changes. Status set to open (to be implemented before the first CRAN release).
