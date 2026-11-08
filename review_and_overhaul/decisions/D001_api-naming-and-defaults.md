---
id: D001
title: Scope and timing of argument renames and default changes (API proposal I004)
area: architecture
status: verified
related: [I004, F094, F093, F035, F034]
origin: R05
created: 2026-11-08
updated: 2026-11-08
---

# D001 API naming and defaults

## Context

R05 collected all naming and default inconsistencies of `plot2()` in one proposal, I004, as the maintainer asked at the start of the round. The package is not on CRAN, is beta and has no known public users, so breaking changes cost nothing until the first CRAN release and become expensive after it; that release is the deadline for this decision's changes. Root causes are F094 (visible defaults acting as sentinels), F093 (descriptions that differ from behaviour) and F035 (arguments duplicated across eight functions, which makes any rename expensive until it is fixed).

## Options

1. **Fix defaults only, keep all names.** Make automatic defaults `NULL` (F094), correct the texts (F093, F099), document conventions for new arguments. Pros: no breaking change, small effort. Cons: the inconsistencies in I004 A.1 to A.9 stay; `colour_opacity` keeps its inverted meaning.
2. **Minimal renames before CRAN.** Option 1 plus `colour_opacity` semantics, the `plot2.` prefix for the two unprefixed options, and `stacked`/`stacked_fill` merged, renamed directly without aliases. Pros: fixes what misleads, limited churn. Cons: the remaining naming exceptions become permanent at the CRAN release.
3. **Full harmonisation before CRAN.** All candidate renames of I004, made directly. Pros: one consistent scheme, fixed at the only moment it is free. Cons: largest effort; best done after F035; every example, the vignettes, tests and the Shiny app change.
4. **Defer the choice to after R06** (cross-cutting themes), still before CRAN. Pros: decision with more information. Cons: none, provided the decision precedes the CRAN release.

## Decision

Partly decided by the maintainer on 2026-11-08. The scope options 1 to 4 above are not chosen as a group: the maintainer consents per item (answer, verbatim: "I think it's in between 1 to 3, ... I need to consent per item and can't decide on the group level here"). Items decided so far:

| # | Item | Decision |
|---|---|---|
| D001.1 | `colour_opacity` (0 = solid, 1 = transparent) | Rename to `colour_transparency`, keeping the current meaning. |
| D001.2 | Representation of automatic defaults (F094) | `NULL` in the usage, documented as "determined automatically"; explicit values mean what they say. |
| D001.3 | `x.mic` and `x.lbl_taxonomy` | Move out of `plot2()` into separate helpers (e.g. a labeller or scale function). |
| D001.4 | `get_colour(opacity = )` | Rename to `transparency`, same meaning (follows D001.1). |
| D001.5 | `jitter_seed` | Keep: `jitter` is not a `plot2()` argument, so snake case is correct under D001.R. |
| D001.6 | `violin_scale` | Keep (rejected `violin.scale`). |
| D001.7 | `x.lbl_angle`, `x.lbl_align`, `x.lbl_italic` | Rename to `x.label_angle`, `x.label_align`, `x.label_italic` (first rejected, then revised by the maintainer together with D001.8). |
| D001.8 | `facet.repeat_lbls_x`, `facet.repeat_lbls_y` | Rename to `facet.repeat_labels_x`, `facet.repeat_labels_y`. |
| D001.R | Naming rule | `<argument>.<setting>` with the setting in snake case, where the part before the dot is itself a `plot2()` argument (x, y, category, facet, y_secondary, title, subtitle, datalabels, smooth); everything else plain snake case. Documented exceptions: base-R names `na.rm`, `na.replace`, `decimal.mark`, `big.mark`. |
| D001.9 | `sankey.node_width`, `sankey.node_whitespace`, `sankey.alpha`, `sankey.remove_axes` | Rename to `sankey_node_width`, `sankey_node_whitespace`, `sankey_alpha`, `sankey_remove_axes` (no `sankey` argument). |
| D001.10 | `legend` argument | New argument `legend = NULL` (automatic), `TRUE` forces, `FALSE` hides. Principle (maintainer): the legend is only the plot element, the box explaining what is done in `category`. The eight `legend.*` settings keep their names (they now follow D001.R). |
| D001.11 | `legend.position` | Keeps `"none"` as an alternative way to hide; `NULL` means automatic (D001.2). Inside placement remains part of F050. |
| D001.12 | `legend.title` and `category.title` | Keep both; giving both stops with an error. |
| D001.14 | `facet.fill` | Rename to `facet.colour_fill`. |
| D001.15 | `stacked`, `stacked_fill` | Keep `stacked_fill`; `stacked` additionally accepts `"fill"` (equivalent to `stacked_fill = TRUE`). On conflict, `stacked_fill = TRUE` wins (always a filled stack). |
| D001.16 | Options `dec_mark`, `big_mark` | Rename to `plot2.decimal.mark`, `plot2.big.mark`. |
| D001.17 | Functions `dec_mark()`, `big_mark()` | Keep the names; they read the renamed options. |
| D001.18 | `category.type` | Keep. Maintainer's reasoning: plot2 puts `type` and `category` at the front and keeps geoms and aesthetics in the background; the name follows the user's view, not ggplot2's internals. |
| D001.19 | Prefix `y_secondary` | Keep (valid under D001.R). |

Design principle stated by the maintainer during this decision (binding for later API work): plot2 exists to streamline ggplot2, whose API requires knowledge of hundreds of arguments across tens of functions. User-facing concepts (`type`, `category`) come first and ggplot2 concepts (geoms, aesthetics) stay in the background. Follow ggplot2's names where that is not unclear or complex, depart from them where it helps users.

All candidate items of I004 are now decided. Not breaking, so not part of this decision: adding counterparts such as `caption.colour`, and documenting `reverse` next to `legend.reverse`.
| D001.13 | `legend.value` in `add_line()`, `add_point()`, `add_col()`, `add_smooth()` | Keep, as a documented exception to the legend principle of D001.10. |

The remaining candidate items of I004 are listed for per-item consent; status is open until each is decided.

## Consequences

Breaking changes to make before the first CRAN release, directly and without aliases (README, vignettes, examples, tests, the Shiny app and NEWS change in the same commit; doing F035 first reduces the work):

- Renames: `colour_opacity` to `colour_transparency`; `get_colour(opacity)` to `transparency`; `x.lbl_*` to `x.label_*`; `facet.repeat_lbls_*` to `facet.repeat_labels_*`; `sankey.*` to `sankey_*`; `facet.fill` to `facet.colour_fill`; options `dec_mark`/`big_mark` to `plot2.decimal.mark`/`plot2.big.mark`.
- New: argument `legend` (NULL/TRUE/FALSE); `stacked = "fill"`.
- Changed: automatic defaults shown as `NULL` (F094); error when both `category.title` and `legend.title` are given.
- Removed from `plot2()`: `x.mic` and `x.lbl_taxonomy` (moved to helpers).
- Documentation: the naming rule D001.R and its exceptions (base-R names; `legend.value` in `add_*()`) in `?plot2` and CLAUDE.md.

## History

- 2026-11-08: opened in R05.
- 2026-11-08: options revised after the maintainer stated that the package is not on CRAN, is beta and has no known public users: no deprecation path needed, deadline is the first CRAN release.
- 2026-11-08: D001.1 to D001.3 decided; group-level scope rejected in favour of per-item consent.
- 2026-11-08: all items decided (D001.1 to D001.19, rule D001.R); status verified.
