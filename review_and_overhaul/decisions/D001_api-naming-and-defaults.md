---
id: D001
title: Scope and timing of argument renames and default changes (API proposal I004)
area: architecture
status: open
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

To be filled in by the maintainer.

## Consequences

Depend on the option. Options 2 and 3 need no aliases or deprecation messages; README, vignettes, examples, tests, the Shiny app (R07) and NEWS change in the same commit. Doing F035 first reduces the work. All of it must precede the first CRAN release.

## History

- 2026-11-08: opened in R05.
- 2026-11-08: options revised after the maintainer stated that the package is not on CRAN, is beta and has no known public users: no deprecation path needed, deadline is the first CRAN release.
