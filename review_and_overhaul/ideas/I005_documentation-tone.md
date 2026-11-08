---
id: I005
title: State plot2's case against ggplot2's user experience more directly in README, vignettes and website
area: housekeeping
status: open
effort: M
api_change: no
related: [D001, F100, F101]
origin: user
created: 2026-11-08
updated: 2026-11-08
---

# I005 A more outspoken documentation tone

## Idea

From the maintainer, during D001: "perhaps we can be a bit more aggressive (but polite) in the explanations/readme/website/etc. ggplot is just a dev's masterpiece and an absolute infuriating experience for users: horrible."

## Motivation

The README and vignette now present plot2 as a "friendly companion" to ggplot2. The maintainer's view is that ggplot2's API demands knowledge of hundreds of arguments across tens of functions, and that plot2 exists to remove that burden (design principle recorded in D001). A clearer statement of that problem explains why plot2 makes the choices it does (`type` and `category` up front, geoms and aesthetics in the background).

## Open questions

- Where: README opening, vignette introduction, pkgdown home page, DESCRIPTION.
- How far: firm but polite; acknowledge ggplot2's strengths as a developer tool while being plain about its cost for users.

## Possible approach

Combine with the documentation pass (F100, F101) so that the texts are rewritten once. Draft for the maintainer's review before publishing.

## History

- 2026-11-08: filed from the maintainer's remark during D001.
