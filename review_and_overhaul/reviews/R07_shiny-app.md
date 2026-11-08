---
id: R07
title: Review of the Shiny app create_interactively(), with server tests
status: verified
commit: d09e04d
created: 2026-11-08
updated: 2026-11-08
---

# R07 The Shiny app

## Scope

`R/shiny.R` in full (1,270 lines): `create_interactively()`, its UI and server, and the helpers `make_input()`, `create_field()`, `label_with_dims()` and `install_shiny_deps()`. Including F006.

Maintainer's decisions (2026-11-08): sheets per part of the app with `shiny::testServer()` tests; review only, F006 too; current code reviewed, D001 dependencies noted; server deployment in scope, so every input is treated as untrusted.

## Method

Every line read. The server was driven with `shiny::testServer()` against the app object that `create_interactively()` returns outside an interactive session, including two sessions of one app to check shared state. Code execution was confirmed only by writing a marker file into a temporary folder. The export at the app's maximum size was not executed (memory). One sheet and one test file per part of the app (`reviews/R07_shiny/`, `tests/testthat/test-shiny-S*.R`); new helpers in `helper-plot2.R` with a self-test. Each parked test was run once without its skip and failed; all others pass. Widget probe: `repro/shiny_widgets_r07.R`.

A correction made during the round, recorded for transparency: one test first failed because the stored plot was read after a second `plot2()` call; the cause turned out to be a defect itself (every `plot2()` call clears `plot2_env`, F112), and the test now reads the plot first.

## Coverage

All of `R/shiny.R`. Not covered: the browser side (JavaScript, CSS, layout), since shinytest2 is not installed; behaviour under Shiny Server or Posit Connect themselves.

## Outcome

15 new findings, F111 to F125 (4 critical, 8 medium, 3 low). F006 (critical) verified: four routes to arbitrary code instead of the one described. All security findings are critical (maintainer, 2026-11-08).

- Critical (security): F111 (R serialisation uploads; uploads cannot be disabled), F112 (state in `plot2_env`: shared between sessions, cleared by every `plot2()` call), F113 (the folder of the logo is served), F122 (export size unbounded).
- Medium: F114 (data set lookup by name), F115 (errors end the session), F116 (Automatic type again), F117 (copy button uses the server's clipboard), F118 (value types guessed), F120 (upload reader options), F121 (messages panel), F124 (inputs derived by pattern, coupled to D001).
- Low: F119 (missing and unusable inputs), F123 (change tracking), F125 (housekeeping, deployment statement).

Headline conclusions:

1. **The app is not safe to deploy on a server.** Four inputs run arbitrary R code (F006), uploads deserialise R objects (F111), files next to the logo are downloadable (F113), visitors can receive each other's plots (F112) and exhaust memory (F122). For local use by one person these are not risks, but the documentation does not say which use is supported (F125).
2. **Code generation is the root of most defects.** Building code as text and evaluating it causes the injection, the escaping and quoting defects and the guessed value types (F006, F116, F118). Building a language object and deparsing it for display solves all of them together.
3. **The inputs follow plot2's arguments by pattern, not by design** (F124). D001 will change many names and defaults before the first CRAN release; without an explicit input specification the app will change silently with it. Best done in one step with D001.
4. **plot2's own state handling reaches into the app** (F112): `clear_plot2_env()` at the start of every `plot2()` call wipes the app's stored defaults and last plot, the same shared-state design that F001 addresses for plot2 itself.
5. **On a server the app loses its explanations** (F121): plot2's messages only print in interactive sessions.

Test result: see JOURNAL.md (session 6).

Suggested next step: no review rounds are planned after R07. The fix phases of `plans/roadmap.md` can start, with D001 (I004) before the first CRAN release; the Shiny security fixes (Phase 6, step 1) if the app is to be deployed earlier.
