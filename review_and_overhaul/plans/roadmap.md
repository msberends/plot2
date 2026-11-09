# Roadmap

Phases group findings (and later ideas) into an order of work. A finding can only be in one phase. Status lives in the finding files; this file only holds the order and the reasoning. Update it when phases change.

## Phase 0: safety net (before touching internals)

Decisions for the fix phases (maintainer, 2026-11-08): I002 for the Phase 1 sheets first; fixes on the branch review-and-overhaul; one commit per finding (fix, unparked test, finding status, NEWS entry); work autonomously through Phase 1, stopping only for decisions or failures. Findings with status needs-decision are not fixed without the maintainer's choice.

Goal: tests that would catch the defects found, so the refactors that follow are safe.

- I001 testthat infrastructure: helper and fixture files, accessors and expectations, edition 3, per-topic files (resolves F038).
- I002 end-to-end tests for all 24 `validate.R` functions, specified as 231 cases in `reviews/R02_validate/`. Start with sheets 03, 05, 06, 07, 08, 09, 23 (the functions touched in Phase 1).
- R03 end-to-end tests for `plot2_exec()` (15 files `test-plot2_exec-E*.R`, written in R03).
- Cases for open findings are written as desired behaviour and skipped with the finding ID; fixing a finding means removing its skip.

## Phase 1: quick correctness fixes (small, isolated)

Goal: stop wrong figures and crashes with minimal code movement.

- F002 RNG side effect
- F004 POSIXct category scale
- F005 theme function shadowing
- F007 data tested instead of arguments
- F009 "bar" regex
- F010 UpSet print fall-through
- F012 POSIXTt typo
- F015 dec_mark fallback
- F016 short hex colours
- F021 back-to-back facet expressions
- F023 category.percent
- F024 y.limits overwritten
- F026 factor y
- F027 garbled message
- F033 call. = FALSE
- F041 small-integer breaks drop negatives
- F042 percentage breaks not anchored
- F043 transformation objects
- F044 reverse expansion permutation
- F045 area dodged
- F046 histogram of constant values
- F047 fewer colours than categories
- F048 complete_direction types
- F049 column named n overwritten
- F051 horizontal label angle
- F053 summarise_function contract
- F054 x.limits on discrete x
- F056 title wrapping
- F065 numeric y vectors as column positions; back-to-back with small counts
- F067 category.focus with default colours
- F069 internal columns left in p$data
- F070 density y title
- F071 type requirement checks too late
- F072 long vector expressions crash
- F073 summarise_function = NULL crashes (with F053)
- F074 identical x and y give a histogram (with F007)
- F075 UpSet tidyselect deprecation
- F076 explicit datalabels dropped
- F077 y_secondary.colour ignored
- F078 boxplot fatten deprecated
- F079 y only on a single-column data frame (with F007)
- F080 sf method with x or y given
- F082 add_errorbar() without a category
- F083 move_layer() positive moves
- F085 add_smooth() legend on non-date x
- F086 spider radial labels and start
- F087 theme_minimal2(...) replaces elements
- F090 colour class lost on subsetting
- F091 theme sizes as grid units
- F092 zoom arguments
- F095 plot2.default_type aliases
- F096 colour_opacity with palettes (with F067)
- F097 barpercent without y
- F106 counting without a numeric column (with F097)
- F107 scientific-label sign and euro sign on pdf()
- F126 x.limits keeps the default expansion
- F127 linear breaks on a transformed x axis
- F128 x.mic with the current AMR package
- F129 horizontal numeric x gives an empty plot
- F131 violins with single-observation groups

## Documentation pass (with or after the related Phase 1 fixes)

Goal: help pages, examples, vignettes and README that match the behaviour. Correct texts only after the behaviour they describe is settled (for example F093 after F094 and D001).

- F093 argument descriptions versus behaviour
- F098 type and sorting values
- F099 typos and small errors
- F100 README examples
- F101 vignette prose
- F102 vignettes and Suggests
- F103 reproducible help pages
- F104 missing examples, return values, NEWS
- F109 CI: R 4.1, hard-dependencies job, dev-version verification, coverage
- I005 more outspoken tone in README, vignettes and website (rewrite texts once, with F100 and F101)

## Phase 2: state and messaging redesign

Goal: per-call context, real R conditions, predictable diagnostics.

- F001 shared state (core of this phase)
- F014 messages as conditions, plot2.silent
- F036 data flow (context object, central guessing step)

## Phase 3: dependencies, locale, fonts

- F008 Imports versus Suggests
- F003 locale (needs decision)
- F029 date formatting
- F011 fonts and showtext (needs decision)
- F108 dependency footprint (decide together with F008)

## Phase 4: input handling and parsing

- F017 like() and data-as-regex
- F018 markdown escaping
- F019 formula method
- F020 data argument (needs decision)
- F030 plot2.default lookup
- F031 type matching
- F013 numeric x heuristics (needs decision)
- F028 percentage denominator (needs decision)
- F022 secondary axis
- F068 secondary axis default type and colour (needs decision)
- F032 partial themes
- F040 add_type no-op
- F050 legend.position options (needs decision)
- F052 facet scales default (needs decision)
- F055 datalabel background vector
- F057 text_factor and rel()
- F059 category limits heuristic
- F060 fallback geom arguments
- F063 automatic scientific notation (needs decision)
- F081 formula after a pipe (with F019)
- F084 add_sf() colours, geometry name and nudge default
- F088 invalid colours collapse the colour vector; registration validation
- F089 plain matrix semantics
- F130 limits of length 1 (needs decision)
- I003 spider plots with facets

## Phase 5: structure and performance

- F035 single argument definition
- F037 ggplot2 internals
- F025 performance (now also scatter plots)
- F110 fixed overhead per call (theme caching)
- F105 titles in programmatic use (with F035 quosure capture)
- F034 documented defaults
- F094 visible defaults acting as sentinels (with F035; scope set by D001)
- F036 remainder: central guessing step for the data-driven defaults (maintainer, 2026-11-08: moved here from Phase 2; any `plot2.guess` option to be decided with D001/I004)
- I004 consolidated API proposal: implement D001 (decided) before the first CRAN release
- F058 taxonomy performance and scope
- F061 validate_colour hack
- F062 set_max_items details
- F064 and F066 back-to-back shared scales and colours: build composite types (back-to-back, UpSet) from one prepared data set with shared scales instead of nested `plot2()` calls; depends on F001.
- Split `validate_data()` along its 17 stages (see R02 sheet 03) as part of F036.

## Phase 6: Shiny app

Reviewed in R07 (sheets in `reviews/R07_shiny/`, tests `test-shiny-S*.R`). The security findings must be fixed before the app is deployed on any server; for local use by one person, F112, F114 to F121 matter on their own. Order:

1. Security, before any deployment (can be pulled forward): F006 safe call construction (language objects, whitelisted values, no `eval(parse())`); F113 serve the logo alone; F111 no R serialisation uploads by default, `upload_tab = FALSE` disables uploads; F122 export size budget; F112 per-session state in reactive values instead of `plot2_env` (also fixes the lost defaults).
2. Input specification, together with D001: F124 one explicit table of the app's inputs (tab, widget, kind of value, label, range), checked against `formals(plot2)`; this carries F118 (value types), F119 (missing and unusable inputs) and F123 (change tracking without observers).
3. Robustness and usability: F114 data set lookup by key, F115 errors as notifications, F116 Automatic type, F117 copy in the browser, F120 upload reader options, F121 messages on servers, escaped, with real error text.
4. F125 housekeeping and a documented statement on deployment.

## Housekeeping (any time)

- F039 project metadata

## End of the overhaul: traceability

Maintainer's decision (2026-11-08): when the overhaul ends, move findings that are still open to GitHub issues and replace the `F###` references in the tests (`skip_open_finding()`, `local_known_deprecation()`, test descriptions) by the issue numbers. Closed findings stay referenced by ID.
