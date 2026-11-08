# S06 Sessions and deployment

## Checked

Everything that is shared between sessions or depends on where the app runs, with the threat model of the README.

Severity overview for a deployed app (one R process, several visitors); the maintainer set all security findings to critical (2026-11-08):

| Risk | Finding | Severity |
|---|---|---|
| Arbitrary R code from four inputs | F006 | critical |
| Deserialising uploaded R objects; uploads cannot be disabled | F111 | critical |
| One visitor's plot exported by another | F112 | critical |
| Files next to the logo downloadable | F113 | critical |
| Memory exhaustion through export size | F122 | critical |
| Global data frames of the host offered to visitors | F114 | medium |
| Session ends on errors (copy button, missing reader, odd names) | F115, F117 | medium |
| HTML from column names in messages (only where messages are shown) | F121 | medium |

- The app keeps no state in `plot2_env` that belongs to one session only; both items it stores there are shared and cleared by any `plot2()` call (F112).
- The documentation does not say whether deployment is supported; `install_shiny_deps()` addresses administrators, which suggests it is (F125).
- None of this matters for the intended local use by one person, except F112 (defaults lost), F114, F115, F116, F118 to F121, which also affect local use.

## Findings

F112, F114, F115, F122, F125 (new); F006.

## End-to-end test cases

- [x] S06.01 (skipped: F112) A session never sees the plot of another session.
- [x] S06.02 (skipped: F114) Outside an interactive session, global data frames are offered only when passed.
- [x] S06.03 (skipped: F115) Errors in server code do not end the session.
- [x] S06.04 (skipped: F112) An app instance leaves the global state of plot2 alone.
