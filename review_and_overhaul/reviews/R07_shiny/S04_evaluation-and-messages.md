# S04 Evaluation, messages and errors

## Checked

`output$plot` (987 to 1023): evaluation of the generated code, message capture, error handling.

- The code string is evaluated with `eval(parse())` in the server's environment: all three code routes of F006 lead here and were confirmed by writing a marker file (S04.05 to S04.07).
- plot2's messages are printed only when `interactive()`, so the messages panel is empty on a server (F121).
- Messages are converted with `fansi::sgr_to_html()`, which does not escape HTML, and inserted with `shiny::HTML()`; column names in messages become markup (F121).
- Errors are replaced by "This code contains an error."; warnings are not shown (F121).
- The plot renders at 100 dpi into the page; `plot2_env$last_shiny_plot` is set here (F112).

## Findings

F121 (new); F006 (extended), F112.

## End-to-end test cases

- [x] S04.01 The plot is rendered.
- [x] S04.02 (skipped: F121) An error is shown with its own message.
- [x] S04.03 (skipped: F121) plot2's messages are shown on a server.
- [x] S04.04 (skipped: F121) Messages are shown as text, not as HTML.
- [x] S04.05 (skipped: F006) Text typed in a direction selector is not run.
- [x] S04.06 (skipped: F006) Text typed as a title is not run.
- [x] S04.07 (skipped: F006) Text typed as `summarise_function` is not run.
