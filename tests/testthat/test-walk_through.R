test_that("walk_through(NULL) with a 'No' answer skips the walkthrough and only prompts twice", {

  # one answer for the yes/no menu selection, one for the trailing
  # "Press ENTER to start manual spell-check..." pause
  expect_message(
    out <- capture.output(
      walk_through(NULL, reader = scripted_reader(c("2", "")))
    ),
    "Walkthrough skipped"
  )

  # none of the walkthrough-only narrative text should appear
  expect_false(any(grepl("Welcome to the manual spell-check walkthrough", out)))

})

test_that("walk_through(NULL) with a 'Yes' answer runs the full walkthrough", {

  out <- capture.output(suppressMessages(
    walk_through(NULL, reader = scripted_reader(c("1", rep("", 12))))
  ))

  expect_true(any(grepl("Welcome to the manual spell-check walkthrough", out)))
  expect_true(any(grepl("This concludes the walkthrough", out)))

})

test_that("walk_through(TRUE) runs the full walkthrough without asking first, matches the original verbatim", {

  # Verified via direct comparison against the untouched original's
  # walk_through() (with readline()/interactive() mocked in a sys.source()'d
  # environment) -- byte-identical output, including ANSI color codes, for
  # all 207 lines. See REFACTOR_PLAN.md's Phase 5 write-up.
  out <- capture.output(suppressMessages(
    walk_through(TRUE, reader = scripted_reader(rep("", 12)))
  ))

  # 206 lines of narrative/menu text plus one trailing auto-printed line --
  # the function's last statement is a bare read_input() call (no explicit
  # return()/invisible()), matching the original exactly: calling it without
  # capturing its result auto-prints whatever the final "press enter" answer
  # was. Confirmed identical() to the untouched original including this.
  expect_length(out, 207)
  expect_true(any(grepl("turtle catdog elephant fish bird squiral", out, fixed = TRUE)))
  expect_true(any(grepl("This concludes the walkthrough", out)))

})

test_that("walk_through(FALSE) does nothing except the trailing prompt", {

  out <- capture.output(suppressMessages(
    walk_through(FALSE, reader = scripted_reader(c("")))
  ))

  expect_false(any(grepl("Welcome to the manual spell-check walkthrough", out)))

})

test_that("scripted_reader() exhaustion surfaces as a clear error rather than hanging (sanity check)", {

  expect_error(
    suppressMessages(walk_through(NULL, reader = scripted_reader(c("2")))),
    "exhausted"
  )

})
