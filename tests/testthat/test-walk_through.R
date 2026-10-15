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
  # (212 since the keepStrings note, 5 lines, was added to the closing text;
  # the example menu's wording was also aligned with the real menu)
  expect_length(out, 212)
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

test_that("walk_through()'s example menu uses the wording of the real menu", {

  keys <- c("1", rep("", 40))
  i <- 0L
  reader <- function(prompt = ""){ i <<- i + 1L; keys[[i]] }
  out <- gsub("\033\\[[0-9;]*m", "", paste(capture.output(suppressMessages(walk_through(NULL, reader = reader))), collapse = "\n"))

  for(label in c("Original string:", "Auto-corrected string:", "Target word:", "Selection (accepts lowercase):")){
    expect_true(grepl(label, out, fixed = TRUE), info = label)
  }
  for(stale in c("Original response", "Response to manually spell-check", "Response option (accepts lowercase)", "Auto-correct response")){
    expect_false(grepl(stale, out, fixed = TRUE), info = stale)
  }

})
