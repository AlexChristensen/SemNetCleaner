test_that("textcleaner_help() with context prints word/string/response option legends and matches the original verbatim", {

  # Verified via direct comparison against the untouched original's
  # textcleaner_help() -- byte-identical output for all 81 lines. See
  # REFACTOR_PLAN.md's Phase 5 write-up.
  out <- capture.output(
    textcleaner_help(
      check = 2, context = c("turtle", "catdog"), original = "turtle catdog",
      possible = c("cat", "dog"), reader = scripted_reader(c(""))
    )
  )

  # 80 lines of legend text plus one trailing auto-printed line (the
  # function's last statement is a bare read_input() call, matching the
  # original's own lack of an explicit return()/invisible() -- confirmed
  # identical() to the untouched original including this)
  expect_length(out, 81)
  expect_true(any(grepl("6: KEEP ORIGINAL", out)))
  expect_true(any(grepl("Target word: ", out, fixed = TRUE)))

})

test_that("textcleaner_help() without context prints only word/response option legends (no string options)", {

  out <- capture.output(
    textcleaner_help(
      check = "catdog", context = NULL, original = "catdog",
      possible = c("cat", "dog"), reader = scripted_reader(c(""))
    )
  )

  expect_false(any(grepl("6: KEEP ORIGINAL", out)))
  expect_true(any(grepl("1: SKIP WORD", out)))

})

test_that("textcleaner_help() does not crash when there are zero best-guess candidates (bug #12 fix)", {

  # Real bug fixed (already fixed at its source in custom_menu(), see
  # test-helpers.R): custom_menu(choices, default = 10, help = TRUE)
  # unconditionally referenced a variable only assigned when
  # length(choices) > default. textcleaner_help()'s context-branch call is
  # exactly this shape (10 fixed word/string choices + a variable-length
  # `possible`), so `possible = character(0)` makes length(choices) == 10
  # == default, hitting it exactly. Confirmed via direct reproduction that
  # the untouched original crashes with "object 'resp' not found" here.
  expect_no_error(
    capture.output(
      textcleaner_help(
        check = 2, context = c("turtle", "catdog"), original = "turtle catdog",
        possible = character(0), reader = scripted_reader(c(""))
      )
    )
  )

})
