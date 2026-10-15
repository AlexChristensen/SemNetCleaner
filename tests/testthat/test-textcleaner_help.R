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

test_that("textcleaner_help() describes the keepStrings = TRUE menu numbering (no KEEP AUTO-CORRECT, BAD STRING is 9)", {

  # Real bug fixed: the legend always described the keepStrings = FALSE
  # numbering, so under keepStrings = TRUE it listed "7: KEEP AUTO-CORRECT"
  # (7 is TYPE MY OWN STRING there) and "10: BAD STRING" (BAD STRING is 9)
  show_help <- function(keep_strings){
    paste(capture.output(suppressMessages(
      textcleaner_help(
        check = 2, context = c("turtle", "catdog", "elephant"), original = "turtle catdog elephant",
        possible = c("cat", "dog"), reader = scripted_reader(""), keep_strings = keep_strings
      )
    )), collapse = "\n")
  }

  strip <- function(x) gsub("\033\\[[0-9;]*m", "", x)

  default <- strip(show_help(FALSE))
  expect_match(default, "7: KEEP AUTO-CORRECT", fixed = TRUE)
  expect_match(default, "10: BAD STRING", fixed = TRUE)

  kept <- strip(show_help(TRUE))
  expect_no_match(kept, "KEEP AUTO-CORRECT", fixed = TRUE)
  expect_match(kept, "6: KEEP ORIGINAL", fixed = TRUE)
  expect_match(kept, "7: TYPE MY OWN STRING", fixed = TRUE)
  expect_match(kept, "8: GOOGLE STRING", fixed = TRUE)
  expect_match(kept, "9: BAD STRING", fixed = TRUE)
  expect_no_match(kept, "10: BAD STRING", fixed = TRUE)

})

test_that("textcleaner_help() can open the legend in a separate window instead of the console", {

  # Added so the help can sit beside the console while working through the
  # menu. `open_fn` stands in for utils::browseURL() so no browser is started
  opened <- NULL
  open_fn <- function(path){ opened <<- path; invisible(NULL) }

  # no ENTER is requested (the reader has no answers to give) and the console
  # legend is not printed
  out <- capture.output(
    messages <- testthat::capture_messages(
      textcleaner_help(
        check = 2, context = c("turtle", "catdog"), original = "turtle catdog",
        possible = c("cat", "dog"), reader = scripted_reader(character(0)),
        window = TRUE, open_fn = open_fn
      )
    )
  )

  expect_length(out, 0)
  expect_match(paste(messages, collapse = ""), "separate window")
  expect_true(file.exists(opened))

  page <- paste(readLines(opened, warn = FALSE), collapse = "\n")
  expect_match(page, "SKIP WORD", fixed = TRUE)
  expect_match(page, "BAD STRING", fixed = TRUE)
  expect_match(page, "GO BACK", fixed = TRUE)

  # falls back to the console legend when the window cannot be opened
  out <- capture.output(
    suppressMessages(textcleaner_help(
      check = "catdog", context = NULL, original = "catdog", possible = c("cat", "dog"),
      reader = scripted_reader(""), window = TRUE, open_fn = function(path) stop("no browser")
    ))
  )
  expect_true(any(grepl("1: SKIP WORD", out)))

})

test_that("the help window describes every menu label, numbered as the menu numbers them (both keepStrings settings)", {

  for(keep_strings in c(TRUE, FALSE)){

    labels <- unique(c(wizard_menu_labels(NULL, keep_strings), wizard_menu_labels(c("a", "b"), keep_strings)))
    expect_true(all(labels %in% names(help_descriptions)))

    page <- build_help_html(keep_strings)

    multi <- wizard_menu_labels(c("a", "b"), keep_strings)
    for(i in seq_along(multi)){
      expect_match(page, sprintf('<span class="key">%d</span><span class="what"><strong>%s</strong>', i, multi[i]), fixed = TRUE)
    }

  }

  # the options-based switch keeps the console help
  withr::local_options(SemNetCleaner.help_window = FALSE)
  expect_false(help_window_enabled())
  withr::local_options(SemNetCleaner.help_window = TRUE)
  expect_true(help_window_enabled())

})

test_that("textcleaner_help() shows the Auto-corrected string section only for the menu that shows it", {

  show <- function(ks) paste(gsub("\033\\[[0-9;]*m", "", capture.output(suppressMessages(textcleaner_help(
    check = 2, context = c("big", "dgo", "runs"), original = "big dgo runs", possible = c("dog", "dig"),
    reader = scripted_reader(""), keep_strings = ks, window = FALSE)))), collapse = "\n")

  expect_true(grepl("Auto-corrected string:", show(FALSE), fixed = TRUE))
  expect_false(grepl("Auto-corrected string:", show(TRUE), fixed = TRUE))

})
