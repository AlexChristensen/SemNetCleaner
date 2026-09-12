test_that("advance_wizard(): a fresh wizard call with an always-empty reader stops at the first manual decision, with $pending populated", {

  raw <- data.frame(
    ID = c("P1", "P2"), R1 = c("catdog", "bird"), R2 = c("cat", "fish"),
    stringsAsFactors = FALSE
  )

  # An always-"" reader is exactly what textcleaner_shiny()'s automated-pass
  # step (mod_auto_progress.R) uses: run the full (parallelizable)
  # auto_spellcheck() pass, then pause at the FIRST decision manual review
  # actually needs, instead of driving the console wizard for real
  initial <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = "animals", spelling = "US",
    reader = function(prompt = "") "", edit_fn = function(x) x
  ))

  expect_true(isTRUE(initial$stop))
  expect_equal(initial$pending$check, "catdog")
  expect_null(initial$pending$context)

})

test_that("advance_wizard() + build_answer_burst() end to end: a single SKIP decision completes the wizard and the response survives unchanged", {

  raw <- data.frame(
    ID = c("P1", "P2"), R1 = c("catdog", "bird"), R2 = c("cat", "fish"),
    stringsAsFactors = FALSE
  )

  initial <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = "animals", spelling = "US",
    reader = function(prompt = "") "", edit_fn = function(x) x
  ))

  final <- suppressMessages(advance_wizard(initial, build_answer_burst(1)))

  expect_false(isTRUE(final$stop))
  expect_s3_class(final, "textcleaner")
  expect_equal(unname(final$responses$clean["P1", ]), c("catdog", "cat"))

})

test_that("advance_wizard() + build_answer_burst() end to end: TYPE MY OWN WORD with an add-to-dictionary answer resolves correctly (the correctness-critical burst-sizing case)", {

  raw <- data.frame(
    ID = c("P1", "P2"), R1 = c("catdog", "bird"), R2 = c("cat", "fish"),
    stringsAsFactors = FALSE
  )

  initial <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = "animals", spelling = "US",
    reader = function(prompt = "") "", edit_fn = function(x) x
  ))

  # menu choice 3 = TYPE MY OWN WORD (single-response branch): types
  # "zzznotarealword", confirmed NOT in the animals dictionary (unlike
  # "zebra", which is already a real dictionary entry and would make this
  # test's premise wrong) -- offer_add_missing_to_dictionary() will prompt
  # exactly once about it, and build_answer_burst() must supply exactly one
  # more answer for that prompt (add_to_dictionary = FALSE here)
  expect_false("zzznotarealword" %in% SemNetDictionaries::animals.dictionary)

  burst <- build_answer_burst(
    3, typed_text = "zzznotarealword", add_to_dictionary = FALSE,
    full_dictionary = SemNetDictionaries::animals.dictionary
  )
  expect_equal(length(burst), 3)

  final <- suppressMessages(advance_wizard(initial, burst))

  expect_false(isTRUE(final$stop))
  expect_equal(unname(final$responses$clean["P1", ]), c("zzznotarealword", "cat"))

})

test_that("advance_wizard() resumes a multi-word response's decision using $pending's own context/check (Phase 11 render-context regression guard)", {

  raw <- data.frame(
    ID = c("P1", "P2"), R1 = c("turtle catdog elephant", "bird"), R2 = c("cat", "fish"),
    stringsAsFactors = FALSE
  )

  initial <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = "animals", spelling = "US",
    reader = function(prompt = "") "", edit_fn = function(x) x
  ))

  expect_true(isTRUE(initial$stop))
  expect_equal(initial$pending$context, c("turtle", "catdog", "elephant"))
  expect_equal(initial$pending$check, 2L)

  final <- suppressMessages(advance_wizard(initial, build_answer_burst(1)))

  expect_false(isTRUE(final$stop))
  expect_equal(unname(final$responses$clean["P1", ]), c("turtle", "catdog", "elephant", "cat"))

})
