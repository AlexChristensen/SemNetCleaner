test_that("wizard_menu_labels() returns the single-response choice set", {

  expect_equal(
    wizard_menu_labels(context = NULL, keep_strings = FALSE),
    c("SKIP", "ADD TO DICTIONARY", "TYPE MY OWN", "GOOGLE IT", "BAD WORD")
  )

})

test_that("wizard_menu_labels() returns 9 labels (5 word + 4 string) when keep_strings = TRUE", {

  labels <- wizard_menu_labels(context = c("a", "b"), keep_strings = TRUE)

  expect_equal(length(labels), 9)
  expect_equal(labels[6], "KEEP ORIGINAL")
  expect_equal(labels[7], "TYPE MY OWN STRING")
  expect_false("KEEP AUTO-CORRECT" %in% labels)

})

test_that("wizard_menu_labels() returns 10 labels (5 word + 5 string) when keep_strings = FALSE", {

  labels <- wizard_menu_labels(context = c("a", "b"), keep_strings = FALSE)

  expect_equal(length(labels), 10)
  expect_equal(labels[6], "KEEP ORIGINAL")
  expect_equal(labels[7], "KEEP AUTO-CORRECT")
  expect_equal(labels[8], "TYPE MY OWN STRING")

})

test_that("wizard_response_letters() assigns QWERTY letters in order, capped at 10", {

  expect_equal(wizard_response_letters(3), c("Q", "W", "E"))
  expect_equal(length(wizard_response_letters(15)), 10)

})

test_that("end to end: sending wizard_menu_labels()'s own displayed position for KEEP ORIGINAL correctly reverts a multi-word response (keep_strings = FALSE)", {

  # Real hazard this guards against: spellcheck_menu()'s internal
  # `if(keep_strings){if(ans >= 7 ...) ans <- ans + 1}` remap means the RAW
  # number to send for the SAME visible label differs depending on
  # keep_strings -- this test (keep_strings = FALSE, no remap needed) and
  # the one below (keep_strings = TRUE, remap needed) both drive the real
  # wizard via the exact displayed position, proving no separate remap
  # logic is needed in the Shiny UI itself.
  raw <- data.frame(
    ID = c("P1", "P2"), R1 = c("turtle catdog elephant", "bird"), R2 = c("cat", "fish"),
    stringsAsFactors = FALSE
  )

  initial <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = "animals", spelling = "US",
    keepStrings = FALSE, reader = function(prompt = "") "", edit_fn = function(x) x
  ))

  labels <- wizard_menu_labels(initial$pending$context, initial$pending$keep_strings)
  keep_original_position <- which(labels == "KEEP ORIGINAL")

  # Real hazard found via a genuine hang while building this test (see
  # build_answer_burst.R's own header): KEEP ORIGINAL has its own
  # independent dictionary-membership check on the WHOLE, un-split original
  # response text ("turtle catdog elephant") -- essentially never itself a
  # dictionary entry -- which triggers one more yes/no prompt this burst
  # must account for via `original_check`.
  burst <- build_answer_burst(
    keep_original_position, original_check = initial$pending$original,
    full_dictionary = SemNetDictionaries::animals.dictionary
  )
  expect_equal(length(burst), 2)

  final <- suppressMessages(advance_wizard(initial, burst))

  expect_false(isTRUE(final$stop))
  # KEEP ORIGINAL reverts the WHOLE response to its pre-auto-correct text,
  # un-split (regardless of keep_strings, for type = "fluency" --
  # spellcheck_dictionary.R's own `target <- result$target` always takes
  # the whole-string replacement path here, never the per-word one, which
  # only ever applies for type = "free")
  expect_equal(unname(final$responses$clean["P1", ]), c("turtle catdog elephant", "cat"))

})

test_that("end to end: sending wizard_menu_labels()'s own displayed position for TYPE MY OWN STRING correctly resolves (keep_strings = TRUE, exercises the internal remap)", {

  raw <- data.frame(
    ID = c("P1", "P2"), R1 = c("turtle catdog elephant", "bird"), R2 = c("cat", "fish"),
    stringsAsFactors = FALSE
  )

  initial <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = "animals", spelling = "US",
    keepStrings = TRUE, reader = function(prompt = "") "", edit_fn = function(x) x
  ))

  labels <- wizard_menu_labels(initial$pending$context, initial$pending$keep_strings)
  type_own_string_position <- which(labels == "TYPE MY OWN STRING")

  burst <- build_answer_burst(
    type_own_string_position, typed_text = "turtle, dog, elephant",
    add_to_dictionary = FALSE, full_dictionary = SemNetDictionaries::animals.dictionary
  )

  final <- suppressMessages(advance_wizard(initial, burst))

  expect_false(isTRUE(final$stop))
  # keep_strings = TRUE keeps a resolved multi-word response joined as ONE
  # string (not split back into separate response slots) -- confirmed via
  # spellcheck_dictionary.R's own `if(keep_strings){target <- paste(target,
  # collapse = " ")}` -- so the real assertion is against the joined string,
  # not individual elements
  expect_true("turtle dog elephant" %in% unname(final$responses$clean["P1", ]))
  expect_false(any(grepl("catdog", unname(final$responses$clean["P1", ]))))

})
