dict <- SemNetDictionaries::animals.dictionary

test_that("describe_word_change() builds the correct first/middle/last position message", {

  expect_message(
    describe_word_change(c("catdog", "turtle", "elephant"), 1, "SKIPPED", "catdog"),
    "turtle elephant", fixed = TRUE
  )
  expect_message(
    describe_word_change(c("turtle", "catdog", "elephant"), 2, "SKIPPED", "catdog"),
    "turtle", fixed = TRUE
  )
  expect_message(
    describe_word_change(c("turtle", "elephant", "catdog"), 3, "SKIPPED", "catdog"),
    "turtle elephant", fixed = TRUE
  )

})

test_that("describe_word_change() targets the correct occurrence when the target word recurs elsewhere (bug #53 fix)", {

  # Real bug fixed: the middle-position branch located the target's own
  # index via `which(context[check] == unlist(strsplit(context, split =
  # " ")))` -- a VALUE match, not a position lookup. Confirmed via direct
  # reproduction: with `context = c("the","cat","sat","cat")` and
  # `check = 4` (the SECOND "cat"), `which()` returns BOTH matching
  # positions (c(2, 4)), and the downstream `1:(word_num - 1)` /
  # `(word_num + 1):n_words` arithmetic silently used only the first
  # ("only the first element used" warning), building the message around
  # the WRONG occurrence. Fixed by using `check` directly as the position
  # -- correct regardless of duplicate word values.
  expect_no_warning(
    expect_message(
      describe_word_change(c("the", "cat", "sat", "cat"), 4, "SKIPPED", "cat"),
      "the cat sat", fixed = TRUE
    )
  )

})

test_that("offer_add_missing_to_dictionary() only prompts for words not already in the dictionary", {

  # "cat" is already in the dictionary -- no prompt needed for it
  result <- offer_add_missing_to_dictionary(c("cat"), dict, reader = scripted_reader(character(0)))
  expect_identical(result, dict)

})

test_that("offer_add_missing_to_dictionary() adds a missing word on 'Yes'", {

  result <- offer_add_missing_to_dictionary(c("zzznotarealword"), dict, reader = scripted_reader("y"))
  expect_true("zzznotarealword" %in% result)

})

test_that("offer_add_missing_to_dictionary() does not add a missing word on 'No'", {

  result <- offer_add_missing_to_dictionary(c("zzzalsonotreal"), dict, reader = scripted_reader("n"))
  expect_false("zzzalsonotreal" %in% result)

})

test_that("offer_add_missing_to_dictionary() works against a large (>= 3000-word) dictionary via dictionary_contains()'s hashed branch", {

  # Speed fix, per direct request: `words %in% full_dictionary` was
  # computed twice against whatever dictionary was passed in -- costly
  # against `cocaspell` (~109k words, the default `free`-association
  # dictionary). Replaced with one `dictionary_contains()` call, feeding
  # both the entry check and `non_dict`. Verified here against the real
  # large dictionary so the hashed (>= 3000-word) branch is exercised, not
  # just the small-dictionary `%in%` fallback.
  big_dict <- SemNetDictionaries::load.dictionaries("cocaspell")

  result <- offer_add_missing_to_dictionary(c("the", "zzznotarealwordatall"), big_dict, reader = scripted_reader("n"))
  expect_false("zzznotarealwordatall" %in% result)

})

test_that("spellcheck_menu() fast-paths a word already in the dictionary (context mode) without prompting", {

  # Real bug fixed (bug #32): the original built
  # `res$target <- ifelse(is.null(context), dict_check, context)` --
  # `ifelse()` is vectorized on `test` (length 1 here), so it always
  # returns a length-1 result, silently truncating a multi-word `context`
  # down to just `context[1]`. Confirmed via direct reproduction against
  # the untouched original (returned "turtle" instead of the full 3-word
  # context). Fixed via a plain if/else.
  ctx <- c("turtle", "cat", "elephant")
  result <- suppressMessages(spellcheck_menu(
    check = 2, context = ctx, possible = c("cat", "dog"), original = "turtle cat elephant",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
    reader = scripted_reader(character(0))
  ))

  expect_equal(result$target, ctx)
  expect_false(result$go_back)
  expect_false(result$end)

})

test_that("spellcheck_menu() fast-paths a word already in the dictionary (single mode)", {

  result <- suppressMessages(spellcheck_menu(
    check = "cat", context = NULL, possible = c("cat", "dog"), original = "cat",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
    reader = scripted_reader(character(0))
  ))

  expect_equal(result$target, "cat")
  expect_null(result$end)

})

test_that("spellcheck_menu() SKIP WORD leaves the context unchanged, at first/middle/last position", {

  for(ctx in list(
    c("catdog", "turtle", "elephant"),
    c("turtle", "catdog", "elephant"),
    c("turtle", "elephant", "catdog")
  )){
    check <- which(ctx == "catdog")
    result <- suppressMessages(spellcheck_menu(
      check = check, context = ctx, possible = c("cat", "dog"), original = paste(ctx, collapse = " "),
      current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
      dictionary = "animals", keep_strings = TRUE, go_back_count = 1,
      reader = scripted_reader("1")
    ))
    expect_equal(result$target, ctx)
  }

})

test_that("spellcheck_menu() ADD WORD TO DICTIONARY adds the target word to full_dictionary", {

  ctx <- c("turtle", "catdog", "elephant")
  result <- suppressMessages(spellcheck_menu(
    check = 2, context = ctx, possible = c("cat", "dog"), original = "turtle catdog elephant",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = TRUE, go_back_count = 1,
    reader = scripted_reader("2")
  ))

  expect_true("catdog" %in% result$full_dictionary)

})

test_that("spellcheck_menu() TYPE MY OWN WORD updates the target word and change log", {

  ctx <- c("turtle", "catdog", "elephant")
  result <- suppressMessages(spellcheck_menu(
    check = 2, context = ctx, possible = c("cat", "dog"), original = "turtle catdog elephant",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
    reader = scripted_reader(c("3", "cat, dog"))
  ))

  expect_equal(result$target, c("turtle", "cat", "dog", "elephant"))
  expect_true("1" %in% names(result$changes))

})

test_that("spellcheck_menu() BAD WORD sets the target word to 'NA' and logs the change", {

  ctx <- c("turtle", "catdog", "elephant")
  result <- suppressMessages(spellcheck_menu(
    check = 2, context = ctx, possible = c("cat", "dog"), original = "turtle catdog elephant",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
    reader = scripted_reader("5")
  ))

  expect_equal(result$target, c("turtle", "NA", "elephant"))
  expect_equal(unname(result$changes[["1"]][1, "to_1"]), NA_character_)

})

test_that("spellcheck_menu() KEEP ORIGINAL reverts the whole string and ends the response", {

  ctx <- c("turtle", "catdog", "elephant")
  result <- suppressMessages(spellcheck_menu(
    check = 2, context = ctx, possible = c("cat", "dog"), original = "turtle catdog elephant",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
    reader = scripted_reader(c("6", "n"))
  ))

  expect_equal(result$target, "turtle catdog elephant")
  expect_true(result$end)

})

test_that("spellcheck_menu() KEEP AUTO-CORRECT keeps context as-is and ends the response", {

  ctx <- c("turtle", "catdog", "elephant")
  result <- suppressMessages(spellcheck_menu(
    check = 2, context = ctx, possible = c("cat", "dog"), original = "turtle catdog elephant",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
    reader = scripted_reader("7")
  ))

  expect_equal(result$target, ctx)
  expect_true(result$end)

})

test_that("spellcheck_menu() TYPE MY OWN STRING replaces the whole context and ends the response", {

  ctx <- c("turtle", "catdog", "elephant")
  result <- suppressMessages(spellcheck_menu(
    check = 2, context = ctx, possible = c("cat", "dog"), original = "turtle catdog elephant",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
    reader = scripted_reader(c("8", "dog, cat, bird"))
  ))

  expect_equal(result$target, c("dog", "cat", "bird"))
  expect_true(result$end)

})

test_that("spellcheck_menu() BAD STRING marks every word NA and ends the response (bug #33 fix)", {

  # Real bug fixed: the original's BAD STRING branch never set `end <-
  # TRUE`, unlike every other whole-string-resolving branch (KEEP ORIGINAL,
  # KEEP AUTO-CORRECT, TYPE MY OWN STRING). Confirmed via direct
  # reproduction against the untouched original: it sets every word in
  # `context` to "NA" but leaves `res$end` FALSE, so
  # spellcheck_dictionary()'s multi-word loop would not stop early --
  # continuing to search for the next not-yet-checked word inside a
  # context that has already been entirely overwritten to "NA".
  ctx <- c("turtle", "catdog", "elephant")
  result <- suppressMessages(spellcheck_menu(
    check = 2, context = ctx, possible = c("cat", "dog"), original = "turtle catdog elephant",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
    reader = scripted_reader("10")
  ))

  expect_equal(result$target, rep("NA", 3))
  expect_true(result$end)

})

test_that("spellcheck_menu() GO BACK sets go_back without changing the target", {

  ctx <- c("turtle", "catdog", "elephant")
  result <- suppressMessages(spellcheck_menu(
    check = 2, context = ctx, possible = c("cat", "dog"), original = "turtle catdog elephant",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
    reader = scripted_reader("b")
  ))

  expect_true(result$go_back)

})

test_that("spellcheck_menu() shows HELP then continues to the requested option", {

  ctx <- c("turtle", "catdog", "elephant")
  result <- suppressMessages(spellcheck_menu(
    check = 2, context = ctx, possible = c("cat", "dog"), original = "turtle catdog elephant",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
    reader = scripted_reader(c("h", "", "1"))
  ))

  expect_equal(result$target, ctx)

})

test_that("spellcheck_menu() returns 'STOP' on an empty or 'x' answer", {

  ctx <- c("turtle", "catdog", "elephant")
  result <- suppressMessages(spellcheck_menu(
    check = 2, context = ctx, possible = c("cat", "dog"), original = "turtle catdog elephant",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
    reader = scripted_reader("")
  ))

  expect_equal(result, "STOP")

})

test_that("spellcheck_menu() applies a response (best-guess) option by letter", {

  ctx <- c("turtle", "catdog", "elephant")
  result <- suppressMessages(spellcheck_menu(
    check = 2, context = ctx, possible = c("cat", "dog"), original = "turtle catdog elephant",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
    reader = scripted_reader("q")
  ))

  expect_equal(result$target, c("turtle", "cat", "elephant"))

})

test_that("spellcheck_menu() GOOGLE WORD/STRING call the injected search_fn with a correctly-collapsed query (bug #7 fix)", {

  # Real bug fixed (bug #7): the original passed a misspelled `collpase =
  # ""` argument to paste() -- silently absorbed into `...` instead of
  # setting `collapse`, so the intended collapse never happened. Fixed to
  # `collapse = ""`. Verified here via an injected search_fn (a real
  # browser launch would be just as wrong to fire in a test as a real
  # readline() block).
  captured <- NULL
  mock_search <- function(query, site, rlang){ captured <<- list(query = query, site = site) }

  ctx <- c("turtle", "catdog", "elephant")
  suppressMessages(spellcheck_menu(
    check = 2, context = ctx, possible = c("cat", "dog"), original = "turtle catdog elephant",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
    reader = scripted_reader(c("4", "1")), search_fn = mock_search
  ))

  expect_equal(captured$query, "animal 'catdog'")
  expect_equal(captured$site, "google")

})

## ---- single-response branch ----

test_that("spellcheck_menu() single-response SKIP/ADD/TYPE-OWN/BAD/GO-BACK/RESPONSE all work", {

  skip_result <- suppressMessages(spellcheck_menu(
    check = "catdog", context = NULL, possible = c("cat", "dog"), original = "catdog",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
    reader = scripted_reader("1")
  ))
  expect_equal(skip_result$target, "catdog")

  add_result <- suppressMessages(spellcheck_menu(
    check = "catdog", context = NULL, possible = c("cat", "dog"), original = "catdog",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
    reader = scripted_reader("2")
  ))
  expect_true("catdog" %in% add_result$full_dictionary)

  own_result <- suppressMessages(spellcheck_menu(
    check = "catdog", context = NULL, possible = c("cat", "dog"), original = "catdog",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
    reader = scripted_reader(c("3", "cat, dog"))
  ))
  expect_equal(own_result$target, c("cat", "dog"))

  bad_result <- suppressMessages(spellcheck_menu(
    check = "catdog", context = NULL, possible = c("cat", "dog"), original = "catdog",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
    reader = scripted_reader("5")
  ))
  expect_equal(bad_result$target, "NA")

  go_back_result <- suppressMessages(spellcheck_menu(
    check = "catdog", context = NULL, possible = c("cat", "dog"), original = "catdog",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
    reader = scripted_reader("b")
  ))
  expect_true(go_back_result$go_back)

  response_result <- suppressMessages(spellcheck_menu(
    check = "catdog", context = NULL, possible = c("cat", "dog"), original = "catdog",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
    reader = scripted_reader("q")
  ))
  expect_equal(response_result$target, "cat")

  stop_result <- suppressMessages(spellcheck_menu(
    check = "catdog", context = NULL, possible = c("cat", "dog"), original = "catdog",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
    reader = scripted_reader("x")
  ))
  expect_equal(stop_result, "STOP")

})
