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

test_that("spellcheck_menu() TYPE MY OWN WORD/STRING treat blank input as 'go back' instead of crashing", {

  # Real bug fixed: pressing ENTER at "Use commas for multiple words" split
  # into zero words, and building the change matrix then failed with
  # "length of 'dimnames' [2] not equal to array extent". Blank (or comma-
  # only) input now returns to the menu, like typing '30'.
  ctx <- c("turtle", "catdog", "elephant")

  for(blank in c("", "  ", ",", " , ")){

    # TYPE MY OWN WORD, then blank, then SKIP WORD (1)
    word_result <- suppressMessages(spellcheck_menu(
      check = 2, context = ctx, possible = c("cat", "dog"), original = "turtle catdog elephant",
      current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
      dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
      reader = scripted_reader(c("3", blank, "1"))
    ))

    expect_equal(word_result$target, ctx)

    # TYPE MY OWN STRING, then blank, then KEEP AUTO-CORRECT (7)
    string_result <- suppressMessages(spellcheck_menu(
      check = 2, context = ctx, possible = c("cat", "dog"), original = "turtle catdog elephant",
      current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
      dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
      reader = scripted_reader(c("8", blank, "7"))
    ))

    expect_equal(string_result$target, ctx)
    expect_true(string_result$end)

  }

})

test_that("parse_typed_words() splits on commas and drops blanks", {

  expect_equal(parse_typed_words("dog, cat ,bird"), c("dog", "cat", "bird"))
  expect_equal(parse_typed_words(""), character(0))
  expect_equal(parse_typed_words(" , ,"), character(0))
  expect_equal(parse_typed_words("french window"), "french window")

})

test_that("spellcheck_menu() single-response TYPE MY OWN treats blank input as 'go back'", {

  result <- suppressMessages(spellcheck_menu(
    check = "catdog", context = NULL, possible = c("cat", "dog"), original = "catdog",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
    reader = scripted_reader(c("3", "", "1"))
  ))

  expect_equal(result$target, "catdog")

})

test_that("spellcheck_menu() context mode recognizes GO BACK / HELP after a mistyped answer (no word corrupted to NA)", {

  # Real bug fixed: GO BACK/HELP were matched against the FIRST thing typed
  # (`original_ans == "b"`), so "zz" followed by "b" fell through to the
  # RESPONSE OPTION branch and replaced the target word with NA
  # (`possible[ans - 10]` out of range)
  ctx <- c("turtle", "catdog", "elephant")

  for(ks in c(FALSE, TRUE)){

    back <- suppressMessages(spellcheck_menu(
      check = 2, context = ctx, possible = c("cat", "dog", "cow"), original = "turtle catdog elephant",
      current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
      dictionary = "animals", keep_strings = ks, go_back_count = 1,
      reader = scripted_reader(c("zz", "b"))
    ))

    expect_true(back$go_back)
    expect_equal(back$target, ctx)

    # HELP after a mistyped answer shows help (one ENTER) and keeps asking
    helped <- suppressMessages(spellcheck_menu(
      check = 2, context = ctx, possible = c("cat", "dog", "cow"), original = "turtle catdog elephant",
      current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
      dictionary = "animals", keep_strings = ks, go_back_count = 1,
      reader = scripted_reader(c("99", "h", "", "1"))
    ))

    expect_false(helped$go_back)
    expect_equal(helped$target, ctx)

  }

})

test_that("spellcheck_menu() with keepStrings = TRUE and no suggestions treats 'b' as GO BACK, not BAD STRING", {

  # Real bug fixed: with `keep_strings = TRUE` and an empty `possible`,
  # GO BACK's position (length(choices) + 1 = 10) collided with BAD STRING's
  # renumbered position (9 -> 10), and the BAD STRING branch came first --
  # pressing "b" marked EVERY word of the response NA and ended it
  ctx <- c("turtle", "catdog", "elephant")

  result <- suppressMessages(spellcheck_menu(
    check = 2, context = ctx, possible = character(0), original = "turtle catdog elephant",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = TRUE, go_back_count = 1,
    reader = scripted_reader("b")
  ))

  expect_true(result$go_back)
  expect_equal(result$target, ctx)
  expect_false(isTRUE(result$end))
  expect_length(result$changes, 0)

  # ...while the real BAD STRING (9 when keep_strings = TRUE) still works
  bad <- suppressMessages(spellcheck_menu(
    check = 2, context = ctx, possible = character(0), original = "turtle catdog elephant",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = TRUE, go_back_count = 1,
    reader = scripted_reader("9")
  ))

  expect_equal(bad$target, rep("NA", 3))
  expect_true(bad$end)

})

test_that("normalize_typed_words() applies the data's cleaning to typed words but leaves the NA marker alone", {

  normalize <- function(words){ normalize_responses(words, "-", FALSE) }

  expect_equal(normalize_typed_words(c("Dog", "CAT!", "123", "polar bear"), normalize), c("dog", "cat", "polar bear"))
  expect_equal(normalize_typed_words(c("NA", "Dog"), normalize), c("NA", "dog"))
  expect_equal(normalize_typed_words(c("Dog"), NULL), "Dog")
  expect_equal(normalize_typed_words(character(0), normalize), character(0))
  expect_equal(normalize_typed_words("123", normalize), character(0))

})

test_that("spellcheck_menu() cleans typed replacement words with normalize_fn (typed 'Dog' must not become a second vocabulary entry)", {

  # Real bug fixed: typed words bypassed the lowercase/punctuation/number
  # cleaning every other response went through
  normalize <- function(words){ normalize_responses(words, "-", FALSE) }

  single <- suppressMessages(spellcheck_menu(
    check = "catdog", context = NULL, possible = c("cat", "dog"), original = "catdog",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
    reader = scripted_reader(c("3", "Dog, CAT!, 123")), normalize_fn = normalize
  ))

  expect_equal(single$target, c("dog", "cat"))

  # A typed answer that cleans away to nothing returns to the menu, like blank input
  empty <- suppressMessages(spellcheck_menu(
    check = "catdog", context = NULL, possible = c("cat", "dog"), original = "catdog",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
    reader = scripted_reader(c("3", "123", "1")), normalize_fn = normalize
  ))

  expect_equal(empty$target, "catdog")

})

test_that("menu answers and Y/N responses tolerate surrounding whitespace", {

  result <- suppressMessages(spellcheck_menu(
    check = "catdog", context = NULL, possible = c("cat", "dog"), original = "catdog",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
    reader = scripted_reader(c(" q "))
  ))

  expect_equal(result$target, "cat")
  expect_equal(yes_no_menu("Continue?", reader = scripted_reader(" Y ")), 1)
  expect_equal(appropriate_answer(" 2 ", c("a", "b", "c"), default = 3, reader = scripted_reader(character(0))), 2)

})

test_that("spellcheck_menu() decides a repeated word once and replaces every occurrence (free-association multi-word fix)", {

  # Real bug fixed (found driving the free-association multi-word menu): a
  # response that repeats the unknown word ("teh cat teh") hands the menu
  # several positions at once (`check = c(1, 3)`). The confirmation message
  # used `if(check == 1)` (error "the condition has length > 1"), and TYPE MY
  # OWN WORD/BAD WORD used `context_change[[check]] <- ...`, which indexes
  # recursively with several positions
  ctx <- c("teh", "cat", "teh")
  run <- function(answers, keep_strings){
    suppressMessages(spellcheck_menu(
      check = c(1L, 3L), context = ctx, possible = c("the", "tea"), original = "teh cat teh",
      current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
      dictionary = "animals", keep_strings = keep_strings, go_back_count = 1,
      reader = scripted_reader(answers)
    ))
  }

  for(keep_strings in c(TRUE, FALSE)){

    suggestion <- run(if(keep_strings) "q" else "q", keep_strings)
    expect_equal(suggestion$target, c("the", "cat", "the"))

    typed <- run(c("3", "dog, fish", "n"), keep_strings)
    expect_equal(typed$target, c("dog", "fish", "cat", "dog", "fish"))

    bad <- run("5", keep_strings)
    expect_equal(bad$target, c("NA", "cat", "NA"))

    # the change log records the word once, not once per occurrence
    expect_equal(unname(suggestion$changes[["1"]][, "from"]), "teh")

  }

  message_text <- paste(
    testthat::capture_messages(describe_word_change(c("teh", "cat", "teh"), c(1L, 3L), "CHANGED TO", "the")),
    collapse = ""
  )
  expect_match(gsub("\033\\[[0-9;]*m", "", message_text), "the cat the", fixed = TRUE)

})

test_that("spellcheck_menu() 'x' exits from the re-prompt after an invalid selection (both menus)", {

  # The menu says "X to EXIT" at every prompt; the re-prompt after an invalid
  # answer used to reject it
  single <- suppressMessages(capture.output(res <- spellcheck_menu(
    check = "dgo", context = NULL, possible = c("dog", "dig"), original = "dgo",
    current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
    dictionary = "animals", keep_strings = FALSE, go_back_count = 1,
    reader = scripted_reader(c("99", "x"))
  )))
  expect_identical(res, "STOP")

  for(ks in c(TRUE, FALSE)){
    capture.output(res <- suppressMessages(spellcheck_menu(
      check = 2, context = c("big", "dgo", "runs"), possible = c("dog", "dig"), original = "big dgo runs",
      current_index = 1, changes = list(), full_dictionary = dict, category = "animal",
      dictionary = "animals", keep_strings = ks, go_back_count = 1,
      reader = scripted_reader(c("99", ""))
    )))
    expect_identical(res, "STOP")
  }

})
