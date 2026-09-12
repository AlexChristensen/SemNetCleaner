test_that("multiple_words() matches the original on simple, single-merge cases", {

  dict <- SemNetDictionaries::animals.dictionary

  result <- multiple_words("polar bear", multi_min = 2, full_dictionary = dict, dictionary = "animals", spelling = "US")
  expect_equal(result$response, c("polar bear", "bear"))
  expect_equal(result$correct, c(TRUE, TRUE))

})

test_that("multiple_words() leaves a single word untouched", {

  dict <- SemNetDictionaries::animals.dictionary
  result <- multiple_words("dog", multi_min = 2, full_dictionary = dict, dictionary = "animals", spelling = "US")
  expect_equal(result$response, "dog")
  expect_true(result$correct)

})

test_that("multiple_words() returns the already-combined word directly when it's a dictionary entry", {

  dict <- c(SemNetDictionaries::animals.dictionary, "polarbear")
  result <- multiple_words("polar bear", multi_min = 2, full_dictionary = dict, dictionary = "animals", spelling = "US")
  expect_equal(result$response, "polarbear")
  expect_true(result$correct)

})

test_that("multiple_words() index-staleness bug fix: correctly processes every word after multiple merges shrink the vector", {

  # Regression test for the confirmed bug mechanism: the original's
  # `for(i in misspl)` used indices computed once, before merges shrink
  # `spl` -- reproduced directly against the untouched original with
  # spl = "black board big game", where positions 3-4 ("big"/"game") never
  # actually get inspected once positions 1-2 merge (stale `i` runs past
  # the shrunk vector's length). The rebuilt version's while-loop pointer
  # re-derives from the CURRENT split_words every iteration, so it
  # actually finds and applies every real merge opportunity present,
  # regardless of how many earlier merges already happened.
  dict <- c("blackboard", "biggame")
  result <- multiple_words("black board big game", multi_min = 2, full_dictionary = dict, dictionary = NULL, spelling = "US")

  expect_equal(sort(result$response), sort(c("blackboard", "biggame")))
  expect_true(all(result$correct))

})

test_that("multiple_words() index-staleness bug fix: a later merge opportunity is found even after multiple earlier shrinks", {

  dict <- c("aabb", "ccdd", "gghh")
  result <- multiple_words("aa bb cc dd ee ff gg hh", multi_min = 2, full_dictionary = dict, dictionary = NULL, spelling = "US")

  # All three independent merges should be found and applied
  expect_true("aabb" %in% result$response)
  expect_true("ccdd" %in% result$response)
  expect_true("gghh" %in% result$response)

})

test_that("multiple_words() out-of-bounds access no longer occurs (bug fix, direct mechanism check)", {

  # This exact input previously ran an iteration with a stale index beyond
  # the shrunk vector's length in the original (silently indexing NA).
  # The rebuilt version should complete without error or warning and
  # correctly leave the genuinely unfixable word ("cc") alone.
  dict <- c("aabb", SemNetDictionaries::animals.dictionary[1:50])
  expect_silent(
    result <- multiple_words("aa bb cc", multi_min = 2, full_dictionary = dict, dictionary = "animals", spelling = "US")
  )
  expect_equal(result$response, c("aabb", "cc"))
  expect_equal(result$correct, c(TRUE, FALSE))

})

test_that("moniker() mis-argument bug fix: passing the loaded moniker list (not a bare dictionary name) actually converts monikers", {

  # Direct mechanism check for the confirmed bug: two of multiple_words()'s
  # three moniker() call sites (the before/after merge-candidate checks)
  # passed the bare dictionary NAME (e.g. "animals") as moniker()'s
  # `misnom` argument, instead of the actual loaded moniker list --
  # `unlist("animals")` is just `"animals"`, which no real word ever
  # matches, so moniker conversion was silently a no-op there. The third
  # call site (after the loop) already did this correctly, which is what
  # exposed the inconsistency. multiple_words() now loads the moniker
  # list once, up front, and passes it to all three call sites.
  monikers <- SemNetDictionaries::load.monikers("animals")

  old_buggy_call <- moniker("aligator", misnom = "animals", spelling = "US")
  fixed_call <- moniker("aligator", misnom = monikers, spelling = "US")

  expect_equal(unlist(old_buggy_call), "aligator")  # bug: left unconverted
  expect_equal(unlist(fixed_call), "alligator")     # fix: correctly converted

})

test_that("multiple_words() correctly de-combines a genuinely unrelated multi-word phrase", {

  dict <- c("fish", "dog", "cat")
  result <- multiple_words("fish dog cat", multi_min = 2, full_dictionary = dict, dictionary = NULL, spelling = "US")

  expect_equal(sort(result$response), c("cat", "dog", "fish"))
  expect_true(all(result$correct))

})

test_that("multiple_words(keep_strings = TRUE) keeps the original word when it can't be verified", {

  dict <- c("cat", "dog")
  result <- multiple_words("xyz qrs", multi_min = 2, full_dictionary = dict, dictionary = NULL, spelling = "US", keep_strings = TRUE)

  expect_equal(result$response, "xyz qrs")
  expect_false(result$correct)

})
