test_that("ind_word_check() matches the original on a single misspelled word", {

  # NOTE: returns a length-1 list, not a bare character scalar -- inherited
  # from moniker()'s own list-returning shape (see test-helpers.R's own
  # note on this from Phase 1: moniker() calls brit_us_conversion(), which
  # returns a list at dictionary = FALSE). Confirmed identical to the
  # untouched original via a direct reference comparison, not introduced here.
  result <- ind_word_check(
    "dogg", full_dictionary = SemNetDictionaries::animals.dictionary,
    dictionary = "animals", spelling = "US"
  )
  expect_equal(result, list("dog"))

})

test_that("ind_word_check() matches the original on multiple misspelled words", {

  result <- ind_word_check(
    "dogg catt", full_dictionary = SemNetDictionaries::animals.dictionary,
    dictionary = "animals", spelling = "US"
  )
  expect_equal(result, list("dog cat"))

})

test_that("ind_word_check() strips non-alphanumeric characters before checking", {

  result <- ind_word_check(
    "dog!", full_dictionary = SemNetDictionaries::animals.dictionary,
    dictionary = "animals", spelling = "US"
  )
  expect_equal(result, list("dog"))

})
