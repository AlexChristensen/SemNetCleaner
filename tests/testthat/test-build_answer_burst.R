test_that("build_answer_burst() validates its inputs", {

  expect_error(build_answer_burst(c(1, 2)), "build_answer_burst")
  expect_error(build_answer_burst(1, typed_text = 5, full_dictionary = "dog"), "build_answer_burst")
  expect_error(build_answer_burst(1, typed_text = "dog", full_dictionary = character(0)), "must not be empty")
  expect_error(build_answer_burst(1, add_to_dictionary = "yes"), "build_answer_burst")

})

test_that("build_answer_burst() returns just the menu number for a plain button (no typed text)", {

  expect_equal(build_answer_burst(1), "1")
  expect_equal(build_answer_burst(5), "5")

})

test_that("build_answer_burst() appends the typed text with zero y/n answers when every typed word is already in the dictionary", {

  burst <- build_answer_burst(3, typed_text = "dog, cat", full_dictionary = c("dog", "cat", "bird"))

  expect_equal(burst, c("3", "dog, cat"))

})

test_that("build_answer_burst() appends exactly one y/n answer per typed word NOT already in the dictionary (the correctness-critical case)", {

  # Confirmed hazard (see build_answer_burst.R's own header): under-supplying
  # even ONE of these answers hangs offer_add_missing_to_dictionary()'s
  # unguarded yes_no_menu() while() loop in a real resume call -- this test
  # pins the exact count and order against the same dictionary_contains()
  # check that function itself uses.
  burst <- build_answer_burst(
    3, typed_text = "dog, zzznotaword1, zzznotaword2",
    add_to_dictionary = TRUE, full_dictionary = c("dog", "cat")
  )

  expect_equal(burst, c("3", "dog, zzznotaword1, zzznotaword2", "y", "y"))

})

test_that("build_answer_burst() answers 'n' for every not-in-dictionary word when add_to_dictionary is FALSE", {

  burst <- build_answer_burst(
    3, typed_text = "zzznotaword",
    add_to_dictionary = FALSE, full_dictionary = c("dog", "cat")
  )

  expect_equal(burst, c("3", "zzznotaword", "n"))

})

test_that("build_answer_burst()'s burst, fed through a real scripted resume, matches offer_add_missing_to_dictionary()'s real prompt count end to end", {

  # End-to-end confirmation against the REAL function this burst is built to
  # satisfy -- not just a re-implementation of its own logic. A 2-word typed
  # response, one already in the dictionary and one not, should consume
  # exactly the burst's own 1 extra "y" answer with nothing left over
  # (scripted_reader() would error on being asked for more than exists,
  # and error on leftover unused answers being silently ignored isn't
  # possible to detect this way, but under-supply IS caught: an exhausted
  # reader errors instead of hanging, unlike the real Shiny queue reader).
  burst <- build_answer_burst(
    3, typed_text = "dog, zzznotaword",
    add_to_dictionary = TRUE, full_dictionary = c("dog", "cat")
  )

  expect_equal(length(burst), 3)

  result <- suppressMessages(
    offer_add_missing_to_dictionary(
      c("dog", "zzznotaword"),
      full_dictionary = c("dog", "cat"),
      reader = scripted_reader(burst[-c(1, 2)])
    )
  )

  expect_true("zzznotaword" %in% result)

})
