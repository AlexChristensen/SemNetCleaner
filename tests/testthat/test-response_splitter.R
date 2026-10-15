test_that("response_splitter() splits a multi-word response when both halves are dictionary words", {

  expect_equal(response_splitter("cat dog", full_dictionary = c("cat", "dog")), c("cat", "dog"))

})

test_that("response_splitter() leaves a combined non-dictionary response alone", {

  expect_equal(response_splitter("catdog", full_dictionary = c("catdog")), "catdog")

})

test_that("response_splitter() leaves a response alone when neither the whole nor the split form is in the dictionary", {

  expect_equal(response_splitter("xyz abc", full_dictionary = c("cat", "dog")), "xyz abc")

})

test_that("response_splitter() prefers the whole response when it's already a dictionary entry itself", {

  expect_equal(response_splitter("cat dog", full_dictionary = c("cat", "dog", "cat dog")), "cat dog")

})

test_that("response_splitter() leaves a single word or a length>1 vector untouched", {

  expect_equal(response_splitter("cat", full_dictionary = c("cat", "dog")), "cat")
  expect_equal(response_splitter(c("cat", "dog"), full_dictionary = c("cat", "dog")), c("cat", "dog"))

})

test_that("response_splitter() keeps a dictionary entry inside a longer string whole (sea slug + dog)", {

  dict <- SemNetDictionaries::animals.dictionary

  expect_identical(response_splitter("sea slug dog", dict), c("sea slug", "dog"))
  expect_identical(response_splitter("sea slug", dict), "sea slug")
  expect_identical(response_splitter("dog cat", dict), c("dog", "cat"))

})
