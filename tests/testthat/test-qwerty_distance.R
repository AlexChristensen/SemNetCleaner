test_that("qwerty_distance() matches the original qwerty.dist() on real inputs", {

  expect_equal(qwerty_distance("big", "pig"), 7)
  expect_equal(qwerty_distance("big", "bug"), 1)
  expect_equal(qwerty_distance("cat", "cat"), 0)

})

test_that("qwerty_distance() returns NA for words of different lengths", {

  expect_true(is.na(qwerty_distance("cat", "cats")))

})

test_that("qwerty_distance() handles two empty strings without crashing (bug fix)", {

  # The original crashed with "subscript out of bounds": `for(i in
  # 1:nchar(wordA))` reverse-ranges to `1:0` whenever both words are empty,
  # confirmed by direct reproduction against the untouched original package
  expect_equal(qwerty_distance("", ""), 0)

})

test_that("qwerty_distance() validates its inputs", {

  expect_error(qwerty_distance(1, "cat"), "qwerty_distance")
  expect_error(qwerty_distance("cat", 1), "qwerty_distance")
  expect_error(qwerty_distance(c("cat", "dog"), "cat"), "qwerty_distance")

})
