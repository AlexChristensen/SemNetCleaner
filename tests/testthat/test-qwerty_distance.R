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

test_that("qwerty_distance() accepts capital letters, digits and unknown characters instead of 'subscript out of bounds'", {

  # Real bug fixed (found with `textcleaner(open.animals, lowercase = FALSE)`):
  # the key table only had lowercase letters and a few symbols, so the
  # first capitalized (or digit-containing, with `allowNumbers = TRUE`)
  # candidate crashed the whole automated spell-check
  expect_equal(qwerty_distance("Dog", "dog"), 0)
  expect_equal(qwerty_distance("DOG", "dog"), 0)
  expect_equal(qwerty_distance("dog", "fog"), qwerty_distance("DOG", "FOG"))

  # digits sit on the number row above "qwertyuiop": "0" is directly above "p"
  expect_equal(qwerty_distance("0", "p"), 1)
  expect_equal(qwerty_distance("1", "q"), 1)
  expect_false(is.na(qwerty_distance("d0g", "dog")))

  # no key position -> no distance (like words of different lengths)
  expect_true(is.na(qwerty_distance("a&b", "a b")))
  expect_true(is.na(qwerty_distance("日本", "dog")))

})
