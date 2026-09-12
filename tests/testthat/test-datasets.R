test_that("bundled datasets load with expected shapes", {

  expect_true(is.matrix(open.clean))
  expect_equal(dim(open.clean), c(516, 34))

  expect_s3_class(open.animals, "data.frame")
  expect_equal(dim(open.animals), c(516, 38))

  expect_s3_class(open.preprocess, "textcleaner")
  expect_named(open.preprocess, c("responses", "spellcheck", "behavioral", "moniker"))

  expect_type(letter.freq, "double")
  expect_length(letter.freq, 26)
  expect_named(letter.freq)

})
