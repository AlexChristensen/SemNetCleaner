test_that("brit_us_conversion_vector() matches the original on real conversions", {

  expect_equal(brit_us_conversion_vector(c("color", "flavor", "cat"), spelling = "UK"),
               c("colour", "flavour", "cat"))
  expect_equal(brit_us_conversion_vector(c("colour", "flavour", "cat"), spelling = "US"),
               c("color", "flavor", "cat"))

})

test_that("brit_us_conversion_vector() converts ALL occurrences of a duplicated word (bug fix)", {

  # The original's `vec[na.omit(match(names(brit2us), vec))] <- spelling_gb`
  # found only the FIRST vec position matching each dictionary entry,
  # leaving later duplicate occurrences unconverted -- confirmed via direct
  # reproduction against the untouched original: it returns
  # c("colour", "other", "color"), not c("colour", "other", "colour").
  result <- brit_us_conversion_vector(c("color", "other", "color"), spelling = "UK")
  expect_equal(result, c("colour", "other", "colour"))

  result_us <- brit_us_conversion_vector(c("colour", "other", "colour"), spelling = "US")
  expect_equal(result_us, c("color", "other", "color"))

})

test_that("brit_us_conversion_vector(dictionary = TRUE) alphabetizes and dedupes", {

  result <- brit_us_conversion_vector(c("dog", "cat", "cat"), spelling = "US", dictionary = TRUE)
  expect_equal(result, c("cat", "dog"))

})

test_that("brit_us_conversion() handles multi-word strings, dropping the dead 'dictionary' inner param (no behavior change)", {

  # NOTE: only "color" converts to "colour" -- "favorite" itself isn't a
  # key in SemNetDictionaries::brit2us, so it passes through unchanged;
  # confirmed identical to the untouched original
  expect_equal(
    brit_us_conversion(c("favorite color", "just a cat"), spelling = "UK", dictionary = FALSE),
    list("favorite colour", "just a cat")
  )

})

test_that("brit_us_conversion(dictionary = TRUE) sorts the unlisted result", {

  result <- brit_us_conversion(c("color", "apple"), spelling = "US", dictionary = TRUE)
  expect_equal(result, sort(unlist(result)))

})

test_that("get_brit2us_unlisted() caches and matches a direct unlist(brit2us) call (optimization)", {

  direct <- unlist(SemNetDictionaries::brit2us)
  cached <- get_brit2us_unlisted()

  expect_equal(cached, direct)
  expect_identical(get_brit2us_unlisted(), get_brit2us_unlisted())

})
