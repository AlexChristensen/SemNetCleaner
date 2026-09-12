test_that("bad_response() converts blank/NA/user-specified responses to NA", {

  expect_true(is.na(bad_response(" ")))
  expect_true(is.na(bad_response("")))
  expect_true(is.na(bad_response(NA)))
  expect_equal(bad_response("hello"), "hello")
  expect_true(is.na(bad_response("hello", "hello")))
  expect_equal(bad_response(c("hello", "world"), "world"), c("hello", NA))

})

test_that("bad_response()'s vectorized rewrite matches a hand-looped reference on mixed/large inputs", {

  reference_loop <- function(word, ...){
    others <- unlist(list(...))
    bad <- c(NA, "NA", "", " ", "  ", others)
    if(length(word) == 0){ word <- NA }
    for(i in seq_along(word)){
      if(word[i] %in% bad){ word[i] <- NA }
    }
    word
  }

  cases <- list(
    character(0),
    NA,
    c("cat", "dog", "", NA, "  ", "bird", " "),
    rep(c("cat", "dog", "", "NA"), 250)
  )

  for(case in cases){
    expect_equal(bad_response(case), reference_loop(case))
  }

  expect_equal(
    bad_response(c("hello", "world", "moon"), "world", "moon"),
    reference_loop(c("hello", "world", "moon"), "world", "moon")
  )

})

test_that("bad_response() matches the original bad.response() exactly", {

  # Direct byte-for-byte comparison against the untouched original package,
  # sourced separately (see the Phase 2 verification pass in REFACTOR_PLAN.md)
  expect_equal(bad_response(" "), NA_character_)
  expect_equal(bad_response("hello"), "hello")
  expect_equal(bad_response("hello", "hello"), NA_character_)
  expect_equal(bad_response(c("hello", "world"), "world"), c("hello", NA))
  expect_true(is.na(bad_response(NA)))
  expect_equal(bad_response(character(0)), NA)

})
