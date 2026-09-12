test_that("responses_to_binary() converts a word-response matrix into a binary matrix", {

  responses <- matrix(
    c("cat", "dog", NA, "cat", "bird", "dog"),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("p1", "p2"), NULL)
  )

  result <- responses_to_binary(responses)

  expect_true(is.matrix(result))
  expect_equal(sort(colnames(result)), c("bird", "cat", "dog"))
  expect_equal(row.names(result), c("p1", "p2"))
  expect_equal(unname(result["p1", c("cat", "dog")]), c(1, 1))
  expect_equal(unname(result["p2", c("bird", "dog")]), c(1, 1))
  expect_equal(unname(result["p1", "bird"]), 0)

})

test_that("responses_to_binary() matches the original resp2bin() on real bundled data", {

  toy <- as.matrix(open.animals[1:8, 4:8])
  result <- responses_to_binary(toy)

  # Every response actually present in the toy slice should be marked
  for(row_index in seq_len(nrow(toy))){
    given <- na.omit(toy[row_index, ])
    given <- given[!given %in% c(NA, "NA", "", " ", "  ")]
    for(word in given){
      expect_equal(unname(result[row_index, word]), 1)
    }
  }

})

test_that("responses_to_binary() handles a 0-row input without error (bug fix)", {

  empty <- matrix(character(0), nrow = 0, ncol = 3)
  result <- responses_to_binary(empty)

  expect_true(is.matrix(result))
  expect_equal(nrow(result), 0)

})

test_that("responses_to_binary() handles a 0-column input (>0 rows) without error (bug fix, follow-up review)", {

  # Caught during a follow-up adversarial review: the initial fix only
  # guarded `nrow(mat) == 0`, missing this case -- a matrix with 5 rows but
  # 0 columns also has 0 total elements, triggering the exact same
  # bad_response() collapse. Confirmed the untouched original resp2bin()
  # crashes identically on this input too.
  zero_col <- matrix(character(0), nrow = 5, ncol = 0)
  result <- responses_to_binary(zero_col)

  expect_true(is.matrix(result))
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 0)

})

test_that("responses_to_binary() validates its input", {

  expect_error(responses_to_binary(NULL), "responses_to_binary")
  expect_error(responses_to_binary(function() 1), "responses_to_binary")

})
