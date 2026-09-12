test_that("binary_to_responses() converts a binary matrix back into word responses", {

  binary_matrix <- matrix(
    c(1, 0, 1, 0, 1, 1),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("p1", "p2"), c("cat", "dog", "bird"))
  )

  result <- binary_to_responses(binary_matrix)

  expect_type(result, "list")
  expect_equal(sort(unname(result$p1)), c("bird", "cat"))
  expect_equal(sort(unname(result$p2)), c("bird", "dog"))

})

test_that("binary_to_responses(to_data_frame = TRUE) transposes into a padded matrix", {

  # NOTE: despite the parameter's name, this returns a matrix, not a
  # data.frame -- confirmed inherited from the original bin2resp(): its
  # final `t()` call (on a data.frame it just built) strips the data.frame
  # class, since base t.data.frame() coerces via as.matrix() first. Verified
  # directly against the untouched original, byte-for-byte same behavior --
  # not something introduced or fixed here, just faithfully preserved.
  binary_matrix <- matrix(
    c(1, 0, 1, 0, 1, 1),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("p1", "p2"), c("cat", "dog", "bird"))
  )

  result <- binary_to_responses(binary_matrix, to_data_frame = TRUE)

  expect_true(is.matrix(result))
  expect_equal(row.names(result), c("p1", "p2"))

})

test_that("responses_to_binary() and binary_to_responses() round-trip on real bundled data", {

  toy <- as.matrix(open.animals[1:8, 4:8])
  binary_matrix <- responses_to_binary(toy)
  recovered <- binary_to_responses(binary_matrix)

  for(row_index in seq_len(nrow(toy))){

    given <- toy[row_index, ]
    given <- given[!given %in% c(NA, "NA", "", " ", "  ")]

    recovered_row <- sort(unname(recovered[[row_index]]))
    expect_equal(recovered_row, sort(unname(given)))

  }

})

test_that("binary_to_responses() handles 0-row/0-column input without error (bug fix)", {

  empty_rows <- matrix(numeric(0), nrow = 0, ncol = 2, dimnames = list(NULL, c("cat", "dog")))
  result_rows <- binary_to_responses(empty_rows)
  expect_type(result_rows, "list")
  expect_length(result_rows, 0)

  empty_cols <- matrix(numeric(0), nrow = 2, ncol = 0, dimnames = list(c("p1", "p2"), NULL))
  result_cols <- binary_to_responses(empty_cols)
  expect_type(result_cols, "list")
  expect_length(result_cols, 2)

})

test_that("binary_to_responses(to_data_frame = TRUE) handles 0 participants without error (bug fix, follow-up review)", {

  # Found during a follow-up review: `max(response_lengths)` on a length-0
  # vector returns -Inf (with a warning), crashing matrix(nrow = -Inf, ...).
  # This was never reachable in the original -- its own `1:p` reverse-range
  # bug crashed first, on the main collection loop, masking this one.
  empty <- matrix(numeric(0), nrow = 0, ncol = 2, dimnames = list(NULL, c("cat", "dog")))
  result <- binary_to_responses(empty, to_data_frame = TRUE)

  expect_true(is.matrix(result))
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 0)

})

test_that("binary_to_responses() validates its input", {

  binary_matrix <- matrix(c(1, 0), nrow = 1, dimnames = list("p1", c("cat", "dog")))

  expect_error(binary_to_responses(NULL), "binary_to_responses")
  expect_error(binary_to_responses(binary_matrix, to_data_frame = "yes"), "binary_to_responses")
  expect_error(binary_to_responses(binary_matrix, to_data_frame = c(TRUE, FALSE)), "binary_to_responses")

})
