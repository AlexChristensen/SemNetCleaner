test_that("convert_miss() matches the original for fluency data", {

  d <- matrix(c("cat", "999", "dog", NA, " ", "bird"), nrow = 2)
  result <- convert_miss(d, miss = "999", type = "fluency")

  expect_true(is.matrix(result))
  expect_false(any(result == "999", na.rm = TRUE))
  expect_equal(result[1, 1], "cat")

})

test_that("convert_miss() matches the original for free-association data", {

  d <- data.frame(ID = 1:3, Response = c("cat", "999", NA))
  result <- convert_miss(d, miss = "999", type = "free")

  expect_true(is.matrix(result))
  expect_equal(unname(result[, "Response"]), c("cat", "", ""))

})

test_that("convert_miss() preserves a 0-row fluency matrix's shape instead of manufacturing a phantom participant (bug #51 fix)", {

  # Real bug fixed, found during a Phase 3 review: bad_response()'s own
  # `if(length(word) == 0) word <- NA` guard turns a 0-row column into a
  # scalar NA instead of `character(0)` -- since every column of a 0-row
  # matrix behaves identically, `apply(data, 2, bad_response)` then
  # simplified its result into a 1-row matrix, silently manufacturing a
  # phantom participant instead of preserving 0 rows. Confirmed inherited
  # identically from the untouched original, and the same mechanism
  # already fixed in responses_to_binary() (Phase 2).
  d <- matrix(character(0), nrow = 0, ncol = 3, dimnames = list(NULL, c("a", "b", "c")))
  result <- convert_miss(d, miss = 99, type = "fluency")

  expect_equal(dim(result), c(0, 3))

})
