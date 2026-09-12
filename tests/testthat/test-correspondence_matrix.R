test_that("correspondence_matrix() matches the original against real open.preprocess data", {

  corr <- open.preprocess$spellcheck$correspondence
  from <- as.list(corr[, "from"])
  if(any(is.na(corr[, grep("to", colnames(corr))]))){
    to <- apply(as.matrix(corr[, grep("to", colnames(corr))]), 1, function(x){ unname(na.omit(x)) })
  }else{
    to <- as.matrix(corr[, grep("to", colnames(corr))])
  }

  result <- correspondence_matrix(from, to)

  expect_equal(dim(result), dim(corr))
  expect_equal(colnames(result), colnames(corr))

})

test_that("correspondence_matrix() builds a simple from/to matrix correctly", {

  from <- list("dog", "catt", "bird")
  to <- list("dog", "cat", "bird")

  result <- correspondence_matrix(from, to)

  expect_equal(unname(result[1, ]), c("dog", "dog"))
  expect_equal(unname(result[2, ]), c("catt", "cat"))
  expect_equal(colnames(result), c("from", "to_1"))

})

test_that("correspondence_matrix() pads narrower responses with NA up to the widest", {

  from <- list("dog", "catdog")
  to <- list("dog", c("cat", "dog"))

  result <- correspondence_matrix(from, to)

  expect_equal(colnames(result), c("from", "to_1", "to_2"))
  expect_true(is.na(result[1, "to_2"]))
  expect_equal(unname(result[2, ]), c("catdog", "cat", "dog"))

})

test_that("correspondence_matrix() handles zero responses without crashing (bug #44 fix)", {

  # Real bug fixed: with 0 responses, max(unlist(lapply(to, length))) is
  # max() over nothing -- -Inf, with a warning -- and matrix(NA, ncol =
  # -Inf + 1) then crashes ("invalid 'ncol' value"). Confirmed the
  # untouched original crashes identically. A second, compounding issue
  # exposed once that's fixed: paste("to_", seq_len(0), sep = "") isn't
  # character(0) the way it looks like it should be -- paste()'s default
  # recycle0 = FALSE recycles a zero-length argument to "" instead,
  # producing one bogus "to_" column name rather than none. Fixed via an
  # explicit to_width == 0 guard and recycle0 = TRUE.
  result <- expect_no_error(correspondence_matrix(list(), list()))

  expect_equal(dim(result), c(0, 1))
  expect_equal(colnames(result), "from")

})
