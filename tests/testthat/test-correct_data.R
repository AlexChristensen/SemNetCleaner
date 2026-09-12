test_that("correct_data() matches the original against real open.preprocess data (for fields the original computes correctly)", {

  corr <- open.preprocess$spellcheck$correspondence
  from <- as.list(corr[, "from"])
  if(any(is.na(corr[, grep("to", colnames(corr))]))){
    to <- apply(as.matrix(corr[, grep("to", colnames(corr))]), 1, function(x){ unname(na.omit(x)) })
  }else{
    to <- as.matrix(corr[, grep("to", colnames(corr))])
  }
  corr_mat <- correspondence_matrix(from, to)

  data_mat <- as.matrix(open.preprocess$responses$original)

  result <- correct_data(data_mat, corr_mat)

  expect_named(result, c("behavioral", "corrected"))
  expect_equal(nrow(result$behavioral), nrow(data_mat))
  expect_equal(row.names(result$corrected), row.names(data_mat))
  expect_true(all(c("Perseverations", "Intrusions") %in% colnames(result$behavioral)))

})

test_that("correct_data() counts a perseveration (duplicate response) correctly (bug fix)", {

  # Bug #39, real and severe: the original's `correct.ord[-which(correct.ord
  # == "NA")]` -- like every other `x[-which(cond)]` pattern on a plain
  # vector -- silently EMPTIES `correct.ord` instead of leaving it
  # unchanged whenever `which()` itself is empty (i.e. whenever there's no
  # literal "NA"-string response to exclude, the ordinary case for any
  # participant with no bad responses). Perseverations were therefore
  # always reported as 0 for such participants, regardless of how many
  # real duplicate responses they had. Confirmed directly against real
  # open.preprocess data: the untouched original reports a total of just
  # 3 perseverations across all 516 participants; the correct total,
  # computed the same way but without this bug, is 116 -- a ~38x
  # undercount, affecting 73 of 516 participants (14%). Fixed via a
  # positive `!=` filter, which behaves correctly whether or not anything
  # actually matches.
  data_mat <- matrix(c("dog", "dog", "cat"), nrow = 1, dimnames = list("p1", NULL))
  corr_mat <- matrix(c("dog", "dog", NA, "cat", "cat", NA), nrow = 2, byrow = TRUE,
                     dimnames = list(NULL, c("from", "to_1", "to_2")))

  result <- correct_data(data_mat, corr_mat)

  expect_equal(unname(result$behavioral["p1", "Perseverations"]), 1)
  expect_equal(unname(result$behavioral["p1", "Intrusions"]), 0)

})

test_that("correct_data() counts an intrusion (response marked bad, 'NA') correctly", {

  data_mat <- matrix(c("dog", "zzznotreal"), nrow = 1, dimnames = list("p1", NULL))
  corr_mat <- matrix(c("dog", "dog", "zzznotreal", "NA"), nrow = 2, byrow = TRUE,
                     dimnames = list(NULL, c("from", "to_1")))

  result <- correct_data(data_mat, corr_mat)

  expect_equal(unname(result$behavioral["p1", "Intrusions"]), 1)

})

test_that("correct_data() does not crash on a single-participant dataset (bug fix)", {

  # Bug #37: `apply(data, 2, function(x){ !is.na(x) })` returns a
  # length-1 logical per column when `data` has exactly 1 row, and
  # apply() simplifies that down to a plain vector instead of a matrix --
  # rowSums() then crashes ("'x' must be an array of at least two
  # dimensions"). Confirmed the untouched original crashes identically.
  # Fixed by using `!is.na(data)` directly (already vectorized over the
  # whole matrix, with no row-count-dependent simplification).
  data_mat <- matrix(c("dog", "dog", "cat"), nrow = 1, dimnames = list("p1", NULL))
  corr_mat <- matrix(c("dog", "dog", NA, "cat", "cat", NA), nrow = 2, byrow = TRUE,
                     dimnames = list(NULL, c("from", "to_1", "to_2")))

  expect_no_error(correct_data(data_mat, corr_mat))

})

test_that("correct_data() does not collapse to a zero-column matrix when every response slot is used (compounding bug fix)", {

  # Bug #40, the same `x[-which(cond)]`-on-empty
  # pattern as bug #39 above, just on the matrix's column
  # dimension instead of a plain vector: `correct_mat[, -which(apply(...))]`
  # ("remove all-NA columns") collapses to a *zero-column* matrix whenever
  # there are no all-NA columns to remove -- i.e. whenever every allocated
  # response slot actually got used, here because one response ("catdog")
  # maps to 2 "to_" alternatives, filling both of the 2*data_max = 2
  # allocated columns for a 1-response-per-participant dataset. Confirmed
  # the untouched original crashes one line later at
  # `colnames(correct_mat) <- ...` (`1:ncol(correct_mat)` reverse-ranging
  # to `1:0` on the now-zero-width matrix -- also fixed here, via
  # `seq_len()`, same class as bugs #15-17). Fixed via a positive `!`
  # filter for the column removal itself.
  data_mat <- matrix(c("catdog", "catdog"), nrow = 2, dimnames = list(c("p1", "p2"), NULL))
  corr_mat <- matrix(c("catdog", "cat", "dog"), nrow = 1)
  colnames(corr_mat) <- c("from", "to_1", "to_2")

  result <- expect_no_error(correct_data(data_mat, corr_mat))

  expect_equal(dim(result$corrected), c(2, 2))
  expect_equal(unname(result$corrected["p1", ]), c("cat", "dog"))

})

test_that("correct_data() does not crash when every response is entirely unresolvable (bug #45 fix)", {

  # Real bug fixed, compounding bug #37 on the opposite edge (only ever
  # exposed once it's fixed): when every response for a participant is
  # entirely NA/unresolvable, ncol(correct_mat) is 0 after the all-NA-
  # column removal. paste("Response_", formatC(seq_len(0), ...), sep = "")
  # isn't character(0) the way it looks like it should be -- paste()'s
  # default recycle0 = FALSE recycles the zero-length formatC() result to
  # "" instead, producing one bogus "Response_" column name for a matrix
  # that actually has zero columns, and colnames(correct_mat) <- ... then
  # crashes ("length of 'dimnames' [2] not equal to array extent"). Same
  # underlying paste() gotcha as bug #44 in correspondence_matrix(); fixed
  # the same way, via recycle0 = TRUE.
  data_mat <- matrix(NA_character_, nrow = 1, dimnames = list("p1", NULL))
  corr_mat <- matrix(c("dog", "dog"), nrow = 1, dimnames = list(NULL, c("from", "to_1")))

  expect_no_error(correct_data(data_mat, corr_mat))

})
