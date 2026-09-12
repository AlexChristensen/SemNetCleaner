test_that("change_format() initializes a fresh change log for a response's first change", {

  cm <- matrix(c("cat", "dog"), nrow = 1, dimnames = list(NULL, c("from", "to_1")))
  result <- change_format(list(), cm, current_index = 1)

  expect_equal(result[["1"]], cm)

})

test_that("change_format() appends a same-width change as a new row", {

  cm1 <- matrix(c("cat", "dog"), nrow = 1, dimnames = list(NULL, c("from", "to_1")))
  cm2 <- matrix(c("bird", "birb"), nrow = 1, dimnames = list(NULL, c("from", "to_1")))

  result <- change_format(list(), cm1, current_index = 1)
  result <- change_format(result, cm2, current_index = 1)

  expect_equal(nrow(result[["1"]]), 2)
  expect_equal(result[["1"]][2, ], c(from = "bird", to_1 = "birb"))

})

test_that("change_format() pads a narrower incoming change with NA (existing has more columns)", {

  cm1 <- matrix(c("cat", "d1", "d2"), nrow = 1, dimnames = list(NULL, c("from", "to_1", "to_2")))
  cm2 <- matrix(c("bird", "birb"), nrow = 1, dimnames = list(NULL, c("from", "to_1")))

  result <- change_format(list(), cm1, current_index = 1)
  result <- change_format(result, cm2, current_index = 1)

  expect_equal(nrow(result[["1"]]), 2)
  expect_true(is.na(result[["1"]][2, "to_2"]))
  expect_equal(unname(result[["1"]][2, 1:2]), c("bird", "birb"))

})

test_that("change_format() widens the log without crashing when an incoming change has more columns (bug fix)", {

  # Real bug fixed: the original wrote
  # `new.matrix[i, length(row.target)] <- row.target` -- assigning a whole
  # multi-element row vector into a SINGLE cell instead of spreading it
  # across `1:length(row.target)` columns. Confirmed via direct
  # reproduction that the untouched original crashes here with "number of
  # items to replace is not a multiple of replacement length".
  cm1 <- matrix(c("cat", "dog"), nrow = 1, dimnames = list(NULL, c("from", "to_1")))
  cm2 <- matrix(c("cat", "d1", "d2", "d3"), nrow = 1, dimnames = list(NULL, c("from", "to_1", "to_2", "to_3")))

  result <- change_format(list(), cm1, current_index = 1)

  expect_no_error(result <- change_format(result, cm2, current_index = 1))

  expect_equal(nrow(result[["1"]]), 2)
  expect_equal(unname(result[["1"]][1, ]), c("cat", "dog", NA, NA))
  expect_equal(unname(result[["1"]][2, ]), c("cat", "d1", "d2", "d3"))

})

test_that("change_format() does not insert a phantom all-NA row when widening (second bug fix)", {

  # Real, compounding bug fixed in the same branch as the crash above, only
  # ever exposed once that crash is fixed: `matrix(NA, nrow = r_list + 1,
  # ...)` reserved one MORE row than there are existing rows to copy in --
  # the copy loop only ever fills `1:r_list`, so row `r_list + 1` stayed
  # permanently NA, and the incoming change was then appended as a further
  # row via rbind() regardless -- an extra all-NA row before every
  # column-widening change. Fixed via `nrow = r_list` (the incoming row's
  # own space comes from the rbind() below, not from this allocation).
  cm1 <- matrix(c("cat", "dog"), nrow = 1, dimnames = list(NULL, c("from", "to_1")))
  cm2 <- matrix(c("cat", "d1", "d2", "d3"), nrow = 1, dimnames = list(NULL, c("from", "to_1", "to_2", "to_3")))

  result <- change_format(list(), cm1, current_index = 1)
  result <- change_format(result, cm2, current_index = 1)

  # exactly 2 rows: the original (now NA-padded) change, then the widened one
  expect_equal(nrow(result[["1"]]), 2)
  expect_false(any(apply(result[["1"]], 1, function(row) all(is.na(row)))))

})

test_that("change_format() tracks separate response indices independently", {

  cm1 <- matrix(c("cat", "dog"), nrow = 1, dimnames = list(NULL, c("from", "to_1")))
  cm2 <- matrix(c("bird", "birb"), nrow = 1, dimnames = list(NULL, c("from", "to_1")))

  result <- change_format(list(), cm1, current_index = 1)
  result <- change_format(result, cm2, current_index = 2)

  expect_equal(names(result), c("1", "2"))
  expect_equal(nrow(result[["1"]]), 1)
  expect_equal(nrow(result[["2"]]), 1)

})
