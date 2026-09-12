test_that("convert2snafu() writes real data for a single dataset, not an empty file (real bug fix)", {

  # Real bug fixed: `if(length(data.list) == 1){}else{...}` -- the
  # untouched original's single-dataset branch was completely EMPTY, so
  # the function's own documented single-argument use
  # (`convert2snafu(open.clean, category = "animals")`) silently wrote
  # out an empty, header-only CSV instead of the real data. Confirmed the
  # `else` branch's own group-name derivation works correctly for a
  # single dataset too, so the special case was never necessary.
  m <- matrix(c("dog", "cat", NA, "bird"), nrow = 2, dimnames = list(c("1", "2"), NULL))
  tmp <- tempfile()
  dir.create(tmp)

  expect_message(
    convert2snafu(m, category = "animals", path = tmp, reader = scripted_reader("output")),
    "SNAFU formatted file was saved in"
  )

  result <- read.csv(file.path(tmp, "output.csv"), stringsAsFactors = FALSE)

  expect_equal(nrow(result), 3)
  expect_equal(result$item, c("dog", "cat", "bird"))
  expect_true(all(result$category == "animals"))
  expect_equal(result$id, c("A001", "A002", "A002"))

})

test_that("convert2snafu() handles multiple datasets, one 'group' per argument", {

  group1 <- matrix(c("dog", "cat"), nrow = 1, dimnames = list("1", NULL))
  group2 <- matrix(c("bird", "fish"), nrow = 1, dimnames = list("1", NULL))
  tmp <- tempfile()
  dir.create(tmp)

  convert2snafu(group1, group2, category = "animals", path = tmp, reader = scripted_reader("multi"))

  result <- read.csv(file.path(tmp, "multi.csv"), stringsAsFactors = FALSE)

  expect_equal(nrow(result), 4)
  expect_equal(sort(unique(result$group)), sort(c("group1", "group2")))

})

test_that("convert2snafu() uses an injectable reader instead of real readline() (binding decision #3)", {

  m <- matrix("dog", nrow = 1, dimnames = list("1", NULL))
  tmp <- tempfile()
  dir.create(tmp)

  convert2snafu(m, category = "animals", path = tmp, reader = scripted_reader("my_custom_name"))

  expect_true(file.exists(file.path(tmp, "my_custom_name.csv")))

})

test_that("convert2snafu() no longer depends on easycsv -- takes an explicit path argument", {

  # Binding decision #5: `easycsv::choose_dir()` replaced with an explicit
  # `path =` argument
  expect_true("path" %in% names(formals(convert2snafu)))

})
