test_that("read_data() reads a plain-text file (matching the original's own @examples)", {

  test_lines <- c(1:5, "6,7", "8,9,10")
  tf <- tempfile()
  writeLines(test_lines, tf)

  result <- read_data(tf)
  expect_s3_class(result, "data.frame")

})

test_that("read_data() reads a .csv file", {

  tf <- tempfile(fileext = ".csv")
  write.csv(data.frame(a = 1:3, b = c("x", "y", "z")), tf, row.names = FALSE)

  result <- read_data(tf)
  expect_s3_class(result, "data.frame")
  expect_equal(result$a, 1:3)
  expect_equal(result$b, c("x", "y", "z"))

})

test_that("read_data() rejects an unsupported file extension with a clear, informative message (robustness fix)", {

  # Real robustness gap found and fixed during a Phase 2 review, per direct
  # request: the original's bare "File extension not supported" gave no
  # indication of what was actually received or what's actually
  # supported -- every other `<name>_errors()` message in this rebuild
  # names both. Matched here for consistency.
  expect_error(read_data("somefile.xyz"), "'xyz' is not supported")
  expect_error(read_data("somefile.xyz"), "'csv'")

})

test_that("read_data() names the package a file type needs when it is not installed", {

  local_mocked_bindings(package_installed = function(package) FALSE)

  tf <- tempfile(fileext = ".xlsx")
  file.create(tf)

  expect_error(read_data(tf), "readxl", fixed = TRUE)

  tf_sav <- tempfile(fileext = ".sav")
  file.create(tf_sav)

  expect_error(read_data(tf_sav), "foreign", fixed = TRUE)

  # delimited files need no extra package
  tf_csv <- tempfile(fileext = ".csv")
  writeLines(c("a,b", "1,2"), tf_csv)

  expect_equal(nrow(read_data(tf_csv)), 1)

})
