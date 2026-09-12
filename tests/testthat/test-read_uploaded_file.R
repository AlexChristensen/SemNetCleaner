test_that("read_uploaded_file() validates its inputs", {

  expect_error(read_uploaded_file(1, "x.csv"), "read_uploaded_file")
  expect_error(read_uploaded_file(c("a", "b"), "x.csv"), "read_uploaded_file")
  expect_error(read_uploaded_file("does_not_exist.csv", "x.csv"), "does not exist")

})

test_that("read_uploaded_file() reads a .csv upload correctly, regardless of the temp datapath's own name", {

  content <- data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE)

  # Simulate shiny::fileInput()'s real shape: an extensionless temp path,
  # unrelated to the real file name the browser reported
  datapath <- tempfile()
  write.csv(content, datapath, row.names = FALSE)

  result <- read_uploaded_file(datapath, "responses.csv")

  expect_equal(result$x, content$x)
  expect_equal(result$y, content$y)

})

test_that("read_uploaded_file() loads an .Rdata upload's single object, not into .GlobalEnv", {

  saved_object <- data.frame(a = 1:2, b = c("x", "y"))

  datapath <- tempfile()
  save(saved_object, file = datapath)

  # A pre-existing global of the SAME name the .Rdata file itself uses
  # ("saved_object") confirms the load target was NOT .GlobalEnv -- the real
  # hazard read_data()'s own .Rdata branch (`load(file, envir = .GlobalEnv)`)
  # has inside a shared Shiny process. Explicitly assigned into .GlobalEnv
  # (not just the test's own local frame, which testthat does not run in
  # .GlobalEnv) so this is a real, meaningful check.
  assign("saved_object", "should not be overwritten", envir = .GlobalEnv)
  on.exit(rm("saved_object", envir = .GlobalEnv), add = TRUE)

  result <- read_uploaded_file(datapath, "upload.Rdata")

  expect_equal(result, data.frame(a = 1:2, b = c("x", "y")))
  expect_equal(get("saved_object", envir = .GlobalEnv), "should not be overwritten")

})

test_that("read_uploaded_file() errors clearly on an .Rdata file with more than one object", {

  first_object <- 1:3
  second_object <- "extra"

  datapath <- tempfile()
  save(first_object, second_object, file = datapath)

  expect_error(read_uploaded_file(datapath, "upload.Rdata"), "exactly one object")

})

test_that("read_uploaded_file() dispatches on the real filename's extension, not the datapath's own name", {

  content <- data.frame(x = 1:2, stringsAsFactors = FALSE)

  # datapath has NO extension at all -- exactly what shiny::fileInput() may
  # hand this function
  datapath <- tempfile()
  write.csv(content, datapath, row.names = FALSE)

  result <- read_uploaded_file(datapath, "data.csv")

  expect_equal(result$x, content$x)

})
