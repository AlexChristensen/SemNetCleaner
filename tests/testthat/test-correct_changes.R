test_that("correct_changes() reports 'No responses changed' and returns the object as-is when nothing changed", {

  expect_message(
    result <- correct_changes(
      open.preprocess, type = "fluency",
      reader = scripted_reader(c("", "", "")), edit_fn = function(x) x
    ),
    "No responses changed"
  )

  expect_identical(result, open.preprocess)

})

test_that("correct_changes() folds a real manual edit back into $spellcheck$correspondence and $verified, matching the original verbatim", {

  # Verified via direct identical() comparison against the untouched
  # original's correct.changes() (readline()/Sys.info() mocked in a
  # sys.source()'d environment, edit() replaced with the same injected
  # edit_fn), using real open.preprocess data. See REFACTOR_PLAN.md's
  # Phase 6 write-up.
  make_change <- function(x){
    x[1, "to_1"] <- "TESTCHANGE"
    x
  }

  result <- correct_changes(
    open.preprocess, type = "fluency",
    reader = scripted_reader(c("", "", "")), edit_fn = make_change
  )

  expect_true("TESTCHANGE" %in% result$spellcheck$automated[, "to_1"])
  expect_length(result$spellcheck$verified, 1)
  expect_true(any(result$spellcheck$correspondence[, "to_1"] == "TESTCHANGE", na.rm = TRUE))

})

test_that("correct_changes() detects a change from NA to a real value (bug #36 fix)", {

  # Real bug fixed: the original's `differences <- automated !=
  # as.matrix(changes)` produces NA (not TRUE) whenever exactly one side
  # is NA -- any()/which() only ever select TRUE, so a genuine NA -> value
  # edit (e.g. filling in a previously-empty "to_" alternative) was
  # silently treated as "no difference detected". Confirmed directly
  # against the untouched original: it takes the "No responses changed"
  # early return and discards the edit entirely. Fixed by treating a
  # mismatched NA-ness between the two sides as a real difference too.
  automated <- matrix(c("dog", "dogg", "cat", "catt"), nrow = 2, byrow = TRUE,
                      dimnames = list(c("1", "2"), c("from", "to_1")))
  correspondence <- matrix(c("dog", "dogg", NA, "cat", "catt", NA, "bird", "bird", NA), nrow = 3, byrow = TRUE,
                           dimnames = list(c("1", "2", "3"), c("from", "to_1", "to_2")))
  tc_obj <- list(
    spellcheck = list(automated = automated, correspondence = correspondence),
    responses = list(original = matrix(c("dog", "cat", "bird"), ncol = 1))
  )
  class(tc_obj) <- "textcleaner"

  make_change <- function(x){
    x["1", "to_2"] <- "EXTRA"
    x
  }

  result <- correct_changes(
    tc_obj, type = "fluency",
    reader = scripted_reader(c("", "", "")), edit_fn = make_change
  )

  expect_true(any(result$spellcheck$correspondence[, "to_2"] == "EXTRA", na.rm = TRUE))

})

test_that("correct_changes() pads correspondence/changes to matching widths when they differ", {

  automated <- matrix(c("dog", "dogg", "cat", "catt"), nrow = 2, byrow = TRUE,
                      dimnames = list(c("1", "2"), c("from", "to_1")))
  correspondence <- matrix(c("dog", "dogg", NA, "cat", "catt", NA, "bird", "bird", NA), nrow = 3, byrow = TRUE,
                           dimnames = list(c("1", "2", "3"), c("from", "to_1", "to_2")))
  tc_obj <- list(
    spellcheck = list(automated = automated, correspondence = correspondence),
    responses = list(original = matrix(c("dog", "cat", "bird"), ncol = 1))
  )
  class(tc_obj) <- "textcleaner"

  # automated gets widened by max_length (1 extra "to_" column, since
  # "dog"/"cat" are single words) before edit_fn ever sees it -- fill in
  # that already-widened structure without adding more columns
  make_change <- function(x){
    x["1", "to_2"] <- "EXTRA"
    x
  }

  result <- correct_changes(
    tc_obj, type = "fluency",
    reader = scripted_reader(c("", "", "")), edit_fn = make_change
  )

  expect_equal(ncol(result$spellcheck$correspondence), 3)
  expect_equal(unname(result$spellcheck$correspondence["1", "to_2"]), "EXTRA")

})

test_that("correct_changes() normalizes a blank cell to real NA regardless of which edit_fn produced it (Phase 9: previously untested, Shiny-only code path)", {

  # Real gap closed: the blank -> NA normalization
  # (`ifelse(changes == "", NA, changes)`) used to run ONLY on the
  # non-Linux/Shiny branch of a now-removed OS fork -- it had zero test
  # coverage before Phase 9, since it only ever ran through the
  # un-mockable original Shiny app. Now that every `edit_fn` shares one
  # code path, this needs to hold for a plain function-based `edit_fn`
  # too, not just the real `automated_edit()`.
  automated <- matrix(c("dog", "dogg", "cat", "catt"), nrow = 2, byrow = TRUE,
                      dimnames = list(c("1", "2"), c("from", "to_1")))
  correspondence <- matrix(c("dog", "dogg", NA, "cat", "catt", NA, "bird", "bird", NA), nrow = 3, byrow = TRUE,
                           dimnames = list(c("1", "2", "3"), c("from", "to_1", "to_2")))
  tc_obj <- list(
    spellcheck = list(automated = automated, correspondence = correspondence),
    responses = list(original = matrix(c("dog", "cat", "bird"), ncol = 1))
  )
  class(tc_obj) <- "textcleaner"

  blank_edit_fn <- function(x){
    x["1", "to_1"] <- ""
    x
  }

  result <- correct_changes(
    tc_obj, type = "fluency",
    reader = scripted_reader(c("", "", "")), edit_fn = blank_edit_fn
  )

  expect_true(is.na(result$spellcheck$correspondence["1", "to_1"]))
  expect_false(identical(result$spellcheck$correspondence["1", "to_1"], ""))

})

test_that("correct_changes()'s edit_fn defaults to automated_edit(), not base edit() (Phase 9: guards the OS-fork removal)", {

  # Real behavior change, documented in NEWS.md: the OS fork (Linux ->
  # base edit(), every other OS -> the Shiny app) is gone -- every OS now
  # defaults to the same automated_edit() review app. This guard exists so
  # a future edit can't silently revert the default back to `edit` without
  # a test noticing.
  expect_identical(formals(correct_changes)$edit_fn, quote(automated_edit))

})
