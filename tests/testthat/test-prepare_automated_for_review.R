test_that("prepare_automated_for_review() restores matrix-ness and the correspondence row name for a collapsed single-row selection", {

  # Same real bug/mechanism correct_changes.R's own header documents:
  # a single-row `res$spellcheck$automated` selection collapses to a plain
  # named vector, losing its correspondence-matrix row name entirely
  correspondence <- matrix(
    c("dog", "dogg"), nrow = 1, dimnames = list("03", c("from", "to_1"))
  )

  res <- list(spellcheck = list(
    automated = correspondence[1, ],
    correspondence = correspondence
  ))

  result <- prepare_automated_for_review(res)

  expect_true(is.matrix(result))
  expect_equal(row.names(result), "03")
  expect_equal(unname(result[1, "from"]), "dog")

})

test_that("prepare_automated_for_review() pads with enough to_# columns for the widest multi-word 'from' entry", {

  automated <- matrix(
    c("dog", "dogg", "turtle catdog elephant", "turtle catdog elephant"),
    nrow = 2, byrow = TRUE, dimnames = list(c("01", "02"), c("from", "to_1"))
  )

  res <- list(spellcheck = list(automated = automated, correspondence = automated))

  result <- prepare_automated_for_review(res)

  # 3-word "from" entry needs 3 "to_" columns, beyond the 1 already there
  expect_equal(ncol(result), 1 + 1 + 3)
  expect_true(all(grepl("^to_", colnames(result)[-1])))

})

test_that("prepare_automated_for_review()'s own output round-trips through finalize_textcleaner() without a width mismatch (real bug, found via live Shiny verification)", {

  # Real bug: textcleaner_shiny()'s corrections-review module rendered the
  # RAW res$spellcheck$automated (un-padded) for editing, but
  # correct_changes() internally pads its own copy before calling edit_fn --
  # handing back the narrower, un-padded shape crashed with "non-conformable
  # arrays" the moment the two were compared. prepare_automated_for_review()
  # must be used for BOTH sides of that hand-off.
  raw <- data.frame(
    ID = c("P1", "P2", "P3"), R1 = c("dog", "bird", "coww"),   # only changed responses are reviewed
    R2 = c("catdog", "fish", "pig"), R3 = c("cat", "turtle", "horse"),
    stringsAsFactors = FALSE
  )

  initial <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = NULL, spelling = "US",
    reader = function(prompt = "") "", edit_fn = function(x) x
  ))
  initial <- suppressMessages(advance_wizard(initial, build_answer_burst(1)))

  review_matrix <- prepare_automated_for_review(initial)
  target_row <- row.names(review_matrix)[review_matrix[, "from"] == "coww"]
  edited <- review_matrix
  edited[target_row, "to_1"] <- "puppy"

  final <- suppressMessages(finalize_textcleaner(
    initial, initial$responses$original, "fluency", row.names(initial$responses$original),
    reader = function(prompt = "") "", edit_fn = function(x) edited
  ))

  expect_s3_class(final, "textcleaner")
  expect_equal(unname(final$responses$clean["P3", ]), c("puppy", "pig", "horse"))

})
