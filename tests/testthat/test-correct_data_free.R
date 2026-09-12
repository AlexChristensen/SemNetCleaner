test_that("correct_data_free() applies straightforward 1:1 corrections and counts a perseveration", {

  data <- data.frame(
    ID = c(1, 1, 1, 1, 2, 2, 2),
    Cue = c("animal", "animal", "food", "food", "animal", "animal", "animal"),
    Response = c("dog", "dog", "apple", "banana", "cat", "cat", "cat"),
    stringsAsFactors = FALSE
  )
  row.names(data) <- as.character(seq_len(nrow(data)))

  corr_mat <- matrix(
    c("dog", "dog", NA, "apple", "apple", NA, "banana", "banana", NA, "cat", "cat", NA),
    nrow = 4, byrow = TRUE,
    dimnames = list(NULL, c("from", "to_1", "to_2"))
  )

  result <- correct_data_free(data, corr_mat, ids = c(1, 2))

  expect_named(result, c("behavioral", "corrected"))
  # participant 1 repeated "dog" for the same cue -> 1 perseveration;
  # participant 2 repeated "cat" 3x for the same cue -> 2 perseverations
  expect_equal(unname(result$behavioral["1", "Perseverations"]), 1)
  expect_equal(unname(result$behavioral["2", "Perseverations"]), 2)

})

test_that("correct_data_free() splices a response that was split into multiple words into extra rows, without duplicating its neighbors (bugs #42/#43 fix)", {

  # This 3-row, single-cue dataset has all 3 row positions contiguous
  # (row names "1","2","3"), so seq_min_max() bundles all 3 responses into
  # ONE sequence/iteration (its "no breaks" branch) rather than isolating
  # "catdog" alone -- meaning `target_resps` here is c(1,2,3): `minPosition
  # == 1` AND `max(target_resps) == nrow(correct_mat)` simultaneously,
  # exercising both bug #42 (`data_top`) and bug #43 (`data_bottom`) at
  # once. Confirmed directly against the untouched original: it returns
  # 6 rows with "turtle" and "elephant" each duplicated (a phantom row
  # from data_top's `1:0` and a phantom/out-of-bounds row from
  # data_bottom's descending `4:3`), instead of the correct 4 (turtle,
  # cat, dog, elephant, each once). Fixed via `seq_len()` for data_top and
  # an explicit bounds check for data_bottom (no single-expression form of
  # `:`/`seq()` produces an empty result the way `seq_len()` does for a
  # from-1 range). This was NOT caught by this test's original, looser
  # assertions (which merely checked containment) before this fix was
  # found during a second, closer re-read after Phase 6 landed --
  # confirmed both old and new were equally buggy here at that time.
  data <- data.frame(
    ID = c(1, 1, 1),
    Cue = c("animal", "animal", "animal"),
    Response = c("turtle", "catdog", "elephant"),
    stringsAsFactors = FALSE
  )
  row.names(data) <- as.character(seq_len(nrow(data)))

  corr_mat <- matrix(
    c("turtle", "turtle", NA, "catdog", "cat", "dog", "elephant", "elephant", NA),
    nrow = 3, byrow = TRUE,
    dimnames = list(NULL, c("from", "to_1", "to_2"))
  )

  result <- correct_data_free(data, corr_mat, ids = c(1))

  expect_equal(nrow(result$corrected), 4)
  expect_equal(result$corrected$Response, c("turtle", "cat", "dog", "elephant"))

})

test_that("correct_data_free() handles a single-response dataset needing a split (bugs #42/#43, minimal isolation)", {

  # The cleanest possible isolation of bugs #42/#43: a single participant,
  # single cue, single response that is the ONLY row in the dataset, so
  # minPosition == 1 == nrow(data) simultaneously. Confirmed the untouched
  # original returns 4 rows (with the original unsplit "catdog" appearing
  # twice) instead of the correct 2 (just "cat", "dog").
  data <- data.frame(ID = 1, Cue = "animal", Response = "catdog", stringsAsFactors = FALSE)
  row.names(data) <- "1"

  corr_mat <- matrix(c("catdog", "cat", "dog"), nrow = 1)
  colnames(corr_mat) <- c("from", "to_1", "to_2")

  result <- correct_data_free(data, corr_mat, ids = c(1))

  expect_equal(nrow(result$corrected), 2)
  expect_equal(result$corrected$Response, c("cat", "dog"))

})

test_that("correct_data_free() converts 'NA' string responses to real NA and drops those rows", {

  # Note: correct_data_free()'s Intrusions count checks `is.na(correct_ord)`
  # (real NA), unlike correct_data()'s `correct.ord == "NA"` (string
  # comparison) -- confirmed via direct identical() comparison that this
  # asymmetry is a genuine, verified characteristic of the untouched
  # original itself (both functions kept separate per binding decision
  # #2), not something this port introduced. A "to_" alternative of the
  # literal string "NA" is still correctly converted to real NA and
  # dropped -- just not counted as an intrusion here.
  data <- data.frame(
    ID = c(1, 1),
    Cue = c("animal", "animal"),
    Response = c("dog", "zzznotreal"),
    stringsAsFactors = FALSE
  )
  row.names(data) <- as.character(seq_len(nrow(data)))

  corr_mat <- matrix(
    c("dog", "dog", NA, "zzznotreal", "NA", NA),
    nrow = 2, byrow = TRUE,
    dimnames = list(NULL, c("from", "to_1", "to_2"))
  )

  result <- correct_data_free(data, corr_mat, ids = c(1))

  expect_false("zzznotreal" %in% result$corrected$Response)
  expect_equal(nrow(result$corrected), 1)

})

test_that("correct_data_free() reshapes a single-to_-column correspondence table correctly instead of misrouting ordinary responses (bug #46 fix)", {

  # Real bug fixed: when NO response anywhere in the whole correspondence
  # table ever needed a multi-word split, corr_mat has exactly one "to_"
  # column. `corr_mat[ind, -1]` then collapses to a plain vector for any
  # cue-sequence with more than one response, and the old
  # `matrix(corr, ncol = length(corr))` reshaped it into a 1-row,
  # multi-column matrix instead of the intended multi-row, 1-column one --
  # forcing the row-splicing branch to run for perfectly ordinary,
  # non-splitting responses. Confirmed the untouched original visibly
  # corrupts data here (duplicated rows) since this scenario also lands on
  # the bug #42/#43 boundary (single cue-sequence spanning the whole
  # dataset). With bugs #42/#43/#46/#47 all fixed, this should behave
  # identically to the straightforward 1:1 case.
  data <- data.frame(
    ID = c(1, 1, 1),
    Cue = c("animal", "animal", "animal"),
    Response = c("dog", "cat", "bird"),
    stringsAsFactors = FALSE
  )
  row.names(data) <- as.character(seq_len(nrow(data)))

  corr_mat <- matrix(
    c("dog", "dog", "cat", "cat", "bird", "bird"),
    nrow = 3, byrow = TRUE,
    dimnames = list(NULL, c("from", "to_1"))
  )

  result <- correct_data_free(data, corr_mat, ids = c(1))

  expect_equal(nrow(result$corrected), 3)
  expect_equal(result$corrected$Response, c("dog", "cat", "bird"))
  expect_equal(unname(result$behavioral["1", "Perseverations"]), 0)

})

test_that("correct_data_free() counts a perseveration that only appears once a split response's later to_ column is considered (bug #47 fix)", {

  # Real bug fixed, isolated from bugs #42/#43/#46 via bracketing rows so
  # this cue-sequence sits in the middle of the dataset (not at either
  # boundary), and via a corr_mat that already has 2 genuine "to_"
  # columns (not the single-column collapse bug #46 covers separately):
  # one response ("catdog") splits into "cat"+"dog", while a separate,
  # unrelated response in the same cue-sequence is itself just "dog" --
  # a real duplicate. `correct_ord <- as.vector(t(corr[, 1]))` only ever
  # read the FIRST "to_" column across all matched responses, silently
  # missing any duplicate that only shows up in a later column -- so this
  # genuine duplicate (correctly detected and dropped from the output via
  # `add_responses`/`duplicated()`) was never counted in `behav_mat`.
  # Confirmed identically reproduced against the untouched original.
  data <- data.frame(
    ID = c("p0", "p1", "p1", "p1", "p1", "p2"),
    Cue = c("x", "animals", "animals", "animals", "animals", "y"),
    Response = c("zzz", "bird", "catdog", "dog", "fish", "yyy"),
    stringsAsFactors = FALSE
  )
  row.names(data) <- as.character(seq_len(nrow(data)))

  corr_mat <- matrix(
    c("zzz", "zzz", NA,
      "bird", "bird", NA,
      "catdog", "cat", "dog",
      "dog", "dog", NA,
      "fish", "fish", NA,
      "yyy", "yyy", NA),
    nrow = 6, byrow = TRUE,
    dimnames = list(NULL, c("from", "to_1", "to_2"))
  )

  result <- correct_data_free(data, corr_mat, ids = c("p0", "p1", "p2"))

  expect_equal(unname(result$behavioral["p1", "Perseverations"]), 1)
  expect_equal(result$corrected$Response[result$corrected$ID == "p1"], c("bird", "cat", "dog", "fish"))

})
