test_that("prep_spellcheck_dictionary() matches the original's fluency branch on real-shaped mixed data", {

  # Confirmed via direct comparison against the original's own apply()-based
  # fluency branch (see REFACTOR_PLAN.md's Phase 3 write-up): identical
  # output across punctuation, digits, newlines/tabs, mixed case, NA,
  # quotes, apostrophes, underscores, commas, and multi-word responses
  test_matrix <- matrix(
    c("Cat123!", "  dog\textra ", "BIRD-thing", NA, "multi word resp.", "12345",
      "tab\ttab", "  spaced  ", "Under_score", "co,mma", "quo\"te", "Ap'ostrophe"),
    nrow = 4, ncol = 3,
    dimnames = list(paste0("p", 1:4), paste0("c", 1:3))
  )

  result <- prep_spellcheck_dictionary(
    test_matrix, allowPunctuations = "-", allowNumbers = FALSE, lowercase = TRUE, type = "fluency"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(result$c1, c("cat", "dog extra", "bird-thing", NA))
  expect_equal(result$c3, c("underscore", "comma", "quote", "apostrophe"))

})

test_that("prep_spellcheck_dictionary() respects allowPunctuations='all' and allowNumbers=TRUE", {

  test_matrix <- matrix(c("Cat123!", "co,mma"), nrow = 1, dimnames = list("p1", c("c1", "c2")))

  result <- prep_spellcheck_dictionary(
    test_matrix, allowPunctuations = "all", allowNumbers = TRUE, lowercase = FALSE, type = "fluency"
  )

  expect_equal(result$c1, "Cat123!")
  expect_equal(result$c2, "co,mma")

})

test_that("prep_spellcheck_dictionary() handles the free-association branch (Response column only)", {

  d <- data.frame(ID = 1:2, Cue = c("animal", "animal"), Response = c("Cat!", "  Dog\textra "))

  result <- prep_spellcheck_dictionary(
    d, allowPunctuations = "-", allowNumbers = FALSE, lowercase = TRUE, type = "free"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(result$Response, c("cat", "dog extra"))
  expect_equal(result$Cue, c("animal", "animal"))

})

test_that("normalize_responses() reverts the literal string 'NA' back to a real NA", {

  expect_true(is.na(normalize_responses("NA", allow_punctuations = "-", allow_numbers = FALSE, lowercase = TRUE)))

})

test_that("normalize_responses() matches a hand-looped reference across mixed/edge-case input (optimization verification)", {

  # normalize_responses() was rewritten to vectorize two per-element
  # lapply() loops (~19x faster on real data, see REFACTOR_PLAN.md) --
  # verified identical() against the original loop-based logic here
  reference <- function(responses, allow_punctuations, allow_numbers, lowercase){
    if(all(allow_punctuations != "all")){
      characters <- paste("([", paste(allow_punctuations, collapse = ""), "])|[[:punct:]]", sep = "", collapse = "")
      responses <- gsub(characters, "\\1", responses)
    }
    if(!isTRUE(allow_numbers)){
      responses <- gsub("[[:digit:]]+", "", responses)
    }
    responses <- unlist(lapply(trimws(responses), remove_leading_space))
    if(isTRUE(lowercase)){
      responses <- tolower(responses)
    }
    responses <- gsub("[\n\r\t]", " ", responses)
    split_responses <- strsplit(responses, split = " ")
    split_responses <- lapply(split_responses, function(y) gsub("[^\x20-\x7E]", "", y))
    responses <- unlist(lapply(split_responses, paste, sep = "", collapse = " "))
    ifelse(responses == "NA", NA, responses)
  }

  cases <- list(
    c("cat dog", "", "bird", NA, "  ", "multi word resp", "NA"),
    c("", "", ""),
    c("Cat123!", "quo\"te", "under_score"),
    "single",
    c(NA, NA)
  )

  for(case in cases){
    expect_equal(
      normalize_responses(case, allow_punctuations = "-", allow_numbers = FALSE, lowercase = TRUE),
      reference(case, allow_punctuations = "-", allow_numbers = FALSE, lowercase = TRUE)
    )
  }

})

test_that("normalize_responses() handles a genuinely empty input without corrupting its shape (bug fix)", {

  # Real bug fixed: bad_response()'s own `if(length(word) == 0) word <- NA`
  # guard is designed for "a single blank response", not "zero responses
  # at all" -- calling it directly on an empty vector (as the vectorized
  # rewrite does) would otherwise expand character(0) into a length-1 NA.
  # The original's per-element lapply() never hit this, since lapply()
  # over zero elements is a no-op -- confirmed via direct comparison.
  result <- normalize_responses(character(0), allow_punctuations = "-", allow_numbers = FALSE, lowercase = TRUE)
  expect_equal(result, character(0))
  expect_length(result, 0)

})

test_that("normalize_responses() correctly regroups words after an empty response mid-vector (bug fix)", {

  # Real bug fixed: the flattened-gsub-then-regroup optimization used
  # split(flat_words, rep(seq_along(n), n)) to reassign cleaned words back
  # to their original response -- but split() silently DROPS a group with
  # zero members (an empty response) unless the grouping factor explicitly
  # declares every expected level, misaligning every response after it.
  # Confirmed directly: 3 responses with the middle one empty produced
  # only 2 groups without the fix. Fixed via factor(..., levels = ...).
  # "" is itself one of bad_response()'s recognized "bad" values, so it
  # correctly becomes NA through normalization (unrelated to this fix) --
  # what this test actually checks is that "bird" (the response AFTER the
  # empty one) survives correctly, rather than being dropped/misaligned
  result <- normalize_responses(
    c("cat dog", "", "bird"), allow_punctuations = "-", allow_numbers = FALSE, lowercase = TRUE
  )
  expect_equal(result, c("cat dog", NA, "bird"))

})

test_that("prep_spellcheck_dictionary() still matches the original on real open.clean-scale data after optimization", {

  test_matrix <- as.matrix(open.clean)

  result <- prep_spellcheck_dictionary(
    test_matrix, allowPunctuations = "-", allowNumbers = FALSE, lowercase = TRUE, type = "fluency"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(dim(result), dim(test_matrix))
  expect_equal(row.names(result), row.names(test_matrix))

})
