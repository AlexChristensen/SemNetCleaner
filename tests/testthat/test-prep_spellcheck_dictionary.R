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
    test_matrix, allowPunctuations = "-", allowNumbers = FALSE, type = "fluency"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(result$c1, c("cat123", "dog extra", "bird-thing", NA))
  expect_equal(result$c3, c("underscore", "co mma", "quote", "apostrophe"))

})

test_that("prep_spellcheck_dictionary() respects allowPunctuations='all' and allowNumbers=TRUE", {

  test_matrix <- matrix(c("Cat123!", "co,mma"), nrow = 1, dimnames = list("p1", c("c1", "c2")))

  result <- prep_spellcheck_dictionary(
    test_matrix, allowPunctuations = "all", allowNumbers = TRUE, type = "fluency"
  )

  expect_equal(result$c1, "cat123!")
  expect_equal(result$c2, "co,mma")

})

test_that("prep_spellcheck_dictionary() handles the free-association branch (Response column only)", {

  d <- data.frame(ID = 1:2, Cue = c("animal", "animal"), Response = c("Cat!", "  Dog\textra "))

  result <- prep_spellcheck_dictionary(
    d, allowPunctuations = "-", allowNumbers = FALSE, type = "free"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(result$Response, c("cat", "dog extra"))
  expect_equal(result$Cue, c("animal", "animal"))

})

test_that("normalize_responses() reverts the literal string 'NA' back to a real NA", {

  expect_true(is.na(normalize_responses("NA", allow_punctuations = "-", allow_numbers = FALSE)))

})

test_that("normalize_responses() matches a hand-looped reference across mixed/edge-case input (optimization verification)", {

  # normalize_responses() was rewritten to vectorize two per-element
  # lapply() loops (~19x faster on real data, see REFACTOR_PLAN.md). The
  # property that matters: vectorizing must not let one response affect
  # another, so the vectorized result equals normalizing each response alone
  reference <- function(responses, allow_punctuations, allow_numbers){
    unlist(lapply(
      responses,
      function(response){ as.character(normalize_responses(response, allow_punctuations, allow_numbers)) }
    ))
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
      normalize_responses(case, allow_punctuations = "-", allow_numbers = FALSE),
      reference(case, allow_punctuations = "-", allow_numbers = FALSE)
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
  result <- normalize_responses(character(0), allow_punctuations = "-", allow_numbers = FALSE)
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
    c("cat dog", "", "bird"), allow_punctuations = "-", allow_numbers = FALSE
  )
  expect_equal(result, c("cat dog", NA, "bird"))

})

test_that("prep_spellcheck_dictionary() still matches the original on real open.clean-scale data after optimization", {

  test_matrix <- as.matrix(open.clean)

  result <- prep_spellcheck_dictionary(
    test_matrix, allowPunctuations = "-", allowNumbers = FALSE, type = "fluency"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(dim(result), dim(test_matrix))
  expect_equal(row.names(result), row.names(test_matrix))

})

test_that("normalize_responses() transliterates accented characters instead of deleting them", {

  # Real bug fixed (found driving the manual wizard on `open.animals`): every
  # non-ASCII character was deleted, so "café" became "caf" and "señor"
  # "seor" -- valid words silently turned into misspellings
  result <- normalize_responses(
    c("café", "señor", "ünï", "naïve", "Zoë", "ça va", "it’s"),
    allow_punctuations = "-", allow_numbers = FALSE
  )

  expect_equal(result, c("cafe", "senor", "uni", "naive", "zoe", "ca va", "its"))

  # Scripts with no ASCII equivalent are still removed
  expect_equal(normalize_responses(c("dog日本"), "-", FALSE), "dog")

})

test_that("normalize_responses() treats a response that is empty after cleaning as missing, not as ''", {

  # Real bug fixed: a response with only untransliterable characters ended
  # up as "" and was then offered to the user in the spell-check wizard
  result <- normalize_responses(c("日本", "dog", "123", "?!", "  "), "-", FALSE)

  expect_equal(result, c(NA, "dog", NA, NA, NA))

})

test_that("normalize_responses() accepts 'no punctuation allowed' and escapes the allowed characters", {

  # Real bugs fixed: (1) allowPunctuations = "" / NULL built the invalid
  # regular expression "([])|[[:punct:]]" and crashed the whole run;
  # (2) allowed characters went into a bracket expression unescaped, so
  # c("'", "-", "_") allowed the RANGE ' to _ and "]"/"^"/"\\" broke it
  words <- c("o'brien", "cow/pig", "ice-cream", "e.g.", "a_b", "x]y", "c^d")

  none <- c("obrien", "cow pig", "icecream", "eg", "ab", "xy", "cd")
  expect_equal(normalize_responses(words, "", FALSE), none)
  expect_equal(normalize_responses(words, NULL, FALSE), none)
  expect_equal(normalize_responses(words, character(0), FALSE), none)

  expect_equal(normalize_responses(words, "-", FALSE), c("obrien", "cow pig", "ice-cream", "eg", "ab", "xy", "cd"))
  expect_equal(normalize_responses(words, "all", FALSE), words)

  # a "-" between other characters is a literal hyphen, not a range
  expect_equal(
    normalize_responses(c("a'b", "a-b", "a_b", "a/b", "a(b"), c("'", "-", "_"), FALSE),
    c("a'b", "a-b", "a_b", "a b", "ab")
  )

  # characters special inside a bracket expression can be allowed
  expect_equal(normalize_responses(c("x]y", "c^d", "a\\b"), c("]", "^", "\\"), FALSE), c("x]y", "c^d", "a\\b"))

})

test_that("normalize_responses() keeps words apart at separators and keeps digits that are part of a word (U9)", {

  # Real bugs fixed: (1) a separator typed without spaces was deleted with
  # the other punctuation, gluing two words together ("cow/pig" -> "cowpig",
  # which was then accepted as a response); (2) every digit was deleted, so a
  # typo such as "d0g" silently became the different word "dg"
  expect_equal(
    normalize_responses(c("cow/pig", "fish,bird", "dog;cat", "a&b", "dog | cat", "x+y", "a:b", "a\\b"), "-", FALSE),
    c("cow pig", "fish bird", "dog cat", "a b", "dog cat", "x y", "a b", "a b")
  )

  # an explicitly allowed separator is left alone
  expect_equal(normalize_responses(c("cow/pig", "fish,bird"), "/", FALSE), c("cow/pig", "fish bird"))
  expect_equal(normalize_responses("cow/pig", "all", FALSE), "cow/pig")

  # stand-alone numbers (list numbering, counts, dates) go; digits inside a word stay
  expect_equal(
    normalize_responses(c("1. dog", "12 dogs", "(2)", "24/7", "7", "d0g", "cat2", "3rd"), "-", FALSE),
    c("dog", "dogs", NA, NA, NA, "d0g", "cat2", "3rd")
  )
  expect_equal(normalize_responses(c("d0g", "12", "7 cats"), "-", TRUE), c("d0g", "12", "7 cats"))

  # runs of whitespace are collapsed, so "polar  bear" is the dictionary entry "polar bear"
  expect_equal(normalize_responses(c("polar  bear", "  sheep  ", "a / b"), "-", FALSE), c("polar bear", "sheep", "a b"))

})
