test_that("obtain_id() finds a named ID column for fluency data", {

  d <- data.frame(ID = 1:3, animal1 = c("cat", "dog", "bird"), animal2 = c("fish", "cow", "pig"))
  result <- suppressMessages(obtain_id(d, type = "fluency"))

  expect_equal(result$ids, 1:3)
  expect_equal(colnames(result$data), c("animal1", "animal2"))

})

test_that("obtain_id() falls back to row numbers for fluency data with no ID column", {

  # No named ID column, and no uniquely-valued column either
  d <- data.frame(a = c("x", "x"), b = c("y", "y"))
  result <- suppressMessages(obtain_id(d, type = "fluency"))

  expect_equal(result$ids, seq_len(nrow(d)))

})

test_that("obtain_id() detects a single genuine unique-value candidate column with no named ID column (bug #50 fix)", {

  # Real bug fixed: this is the actual intended job of the "check for
  # variable with unique values" heuristic, but it had NO test coverage
  # at all before this fix -- the only two prior tests covered "named
  # column found" and "zero candidates," neither of which exercises this
  # code path's real purpose. `apply(data, 2, unique)` returns a MATRIX
  # (not a list) whenever every column's unique-value count happens to
  # tie -- `lapply()` on a matrix then iterates per CELL, not per column,
  # so every element has length 1 and the heuristic silently finds ZERO
  # candidates regardless of how many genuine ones exist. Confirmed
  # inherited identically from the untouched original. Here, `animal1`
  # and `animal2` both repeat values (2 unique each), while `SubjectCode`
  # is genuinely all-unique (4) -- ragged lengths, so this specific
  # construction actually worked even before the fix; `vapply()` fixes
  # the general case (see the file header and the two tests below for
  # constructions that DID fail before this fix).
  d <- data.frame(
    SubjectCode = c("s1", "s2", "s3", "s4"),
    animal1 = c("cat", "dog", "cat", "dog"),
    animal2 = c("fish", "fish", "cow", "cow")
  )
  result <- suppressMessages(obtain_id(d, type = "fluency"))

  expect_equal(result$ids, c("s1", "s2", "s3", "s4"))
  expect_equal(colnames(result$data), c("animal1", "animal2"))

})

test_that("obtain_id() detects a single genuine unique-value candidate even when every column's unique-count ties (bug #50 fix)", {

  # The actual failure mode: ALL columns tying on unique-count (not just
  # ragged-but-coincidentally-fine, like the test above) is exactly what
  # triggers apply()'s silent matrix-simplification. Confirmed directly
  # that the untouched original's mechanism (and this file's own code,
  # before the vapply() fix) found ZERO candidates here despite
  # `SubjectCode` being the sole genuine one.
  d <- data.frame(
    SubjectCode = c("s1", "s2", "s3"),
    animal1 = c("cat", "dog", "cat"),
    animal2 = c("fish", "cow", "fish")
  )
  result <- suppressMessages(obtain_id(d, type = "fluency"))

  expect_equal(result$ids, c("s1", "s2", "s3"))
  expect_equal(colnames(result$data), c("animal1", "animal2"))

})

test_that("obtain_id() handles a 0-row data set's degenerate unique-value check without misreporting total ambiguity", {

  # Every column trivially has 0 unique values in a 0-row data set, which
  # would otherwise trivially equal nrow(data) == 0 for EVERY column at
  # once -- misreporting total ambiguity instead of "no candidate."
  d <- data.frame(a = character(0), b = character(0))
  result <- suppressMessages(obtain_id(d, type = "fluency"))

  expect_equal(result$ids, integer(0))

})

test_that("obtain_id() handles a 0-row fluency data set without reversing the row-number range (bug fix)", {

  d <- data.frame(a = character(0), b = character(0))
  result <- suppressMessages(obtain_id(d, type = "fluency"))

  expect_equal(result$ids, integer(0))

})

test_that("obtain_id() finds a named ID column for free-association data", {

  d <- data.frame(ID = c(1, 1, 2, 2), Cue = c("a", "b", "a", "b"), Response = c("x", "y", "z", "w"))
  result <- suppressMessages(obtain_id(d, type = "free"))

  expect_equal(result$ids, c(1, 2))

})

test_that("obtain_id() raises a clear error for free-association data with no identifiable ID column (bug fix)", {

  # No named ID column, and no uniquely-valued column (Response has a repeat)
  d <- data.frame(Cue = c("a", "a", "b", "b"), Response = c("x", "x", "z", "w"))

  # The original crashed with the confusing internal error
  # "object 'ids' not found" here -- now a real, informative error
  expect_error(
    suppressMessages(obtain_id(d, type = "free")),
    "No IDs were identified"
  )

})

test_that("obtain_id() errors on multiple matching named ID columns, for both fluency and free data (per direct request)", {

  # The original silently let id_col become a multi-element vector here --
  # res$ids became a 2-column data frame instead of a single ID vector,
  # and BOTH matched columns got stripped from res$data. Confirmed
  # byte-identical to the untouched original before this fix. Now a real,
  # informative error instead of silently producing malformed output.
  d_fluency <- data.frame(Subject = 1:3, ID = c(101, 102, 103), animal1 = c("cat", "dog", "bird"))
  expect_error(
    suppressMessages(obtain_id(d_fluency, type = "fluency")),
    "Multiple possible ID columns"
  )

  d_free <- data.frame(
    ID = c(1, 1, 2, 2), Subject = c(1, 1, 2, 2),
    Cue = c("a", "b", "a", "b"), Response = c("x", "y", "z", "w")
  )
  expect_error(
    suppressMessages(obtain_id(d_free, type = "free")),
    "Multiple possible ID columns"
  )

  # Error message names the actual offending columns, to help diagnose
  expect_error(suppressMessages(obtain_id(d_fluency, type = "fluency")), "Subject")
  expect_error(suppressMessages(obtain_id(d_fluency, type = "fluency")), "ID")

})

test_that("obtain_id() errors on multiple ALL-UNIQUE candidate columns, consistent with the named-column ambiguity check above (robustness fix)", {

  # Real inconsistency found and fixed during a Phase 3 review, per direct
  # request: the named-column ambiguity check above already errors clearly
  # for both types, but the fallback "column with one unique value per
  # row" heuristic previously treated 2+ candidates the SAME as 0 --
  # silently indistinguishable. For type == "free" this produced a
  # genuinely misleading "No IDs were identified" error even though
  # candidates WERE found, just ambiguous. Fixed for consistency with the
  # sibling check's own established precedent: a real, informative error
  # naming the ambiguous columns, for both types.
  d_fluency <- data.frame(
    Timestamp = c("t1", "t2", "t3"), Email = c("a@x", "b@x", "c@x"),
    animal1 = c("cat", "dog", "bird")
  )
  expect_error(
    suppressMessages(obtain_id(d_fluency, type = "fluency")),
    "Multiple possible ID columns"
  )
  expect_error(suppressMessages(obtain_id(d_fluency, type = "fluency")), "Timestamp")
  expect_error(suppressMessages(obtain_id(d_fluency, type = "fluency")), "Email")

  d_free <- data.frame(
    Timestamp = c("t1", "t2", "t3", "t4"), Email = c("a@x", "b@x", "c@x", "d@x"),
    Cue = c("a", "b", "a", "b"), Response = c("x", "y", "z", "w")
  )
  expect_error(
    suppressMessages(obtain_id(d_free, type = "free")),
    "Multiple possible ID columns"
  )

})

test_that("obtain_id() detects a unique-value ID column when given a raw MATRIX, not just a data frame (integration bug, found building Phase 7)", {

  # Real bug fixed: `textcleaner()` (both the untouched original and this
  # rebuild's merged version) always calls `obtain_id()` with `data <-
  # as.matrix(data)` done immediately beforehand -- its only real calling
  # context -- but bug #50's own fix, `vapply(data, function(x){
  # length(unique(x))}, integer(1))`, silently assumed a DATA FRAME.
  # `vapply()`/`lapply()`/`sapply()` on a plain matrix iterate per
  # ELEMENT, not per column (a matrix has no per-column list structure to
  # begin with). Confirmed directly: on a matrix version of this same
  # data, every one of the 6 individual elements trivially has 1 unique
  # value, so the old code's `unique_counts` became a 6-element all-1s
  # vector instead of a 2-element per-column vector, and the "single
  # all-unique column is the ID" heuristic silently found NO candidate at
  # all, even though `SubjectCode` genuinely is one -- despite every one
  # of `obtain_id()`'s own prior tests (all data-frame input) passing.
  m <- as.matrix(data.frame(
    SubjectCode = c("s1", "s2", "s3"),
    animal1 = c("cat", "dog", "cat"),
    animal2 = c("fish", "cow", "fish")
  ))

  result <- suppressMessages(obtain_id(m, type = "fluency"))

  expect_equal(result$ids, c("s1", "s2", "s3"))
  expect_equal(colnames(result$data), c("animal1", "animal2"))

})
