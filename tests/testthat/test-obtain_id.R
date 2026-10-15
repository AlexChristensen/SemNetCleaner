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
    Timestamp = c("t1", "t2", "t3"), Email = c("a1@x", "b2@x", "c3@x"),
    animal1 = c("cat", "dog", "bird")
  )
  expect_error(
    suppressMessages(obtain_id(d_fluency, type = "fluency")),
    "Multiple possible ID columns"
  )
  expect_error(suppressMessages(obtain_id(d_fluency, type = "fluency")), "Timestamp")
  expect_error(suppressMessages(obtain_id(d_fluency, type = "fluency")), "Email")

  d_free <- data.frame(
    Timestamp = c("t1", "t2", "t3", "t4"), Email = c("a1@x", "b2@x", "c3@x", "d4@x"),
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

test_that("obtain_id() keeps a one-column matrix when only one response column remains (issue #16)", {

  # Real bug fixed: `data[, -id_col]` collapsed to a plain vector with
  # exactly one response column left, so textcleaner()'s
  # `row.names(data) <- ids` failed ("attempt to set 'rownames' on an
  # object with no dimensions")
  data <- as.matrix(data.frame(ID = c("P1", "P2", "P3"), R1 = c("dog", "cat", "cow")))

  result <- suppressMessages(obtain_id(data, type = "fluency"))

  expect_true(is.matrix(result$data))
  expect_equal(dim(result$data), c(3, 1))
  expect_equal(result$ids, c("P1", "P2", "P3"))

})

test_that("obtain_id() recognizes plural/capitalized ID column names such as 'Participants' (issue #25)", {

  data <- as.matrix(data.frame(
    Participants = c(200, 202, 204), VF = c("a b c", "d e f", "g h i"), stringsAsFactors = FALSE
  ))

  result <- suppressMessages(obtain_id(data, type = "fluency"))

  expect_equal(result$ids, c("200", "202", "204"))
  expect_equal(colnames(result$data), "VF")

  data_ids <- as.matrix(data.frame(IDs = 1:3, R1 = c("dog", "cat", "cow"), R2 = c("cat", "cow", "dog")))
  expect_equal(suppressMessages(obtain_id(data_ids, type = "fluency"))$ids, c("1", "2", "3"))

})

test_that("obtain_id() treats a single participant with no named ID column as row 1, not as 'multiple possible ID columns'", {

  # With one row every column trivially has "one unique value per row", so
  # the unique-values heuristic cannot pick an ID column
  one <- as.matrix(data.frame(R1 = "dog", R2 = "cat", R3 = "cow"))

  result <- suppressMessages(obtain_id(one, type = "fluency"))

  expect_equal(result$ids, 1L)
  expect_equal(dim(result$data), c(1, 3))

})

test_that("obtain_id() rejects duplicated, missing or blank participant IDs instead of letting them be renamed (fluency)", {

  # Real bug fixed: duplicated IDs were silently renamed downstream
  # (`4`, `4` -> `X4`, `X4.1`) and a missing ID became "NA.", so the output no
  # longer matched the input and nothing said so
  make <- function(ids){ as.matrix(data.frame(ID = ids, R1 = c("dog", "cat", "cow", "pig"), R2 = c("cat", "dog", "pig", "cow"), stringsAsFactors = FALSE)) }

  expect_error(obtain_id(make(c("4", "4", "7", "9")), "fluency"), "duplicated values: '4'")
  expect_error(obtain_id(make(c("a", "b", "a", "b")), "fluency"), "duplicated values: 'a', 'b'")
  expect_error(obtain_id(make(c("4", NA, "7", "9")), "fluency"), "missing or blank values in row 2")
  expect_error(obtain_id(make(c("4", "", "7", " ")), "fluency"), "missing or blank values in rows 2, 4")

  # distinct IDs are used exactly as given, including odd-looking ones
  odd <- suppressMessages(obtain_id(make(c("P1", "P 2", "P-3", "3P")), "fluency"))
  expect_equal(odd$ids, c("P1", "P 2", "P-3", "3P"))

})

test_that("obtain_id() allows repeated IDs for long-format free-association data but not missing ones", {

  free <- as.matrix(data.frame(
    ID = c("1", "1", "2", "2"), Cue = "dog", Response = c("cat", "bone", "cat", "leash"), stringsAsFactors = FALSE
  ))

  expect_equal(suppressMessages(obtain_id(free, "free"))$ids, c("1", "2"))

  free[3, "ID"] <- NA
  expect_error(obtain_id(free, "free"), "missing or blank values in row 3")

})

test_that("obtain_id() uses explicit row names as fluency IDs, behind a named ID column and ahead of the unique-values guess", {

  # Real bug fixed (U2): ?textcleaner documents fluency IDs as the row names,
  # but obtain_id() never looked at them -- it fell through to row numbers
  # or, when every response in a column happened to be distinct, reported
  # several "possible ID columns". This is also what makes partBY = "col"
  # work: transposing turns the participants' column names into row names.
  with_row_names <- as.matrix(data.frame(
    R1 = c("dog", "cat", "cow"), R2 = c("cat", "dog", "pig"), row.names = c("alice", "bob", "carl")
  ))

  result <- suppressMessages(obtain_id(with_row_names, type = "fluency"))
  expect_equal(result$ids, c("alice", "bob", "carl"))
  expect_equal(dim(result$data), c(3, 2))

  # a named ID column still wins over row names
  named <- cbind(ID = c("1", "2", "3"), with_row_names)
  expect_equal(unname(suppressMessages(obtain_id(named, type = "fluency"))$ids), c("1", "2", "3"))

  # no row names at all -> row numbers, as before
  repeats <- unname(matrix(c("dog", "dog", "cat", "cow", "cow", "cat", "dog", "cat", "cat"), nrow = 3))
  expect_equal(suppressMessages(obtain_id(repeats, type = "fluency"))$ids, 1:3)

  # duplicated row names are rejected like duplicated ID values
  duplicated_names <- with_row_names
  row.names(duplicated_names) <- c("a", "a", "b")
  expect_error(obtain_id(duplicated_names, type = "fluency"), "duplicated values: 'a'")

})

test_that("obtain_id() never takes a column of response words as the IDs (undocumented inference tightened)", {

  # Without a named ID column, a column whose values are all different used
  # to be inferred as the IDs. In a small sample the first response column
  # can easily be all different ("dog", "cat", "bird"), so it was silently
  # taken as the IDs and dropped from the responses. Only columns that look
  # like IDs (numeric, or every value containing a digit) are inferred now
  words <- data.frame(A = c("dog", "cat", "bird"), B = c("cat", "fish", "cow"), C = c("pig", "dog", "hen"))
  result <- suppressMessages(obtain_id(words, type = "fluency"))
  expect_equal(result$ids, 1:3)
  expect_equal(ncol(result$data), 3)

  # a lone ID-looking column is still found
  coded <- cbind(Code = c("P01", "P02", "P03"), words)
  result <- suppressMessages(obtain_id(coded, type = "fluency"))
  expect_equal(result$ids, c("P01", "P02", "P03"))
  expect_equal(ncol(result$data), 3)

  numbered <- cbind(Nr = 11:13, words)
  expect_equal(suppressMessages(obtain_id(numbered, type = "fluency"))$ids, 11:13)

})
