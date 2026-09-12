test_that("textcleaner() fluency: happy path with all-correct responses produces the expected binary/clean/behavioral output", {

  raw <- data.frame(
    ID = c("P1", "P2", "P3"),
    R1 = c("dog", "bird", "cow"), R2 = c("cat", "fish", "pig"),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = "animals", spelling = "US",
    reader = scripted_reader(c("", "", "")), edit_fn = function(x) x
  ))

  expect_s3_class(result, "textcleaner")
  expect_true("fluency" %in% class(result))
  expect_equal(row.names(result$responses$clean), c("P1", "P2", "P3"))
  expect_equal(unname(result$responses$clean["P1", ]), c("dog", "cat"))
  expect_equal(result$behavioral$Appropriate, c(2, 2, 2))
  expect_equal(result$responses$binary["P1", "dog"], 1)
  expect_equal(result$responses$binary["P1", "pig"], 0)

})

test_that("textcleaner() fluency: does not crash with the default partBY (real bug fix)", {

  # Real bug fixed: `partBY`'s own default, `c("row", "col")`, was never
  # resolved with `match.arg()` in the untouched original --
  # `if(partBY == "col")` on the unresolved 2-element default then
  # compares a length-2 vector against a scalar, which errors outright
  # ("the condition has length > 1") on any R >= 4.3, for the DEFAULT
  # call path of this package's own flagship function. Confirmed directly
  # against the untouched original's own logic.
  raw <- data.frame(ID = c("P1", "P2"), R1 = c("dog", "bird"), R2 = c("cat", "fish"), stringsAsFactors = FALSE)

  result <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = "animals", spelling = "US",
    reader = scripted_reader(c("", "", "")), edit_fn = function(x) x
  ))

  expect_s3_class(result, "textcleaner")

})

test_that("textcleaner() fluency: partBY = 'col' transposes the data", {

  # Before transposing, participants are COLUMNS ("P1"/"P2") and "ID"/"R1"/
  # "R2" are rows; after `t()`, "ID" becomes a real named column obtain_id()
  # can find unambiguously
  raw <- data.frame(P1 = c("P1", "dog", "cat"), P2 = c("P2", "bird", "fish"), stringsAsFactors = FALSE)
  row.names(raw) <- c("ID", "R1", "R2")

  result <- suppressMessages(textcleaner(
    raw, type = "fluency", partBY = "col", dictionary = "animals", spelling = "US",
    reader = scripted_reader(c("", "", "")), edit_fn = function(x) x
  ))

  expect_equal(sort(row.names(result$responses$clean)), c("P1", "P2"))

})

test_that("textcleaner() fluency: keepLength correctly removes only over-length responses (two compounding real bugs fixed)", {

  # Two real, compounding bugs fixed: (1) `response_lengths <- apply(data,
  # 1:2, length)` always measures 1 (a cell's own `length()`, not its word
  # count) regardless of how many words a response has; (2) even once word
  # counts are measured correctly, the removal threshold itself was off by
  # one (`>= keepLength` instead of `> keepLength`), which -- for
  # `keepLength = 1` -- removed EVERY response instead of just multi-word
  # ones (every response trivially has >= 1 words). Bug (1) alone masked
  # bug (2) completely, which is almost certainly why this was never
  # caught. Confirmed directly: only the 2-word "polar bear" should be
  # removed here, not "dog"/"cat"/"cat".
  raw <- data.frame(
    ID = c("P1", "P2"), R1 = c("dog", "polar bear"), R2 = c("cat", "cat"),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(suppressWarnings(textcleaner(
    raw, type = "fluency", dictionary = "animals", spelling = "US",
    keepStrings = TRUE, keepLength = 1,
    reader = scripted_reader(rep("", 20)), edit_fn = function(x) x
  )))

  expect_equal(unname(unlist(result$responses$original["P1", ])), c("dog", "cat"))
  expect_true(is.na(result$responses$original["P2", "R1"]))
  expect_equal(unname(result$responses$original["P2", "R2"]), "cat")

})

test_that("textcleaner() fluency: a manual edit made during correct_changes() actually flows into the final cleaned/binary output (two real bugs fixed)", {

  # Two real, compounding bugs fixed, both only reachable when EXACTLY one
  # response ends up needing automated-change review (an entirely
  # ordinary occurrence):
  #  (1) `res$spellcheck$automated <- corr_mat[spell_check$auto, ]` (no
  #      `drop = FALSE`) collapsed a single-row selection into a plain
  #      named vector, losing the correspondence-matrix ROW NAME (only
  #      column names survive). `correct_changes()`'s own
  #      `row.names(res$spellcheck$automated)` was then NULL, and
  #      `correspondence[NULL, ] <- changes` silently assigns to ZERO
  #      rows -- discarding the user's edit with no error at all.
  #      Confirmed inherited identically from the untouched original.
  #  (2) even with (1) fixed so the edit lands in
  #      `res$spellcheck$correspondence`, the untouched original's
  #      fluency branch never re-synced its local `corr.mat` from it
  #      afterward (unlike the free-association branch, which explicitly
  #      does) -- `correct_data()` a few lines down used the STALE,
  #      pre-edit `corr.mat`, so the edit never reached
  #      `$responses$clean`/`$responses$binary` regardless.
  raw <- data.frame(
    ID = c("P1", "P2"), R1 = c("dogg", "cat"), R2 = c("bird", "cat"),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = "animals", spelling = "US",
    reader = scripted_reader(c("", "", "")),
    edit_fn = function(x){ x[1, "to_1"] <- "zebra"; x }
  ))

  expect_true(any(result$spellcheck$correspondence[, "to_1"] == "zebra", na.rm = TRUE))
  expect_equal(unname(result$responses$clean["P1", ]), c("zebra", "bird"))
  expect_equal(unname(result$responses$binary["P1", "zebra"]), 1)
  expect_false("dog" %in% colnames(result$responses$binary))

})

test_that("textcleaner() fluency: the cleaned response matrix keeps one row per participant even when every participant has the same response count (real bug fix)", {

  # Real bug fixed: `apply(corrected$corrected, 1, function(x){...})`
  # only returns a LIST (one element per participant) when the per-row
  # result lengths are ragged -- whenever every participant ties (the
  # ordinary case, exercised directly here with 3 participants each
  # having exactly 2 responses), `apply()` silently simplifies its result
  # into a MATRIX instead, and the very next step (`lapply()` over it)
  # then iterates per CELL, not per participant, corrupting the whole
  # cleaned-response-matrix construction.
  raw <- data.frame(
    ID = c("P1", "P2", "P3"),
    R1 = c("dog", "bird", "cow"), R2 = c("cat", "fish", "pig"),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = "animals", spelling = "US",
    reader = scripted_reader(c("", "", "")), edit_fn = function(x) x
  ))

  expect_equal(dim(result$responses$clean), c(3, 2))
  expect_equal(row.names(result$responses$clean), c("P1", "P2", "P3"))

})

test_that("textcleaner() free: happy path builds a response x cue frequency matrix", {

  raw <- data.frame(
    ID = c(1, 1, 2, 2, 3, 3),
    Cue = "dog",
    Response = c("cat", "bone", "cat", "leash", "cat", "bone"),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(suppressWarnings(textcleaner(
    raw, type = "free", dictionary = "animals", spelling = "US",
    reader = scripted_reader(rep("", 20)), edit_fn = function(x) x
  )))

  expect_s3_class(result, "textcleaner")
  expect_true("free" %in% class(result))
  expect_equal(unname(result$responses$clean["cat", "dog"]), 3)
  expect_equal(unname(result$responses$clean["bone", "dog"]), 2)
  expect_equal(unname(result$responses$clean["leash", "dog"]), 1)

})

test_that("textcleaner() free: an embedded bad-word marker is only detected as its own word, not a substring (bug #54 fix)", {

  # Real bug fixed: the free-only correspondence-matrix "mark embedded bad
  # words" step used a bare substring match (`grep("NA", x)`), meant to
  # catch a multi-word response like "polar NA" (one word of a compound
  # manually marked bad) and treat the whole compound as unusable -- but a
  # substring match also fires on any ordinary word that merely CONTAINS
  # "NA", e.g. "CANADA", silently corrupting it into the bad-response
  # sentinel. Only reachable with `lowercase = FALSE` (a lowercased word
  # can never contain an uppercase "NA" substring).
  #
  # Exercised via an already-completed `continue` (bypassing
  # spellcheck_dictionary() entirely, which for `type = "free"` always
  # spell-checks against the real, ~109k-word `cocaspell` dictionary
  # regardless of the `dictionary =` argument -- appropriately slow for a
  # genuinely unresolvable word, but unnecessary for testing this one
  # specific downstream correspondence-matrix fix in isolation).
  data <- data.frame(
    ID = c(1, 2), Cue = c("test", "test"), Response = c("polarbear", "canada"),
    stringsAsFactors = FALSE
  )

  spell_check_result <- list(
    type = "free",
    from = list("001" = "polarbear", "002" = "canada"),
    to = list("001" = "polar NA", "002" = "CANADA"),
    manual = numeric(0), auto = c(1, 2),
    data = data, stop = FALSE
  )
  class(spell_check_result) <- "textcleaner"

  result <- suppressMessages(suppressWarnings(textcleaner(
    continue = spell_check_result, reader = scripted_reader(rep("", 20)), edit_fn = function(x) x
  )))

  expect_equal(unname(result$spellcheck$correspondence["001", "to_1"]), "NA")
  expect_equal(unname(result$spellcheck$correspondence["002", "to_1"]), "CANADA")

})

test_that("textcleaner() correctly routes a `continue` that is already a completed spell-check result, instead of a still-in-progress one (real bug fix, bug #10's pattern at a second call site)", {

  # Real bug fixed: the untouched original distinguished "still resuming"
  # from "already completed" `continue` shapes by counting list elements
  # (`length(continue) != 7` for fluency) -- the same fragile pattern as
  # bug #10, just one level up (in `textcleaner.fluency()`/`.free()`
  # rather than inside `spellcheck.dictionary()`/`.free()` itself). Both
  # shapes carry an explicit `$stop` flag (`FALSE` once finished) --
  # routing on that instead is unambiguous regardless of optional fields.
  raw <- data.frame(ID = c("P1", "P2"), R1 = c("dog", "bird"), R2 = c("cat", "fish"), stringsAsFactors = FALSE)

  completed <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = "animals", spelling = "US",
    reader = scripted_reader(c("", "", "")), edit_fn = function(x) x
  ))

  # A "completed" spell-check result, handed back in as `continue` --
  # should skip straight to finalizing, not attempt to resume a wizard
  spell_check_result <- list(
    type = "fluency", from = completed$spellcheck$correspondence[, "from"],
    to = as.list(completed$spellcheck$correspondence[, "from"]),
    manual = numeric(0), auto = seq_len(nrow(completed$spellcheck$correspondence)),
    data = completed$responses$original, stop = FALSE
  )
  class(spell_check_result) <- "textcleaner"

  result <- suppressMessages(textcleaner(
    continue = spell_check_result, reader = scripted_reader(c("", "", "")), edit_fn = function(x) x
  ))

  expect_s3_class(result, "textcleaner")
  expect_false(isTRUE(result$stop))

})

test_that("textcleaner()'s edit_fn defaults to automated_edit(), not base edit() (Phase 9: guards the OS-fork removal)", {

  expect_identical(formals(textcleaner)$edit_fn, quote(automated_edit))

})
