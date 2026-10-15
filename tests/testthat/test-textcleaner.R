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

test_that("textcleaner() free: a bad word dropped from a kept string leaves the rest of it (BAD WORD drops only the word)", {

  # Free association used to turn any correspondence cell containing the word
  # "NA" into "NA" (bug #54 fixed its substring match), so one bad word made
  # the whole kept string unusable. BAD WORD now drops only the word, in both
  # tasks: the string keeps its other words and one "NA" marker records the
  # bad word. Exercised via an already-completed `continue` (bypassing
  # spellcheck_dictionary(), which for type = "free" always checks against the
  # ~109k-word cocaspell dictionary)
  data <- data.frame(
    ID = c(1, 2), Cue = c("test", "test"), Response = c("pitza hut", "canada"),
    stringsAsFactors = FALSE
  )

  spell_check_result <- list(
    type = "free",
    from = list("001" = "pitza hut", "002" = "canada"),
    to = list("001" = c("hut", "NA"), "002" = "canada"),
    manual = 1, auto = 2,
    data = data, stop = FALSE
  )
  class(spell_check_result) <- "textcleaner"

  result <- suppressMessages(suppressWarnings(textcleaner(
    continue = spell_check_result, reader = scripted_reader(rep("", 20)), edit_fn = function(x) x
  )))

  cm <- result$spellcheck$correspondence
  expect_equal(unname(cm["001", "to_1"]), "hut")
  expect_equal(unname(cm["001", "to_2"]), "NA")
  expect_equal(unname(cm["002", "to_1"]), "canada")
  expect_true("hut" %in% result$data$clean$Response)
  expect_false(any(result$data$clean$Response == "NA"))

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

test_that("textcleaner() fluency: handles data with a single response column after the ID column (issue #16)", {

  # Real bug fixed: with exactly one response column left after removing the
  # ID column, `data[, -id_col]` collapsed to a vector and
  # `row.names(data) <- ids` failed with "attempt to set 'rownames' on an
  # object with no dimensions"
  raw <- data.frame(ID = c("P1", "P2", "P3"), R1 = c("dog", "cat", "cow"), stringsAsFactors = FALSE)

  result <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = "animals", spelling = "US",
    reader = scripted_reader(rep("", 5)), edit_fn = function(x) x
  ))

  expect_s3_class(result, "textcleaner")
  expect_equal(row.names(result$responses$clean), c("P1", "P2", "P3"))
  expect_equal(unname(result$responses$clean[, 1]), c("dog", "cat", "cow"))

})

test_that("textcleaner() guesses 'fluency' for small wide-format data, and 'free' only when Cue/Response columns exist (issue #18)", {

  # Real bug fixed: the task type used to be guessed as `ncol(data) > 3`,
  # so an ID column plus one or two response columns was treated as free
  # association and failed with "data[, 'Response']: subscript out of bounds"
  expect_equal(detect_task_type(data.frame(ID = 1, R1 = "a", R2 = "b")), "fluency")
  expect_equal(detect_task_type(data.frame(ID = 1, R1 = "a")), "fluency")
  expect_equal(detect_task_type(data.frame(ID = 1, Cue = "a", Response = "b")), "free")
  expect_equal(detect_task_type(data.frame(id = 1, cue = "a", response = "b", extra = 1)), "free")
  expect_equal(detect_task_type(matrix("a", 1, 5)), "fluency")

  raw <- data.frame(ID = c("P1", "P2"), R1 = c("dog", "cat"), R2 = c("cow", "pig"), stringsAsFactors = FALSE)

  result <- suppressMessages(textcleaner(
    raw, dictionary = "animals", spelling = "US",
    reader = scripted_reader(rep("", 5)), edit_fn = function(x) x
  ))

  expect_true("fluency" %in% class(result))

})

test_that("textcleaner() free: missing Cue/Response columns gives an explanatory error", {

  raw <- data.frame(ID = c(1, 1), Word = c("a", "b"), stringsAsFactors = FALSE)

  expect_error(
    suppressWarnings(suppressMessages(textcleaner(raw, type = "free", reader = scripted_reader(rep("", 5))))),
    'no "Cue" column was found'
  )

})

test_that("textcleaner() fluency: one column holding whole word lists gets a format message, not an unrelated error (issue #25)", {

  raw <- data.frame(
    Participants = c(200, 202, 204),
    VF = c("dog cat bird fish", "cow pig horse sheep goat", "lion tiger bear wolf"),
    stringsAsFactors = FALSE
  )

  expect_error(
    suppressMessages(textcleaner(
      raw, type = "fluency", dictionary = "animals", spelling = "US",
      reader = scripted_reader(rep("", 5)), edit_fn = function(x) x
    )),
    "ONE response per column"
  )

})

test_that("textcleaner() fluency: keepRepeats = TRUE retains repeated responses in $clean, FALSE drops them (issue #17)", {

  raw <- data.frame(
    ID = c("P1", "P2"), R1 = c("dog", "bird"), R2 = c("cat", "dog"),
    R3 = c("dog", "bird"), R4 = c("dog", "fish"), stringsAsFactors = FALSE
  )

  run <- function(...){
    suppressMessages(textcleaner(
      raw, type = "fluency", dictionary = "animals", spelling = "US",
      reader = scripted_reader(rep("", 5)), edit_fn = function(x) x, ...
    ))
  }

  default <- run()
  expect_equal(unname(default$responses$clean["P1", 1:2]), c("dog", "cat"))
  expect_true(is.na(default$responses$clean["P1", 3]))

  kept <- run(keepRepeats = TRUE)
  expect_equal(unname(kept$responses$clean["P1", ]), c("dog", "cat", "dog", "dog"))
  expect_equal(unname(kept$responses$clean["P2", ]), c("bird", "dog", "bird", "fish"))

  # Binary matrix and behavioral counts are the same either way
  expect_equal(kept$responses$binary, default$responses$binary)
  expect_equal(kept$behavioral, default$behavioral)
  expect_equal(kept$behavioral$Perseverations, c(2, 1))

})

test_that("textcleaner() keeps numeric IDs exactly as written instead of space-padding them (open.animals)", {

  # Real bug fixed: `as.matrix()` on a data frame with a numeric ID column
  # and character responses ran `format()` over the IDs, padding them to a
  # common width ("  4", " 10", "100"), so results' row names no longer
  # matched the input IDs (67 of the 516 IDs in `open.animals`)
  raw <- data.frame(
    ID = c(4L, 10L, 100L), R1 = c("dog", "cat", "cow"), R2 = c("cat", "cow", "pig"),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = "animals", spelling = "US",
    reader = scripted_reader(rep("", 5)), edit_fn = function(x) x
  ))

  expect_equal(row.names(result$responses$clean), c("4", "10", "100"))
  expect_equal(row.names(result$responses$binary), c("4", "10", "100"))
  expect_equal(row.names(result$behavioral), c("4", "10", "100"))

  expect_identical(as_unpadded_matrix(raw)[, "ID"], c("4", "10", "100"))
  expect_identical(unname(as_unpadded_matrix(data.frame(a = c(0.5, 12), b = c("x", "y")))[, "a"]), c("0.5", "12"))

})

test_that("textcleaner() handles a single participant", {

  # Real bugs fixed: a one-row matrix lost its dimensions in convert_miss()
  # ("non-numeric matrix extent"), and with no named ID column the single
  # row made every column look like an ID candidate
  run <- function(data){
    suppressMessages(textcleaner(
      data, type = "fluency", dictionary = "animals", spelling = "US",
      reader = scripted_reader(rep("", 5)), edit_fn = function(x) x
    ))
  }

  with_id <- run(data.frame(ID = 7, R1 = "dog", R2 = "cat", R3 = "cow", stringsAsFactors = FALSE))
  expect_equal(row.names(with_id$responses$clean), "7")
  expect_equal(unname(with_id$responses$clean[1, ]), c("dog", "cat", "cow"))
  expect_equal(with_id$behavioral$Appropriate, 3)

  without_id <- run(data.frame(R1 = "dog", R2 = "cat", R3 = "cow", stringsAsFactors = FALSE))
  expect_equal(row.names(without_id$responses$clean), "1")

  one_column <- run(data.frame(ID = 7, R1 = "dog", stringsAsFactors = FALSE))
  expect_equal(unname(one_column$responses$clean[1, ]), "dog")

})

test_that("textcleaner() stops with a plain message, not a silent NULL, when there is nothing to clean", {

  # Real bug fixed: these inputs printed a bug-report block and returned
  # NULL, which a script cannot tell from success
  base <- data.frame(ID = 1:3, R1 = c("dog", "cat", "cow"), R2 = c("cat", "cow", "pig"), stringsAsFactors = FALSE)
  run <- function(data){ textcleaner(data, type = "fluency", dictionary = "animals", spelling = "US", reader = scripted_reader(rep("", 5)), edit_fn = function(x) x) }

  expect_error(run(NULL), "must be a data frame or matrix")
  expect_error(run("dog"), "must be a data frame or matrix")
  expect_error(run(base[0, ]), "has no rows")
  expect_error(run(base[, 0]), "has no columns")

  all_missing <- base
  all_missing[, -1] <- "99"
  expect_error(suppressMessages(run(all_missing)), "No responses were found to spell-check")

  blank <- base
  blank[, -1] <- ""
  expect_error(suppressMessages(run(blank)), "No responses were found to spell-check")

})

test_that("textcleaner() passes obtain_id()'s plain-language errors through", {

  ambiguous <- data.frame(ID = 1:2, Subject = 3:4, R1 = c("dog", "cat"), stringsAsFactors = FALSE)

  expect_error(
    suppressMessages(textcleaner(
      ambiguous, type = "fluency", dictionary = "animals", spelling = "US",
      reader = scripted_reader(rep("", 5)), edit_fn = function(x) x
    )),
    "Multiple possible ID columns"
  )

})

test_that("textcleaner() cleans with the other options too: allowNumbers = TRUE and no-punctuation values run to completion", {

  # Real bugs fixed: capital letters and digits crashed the automated pass
  # (qwerty_distance's key table lacked them), and allowPunctuations = ""
  # built an invalid regular expression
  raw <- data.frame(
    ID = 1:4, R1 = c("Dog", "DOG", "Cat", "cats"), R2 = c("d0g", "cat", "Cow", "pig2"),
    R3 = c("Elefant", "Fish", "bird", "Bird"), stringsAsFactors = FALSE
  )

  # SKIP every manual decision, answer "no" to every add-to-dictionary prompt
  skip_everything <- function(prompt = ""){
    if(grepl("accepts lowercase", prompt)) "1" else if(grepl("Y/n", prompt, fixed = TRUE)) "n" else ""
  }

  run <- function(...){
    suppressMessages(textcleaner(
      raw, type = "fluency", dictionary = "animals", spelling = "US",
      reader = skip_everything, edit_fn = function(x) x, ...
    ))
  }

  for(extra in list(list(allowNumbers = TRUE), list(allowPunctuations = ""), list(allowPunctuations = NULL))){
    result <- do.call(run, extra)
    expect_s3_class(result, "textcleaner")
    expect_equal(nrow(result$responses$clean), 4)
  }

})

test_that("textcleaner() stops on duplicated or missing participant IDs rather than silently renaming them", {

  raw <- data.frame(
    ID = c(4, 4, 7), R1 = c("dog", "cat", "cow"), R2 = c("cat", "dog", "pig"), stringsAsFactors = FALSE
  )
  run <- function(data){ suppressMessages(textcleaner(data, type = "fluency", dictionary = "animals", spelling = "US", reader = scripted_reader(rep("", 5)), edit_fn = function(x) x)) }

  expect_error(run(raw), "ID column 'ID' has duplicated values: '4'")

  raw$ID <- c(4, NA, 7)
  expect_error(run(raw), "missing or blank values in row 2")

})

test_that("textcleaner() uses the documented row-name IDs, including partBY = 'col' with participants as column names", {

  run <- function(data, ...){
    suppressMessages(textcleaner(
      data, type = "fluency", dictionary = "animals", spelling = "US",
      reader = scripted_reader(rep("", 5)), edit_fn = function(x) x, ...
    ))
  }

  by_row <- data.frame(
    R1 = c("dog", "cat", "cow", "dog"), R2 = c("cat", "dog", "dog", "cow"), R3 = c("cow", "cow", "cat", "cat"),
    row.names = c("alice", "bob", "carl", "dana"), stringsAsFactors = FALSE
  )

  expect_equal(row.names(run(by_row)$responses$clean), c("alice", "bob", "carl", "dana"))

  # Participants across the columns: their column names are the IDs
  by_col <- as.data.frame(t(by_row), stringsAsFactors = FALSE)
  result <- run(by_col, partBY = "col")

  expect_equal(row.names(result$responses$clean), c("alice", "bob", "carl", "dana"))
  expect_equal(unname(result$responses$clean["bob", ]), c("cat", "dog", "cow"))
  expect_equal(row.names(result$behavioral), c("alice", "bob", "carl", "dana"))

})

test_that("textcleaner() always lowercases, so capitals are not 'corrected' into other words", {

  # U13: the removed `lowercase = FALSE` made the spell-check case-sensitive
  # against lowercase dictionaries, so "Cat" was auto-corrected to "gnat" and
  # "DOG" was left for the manual wizard. Case does not change the word.
  raw <- data.frame(
    ID = 1:4, R1 = c("Dog", "DOG", "Cat", "cats"), R2 = c("cat", "Cow", "CAT", "pig"),
    R3 = c("Elefant", "Fish", "bird", "Bird"), stringsAsFactors = FALSE
  )

  skip_everything <- function(prompt = ""){
    if(grepl("accepts lowercase", prompt)) "1" else if(grepl("Y/n", prompt, fixed = TRUE)) "n" else ""
  }

  result <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = "animals", spelling = "US",
    reader = skip_everything, edit_fn = function(x) x
  ))

  expect_equal(unname(result$responses$clean["3", 1:2]), c("cat", "bird"))
  expect_false(any(grepl("[A-Z]", result$responses$clean), na.rm = TRUE))
  expect_false("gnat" %in% result$responses$clean)

  # the argument is gone
  expect_false("lowercase" %in% names(formals(textcleaner)))

})

test_that("textcleaner() splits separated words and lets the spell-check correct digit typos instead of fusing or mangling them (U9)", {

  raw <- data.frame(
    ID = 1:3, R1 = c("d0g", "cow/pig", "1. horse"), R2 = c("cat2", "fish,bird", "2 cats"),
    R3 = c("hen", "dog;cat", "polar  bear"), stringsAsFactors = FALSE
  )

  skip_everything <- function(prompt = ""){
    if(grepl("accepts lowercase", prompt)) "1" else if(grepl("Y/n", prompt, fixed = TRUE)) "n" else ""
  }

  result <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = "animals", spelling = "US",
    reader = skip_everything, edit_fn = function(x) x
  ))

  clean <- result$responses$clean
  expect_equal(unname(clean["1", 1:3]), c("dog", "cat", "hen"))
  expect_equal(unname(clean["2", 1:5]), c("cow", "pig", "fish", "bird", "dog"))
  expect_false(any(c("cowpig", "fishbird", "dg") %in% clean))
  expect_equal(unname(clean["3", 1:3]), c("horse", "cat", "polar bear"))

})

test_that("textcleaner() returns the whole dictionary in $dictionary and only the new words in $added_words, and $dictionary can be reused (U10)", {

  raw <- data.frame(
    ID = c("P1", "P2"), R1 = c("dog", "zzzfakeword"), R2 = c("cat", "bird"), stringsAsFactors = FALSE
  )

  # ADD TO DICTIONARY for the unknown word, then "In my results" (1) at the save prompt
  first <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = "animals", spelling = "US",
    reader = function(prompt = ""){
      if(grepl("accepts lowercase", prompt)) "2" else if(identical(prompt, "Selection: ")) "1" else ""
    },
    edit_fn = function(x) x
  ))

  base <- length(SemNetDictionaries::load.dictionaries("animals"))

  expect_equal(first$added_words, "zzzfakeword")
  expect_true("zzzfakeword" %in% first$dictionary)
  expect_true(all(c("dog", "cat") %in% first$dictionary))
  expect_equal(length(first$dictionary), base + 1)

  # the returned dictionary is a complete, reusable dictionary: the same
  # data run against it no longer needs the manual wizard
  second <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = first$dictionary, spelling = "US",
    reader = scripted_reader(rep("", 5)), edit_fn = function(x) x
  ))

  expect_false(isTRUE(second$stop))
  expect_equal(unname(second$responses$clean["P2", 1:2]), c("zzzfakeword", "bird"))
  expect_null(second$added_words)

})

test_that("textcleaner() rejects a finished result as `continue` with a clear message", {

  # The documentation used to say a saved return value of textcleaner() could
  # be passed back; it failed with "argument is of length zero"
  finished <- list(responses = list(), spellcheck = list(), behavioral = data.frame())
  class(finished) <- c("textcleaner", "fluency")

  expect_error(textcleaner(continue = finished), "finished textcleaner\\(\\) result")

})

test_that("textcleaner() checks its arguments up front with clear messages", {

  ok <- data.frame(ID = 1:2, R1 = c("dog", "cat"), R2 = c("cat", "dog"))

  expect_error(textcleaner(ok, keepLength = "a"), "keepLength", fixed = TRUE)
  expect_error(textcleaner(ok, keepLength = 0), "keepLength", fixed = TRUE)
  expect_error(textcleaner(ok, allowNumbers = "yes"), "allowNumbers", fixed = TRUE)
  expect_error(textcleaner(ok, keepStrings = NA), "keepStrings", fixed = TRUE)
  expect_error(textcleaner(ok, keepRepeats = "no"), "keepRepeats", fixed = TRUE)
  expect_error(textcleaner(ok, cores = 0), "cores", fixed = TRUE)
  expect_error(textcleaner(ok, miss = NULL), "miss", fixed = TRUE)
  expect_error(textcleaner(ok, edit_fn = "edit"), "must be functions", fixed = TRUE)
  expect_error(textcleaner(continue = "x"), "unfinished result", fixed = TRUE)
  expect_error(textcleaner(continue = list(a = 1)), "unfinished result", fixed = TRUE)

})

test_that("changed_by_automation() keeps only the rows whose corrected value differs from the original", {

  corr <- matrix(
    c("dog", "dog", NA,        # unchanged
      "dogg", "dog", NA,       # corrected
      "catdog", "cat", "dog",  # split into two
      "xx", NA, NA,            # removed (bad response)
      "bird", "bird", "cat"),  # same first value but an extra response
    ncol = 3, byrow = TRUE, dimnames = list(sprintf("%03d", 1:5), c("from", "to_1", "to_2"))
  )

  expect_equal(changed_by_automation(corr, 1:5), c(2, 3, 4, 5))
  expect_equal(changed_by_automation(corr, c(1, 3)), 3)
  expect_equal(changed_by_automation(corr, 1), integer(0))

  # a matrix with a single corrected column
  one <- corr[, 1:2, drop = FALSE]
  expect_equal(changed_by_automation(one, 1:4), c(2, 3, 4))

})

test_that("textcleaner() sends only responses the automated spell-check changed to the review", {

  toy <- data.frame(
    ID = 1:4,
    R1 = c("dog", "cat", "dogg", "bird"),
    R2 = c("cat", "fish", "fish", "dog")
  )

  seen <- NULL

  result <- suppressMessages(textcleaner(
    toy, dictionary = "animals", spelling = "US",
    reader = scripted_reader(rep("", 6)),
    edit_fn = function(x){ seen <<- x; x }
  ))

  # "dogg" was corrected; every other response was already correct
  expect_equal(unname(seen[, "from"]), "dogg")
  expect_equal(unname(seen[, "to_1"]), "dog")
  expect_equal(nrow(result$spellcheck$automated), 1)

  # the full correspondence matrix still holds every response
  expect_gt(nrow(result$spellcheck$correspondence), 1)
  expect_equal(unname(result$responses$clean[3, 1]), "dog")

  # nothing changed: the review is skipped
  clean_toy <- toy; clean_toy$R1[3] <- "dog"
  called <- FALSE

  suppressMessages(textcleaner(
    clean_toy, dictionary = "animals", spelling = "US",
    reader = scripted_reader(rep("", 6)),
    edit_fn = function(x){ called <<- TRUE; x }
  ))

  expect_false(called)

})
