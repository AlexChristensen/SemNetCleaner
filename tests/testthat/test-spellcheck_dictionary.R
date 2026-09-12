dict_animals <- SemNetDictionaries::animals.dictionary

test_that("spellcheck_dictionary() fluency: nothing needs manual correction (no prompts at all)", {

  result <- suppressMessages(spellcheck_dictionary(
    uniq_resp = c("dog", "cat", "bird"), dictionary = "animals", spelling = "US",
    keep_strings = FALSE, data = NULL, continue = NULL, type = "fluency",
    reader = scripted_reader(character(0))
  ))

  expect_false(result$stop)
  expect_equal(result$type, "fluency")
  expect_equal(sort(result$auto), 1:3)
  expect_equal(result$manual, numeric(0))

})

test_that("spellcheck_dictionary() fluency: single-word SKIP, matches the original verbatim", {

  result <- suppressMessages(spellcheck_dictionary(
    uniq_resp = c("dog", "catdog", "bird"), dictionary = "animals", spelling = "US",
    keep_strings = FALSE, data = NULL, continue = NULL, type = "fluency",
    reader = scripted_reader(c("", "1"))
  ))

  expect_false(result$stop)
  expect_equal(result$to[[2]], "catdog")
  expect_equal(result$manual, 2)

})

test_that("spellcheck_dictionary() fluency: a multi-word response resolves via the manual menu", {

  result <- suppressMessages(spellcheck_dictionary(
    uniq_resp = c("dog", "turtle catdog elephant", "bird"), dictionary = "animals", spelling = "US",
    keep_strings = FALSE, data = NULL, continue = NULL, type = "fluency",
    reader = scripted_reader(c("", "1"))
  ))

  expect_false(result$stop)
  expect_equal(result$to[[2]], c("turtle", "catdog", "elephant"))

})

test_that("spellcheck_dictionary() fluency: ADD TO DICTIONARY grows the dictionary and triggers the save prompt", {

  result <- suppressMessages(spellcheck_dictionary(
    uniq_resp = c("dog", "catdog", "bird"), dictionary = "animals", spelling = "US",
    keep_strings = FALSE, data = NULL, continue = NULL, type = "fluency",
    reader = scripted_reader(c("", "2", "4"))
  ))

  expect_false(result$stop)
  expect_null(result$dictionary) # answer "4" = "Don't save it"

})

test_that("spellcheck_dictionary() fluency: GO BACK reverts to the previous response, matches the original verbatim", {

  # Verified via direct identical() comparison against the untouched
  # original's spellcheck.dictionary() for this exact scripted sequence
  # (press-enter, SKIP "catdog", GO BACK from "zzznotarealword", SKIP
  # "catdog" again, BAD WORD "zzznotarealword") -- byte-identical $to and
  # $manual. See REFACTOR_PLAN.md's Phase 4 write-up.
  resp <- c("catdog", "zzznotarealword")
  answers <- c("", "1", "b", "1", "5")

  result <- suppressMessages(spellcheck_dictionary(
    uniq_resp = resp, dictionary = "animals", spelling = "US",
    keep_strings = FALSE, data = NULL, continue = NULL, type = "fluency",
    reader = scripted_reader(answers)
  ))

  expect_equal(result$to[[1]], "catdog")
  expect_equal(result$to[[2]], "NA")

})

test_that("spellcheck_dictionary() fluency: STOP saves resumable state instead of final results", {

  result <- suppressMessages(spellcheck_dictionary(
    uniq_resp = c("dog", "catdog", "bird"), dictionary = "animals", spelling = "US",
    keep_strings = FALSE, data = NULL, continue = NULL, type = "fluency",
    reader = scripted_reader(c("", ""))
  ))

  expect_true(result$stop)
  expect_true(is.list(result$initial))

})

test_that("spellcheck_dictionary() fluency: STOP's saved state includes the pending decision's render context (Phase 11, single-response branch)", {

  # New, additive-only `$pending` field (Phase 11): the structured render
  # context textcleaner_shiny()'s wizard module needs to draw the next
  # prompt/buttons without re-deriving spellcheck_dictionary()'s own
  # bookkeeping a second time. Must match exactly what the very next
  # spellcheck_menu() call would have been given.
  result <- suppressMessages(spellcheck_dictionary(
    uniq_resp = c("dog", "catdog", "bird"), dictionary = "animals", spelling = "US",
    keep_strings = FALSE, data = NULL, continue = NULL, type = "fluency",
    reader = scripted_reader(c("", ""))
  ))

  expect_true(result$stop)
  expect_null(result$pending$context)
  expect_equal(result$pending$check, "catdog")
  expect_equal(result$pending$original, "catdog")
  expect_true(is.character(result$pending$possible))

})

test_that("spellcheck_dictionary() fluency: STOP's saved state includes the pending decision's render context (Phase 11, multi-word branch)", {

  result <- suppressMessages(spellcheck_dictionary(
    uniq_resp = c("dog", "turtle catdog elephant", "bird"), dictionary = "animals", spelling = "US",
    keep_strings = FALSE, data = NULL, continue = NULL, type = "fluency",
    reader = scripted_reader(c("", ""))
  ))

  expect_true(result$stop)
  expect_equal(result$pending$context, c("turtle", "catdog", "elephant"))
  expect_equal(result$pending$check, 2L)
  expect_true(is.character(result$pending$possible))

})

test_that("spellcheck_dictionary() free: nothing needs manual correction", {

  result <- suppressMessages(spellcheck_dictionary(
    uniq_resp = c("dog", "cat", "bird"), dictionary = "cocaspell", spelling = "US",
    keep_strings = FALSE, keep_length = 2, data = NULL, continue = NULL, type = "free",
    reader = scripted_reader(character(0))
  ))

  expect_false(result$stop)
  expect_equal(result$type, "free")
  expect_named(result$from, c("1", "2", "3"))

})

test_that("spellcheck_dictionary() free: single-word SKIP against the large cocaspell dictionary (bug fix)", {

  # Real bug fixed: dictionary_contains()'s hashed branch (used once the
  # dictionary is >= 3000 words, e.g. cocaspell's ~109k) called exists()
  # without checking that `word` was actually a character scalar --
  # exists() errors ("invalid first argument") on anything else, unlike
  # `%in%` (what dictionary_contains() replaces at every call site), which
  # tolerates a type-mismatched value gracefully. Confirmed reachable: when
  # auto_spellcheck()'s multi-word stage has nothing "all correct",
  # `multi_word` keeps its `list(response=, correct=)` shape into the
  # response-splitting step, so dictionary_contains() gets called with that
  # whole 2-element list as `words` -- one vapply() iteration then sees the
  # logical `$correct` field as `word` instead of a string. Never surfaced
  # against the small dictionaries (e.g. animals.dictionary, 1,252 words)
  # used everywhere else in this test suite, since those take the plain
  # `%in%` branch instead. Fixed via an explicit `!is.character(word)` guard.
  result <- suppressMessages(spellcheck_dictionary(
    uniq_resp = c("dog", "catdog", "bird"), dictionary = "cocaspell", spelling = "US",
    keep_strings = FALSE, keep_length = 2, data = NULL, continue = NULL, type = "free",
    reader = scripted_reader(c("", "1"))
  ))

  expect_false(result$stop)
  expect_equal(result$to[["2"]], "catdog")
  expect_equal(result$manual, 2)

})

test_that("spellcheck_dictionary() free: GO BACK across two responses needing manual correction", {

  result <- suppressMessages(spellcheck_dictionary(
    uniq_resp = c("catdog", "zzzznotreal"), dictionary = "cocaspell", spelling = "US",
    keep_strings = FALSE, keep_length = 2, data = NULL, continue = NULL, type = "free",
    reader = scripted_reader(c("", "1", "b", "1", "5"))
  ))

  expect_false(result$stop)
  expect_equal(result$to[["1"]], "catdog")
  expect_equal(result$to[["2"]], "NA")

})

test_that("dictionary_contains() tolerates a non-character value gracefully instead of erroring", {

  big_dict <- SemNetDictionaries::load.dictionaries("cocaspell")
  expect_false(dictionary_contains(FALSE, big_dict))
  expect_false(dictionary_contains(NA, big_dict))
  expect_true(dictionary_contains("the", big_dict))

})

test_that("GO BACK's changes-list filter doesn't wipe out unrelated entries when nothing matches (bug fix)", {

  # Bug #41, real and severe, found while auditing Phase 6: `changes[-which(
  # names(changes) == key)]` on a LIST -- like the `x[-which(cond)]`
  # pattern on a plain vector -- returns the list *emptied out*, not the
  # list unchanged, whenever `which()` itself is empty (R can't
  # distinguish a "negative empty" index, exclude nothing, from a
  # "positive empty" one, select nothing -- both are just `integer(0)`).
  # This is reachable in `spellcheck_dictionary()`'s GO BACK logic
  # whenever GO BACK reverts to a response that was resolved via SKIP or
  # KEEP AUTO-CORRECT (neither of which ever calls `change_format()`, so
  # `changes` has no entry for it): confirmed directly that this wipes
  # out every OTHER already-recorded change too, not just a no-op for the
  # missing key. A full end-to-end wizard reproduction proved impractical
  # to hand-construct reliably (the go_back_count/main_count bookkeeping
  # is intricate enough that isolating this one path from everything else
  # it touches wasn't tractable) -- covered here instead by a direct
  # regression test of the exact mechanism, same as bug #14's own
  # (best_guess.R) precedent.
  changes <- list(`1` = matrix("from1", nrow = 1), `2` = matrix("from2", nrow = 1))

  # simulates GO BACK reverting to a response with no changes.format()
  # entry (e.g. index "3", which was SKIPped and never recorded)
  result <- changes[names(changes) != "3"]

  expect_equal(names(result), c("1", "2"))
  expect_length(result, 2)

})

test_that("GO BACK within a multi-word response updates its own changes entry in place, instead of replacing the whole list (bug #52 fix)", {

  # Bug #52, real, severe, and reachable regardless of whether a matching
  # entry exists (unlike bug #41 above, which only triggers when `which()`
  # is empty): `changes <- changes[[which(names(changes) == key)]][-(multi_count
  # - 1), ]` doesn't update THIS response's own change matrix within the
  # list -- it REPLACES THE WHOLE `changes` VARIABLE with a bare matrix
  # (this response's history minus one row), silently destroying every
  # other already-recorded change for every OTHER response in the entire
  # session. Confirmed inherited identically from the untouched original
  # (both the fluency and free branches, byte-identical). Worse, when no
  # entry exists yet for this response (GO BACK on word 2+ of a
  # multi-word response, where word 1 was resolved via SKIP/KEEP
  # AUTO-CORRECT -- neither ever calls `change_format()`, per bug #41),
  # `changes[[integer(0)]]` crashes outright, OUTSIDE the surrounding
  # `try()` -- taking down the whole wizard with no save-state recovery.
  # A full end-to-end wizard reproduction proved impractical for the same
  # reason as bug #41's own test -- covered here via the same direct
  # mechanism-level regression test.
  changes <- list(
    `1` = matrix("keep", nrow = 1, dimnames = list(NULL, "from")),
    `2` = matrix(c("row1", "row2"), nrow = 2, dimnames = list(NULL, "from"))
  )
  main_count <- 2
  ind <- c(1, 2)
  multi_count <- 2

  change_idx <- which(names(changes) == paste(ind[main_count]))
  if(length(change_idx) != 0){
    changes[[change_idx]] <- changes[[change_idx]][-(multi_count - 1), , drop = FALSE]
  }

  expect_type(changes, "list")
  expect_length(changes, 2)
  expect_equal(unname(changes[["1"]][1, "from"]), "keep")
  expect_equal(unname(changes[["2"]][, "from"]), "row2")

  # No entry exists yet for the response being backed into (e.g. its
  # first word was SKIPped) -- should be a no-op, not a crash
  changes_no_entry <- list(`1` = matrix("keep", nrow = 1, dimnames = list(NULL, "from")))
  ind2 <- c(1, 2)
  change_idx2 <- which(names(changes_no_entry) == paste(ind2[2]))

  expect_no_error({
    if(length(change_idx2) != 0){
      changes_no_entry[[change_idx2]] <- changes_no_entry[[change_idx2]][-1, , drop = FALSE]
    }
  })
  expect_length(changes_no_entry, 1)

})
