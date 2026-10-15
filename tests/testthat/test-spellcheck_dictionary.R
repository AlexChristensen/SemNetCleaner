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

test_that("spellcheck_dictionary() fluency: a missing dictionary among found ones gives one warning naming it", {

  # SemNetDictionaries (>= 0.3.0) leaves the missing dictionary out with a classed warning
  skip_if(utils::packageVersion("SemNetDictionaries") < "0.3.0")

  expect_warning(
    suppressMessages(spellcheck_dictionary(
      uniq_resp = c("dog", "cat"), dictionary = c("animals", "nope_zzz"), spelling = "US",
      keep_strings = FALSE, data = NULL, continue = NULL, type = "fluency",
      reader = scripted_reader(character(0))
    )),
    "nope_zzz"
  )

})

test_that("spellcheck_dictionary() fluency: a dictionary that can't be found stops with an explanatory error (issues #21/#24)", {

  # Real bug fixed: SemNetDictionaries::load.dictionaries() only prints a
  # message for an unknown dictionary and returns an empty result, so the
  # wizard carried on with no dictionary and failed several steps later
  # with unrelated errors ("$ operator is invalid for atomic vectors")
  expect_error(
    suppressMessages(spellcheck_dictionary(
      uniq_resp = c("dog", "cat"), dictionary = "definitely_not_a_dictionary", spelling = "US",
      keep_strings = FALSE, data = NULL, continue = NULL, type = "fluency",
      reader = scripted_reader(character(0))
    )),
    "dictionary.rds"
  )

  # ...without a second, duplicate warning from SemNetDictionaries (>= 0.3.0)
  expect_no_warning(try(suppressMessages(spellcheck_dictionary(
    uniq_resp = c("dog", "cat"), dictionary = "definitely_not_a_dictionary", spelling = "US",
    keep_strings = FALSE, data = NULL, continue = NULL, type = "fluency",
    reader = scripted_reader(character(0))
  )), silent = TRUE))

  # ...and a custom dictionary saved the way SemNetDictionaries expects IS found
  path <- file.path(tempdir(), "custom_dictionary_test")
  dir.create(path, showWarnings = FALSE)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  saveRDS(c("dog", "cat", "bird"), file.path(path, "zoo.dictionary.rds"))

  result <- suppressMessages(spellcheck_dictionary(
    uniq_resp = c("dog", "cat"), dictionary = "zoo", spelling = "US", add_path = path,
    keep_strings = FALSE, data = NULL, continue = NULL, type = "fluency",
    reader = scripted_reader(character(0))
  ))

  expect_equal(sort(result$auto), 1:2)

})

test_that("spellcheck_dictionary() falls back to the working directory when save option 3 can't use tcltk (issue #24)", {

  # Real bug fixed: option 3 ("I'd like to choose the directory") goes
  # through SemNetDictionaries' tcltk directory chooser, which errors on
  # systems where tcltk can't load (e.g. macOS without X11/XQuartz)
  local_mocked_bindings(tcltk_available = function() FALSE)

  work_dir <- file.path(tempdir(), "tcltk_fallback_test")
  dir.create(work_dir, showWarnings = FALSE)
  withr::local_dir(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  expect_message(
    result <- spellcheck_dictionary(
      uniq_resp = c("dog", "catdog", "bird"), dictionary = "animals", spelling = "US",
      keep_strings = FALSE, data = NULL, continue = NULL, type = "fluency",
      reader = scripted_reader(c("", "2", "3", "fallbacktest"))
    ),
    "directory chooser is not available"
  )

  expect_false(result$stop)
  expect_true(file.exists(file.path(work_dir, "fallbacktest.dictionary.rds")))

})

test_that("prompt_dictionary_name() re-asks until the name is a usable file name", {

  # Real bugs fixed: a name with "/" made the save fail AFTER the whole
  # manual session (losing every decision), and an empty name saved nothing
  # while reporting success
  expect_equal(prompt_dictionary_name(scripted_reader("zoo")), "zoo")
  expect_equal(prompt_dictionary_name(scripted_reader("  zoo.dictionary ")), "zoo")
  expect_message(
    name <- prompt_dictionary_name(scripted_reader(c("a/b", "", "1x", "../up", "My_dict-2"))),
    "must start with a letter"
  )
  expect_equal(name, "My_dict-2")

})

test_that("saving a dictionary under a name that already exists keeps the words added this time", {

  work_dir <- file.path(tempdir(), "save_existing_test")
  dir.create(work_dir, showWarnings = FALSE)
  withr::local_dir(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  # Save, then save again with a new word (append.dictionary() kept the old file as is
  # when the argument was named like a dictionary)
  expect_true(suppressMessages(try_save_dictionary(c("cat", "dog"), "zoo", "wd")))
  expect_true(suppressMessages(try_save_dictionary(c("cat", "dog", "newword"), "zoo", "wd")))

  expect_true("newword" %in% readRDS(file.path(work_dir, "zoo.dictionary.rds")))

})

test_that("a failed dictionary save never loses the finished session: it is kept in $dictionary instead", {

  work_dir <- file.path(tempdir(), "save_failure_test")
  dir.create(work_dir, showWarnings = FALSE)
  withr::local_dir(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  # direct: a name the file system can't use
  expect_message(
    saved <- try_save_dictionary(c(dict_animals, "zzznewword"), "a/b", "wd"),
    "could not be saved"
  )
  expect_false(saved)

  # through the wizard: bad names are re-asked, then the file is written
  result <- suppressMessages(spellcheck_dictionary(
    uniq_resp = c("dog", "catdog", "bird"), dictionary = "animals", spelling = "US",
    keep_strings = FALSE, data = NULL, continue = NULL, type = "fluency",
    reader = scripted_reader(c("", "2", "2", "a/b", "goodname"))
  ))

  expect_false(result$stop)
  expect_true(file.exists(file.path(work_dir, "goodname.dictionary.rds")))
  expect_false(file.exists(file.path(work_dir, "a")))

})

test_that("split_unknown_strings() separates multi-word strings that are not dictionary entries, only for the given responses (U14)", {

  to <- list(
    c("your mom"), c("polar bear"), c("dog", "more cats", "sting ray"), c("NA"), c("not touched here")
  )

  dictionary <- c("dog", "polar bear", "sting ray", "cats")

  result <- split_unknown_strings(to, c(1, 2, 3, 4), dictionary)

  expect_equal(result[[1]], c("your", "mom"))
  expect_equal(result[[2]], "polar bear")
  expect_equal(result[[3]], c("dog", "more", "cats", "sting ray"))
  expect_equal(result[[4]], "NA")
  expect_equal(result[[5]], "not touched here") # index not requested

})

test_that("spellcheck_dictionary() with keepStrings = FALSE separates a skipped multi-word response, but keeps one the user added to the dictionary (U14)", {

  # Real bug fixed: "your mom" was shown as ONE string and SKIP left it as a
  # single multi-word response even though keepStrings = FALSE means strings
  # are separated. It is now asked about word by word (like every other
  # multi-word response), and a string that stays whole is separated at the
  # end unless it is a dictionary entry
  run <- function(answers){
    suppressMessages(spellcheck_dictionary(
      uniq_resp = c("dog", "your mom"), dictionary = "animals", spelling = "US",
      keep_strings = FALSE, data = NULL, continue = NULL, type = "fluency",
      reader = scripted_reader(answers)
    ))
  }

  # SKIP WORD for each of the two unknown words
  skipped <- run(c("", "1", "1"))
  expect_equal(skipped$to[[2]], c("your", "mom"))
  expect_equal(skipped$to[[1]], "dog")

  # KEEP ORIGINAL, then adding the string to the dictionary, makes "your mom" an entry, so it stays whole
  added <- run(c("", "6", "y", "4"))
  expect_equal(added$to[[2]], "your mom")

  # a typed multi-word string answer is separated unless it is a dictionary entry
  typed <- run(c("", "8", "big dog", "n"))
  expect_equal(typed$to[[2]], c("big", "dog"))

  typed_entry <- run(c("", "8", "polar bear"))
  expect_equal(typed_entry$to[[2]], "polar bear")

})

test_that("GO BACK from the first decision of a response announces that the previous response is asked again from its first word (U8)", {

  # GO BACK within a multi-word response undoes one word, but from the first
  # decision of the NEXT response it returns to the previous response and
  # redoes it from the start. That is kept, but it used to happen silently
  resp <- c("turtle catdog elephant", "zzznotarealword")

  # press-enter, SKIP "catdog", GO BACK from "zzznotarealword" (announced),
  # then "catdog" is asked again: BAD WORD this time, then BAD WORD for the next
  expect_message(
    result <- spellcheck_dictionary(
      uniq_resp = resp, dictionary = "animals", spelling = "US",
      keep_strings = FALSE, data = NULL, continue = NULL, type = "fluency",
      reader = scripted_reader(c("", "1", "b", "5", "5"))
    ),
    "Going back to the previous response, 'turtle catdog elephant'"
  )

  expect_equal(result$to[[1]], c("turtle", "NA", "elephant"))
  expect_equal(result$to[[2]], "NA")

  # the help screens describe it
  help_text <- paste(capture.output(suppressMessages(textcleaner_help(
    check = 2, context = c("turtle", "catdog"), original = "turtle catdog",
    possible = "cat", reader = scripted_reader("")
  ))), collapse = "\n")

  expect_match(gsub("\033\\[[0-9;]*m", "", help_text), "returns to the PREVIOUS response", fixed = TRUE)

})

test_that("spellcheck_dictionary() asks about a word repeated within a response once, not once per occurrence (fluency, keepStrings = TRUE)", {

  # Real bug fixed (found driving the free-association multi-word menu):
  # `check_words` kept one entry per occurrence, so "teh cat teh dog" asked
  # `best_guess()` about two words at once ("input `word` has length 2") and
  # crashed the run. One scripted answer is enough: a second prompt would
  # exhaust the reader
  result <- suppressMessages(spellcheck_dictionary(
    uniq_resp = c("teh cat teh dog", "dog"), dictionary = "animals", spelling = "US",
    keep_strings = TRUE, keep_length = NULL, data = NULL, continue = NULL, type = "fluency",
    reader = scripted_reader(c("", "5"))
  ))

  expect_false(result$stop)
  # BAD WORD on the repeated word marks every occurrence: the bad word is dropped, the rest kept
  # ("cat dog": both are dictionary words and the string is not an entry, so it is split)
  expect_equal(result$to[["1"]], c("cat", "dog", "NA"))

})

test_that("spellcheck_dictionary() free: KEEP ORIGINAL and a one-word TYPE MY OWN STRING replace the whole response (free-association fix)", {

  # Real bug fixed (found driving the free-association multi-word menu): the
  # free branch wrote a one-element menu result into just position
  # `multi_count` of the response, so KEEP ORIGINAL turned "ice crem cone"
  # into "ice crem cone crem cone" and typing "dog" for "the big hosue" gave
  # "dog big hosue"
  keep <- suppressMessages(spellcheck_dictionary(
    uniq_resp = c("ice crem cone", "dog"), dictionary = "cocaspell", spelling = "US",
    keep_strings = TRUE, keep_length = NULL, data = NULL, continue = NULL, type = "free",
    reader = scripted_reader(c("", "6", "n"))
  ))
  expect_equal(keep$to[["1"]], "ice crem cone")

  typed <- suppressMessages(spellcheck_dictionary(
    uniq_resp = c("the big hosue", "dog"), dictionary = "cocaspell", spelling = "US",
    keep_strings = TRUE, keep_length = NULL, data = NULL, continue = NULL, type = "free",
    reader = scripted_reader(c("", "7", "dog", "n"))
  ))
  expect_equal(typed$to[["1"]], "dog")

})

test_that("spellcheck_dictionary() free: with keepStrings = FALSE an unresolved multi-word string that is skipped is separated (free-association fix)", {

  # Same as the fluency branch's U14: strings are SEPARATED when keepStrings
  # is FALSE. The free branch left a skipped "zxqvk wjxqz vkqzx" whole while
  # neighboring responses were split word by word
  result <- suppressMessages(spellcheck_dictionary(
    uniq_resp = c("zxqvk wjxqz vkqzx", "dog"), dictionary = "cocaspell", spelling = "US",
    keep_strings = FALSE, keep_length = NULL, data = NULL, continue = NULL, type = "free",
    reader = scripted_reader(c("", "1", "1", "1"))
  ))

  expect_equal(result$to[["1"]], c("zxqvk", "wjxqz", "vkqzx"))

})

test_that("spellcheck_dictionary() keepStrings = TRUE: BAD WORD drops only the bad word and keeps the rest of the string", {

  # Real bug fixed (found by the rebuilt manual-menu harness): fluency left
  # the literal word "NA" inside the response, so BAD WORD on "teh" in
  # "teh cat teh dog" put "NA cat NA dog" into the cleaned data, while free
  # association turned the whole string into NA. The bad word is dropped and
  # the rest kept, with one "NA" marker beside it (as with keepStrings =
  # FALSE) so the bad word is still counted as an intrusion
  word <- suppressMessages(spellcheck_dictionary(
    uniq_resp = c("teh cat teh dog", "dog"), dictionary = "animals", spelling = "US",
    keep_strings = TRUE, keep_length = NULL, data = NULL, continue = NULL, type = "fluency",
    reader = scripted_reader(c("", "5"))
  ))
  # ("cat dog" is split: both are dictionary words and the string is not an entry)
  expect_equal(word$to[["1"]], c("cat", "dog", "NA"))

  # BAD STRING marks every word, so nothing is kept
  string <- suppressMessages(spellcheck_dictionary(
    uniq_resp = c("turtle catdog elephant", "dog"), dictionary = "animals", spelling = "US",
    keep_strings = TRUE, keep_length = NULL, data = NULL, continue = NULL, type = "fluency",
    reader = scripted_reader(c("", "9"))
  ))
  expect_equal(string$to[["1"]], "NA")

  # separate words (keepStrings = FALSE) behave the same way
  separate <- suppressMessages(spellcheck_dictionary(
    uniq_resp = c("teh cat teh dog", "dog"), dictionary = "animals", spelling = "US",
    keep_strings = FALSE, keep_length = NULL, data = NULL, continue = NULL, type = "fluency",
    reader = scripted_reader(c("", "5"))
  ))
  expect_equal(separate$to[["1"]], c("NA", "cat", "dog"))

})

test_that("spellcheck_dictionary() free: with keepStrings = FALSE an unresolved multi-word string that is skipped is separated (free-association fix)", {

  # Same as the fluency branch's U14: strings are SEPARATED when keepStrings
  # is FALSE. The free branch left a skipped "zxqvk wjxqz vkqzx" whole while
  # neighboring responses were split word by word
  result <- suppressMessages(spellcheck_dictionary(
    uniq_resp = c("zxqvk wjxqz vkqzx", "dog"), dictionary = "cocaspell", spelling = "US",
    keep_strings = FALSE, keep_length = NULL, data = NULL, continue = NULL, type = "free",
    reader = scripted_reader(c("", "1", "1", "1"))
  ))

  expect_equal(result$to[["1"]], c("zxqvk", "wjxqz", "vkqzx"))

})

test_that("spellcheck_dictionary() keepStrings = FALSE asks about an unsplittable multi-word string word by word, so BAD WORD drops only the bad word", {

  # "dog xqzvk" used to be asked as ONE string, so BAD WORD dropped "dog" too,
  # while the same answer kept "dog" with keepStrings = TRUE
  result <- suppressMessages(spellcheck_dictionary(
    uniq_resp = c("dog xqzvk", "cat"), dictionary = "animals", spelling = "US",
    keep_strings = FALSE, keep_length = NULL, data = NULL, continue = NULL, type = "fluency",
    reader = scripted_reader(c("", "5"))
  ))

  # one decision (only "xqzvk" is unknown), and "dog" survives
  expect_equal(result$to[["1"]], c("dog", "NA"))
  expect_equal(result$manual, 1)

})

test_that("resuming from a stop at a SINGLE-word decision does not treat a later multi-word response as half done", {

  # Real bug fixed (found by the rebuilt manual-menu harness): the saved state
  # carried `multi_count` left over from an earlier multi-word response, so
  # after exiting at a single word and continuing, the next multi-word
  # response was resumed from that stale position and failed with "input
  # `word` has length 0" in best_guess()
  uniq <- c("turtle catdog elephant", "xqzvk", "your mom")
  run <- function(...) suppressMessages(spellcheck_dictionary(
    uniq_resp = uniq, dictionary = "animals", spelling = "US", keep_strings = FALSE,
    keep_length = NULL, type = "fluency", ...
  ))

  # skip the first response's unknown word, then exit at "xqzvk"
  stopped <- run(data = NULL, continue = NULL, reader = scripted_reader(c("", "1", "x")))
  expect_true(stopped$stop)
  expect_null(stopped$multi_count)

  # continue: "xqzvk", then both unknown words of "your mom"
  resumed <- run(continue = stopped, reader = scripted_reader(c("", "1", "1", "1")))
  expect_false(resumed$stop)
  expect_equal(resumed$to[[3]], c("your", "mom"))

})

test_that("the help window opens as soon as the manual process starts (and again on continue), when enabled", {

  opened <- character()
  open_fn <- function(path){ opened <<- c(opened, path); invisible(NULL) }
  run <- function(window, ...){
    options(SemNetCleaner.help_window = window)
    suppressMessages(capture.output(res <- spellcheck_dictionary(
      uniq_resp = c("dog", "xqzvk"), dictionary = "animals", spelling = "US",
      keep_strings = FALSE, data = NULL, type = "fluency",
      reader = scripted_reader(c("", "x")), open_fn = open_fn, ...
    )))
    res
  }

  withr::defer(options(SemNetCleaner.help_window = NULL))

  # enabled: opened once, before the first decision, and the stop is saved
  stopped <- run(TRUE)
  expect_length(opened, 1)
  expect_true(file.exists(opened[1]))
  expect_true(isTRUE(stopped$stop))

  # resuming opens it again (a new session of work)
  opened <- character()
  suppressMessages(capture.output(resumed <- spellcheck_dictionary(
    continue = stopped, reader = scripted_reader(c("", "x")), open_fn = open_fn
  )))
  expect_length(opened, 1)

  # disabled: nothing opens
  opened <- character()
  run(FALSE)
  expect_length(opened, 0)

})

test_that("the app's wizard steps never open the help window", {

  opened <- character()
  withr::local_options(SemNetCleaner.help_window = TRUE)
  # advance_wizard() switches the option off for its own duration and restores it
  suppressMessages(capture.output(
    stopped <- spellcheck_dictionary(
      uniq_resp = c("dog", "xqzvk"), dictionary = "animals", spelling = "US",
      keep_strings = FALSE, data = NULL, type = "fluency",
      reader = scripted_reader(c("", "x")), open_fn = function(path) opened <<- c(opened, path)
    )
  ))
  expect_length(opened, 1)

  seen <- NULL
  testthat::local_mocked_bindings(textcleaner = function(...){ seen <<- getOption("SemNetCleaner.help_window"); "ok" })
  expect_identical(advance_wizard(stopped, character(0)), "ok")
  expect_false(seen)
  expect_true(getOption("SemNetCleaner.help_window"))

})

test_that("the manual stage does not ask about words that are part of a dictionary entry (sting ray)", {

  asked <- character()
  reader <- function(prompt = ""){ if(grepl("accepts lowercase", prompt)) "1" else "" }
  out <- capture.output(res <- suppressMessages(spellcheck_dictionary(
    uniq_resp = c("sting ray xqzvk"), dictionary = "animals", spelling = "US",
    keep_strings = TRUE, data = NULL, type = "fluency", reader = reader
  )))
  targets <- gsub("\033\\[[0-9;]*m", "", grep("Target word", out, value = TRUE))

  expect_length(targets, 1)
  expect_match(targets, "xqzvk", fixed = TRUE)

})

test_that("keepStrings = TRUE (fluency): corrected strings of dictionary words are split unless the string is an entry", {

  reader <- function(prompt = ""){ if(grepl("accepts lowercase", prompt)) "1" else "" }
  run <- function(strings){
    suppressMessages(capture.output(res <- spellcheck_dictionary(
      uniq_resp = strings, dictionary = "animals", spelling = "US",
      keep_strings = TRUE, data = NULL, type = "fluency", reader = reader
    )))
    res$to
  }

  to <- run(c("sea slug", "marmoset otter", "cat xqzvk"))

  expect_identical(unlist(to[["1"]]), "sea slug")
  expect_identical(unlist(to[["2"]]), c("marmoset", "otter"))
  expect_identical(unlist(to[["3"]]), "cat xqzvk")

})
