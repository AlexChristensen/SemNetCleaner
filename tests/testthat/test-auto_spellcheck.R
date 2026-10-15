responses <- list(
  "dog", "cats", "dogg", "aligator", "elefant", "tigar",
  "polar bear", "big cat", "zzznotarealword", "birman",
  "wofl", "catt", "rabit", "beer", "girafe"
)

dict <- SemNetDictionaries::animals.dictionary

test_that("auto_spellcheck() matches the original auto.spellcheck() on real mixed fluency data", {

  # Verified against the untouched original's auto.spellcheck() on this
  # exact input (see REFACTOR_PLAN.md's Phase 4 write-up). The ONE
  # confirmed divergence -- response 14 ("beer") -- is the original's own
  # bug #29 (its makeCluster()/clusterExport() individual-check stage
  # never actually attaches SemNetDictionaries/SemNetCleaner on the
  # worker, only pushes 3 raw function objects; best.guess()'s internal
  # moniker/dictionaries(TRUE) calls then misbehave on the worker and
  # leave an unambiguous match -- "beer" -> "beaver" -- incorrectly
  # unresolved). auto_spellcheck() runs on parallel_process()
  # (future/future.apply, which correctly propagates package state to
  # workers) and so correctly resolves it -- this is the fixed behavior,
  # not a regression, and is asserted directly below.
  result <- suppressMessages(auto_spellcheck(
    check = responses, full_dictionary = dict, dictionary = "animals",
    spelling = "US", keep_strings = FALSE, type = "fluency"
  ))

  expect_equal(sort(result$manual), c(8, 9))
  expect_equal(sort(result$auto), setdiff(1:15, c(8, 9)))
  expect_equal(result$to[["14"]], "beaver")
  # untouched-original-verified resolutions for the unambiguous cases
  expect_equal(result$to[["01"]], "dog")
  expect_equal(result$to[["02"]], "cat")
  expect_equal(result$to[["03"]], "dog")
  expect_equal(result$to[["04"]], "alligator")
  expect_equal(result$to[["05"]], "elephant")
  expect_equal(result$to[["06"]], "tiger")
  expect_equal(result$to[["10"]], "birman")
  expect_equal(result$to[["11"]], "wolf")
  expect_equal(result$to[["12"]], "cat")
  expect_equal(result$to[["13"]], "rabbit")
  expect_equal(result$to[["15"]], "giraffe")

})

test_that("auto_spellcheck() matches the original auto.spellcheck.free() with type='free'", {

  result <- suppressMessages(auto_spellcheck(
    check = responses, full_dictionary = dict, dictionary = "animals",
    spelling = "US", keep_strings = FALSE, type = "free", keep_length = 2
  ))

  expect_equal(sort(result$manual), c(8, 9))
  expect_equal(result$to[["14"]], "beaver")

})

test_that("auto_spellcheck() matches the original with type='free', keep_strings=TRUE", {

  result <- suppressMessages(auto_spellcheck(
    check = responses, full_dictionary = dict, dictionary = "animals",
    spelling = "US", keep_strings = TRUE, type = "free", keep_length = 2
  ))

  expect_equal(sort(result$manual), c(8, 9))
  expect_equal(result$to[["14"]], "beaver")

})

test_that("auto_spellcheck() matches the original with type='free', keep_length=1 (skips multi-word parsing)", {

  result <- suppressMessages(auto_spellcheck(
    check = responses, full_dictionary = dict, dictionary = "animals",
    spelling = "US", keep_strings = FALSE, type = "free", keep_length = 1
  ))

  # keep_length = 1 skips the multi-word-parsing stage entirely, so
  # "polar bear"/"big cat" (response 7/8) never get split/resolved the
  # same way -- only response 9 (genuinely unresolvable) and 8 remain manual
  expect_true(9 %in% result$manual)
  expect_equal(result$to[["14"]], "beaver")

})

test_that("auto_spellcheck() does not crash with keep_length = NULL (bug #28 fix)", {

  # Real bug fixed: the original's `if(is.null(keepLength) | keepLength > 1)`
  # uses `|`, not `||` -- since `|` does not short-circuit, `keepLength > 1`
  # is still evaluated even when `keepLength` is NULL, producing
  # `logical(0)`, and `if(logical(0))` crashes with "argument is of length
  # zero". keepLength = NULL is the actual DEFAULT threaded all the way from
  # textcleaner.free() through spellcheck.dictionary.free() into
  # auto.spellcheck.free() -- confirmed via direct reproduction that the
  # untouched original crashes on its own default free-association pipeline
  # invocation. auto_spellcheck() uses `||` (short-circuiting), so
  # `type == "fluency"` or `is.null(keep_length)` alone determines the
  # branch without evaluating `keep_length > 1` at all.
  expect_no_error(
    suppressMessages(auto_spellcheck(
      check = responses, full_dictionary = dict, dictionary = "animals",
      spelling = "US", keep_strings = FALSE, type = "free", keep_length = NULL
    ))
  )

})

test_that("auto_spellcheck() returns a consistent numeric res$auto even when every response is already correct (bug #27 fix)", {

  # Real bug fixed: the original's first-stage "all responses already
  # correctly spelled" branch returned `res$auto` as a 0-row CHARACTER
  # MATRIX (`matrix("", nrow = 0, ncol = 2)`, colnames "from"/"to_1") --
  # every other stage/branch returns `res$auto` as a plain numeric index
  # vector via `setdiff(as.numeric(names(...)), res$manual)`. Confirmed by
  # direct reading of both auto.spellcheck() and auto.spellcheck.free() at
  # their first-stage "all spelled correctly" branch. Downstream callers
  # (textcleaner's `corr.mat[spell.check$auto,]`) index by `res$auto`
  # directly, so this type inconsistency was a real, reachable landmine.
  # Fixed structurally by resolve_auto_spellcheck_stage() always returning
  # the numeric-vector shape, for every stage including the first.
  all_correct <- list("dog", "cat", "bear")

  result <- suppressMessages(auto_spellcheck(
    check = all_correct, full_dictionary = dict, dictionary = "animals",
    spelling = "US", keep_strings = FALSE, type = "fluency"
  ))

  expect_type(result$auto, "double")
  expect_equal(sort(result$auto), 1:3)
  expect_equal(result$manual, numeric(0))

})

test_that("auto_spellcheck() resolves correctly with real parallel workers (cores > 1)", {

  # Confirms the fix for bug #29 holds under genuine multisession
  # parallelism, not just the single-process (cores = 1) path -- unlike
  # the original's raw parallel::clusterExport() (which only pushes 3 bare
  # function objects onto each worker without attaching the packages they
  # depend on), parallel_process() is future/future.apply-based and
  # correctly propagates package state to each worker.
  skip_on_cran()

  result <- suppressMessages(auto_spellcheck(
    check = list("beer"), full_dictionary = dict, dictionary = "animals",
    spelling = "US", keep_strings = FALSE, type = "free", keep_length = 2, cores = 2
  ))

  expect_equal(result$to[["1"]], "beaver")
  expect_equal(result$auto, 1)
  expect_equal(result$manual, numeric(0))

})

test_that("auto_spellcheck() does not crash when no multi-word response fully auto-resolves, keepStrings = FALSE (issues #21/#24)", {

  # Real bug fixed: `multi_word` was only rebuilt from `orig` when at least
  # one multi-word response fully resolved. Otherwise it stayed the raw
  # list(response, correct) results of multiple_words(), and the
  # response_splitter()/dictionary_contains() step after it failed with
  # "'length = 2' in coercion to 'logical(1)'" -- the wizard then reported
  # a second, unrelated-looking "$ operator is invalid for atomic vectors".
  # Names must still line up with the response indices. Uses the "general"
  # dictionary because the multi-word stage only runs for responses with at
  # least `get_multi_min()` words, which is 2 there (3 for "animals").
  general_dict <- SemNetDictionaries::load.dictionaries("general")

  for(check in list(
    list("1" = "zzznotareal qqzzvvxx"),
    list("1" = "zzznotareal qqzzvvxx", "2" = "zzznotareal"),
    list("1" = "zzznotareal qqzzvvxx", "2" = "zzznotareal qqzzvvxx")
  )){

    result <- suppressMessages(auto_spellcheck(
      check = check, full_dictionary = general_dict, dictionary = "general",
      spelling = "US", keep_strings = FALSE, type = "fluency"
    ))

    expect_equal(sort(result$manual), seq_along(check))
    expect_length(result$auto, 0)
    expect_equal(unname(unlist(result$to)), unname(unlist(check)))

  }

  # A mix: one multi-word response resolves, one doesn't
  mixed <- suppressMessages(auto_spellcheck(
    check = list("1" = "zzznotareal qqzzvvxx", "2" = "night table", "3" = "dog"),
    full_dictionary = general_dict, dictionary = "general",
    spelling = "US", keep_strings = FALSE, type = "fluency"
  ))

  expect_equal(mixed$manual, 1)
  expect_equal(sort(mixed$auto), c(2, 3))

})

test_that("auto_spellcheck() free: finishes when the stop-word stage removes every unresolved response (free-association fix)", {

  # Real bug fixed (found driving the free-association wizard): responses made
  # only of stop words ("i am") are dropped as the first free-only step. With
  # nothing else unresolved the pipeline went on with an EMPTY candidate list
  # and ended in parallel_process()'s "input `iterations` has a minimum value
  # of 0"
  result <- suppressMessages(auto_spellcheck(
    check = list("i am", "dog", "you are"), full_dictionary = dict, dictionary = "animals",
    spelling = "US", keep_strings = FALSE, type = "free", keep_length = 2
  ))

  expect_equal(result$manual, numeric(0))
  expect_equal(result$auto, 1:3)
  expect_equal(unname(unlist(result$to)), c(NA, "dog", NA))

})
