test_that("best_guess() matches the original best.guess() on real dictionary lookups", {

  expect_equal(
    best_guess("bomba", full_dictionary = SemNetDictionaries::animals.dictionary),
    "bombay"
  )
  expect_equal(
    best_guess("dogg", full_dictionary = SemNetDictionaries::animals.dictionary),
    "dog"
  )

})

test_that("best_guess() returns the word unchanged when it's already in the dictionary", {

  expect_equal(
    best_guess("dog", full_dictionary = SemNetDictionaries::animals.dictionary),
    "dog"
  )

})

test_that("bug #14 mechanism: length(x == 1) always equals length(x), not a length-1 check", {

  # best.guess()'s original moniker-disambiguation branch checked
  # `if(length(uniq.monix == 1))` -- misplaced parentheses. Since `==`
  # returns a same-length logical vector, `length(x == 1)` is always just
  # `length(x)`, and any nonzero number is truthy in `if()`. So the check
  # was always TRUE whenever `uniq.monix` was non-empty, regardless of
  # whether it actually had exactly one element -- confirmed directly here,
  # and fixed in best_guess.R to the clearly-intended `length(x) == 1`.
  ambiguous <- c("a", "b", "c")
  unambiguous <- "a"

  # The OLD (buggy) check: truthy for BOTH cases -- can't distinguish them
  expect_true(isTRUE(as.logical(length(ambiguous == 1))))
  expect_true(isTRUE(as.logical(length(unambiguous == 1))))

  # The NEW (fixed) check: correctly distinguishes them
  expect_false(length(ambiguous) == 1)
  expect_true(length(unambiguous) == 1)

})

test_that("best_guess() validates its inputs", {

  expect_error(best_guess(1, full_dictionary = c("dog", "cat")), "best_guess")
  expect_error(best_guess("dog", full_dictionary = 1), "best_guess")
  expect_error(best_guess("dog", full_dictionary = c("dog", "cat"), tolerance = -1), "best_guess")
  expect_error(best_guess("dog", full_dictionary = c("dog", "cat"), tolerance = "a"), "best_guess")

})

test_that("best_guess() rejects NA `word` and an empty `full_dictionary` clearly instead of silently misbehaving (robustness fixes)", {

  # Real robustness gaps found and fixed during a Phase 2 review, per
  # direct request. Neither was reachable via this rebuild's own internal
  # call sites, but best_guess() is exported -- a direct user call with
  # either shape previously either crashed with a confusing low-level
  # message or silently warned-and-returned character(0), rather than
  # rejecting genuinely invalid input clearly.
  expect_error(best_guess(NA_character_, full_dictionary = c("dog", "cat")), "not a valid word")
  expect_error(best_guess("dog", full_dictionary = character(0)), "must not be empty")

})

test_that("best_guess() resolves a phonetically-obvious typo QWERTY alone can't disambiguate (Phase 10b: real gap closed)", {

  # Real gap closed: "graple" is within DL distance 1 of BOTH "grape" (5
  # letters) and "grapple"/its own near-miss "grapple" (7 letters) --
  # QWERTY's same-length-only guard (`qwerty_distance()`) can't compare
  # "graple" against either candidate (all different lengths), so the
  # original design gives up ambiguously. Metaphone resolves it uniquely:
  # confirmed directly, phonics::metaphone("graple") == "KRPL" ==
  # phonics::metaphone("grapple"), but phonics::metaphone("grape") ==
  # "KRP" (different) -- exactly one phonetic match.
  result <- best_guess("graple", full_dictionary = SemNetDictionaries::coca.dictionary)

  expect_equal(result, "grapple")

})

test_that("best_guess() does NOT auto-resolve when QWERTY and phonetic disagree (Phase 10b: conflict-handling rule)", {

  # Real, reproducible conflict: "gremble" (a typo of "grumble") is
  # equidistant (DL = 1) from both "grumble" and "tremble". QWERTY's
  # adjacent-key logic picks "tremble"; Metaphone picks "grumble"
  # (phonics::metaphone("gremble") == phonics::metaphone("grumble") ==
  # "KRML", while phonics::metaphone("tremble") == "TRML", different).
  # Neither heuristic is trusted over the other on disagreement -- this
  # must fall through to the ambiguous/manual-review list, not silently
  # pick either candidate.
  result <- best_guess("gremble", full_dictionary = SemNetDictionaries::coca.dictionary)

  expect_true(length(result) > 1)
  expect_true("grumble" %in% result)
  expect_true("tremble" %in% result)

})

test_that("best_guess() still lets monikers win outright when unique, even if phonetic also resolves (existing bug #14 priority, unchanged)", {

  # Regression guard: adding phonetic disambiguation as a peer to QWERTY
  # must not disturb monikers' existing, audited priority -- monikers win
  # outright whenever unique, checked BEFORE QWERTY/phonetic are ever
  # consulted.
  expect_equal(
    best_guess("bomba", full_dictionary = SemNetDictionaries::animals.dictionary),
    "bombay"
  )

})

test_that("best_guess() lowercases a moniker's canonical form (free-association fix)", {

  # Real bug fixed (found driving the free-association wizard): the moniker
  # table's canonical forms carry capitals ("Bay"), so "baz" was corrected to
  # "Bay" while every dictionary word and response is lowercase
  cocaspell <- SemNetDictionaries::load.dictionaries("cocaspell")

  result <- suppressWarnings(best_guess("baz", full_dictionary = cocaspell, dictionary = "cocaspell"))

  expect_equal(result, "bay")

})
