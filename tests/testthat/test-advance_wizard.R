test_that("advance_wizard(): a fresh wizard call with an always-empty reader stops at the first manual decision, with $pending populated", {

  raw <- data.frame(
    ID = c("P1", "P2"), R1 = c("catdog", "bird"), R2 = c("cat", "fish"),
    stringsAsFactors = FALSE
  )

  # An always-"" reader is exactly what textcleaner_shiny()'s automated-pass
  # step (mod_auto_progress.R) uses: run the full (parallelizable)
  # auto_spellcheck() pass, then pause at the FIRST decision manual review
  # actually needs, instead of driving the console wizard for real
  initial <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = "animals", spelling = "US",
    reader = function(prompt = "") "", edit_fn = function(x) x
  ))

  expect_true(isTRUE(initial$stop))
  expect_equal(initial$pending$check, "catdog")
  expect_null(initial$pending$context)

})

test_that("advance_wizard() + build_answer_burst() end to end: a single SKIP decision completes the wizard and the response survives unchanged", {

  raw <- data.frame(
    ID = c("P1", "P2"), R1 = c("catdog", "bird"), R2 = c("cat", "fish"),
    stringsAsFactors = FALSE
  )

  initial <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = "animals", spelling = "US",
    reader = function(prompt = "") "", edit_fn = function(x) x
  ))

  final <- suppressMessages(advance_wizard(initial, build_answer_burst(1)))

  expect_false(isTRUE(final$stop))
  expect_s3_class(final, "textcleaner")
  expect_equal(unname(final$responses$clean["P1", ]), c("catdog", "cat"))

})

test_that("GO BACK on the very first single-response manual decision is denied and RE-ASKS the same response, instead of silently skipping it (bug found via code review while investigating the multi-word GO BACK crashes)", {

  # Found by inspection, not a live report: the single-response branch (no
  # surrounding multi-word context) has no loop of its own around its one
  # `spellcheck_menu()` call -- unlike the multi-word branch, which is
  # already wrapped in a real `while(multi_count != ...)` loop, so a
  # denied GO BACK there (leaving `multi_count` unchanged) naturally
  # re-asks. Here, printing "This is the first response. 'GO BACK' is not
  # available." used to fall straight through to `main_count <- main_count
  # + 1` -- silently ADVANCING PAST this response with no real decision
  # ever recorded for it, instead of re-prompting the same choice.
  raw <- data.frame(
    ID = c("P1", "P2"), R1 = c("catdog", "bird"), R2 = c("cat", "fish"),
    stringsAsFactors = FALSE
  )

  initial <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = "animals", spelling = "US",
    reader = function(prompt = "") "", edit_fn = function(x) x
  ))

  expect_equal(initial$pending$check, "catdog")
  expect_equal(initial$main_count, 1)

  denied <- suppressMessages(advance_wizard(initial, build_answer_burst("b")))

  expect_true(isTRUE(denied$stop))
  expect_equal(denied$main_count, 1)
  expect_equal(denied$pending$check, "catdog")
  expect_null(denied$pending$context)

  # And the SAME response is still genuinely answerable afterward --
  # confirms this re-asks the decision rather than merely avoiding a crash
  final <- suppressMessages(advance_wizard(denied, build_answer_burst(1)))

  expect_false(isTRUE(final$stop))
  expect_equal(unname(final$responses$clean["P1", ]), c("catdog", "cat"))

})

test_that("GO BACK on the very first word of the first multi-word response is denied and RE-ASKS the same word (existing correct behavior, guarded against regressing)", {

  raw <- data.frame(
    ID = c("P1", "P2"), R1 = c("turtle catdog elephant", "bird"), R2 = c("cat", "fish"),
    stringsAsFactors = FALSE
  )

  initial <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = "animals", spelling = "US",
    reader = function(prompt = "") "", edit_fn = function(x) x
  ))

  expect_equal(initial$pending$context, c("turtle", "catdog", "elephant"))
  expect_equal(initial$pending$check, 2L)
  expect_equal(initial$main_count, 1)

  denied <- suppressMessages(advance_wizard(initial, build_answer_burst("b")))

  expect_true(isTRUE(denied$stop))
  expect_equal(denied$main_count, 1)
  expect_equal(denied$pending$context, c("turtle", "catdog", "elephant"))
  expect_equal(denied$pending$check, 2L)

  final <- suppressMessages(advance_wizard(denied, build_answer_burst(1)))

  expect_false(isTRUE(final$stop))
  expect_equal(unname(final$responses$clean["P1", ]), c("turtle", "catdog", "elephant", "cat"))

})

test_that("advance_wizard() + build_answer_burst() end to end: TYPE MY OWN WORD with an add-to-dictionary answer resolves correctly (the correctness-critical burst-sizing case)", {

  raw <- data.frame(
    ID = c("P1", "P2"), R1 = c("catdog", "bird"), R2 = c("cat", "fish"),
    stringsAsFactors = FALSE
  )

  initial <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = "animals", spelling = "US",
    reader = function(prompt = "") "", edit_fn = function(x) x
  ))

  # menu choice 3 = TYPE MY OWN WORD (single-response branch): types
  # "zzznotarealword", confirmed NOT in the animals dictionary (unlike
  # "zebra", which is already a real dictionary entry and would make this
  # test's premise wrong) -- offer_add_missing_to_dictionary() will prompt
  # exactly once about it, and build_answer_burst() must supply exactly one
  # more answer for that prompt (add_to_dictionary = FALSE here)
  expect_false("zzznotarealword" %in% SemNetDictionaries::animals.dictionary)

  burst <- build_answer_burst(
    3, typed_text = "zzznotarealword", add_to_dictionary = FALSE,
    full_dictionary = SemNetDictionaries::animals.dictionary
  )
  expect_equal(length(burst), 3)

  final <- suppressMessages(advance_wizard(initial, burst))

  expect_false(isTRUE(final$stop))
  expect_equal(unname(final$responses$clean["P1", ]), c("zzznotarealword", "cat"))

})

test_that("advance_wizard() resumes a multi-word response's decision using $pending's own context/check (Phase 11 render-context regression guard)", {

  raw <- data.frame(
    ID = c("P1", "P2"), R1 = c("turtle catdog elephant", "bird"), R2 = c("cat", "fish"),
    stringsAsFactors = FALSE
  )

  initial <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = "animals", spelling = "US",
    reader = function(prompt = "") "", edit_fn = function(x) x
  ))

  expect_true(isTRUE(initial$stop))
  expect_equal(initial$pending$context, c("turtle", "catdog", "elephant"))
  expect_equal(initial$pending$check, 2L)

  final <- suppressMessages(advance_wizard(initial, build_answer_burst(1)))

  expect_false(isTRUE(final$stop))
  expect_equal(unname(final$responses$clean["P1", ]), c("turtle", "catdog", "elephant", "cat"))

})

test_that("GO BACK across a resume boundary, into an earlier word of the SAME multi-word response, does not crash and correctly undoes that word's decision (regression for the live 'object prev_target not found' crash)", {

  # Needs TWO words in ONE multi-word response that both require a manual
  # decision, with the wizard actually STOPPING (a real resume, a fresh
  # `spellcheck_dictionary()` call) in between them -- reproducing this
  # from a single continuous call (the untouched console wizard's own
  # usage pattern) never exercises the bug, since `target`/`prev_target`
  # then just stay alive as ordinary local variables across loop
  # iterations in the same call. Confirmed via a live `textcleaner_shiny()`
  # session: "did i say bear already" (context = c("did","i","sting ray",
  # "bear","already"), only "did"/"i"/"sting ray"/"already" unrecognized)
  # crashed identically -- this is a smaller instance of the same shape.
  raw <- data.frame(
    ID = c("P1", "P2"), R1 = c("catdog birdbear elephant", "bird"), R2 = c("cat", "fish"),
    stringsAsFactors = FALSE
  )

  initial <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = "animals", spelling = "US",
    reader = function(prompt = "") "", edit_fn = function(x) x
  ))

  expect_equal(initial$pending$context, c("catdog", "birdbear", "elephant"))
  expect_equal(initial$pending$check, 1L)

  # Resolve word 1 ("catdog") with SKIP -- this resume STOPs again at word
  # 2 ("birdbear"), which is the resume boundary the bug needs: `target`/
  # `prev_target` from word 1's resolution only exist across this boundary
  # if actually saved/restored, not merely as this function call's own
  # local variables.
  at_word_2 <- suppressMessages(advance_wizard(initial, build_answer_burst(1)))

  expect_true(isTRUE(at_word_2$stop))
  expect_equal(at_word_2$pending$context, c("catdog", "birdbear", "elephant"))
  expect_equal(at_word_2$pending$check, 2L)

  # GO BACK from word 2: before the fix, this threw
  # "object 'prev_target' not found" -- a real, unguarded error (outside
  # any try()) that took down the whole wizard with no save-state recovery.
  back_to_word_1 <- suppressMessages(advance_wizard(at_word_2, build_answer_burst("b")))

  expect_true(isTRUE(back_to_word_1$stop))
  expect_equal(back_to_word_1$pending$context, c("catdog", "birdbear", "elephant"))
  expect_equal(back_to_word_1$pending$check, 1L)

  # And the wizard is still genuinely usable afterward -- finishing it out
  # confirms GO BACK didn't just avoid crashing but left real, correct
  # state behind (word 1 re-answerable, word 2 still pending after it)
  final <- suppressMessages(advance_wizard(
    back_to_word_1, c(build_answer_burst(1), build_answer_burst(1))
  ))

  expect_false(isTRUE(final$stop))
  expect_equal(unname(final$responses$clean["P1", ]), c("catdog", "birdbear", "elephant", "cat"))

})

test_that("TWO consecutive GO BACKs within the same multi-word response (no real decision in between) correctly undo both words instead of crashing (regression for the live 'best_guess(): input word has length 0' crash)", {

  # Confirmed via a live console `textcleaner()` session on real data: BAD
  # WORDing two words in a row inside one multi-word response ("did i say
  # bear already"), then pressing GO BACK twice in a row from the next
  # word, crashed inside `best_guess()`. Root cause: the original
  # single-scalar `prev_target` undo-buffer only ever remembers the LAST
  # real decision's pre-state; a SECOND consecutive GO BACK (nothing real
  # decided in between) reused that same stale value instead of reverting
  # any further, decrementing `multi_count` without actually restoring
  # `target` to match -- so the next word's ORIGINAL text no longer
  # appeared anywhere in `target` (an earlier, still-un-undone decision
  # had overwritten it), and `which(check_words[multi_count] == target)`
  # came back empty, feeding `best_guess()` a length-0 `word`. Needs
  # several filler response columns: `correct_data()`'s own
  # `2 * max(non-NA cells per row)` column-budget heuristic assumes one
  # original response CELL becomes at most ~1 final response, which a
  # too-narrow fixture (as few as 2 columns) can trip on its own once a
  # single multi-word cell legitimately expands into 4 separate cleaned
  # words -- unrelated to this bug, and not a concern against any
  # realistically-sized real dataset (dozens of response columns).
  raw <- data.frame(
    ID = c("P1", "P2"),
    R1 = c("did i bear already", "cat"), R2 = c("dog", "fish"),
    R3 = c("lion", "lion2"), R4 = c("tiger", "tiger2"), R5 = c("zebra", "zebra2"),
    stringsAsFactors = FALSE
  )

  initial <- suppressMessages(textcleaner(
    raw, type = "fluency", dictionary = "animals", spelling = "US",
    reader = function(prompt = "") "", edit_fn = function(x) x
  ))

  expect_equal(initial$pending$context, c("did", "i", "bear", "already"))
  expect_equal(initial$pending$check, 1L)

  # BAD WORD "did" (-> NA), then BAD WORD "i" (-> NA) -- two REAL decisions
  # in a row, each one refreshing the undo buffer as it goes
  s1 <- suppressMessages(advance_wizard(initial, build_answer_burst(5)))
  s2 <- suppressMessages(advance_wizard(s1, build_answer_burst(5)))
  expect_equal(s2$pending$context, c("NA", "NA", "bear", "already"))
  expect_equal(s2$pending$check, 4L)

  # GO BACK once: undoes "i" (word 2) only -- word 1 ("did") stays NA
  s3 <- suppressMessages(advance_wizard(s2, build_answer_burst("b")))
  expect_equal(s3$pending$context, c("NA", "i", "bear", "already"))
  expect_equal(s3$pending$check, 2L)

  # GO BACK again immediately (no real decision in between): before the
  # fix, this crashed. With a real undo STACK instead of one scalar, it
  # must fully undo word 1 too, landing back on the pristine original
  s4 <- suppressMessages(advance_wizard(s3, build_answer_burst("b")))
  expect_equal(s4$pending$context, c("did", "i", "bear", "already"))
  expect_equal(s4$pending$check, 1L)

  # And the wizard is still genuinely usable afterward, producing correct
  # (not just crash-free) final data
  s5 <- suppressMessages(advance_wizard(s4, build_answer_burst(1)))
  s6 <- suppressMessages(advance_wizard(s5, build_answer_burst(1)))
  final <- suppressMessages(advance_wizard(s6, build_answer_burst(1)))

  expect_false(isTRUE(final$stop))
  expect_equal(
    unname(final$responses$clean["P1", 1:7]),
    c("did", "i", "bear", "already", "dog", "lion", "tiger")
  )

})
