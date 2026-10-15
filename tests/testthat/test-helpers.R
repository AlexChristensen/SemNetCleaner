test_that("validation primitives (ported from SemNeT) work as expected", {

  expect_equal(get_object_type(matrix(1:4, 2)), "matrix")
  expect_equal(get_object_type(data.frame(a = 1)), "data.frame")
  expect_equal(get_object_type(list(1)), "list")
  expect_equal(get_object_type(1:3), "vector")

  expect_error(object_error(1:3, "matrix", "fn"), "fn")
  expect_silent(object_error(matrix(1), "matrix", "fn"))

  expect_error(typeof_error("a", "numeric", "fn"), "fn")
  expect_silent(typeof_error(1L, "numeric", "fn"))
  expect_silent(typeof_error(1.5, "numeric", "fn"))

  expect_error(length_error(1:3, 1, "fn"), "fn")
  expect_silent(length_error(1, 1, "fn"))

  expect_error(range_error(5, c(0, 1), "fn"), "fn")
  expect_silent(range_error(0.5, c(0, 1), "fn"))

  expect_error(na_error(NA_character_, "fn"), "fn")
  expect_error(na_error(c("a", NA), "fn"), "fn")
  expect_silent(na_error("a", "fn"))
  expect_silent(na_error(c("a", "b"), "fn"))

  expect_equal(swiftelse(TRUE, "a", "b"), "a")
  expect_equal(swiftelse(FALSE, "a", "b"), "b")
  expect_equal(swiftelse(c(TRUE, FALSE, NA), "a", "b"), ifelse(c(TRUE, FALSE, NA), "a", "b"))
  expect_equal(swiftelse(c(TRUE, FALSE, TRUE), "a", "b"), c("a", "b", "a"))

})

test_that("range_error() errors clearly on all-NA/empty input instead of silently passing (robustness fix)", {

  # Real robustness gap found and fixed during a Phase 1 review, per direct
  # request to look for more robust implementations: `min()`/`max(...,
  # na.rm = TRUE)` on an all-NA or zero-length `input` return `Inf`/`-Inf`
  # (a warning, not an error) once na.rm strips every element away -- both
  # range comparisons then spuriously pass (`Inf < expected_minimum` and
  # `-Inf > expected_maximum` are both FALSE), silently letting genuinely
  # invalid input through validation with nothing but a stray warning.
  # Confirmed directly before the fix. Now errors clearly instead.
  expect_error(range_error(NA_real_, c(1, Inf), "fn"), "fn")
  expect_error(range_error(numeric(0), c(1, Inf), "fn"), "fn")
  expect_error(range_error(c(NA_real_, NA_real_), c(1, Inf), "fn"), "fn")

  # A mix of NA and valid values should still validate normally, only the
  # non-missing values checked against the range
  expect_silent(range_error(c(NA_real_, 0.5), c(0, 1), "fn"))
  expect_error(range_error(c(NA_real_, 5), c(0, 1), "fn"), "fn")

})

test_that("swiftelse() falls back to ifelse() on mismatched true/false vector lengths instead of silently corrupting output (robustness fix)", {

  # Real robustness gap found and fixed during a Phase 1 review: the
  # direct-index-assignment fast path assumed `true`/`false` are each
  # either length-1 or already the same length as `condition` -- confirmed
  # directly that a `true`/`false` of some OTHER length silently produced
  # WRONG values (out-of-range positions read back as NA) instead of
  # ifelse()'s own correct recycling, with no warning at all. No current
  # call site hits this (both are always scalar or condition-length), but
  # swiftelse() is a general-purpose ifelse() replacement, not a
  # single-call-site-only helper -- it should behave like ifelse() in
  # every case, not just the ones exercised today.
  condition <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
  expect_equal(
    swiftelse(condition, c("a", "b"), "z"),
    ifelse(condition, c("a", "b"), "z")
  )
  expect_equal(
    swiftelse(condition, "z", c("a", "b", "c")),
    ifelse(condition, "z", c("a", "b", "c"))
  )

})

test_that("read_input() calls the injected reader with the given prompt", {

  captured_prompt <- NULL
  fake_reader <- function(prompt = ""){
    captured_prompt <<- prompt
    "42"
  }

  expect_equal(read_input("Pick one: ", reader = fake_reader), "42")
  expect_equal(captured_prompt, "Pick one: ")

})

test_that("scripted_reader() pops answers in order and errors when exhausted", {

  reader <- scripted_reader(c("y", "2", "done"))

  expect_equal(reader(), "y")
  expect_equal(reader("next?"), "2")
  expect_equal(reader(), "done")
  expect_error(reader(), "exhausted")

})

test_that("appropriate_answer() accepts a valid answer without re-prompting", {

  choices <- c("skip", "add", "type", "google", "bad")
  result <- appropriate_answer("1", choices = choices, default = 5, dictionary = TRUE)
  expect_equal(result, 1)

})

test_that("appropriate_answer() re-prompts via the injected reader on an invalid answer", {

  choices <- c("skip", "add", "type", "google", "bad")
  reader <- scripted_reader(c("3"))

  result <- appropriate_answer("99", choices = choices, default = 5, dictionary = TRUE, reader = reader)
  expect_equal(result, 3)

})

test_that("appropriate_answer() produces zero QWERTY options when there are no non-default choices (bug fix)", {

  # 5 choices, default = 5 -> zero non-default choices -> the original's
  # `1:(length(choices[-c(1:default)]))` reverse-ranged to `1:0`, producing
  # one bogus extra "Q" option; seq_len() is correctly empty here
  choices <- c("skip", "add", "type", "google", "bad")
  result <- appropriate_answer("5", choices = choices, default = 5, dictionary = FALSE)
  expect_equal(result, 5)

  # A "Q" answer must NOT be accepted -- confirms no spurious QWERTY option exists
  reader <- scripted_reader(c("1"))
  result_after_bad_q <- appropriate_answer("Q", choices = choices, default = 5, dictionary = FALSE, reader = reader)
  expect_equal(result_after_bad_q, 1)

})

test_that("custom_menu() does not require interactive() and never crashes on a short choice list", {

  # Regression test: choices with length <= default used to hit the
  # `nc > default` == FALSE branch, and calling with help = TRUE would then
  # reference the never-created 'resp' -- "object 'resp' not found". This is
  # a real, reachable call shape: textcleaner_help() calls
  # custom_menu(choices, default = 10, help = TRUE) with a variable-length
  # guess vector that can easily be shorter than 'default'.
  choices <- c("skip", "add", "type", "google", "bad")

  expect_error(capture.output(custom_menu(choices, default = 5, dictionary = TRUE)), NA)
  expect_equal(custom_menu(choices, default = 5, dictionary = TRUE, help = TRUE), "")

})

test_that("custom_menu() formats a longer choice list (nc > default) without error, both default = 5 and default = 10", {

  five_default_choices <- c("skip", "add", "type", "google", "bad", "guessone", "guesstwo")
  expect_error(capture.output(custom_menu(five_default_choices, default = 5)), NA)
  help_text <- custom_menu(five_default_choices, default = 5, help = TRUE)
  expect_type(help_text, "character")
  expect_true(nzchar(help_text))

  ten_default_choices <- c(
    "skip", "add", "type", "google", "bad", "same", "cannot", "not", "none", "misspelled",
    "guessone", "guesstwo", "guessthree"
  )
  expect_error(capture.output(custom_menu(ten_default_choices, default = 10)), NA)
  help_text_ten <- custom_menu(ten_default_choices, default = 10, help = TRUE)
  expect_type(help_text_ten, "character")
  expect_true(nzchar(help_text_ten))

})

test_that("custom_menu() renders on a narrow console instead of 'object def/word/string not found' (issues #20/#22/#26/#28)", {

  # Real bug fixed: `word`/`string`/`def` were only built inside
  # `if(ncol > 1L)`, where `ncol = width %/% item width`. A narrow console
  # (e.g. an RStudio side pane on Windows) gave ncol <= 1, skipped the
  # block, and the later `paste0(..., def, ...)` failed with "object 'def'
  # not found" (or 'word'/'string' for default = 9/10); at ncol = 0,
  # `rep.int(..., ncol - 1L)` failed with "invalid 'times' value".
  old_width <- options(width = 20)
  on.exit(options(old_width), add = TRUE)

  five_default_choices <- c("SKIP", "ADD TO DICTIONARY", "TYPE MY OWN", "GOOGLE IT", "BAD WORD", "giraffe", "griffin")
  ten_default_choices <- c(paste("option", 1:10), "guessone", "guesstwo")

  for(width in c(80, 40, 20)){

    options(width = width)

    out_five <- capture.output(custom_menu(five_default_choices, default = 5))
    expect_true(any(grepl("ADD TO DICTIONARY", out_five)))
    expect_true(any(grepl("giraffe", out_five)))

    out_ten <- capture.output(custom_menu(ten_default_choices, default = 10))
    expect_true(any(grepl("option 6", out_ten)))
    expect_true(any(grepl("guesstwo", out_ten)))

    expect_true(nzchar(custom_menu(five_default_choices, default = 5, help = TRUE)))

  }

})

test_that("yes_no_menu() accepts y/n via the injected reader and re-prompts on garbage", {

  expect_equal(yes_no_menu("Continue?", reader = scripted_reader("y")), 1)
  expect_equal(yes_no_menu("Continue?", reader = scripted_reader("no")), 2)
  expect_equal(yes_no_menu("Continue?", reader = scripted_reader(c("qq", "Y"))), 1)

})

test_that("color_text()/style_text()/text_symbol() return usable text without erroring", {

  expect_type(color_text("hi", defaults = "red"), "character")
  expect_type(style_text("hi", defaults = "bold"), "character")
  expect_equal(text_symbol("bullet"), "•")
  expect_equal(text_symbol("check mark"), "✓")

})

test_that("system_check() reports OS/RSTUDIO/TEXT fields", {

  result <- system_check()
  expect_named(result, c("OS", "RSTUDIO", "TEXT"))
  expect_type(result$TEXT, "logical")

})

test_that("system_check() memoizes: a cleared-cache recomputation matches the cached result", {

  cached_result <- system_check()

  # Force a fresh recomputation by clearing the private cache directly,
  # then confirm it's `identical()` to what was already cached -- this is
  # the optimization's correctness guarantee (OS/RSTUDIO can't change
  # mid-session, so caching must never change the observable result)
  .system_check_cache$value <- NULL
  fresh_result <- system_check()

  expect_identical(cached_result, fresh_result)
  # Calling again now returns the (re-populated) cache, not a fresh object,
  # but must still be identical in content
  expect_identical(system_check(), fresh_result)

})

test_that("remove_leading_space() trims whitespace and passes through bad_response()", {

  expect_equal(remove_leading_space("  cat  "), "cat")
  expect_true(is.na(remove_leading_space(" ")))

})

test_that("seq_min_max() identifies contiguous runs correctly", {

  single_run <- seq_min_max(c(3, 4, 5, 6))
  expect_equal(single_run$min, 3)
  expect_equal(single_run$max, 6)

  two_runs <- seq_min_max(c(1, 2, 3, 7, 8))
  expect_equal(two_runs$min, c(1, 7))
  expect_equal(two_runs$max, c(3, 8))

  single_element <- seq_min_max(5)
  expect_equal(single_element$min, 5)
  expect_equal(single_element$max, 5)

})

test_that("seq_min_max() errors clearly on an empty sequence instead of silently returning Inf/-Inf (robustness fix)", {

  # Real robustness gap found and fixed during a Phase 1 review: `diff()`
  # on a length-0/1 `sequence` is itself length 0, and `all(logical(0))`
  # is vacuously TRUE, so an EMPTY sequence fell into the "one contiguous
  # run" branch -- `min()`/`max()` on `numeric(0)` then silently returned
  # `Inf`/`-Inf` (a warning, not an error) instead of rejecting a run that
  # doesn't exist. Confirmed directly before the fix.
  expect_error(seq_min_max(numeric(0)), "at least one element")

})

test_that("seq_min_max()/moniker() are no longer shadowed by a stale duplicate definition (robustness fix)", {

  # Real bug found and fixed during a Phase 1 review, same duplicate-
  # definition class as bug #1 (`bad.response`): both functions were
  # already fully ported to this file, but a stale, un-deleted copy of
  # each (verbatim from the untouched original) still lived in
  # R/utils-SemNetCleaner.R -- alphabetical file collation ("u" after "h")
  # meant THAT copy silently won at package-load time, so every
  # package-internal call was actually running the unreviewed original,
  # not the code here. Confirmed directly: the shadowed seq_min_max()
  # lacked this file's empty-sequence guard (warned + returned Inf/-Inf
  # instead of erroring), and the shadowed moniker() still called the
  # legacy `brit.us.conv()` instead of `brit_us_conversion()`. Fixed by
  # deleting the stale copies; these two checks would fail again if either
  # duplicate ever reappears.
  expect_error(seq_min_max(numeric(0)), "at least one element")
  expect_identical(environmentName(environment(seq_min_max)), "SemNetCleaner")
  expect_true(any(grepl("brit_us_conversion", deparse(body(moniker)))))
  expect_false(any(grepl("brit\\.us\\.conv\\(", deparse(body(moniker)))))

})

test_that("parallel_process() runs sequentially and in parallel with identical results", {

  sequential <- parallel_process(5, function(i) i^2, cores = 1, progress = FALSE)
  expect_equal(unlist(sequential), c(1, 4, 9, 16, 25))

  parallel_result <- parallel_process(5, function(i) i^2, cores = 2, progress = FALSE)
  expect_equal(unlist(parallel_result), c(1, 4, 9, 16, 25))

})

test_that("moniker() looks up a word in a moniker list and passes through unmatched words", {

  # NOTE: moniker() returns a length-1 list, not a bare character scalar --
  # this is the underlying (unchanged, ported-as-is) brit.us.conv()'s actual
  # return shape at `dictionary = FALSE`, not something introduced here
  misnom <- list(cat = c("kitty", "kitteh"))

  expect_equal(moniker("kitty", misnom, spelling = "US"), list("cat"))
  expect_equal(moniker("dog", misnom, spelling = "US"), list("dog"))

})

test_that("moniker() returns lowercase even when the moniker table's canonical form is capitalized", {

  # Real bug fixed (found driving the free-association wizard): the table's
  # canonical forms carry capitals for proper nouns ("I", "French"), so
  # "i dont know" came back as "I do know"
  misnom <- list(French = c("frenchh", "french"), I = "i", cat1 = "kitty")

  expect_equal(moniker("french", misnom, spelling = "US"), list("french"))
  expect_equal(moniker("i", misnom, spelling = "US"), list("i"))
  expect_equal(moniker("kitty", misnom, spelling = "US"), list("cat"))

})

test_that("parallel_process() restores the previous future plan even when FUN errors", {

  old_plan <- future::plan()
  expect_error(
    # future.apply emits an informational "Caught simpleError..." warning
    # before re-throwing the real error -- suppressed here since the test
    # is about error propagation and plan restoration, not that warning
    suppressWarnings(parallel_process(2, function(i) stop("boom"), cores = 1, progress = FALSE)),
    "boom"
  )
  expect_identical(class(future::plan()), class(old_plan))

})

test_that("parallel_process() caps requested cores to what's actually available (Phase 11: nested-worker guard)", {

  # Simulate running inside an already-nested worker (e.g. textcleaner_shiny()'s
  # own ExtendedTask background process), where future::availableCores() is
  # deliberately capped to 1 -- requesting more workers than that used to trip
  # parallelly's hard localhost-worker-count guard instead of degrading to
  # sequential execution
  testthat::local_mocked_bindings(availableCores = function() 1L, .package = "future")

  plan_before <- future::plan()

  result <- parallel_process(5, function(i) i^2, cores = 4, progress = FALSE)
  expect_equal(unlist(result), (1:5)^2)

  expect_equal(class(future::plan()), class(plan_before))

})

test_that("parallel_process()'s progress file reports a monotonic completion count (Phase 11)", {

  progress_file <- tempfile()
  writeLines("1", progress_file)
  old_option <- options(SemNetCleaner.progress_file = progress_file)
  on.exit(options(old_option), add = TRUE)

  # Multiple iterations across 2 workers -- completion order across workers
  # isn't guaranteed, but the number of completions recorded must still end
  # up exactly matching the iteration count, regardless of order
  parallel_process(6, function(i) i, cores = 2, progress = FALSE)

  total <- as.numeric(readLines(progress_file, n = 1))
  expect_equal(total, 6)

  # One byte appended per completed iteration, not the (out-of-order) raw
  # iteration index itself
  expect_equal(file.info(paste0(progress_file, ".progress"))$size, 6)

})

test_that("dictionary_contains() matches %in% semantics for scalar and vector input, small dictionary", {

  small_dict <- c("cat", "dog", "bird")

  expect_true(dictionary_contains("cat", small_dict))
  expect_false(dictionary_contains("fish", small_dict))
  expect_equal(dictionary_contains(c("cat", "fish", "dog"), small_dict), c(TRUE, FALSE, TRUE))
  expect_equal(dictionary_contains(character(0), small_dict), logical(0))
  expect_false(dictionary_contains("", small_dict))
  expect_false(dictionary_contains(NA_character_, small_dict))

})

test_that("dictionary_contains() matches %in% semantics for a large (hash-path) dictionary", {

  large_dict <- SemNetDictionaries::coca.dictionary

  expect_true(dictionary_contains("cat", large_dict))
  expect_false(dictionary_contains("zzznotarealword", large_dict))
  expect_equal(dictionary_contains(c("cat", "zzznotarealword", "dog"), large_dict), c(TRUE, FALSE, TRUE))
  expect_false(dictionary_contains("", large_dict))
  expect_false(dictionary_contains(NA_character_, large_dict))

})

test_that("dictionary_contains() correctly distinguishes the literal string 'NA' from a real NA dictionary entry (bug fix)", {

  # Real bug fixed: get_dictionary_hash()'s `assign(word, TRUE, envir = ...)`
  # silently coerced an NA dictionary entry into a variable literally named
  # "NA", making the STRING "NA" falsely register as a recognized word --
  # diverging from %in%'s own correct semantics. Confirmed directly:
  # "NA" %in% c(..., NA) is FALSE; only NA %in% c(..., NA) is TRUE.
  dict_with_na <- c(SemNetDictionaries::coca.dictionary, NA)

  expect_false(dictionary_contains("NA", dict_with_na))
  expect_equal(dictionary_contains("NA", dict_with_na), "NA" %in% dict_with_na)
  expect_true(dictionary_contains("cat", dict_with_na))

})

test_that("get_dictionary_hash() caches and reuses the hash for the same dictionary object", {

  large_dict <- SemNetDictionaries::coca.dictionary

  hash1 <- get_dictionary_hash(large_dict)
  hash2 <- get_dictionary_hash(large_dict)

  expect_identical(hash1, hash2)
  expect_true(exists("cat", envir = hash1, inherits = FALSE))

})

test_that("get_dictionary_hash() tolerates empty strings, NA and duplicates in a large dictionary (U7)", {

  # The hash is now built with one vectorized list2env() (4x faster than the
  # former assign() loop, which matters for the 370k-word "general"
  # dictionary); unlike assign() a zero-length name is an error there, so ""
  # is dropped with NA
  words <- paste0("word", 1:3500)
  dict <- c(words, "", NA, "word1", "polar bear")
  hash <- get_dictionary_hash(dict)

  expect_setequal(ls(hash), c(words, "polar bear"))
  expect_equal(
    dictionary_contains(c("word1", "polar bear", "", "NA", "nope"), dict),
    c(TRUE, TRUE, FALSE, FALSE, FALSE)
  )

})

test_that("get_dictionary_hash() builds a distinct, still-correct hash for a genuinely different dictionary", {

  dict_a <- c(SemNetDictionaries::coca.dictionary)
  dict_b <- SemNetDictionaries::general.dictionary

  hash_a <- get_dictionary_hash(dict_a)
  hash_b <- get_dictionary_hash(dict_b)

  expect_false(identical(hash_a, hash_b))

})

test_that("coca_dictionary_contains() matches dictionary_contains() against coca.dictionary directly", {

  expect_equal(coca_dictionary_contains("cat"), dictionary_contains("cat", SemNetDictionaries::coca.dictionary))
  expect_equal(coca_dictionary_contains("zzznotarealword"), FALSE)

})

test_that("word_is_recognized() memoizes its verdict and returns a consistent result across calls", {

  first <- word_is_recognized("elephant")
  second <- word_is_recognized("elephant")

  expect_true(first)
  expect_identical(first, second)

  # A genuine non-word should consistently return FALSE, not error, and
  # not get "stuck" as TRUE from some earlier cached word
  expect_false(word_is_recognized("zzzqqxxnotarealword"))
  expect_false(word_is_recognized(""))
  expect_false(word_is_recognized(NA_character_))

})

test_that("get_phonetic_code() returns NA for empty/NA input without erroring", {

  expect_true(is.na(get_phonetic_code("")))
  expect_true(is.na(get_phonetic_code(NA_character_)))

})

test_that("get_phonetic_code() memoizes and returns the real Metaphone code", {

  first <- get_phonetic_code("physics")
  second <- get_phonetic_code("physics")

  expect_identical(first, second)
  expect_identical(first, phonics::metaphone("physics"))

})

test_that("phonetic_match() normalizes a real phonetic misspelling Soundex alone can't catch (real gap fixed)", {

  # Real blind spot in the current DL-distance + QWERTY-only design: a
  # phonetic substitution like "ph" -> "f" changes the FIRST letter, so
  # Soundex (which keeps a word's first letter literal) can never match
  # them -- confirmed directly: stringdist::phonetic("physics",
  # method="soundex") is "P220", stringdist::phonetic("fysics",
  # method="soundex") is "F220". Metaphone normalizes this correctly.
  expect_true(phonetic_match("physics", "fysics"))

})

test_that("phonetic_match() is always FALSE (never NA/error) for empty/NA words, composing safely inside vapply()", {

  expect_false(phonetic_match("dog", ""))
  expect_false(phonetic_match("dog", NA_character_))
  expect_false(phonetic_match(NA_character_, NA_character_))

})

test_that("get_monikers() caches and returns the same moniker list as a direct load.monikers() call", {

  direct <- SemNetDictionaries::load.monikers("animals")
  cached1 <- get_monikers("animals")
  cached2 <- get_monikers("animals")

  expect_equal(cached1, direct)
  expect_identical(cached1, cached2)

})

test_that("get_monikers() distinguishes different dictionary arguments", {

  animals_monikers <- get_monikers("animals")
  fruits_monikers <- get_monikers("fruits")

  expect_false(identical(animals_monikers, fruits_monikers))

})

test_that("get_available_dictionaries() caches and matches a direct dictionaries(TRUE) call", {

  direct <- SemNetDictionaries::dictionaries(TRUE)
  cached1 <- get_available_dictionaries()
  cached2 <- get_available_dictionaries()

  expect_equal(cached1, direct)
  expect_identical(cached1, cached2)
  expect_true("animals" %in% cached1)

})

test_that("get_multi_min() matches a direct hand-looped computation of multi_min", {

  # Optimization found while profiling Phase 4/5 (per direct request):
  # auto_spellcheck()'s multi-word stage recomputed this from the WHOLE
  # dictionary on every single call -- ~500ms for cocaspell's ~109k words
  # -- even though it's a pure function of full_dictionary alone. See
  # REFACTOR_PLAN.md's optimization write-up.
  reference <- function(full_dictionary){
    dict_lens <- unlist(lapply(full_dictionary, function(x){ length(unlist(strsplit(x, " "))) }))
    m <- ceiling(median(dict_lens) + 2 * sd(dict_lens))
    if(m == 1){ m <- 2 }
    m
  }

  dict <- SemNetDictionaries::animals.dictionary
  expect_equal(get_multi_min(dict), reference(dict))

})

test_that("get_multi_min() caches per-dictionary and distinguishes different dictionaries", {

  animals_min <- get_multi_min(SemNetDictionaries::animals.dictionary)
  cached_again <- get_multi_min(SemNetDictionaries::animals.dictionary)
  fruits_min <- get_multi_min(SemNetDictionaries::fruits.dictionary)

  expect_identical(animals_min, cached_again)
  expect_true(is.numeric(animals_min) && is.numeric(fruits_min))

})

test_that("get_two_letter_combinations() matches a direct expand.grid()-based build and is cached", {

  reference <- apply(expand.grid(letters, letters), 1, paste, collapse = "", sep = "")

  result1 <- get_two_letter_combinations()
  result2 <- get_two_letter_combinations()

  expect_identical(result1, reference)
  expect_identical(result1, result2)
  expect_length(result1, 676)
  expect_true(all(c("aa", "zz", "qx") %in% result1))

})

test_that("error_report(fatal = TRUE) ends with a real R error naming the failing step and its cause", {

  failure <- try(stop("something specific broke"), silent = TRUE)

  output <- capture.output(
    suppressMessages(
      expect_error(
        error_report(failure, "convert_miss", "textcleaner", fatal = TRUE),
        "`textcleaner\\(\\)` stopped in 'convert_miss': something specific broke"
      )
    )
  )

  # non-fatal (the default) still just reports
  expect_no_error(suppressMessages(capture.output(error_report(failure, "convert_miss", "textcleaner"))))

})

test_that("pause() sleeps only when the SemNetCleaner.pause option allows it", {

  # the suite runs with pauses off (setup-no-pause.R)
  expect_lt(system.time(pause(5))[["elapsed"]], 1)

  withr::local_options(SemNetCleaner.pause = TRUE)
  expect_gte(system.time(pause(0.2))[["elapsed"]], 0.15)

})

test_that("resolve_add_path() passes a path through and explains when the chooser cannot be used", {

  # "choose" used to be handed to SemNetDictionaries, which could open the
  # chooser more than once and failed obscurely without an interactive session
  expect_null(resolve_add_path(NULL))
  expect_equal(resolve_add_path("/some/folder"), "/some/folder")

  # tests are not interactive
  expect_error(resolve_add_path("choose"), "interactive R session")

  local_mocked_bindings(is_interactive = function() TRUE, tcltk_available = function() FALSE)
  expect_error(resolve_add_path("choose"), "directory chooser is not available")

})

test_that("drop_repeated_entry_words() removes words that only repeat the rest of an expanded multi-word entry", {

  # "pollar bear": "pollar" is corrected to the two-word entry "polar bear",
  # next to the original word "bear" that the entry already contains
  expect_equal(drop_repeated_entry_words(c("polar bear", "bear")), "polar bear")
  expect_equal(drop_repeated_entry_words(c("polar bear", "bear", "xqzvk")), c("polar bear", "xqzvk"))
  expect_equal(drop_repeated_entry_words(c("african bush elephant", "bush", "elephant")), "african bush elephant")

  # nothing to drop: different following word, a one-word entry, or not all of the rest present
  expect_equal(drop_repeated_entry_words(c("polar bear", "cat")), c("polar bear", "cat"))
  expect_equal(drop_repeated_entry_words(c("cat", "cat")), c("cat", "cat"))
  expect_equal(drop_repeated_entry_words(c("african bush elephant", "bush")), c("african bush elephant", "bush"))
  expect_equal(drop_repeated_entry_words(character(0)), character(0))

})

test_that("ind_word_check() no longer duplicates the second word of a multi-word entry (pollar bear -> polar bear)", {

  dict <- SemNetDictionaries::animals.dictionary

  expect_equal(ind_word_check("pollar bear", dict, "animals", "US"), list("polar bear"))
  expect_equal(ind_word_check("polar bear xqzvk", dict, "animals", "US"), list("polar bear xqzvk"))

})

test_that("custom_menu() prints each key of the 5-option menu once, even on a very wide console", {

  # Real bug fixed: with a console of 132+ columns and at least one
  # suggestion, the row pattern was longer than the 5 default options and
  # recycled "1: SKIP" into a second copy at the end of the row.
  choices <- c("SKIP", "ADD TO DICTIONARY", "TYPE MY OWN", "GOOGLE IT", "BAD WORD", "dog", "dig")

  for(w in c(20, 80, 131, 132, 200, 400)){
    withr::local_options(width = w)
    out <- paste(capture.output(custom_menu(choices, title = "T", default = 5)), collapse = "\n")
    for(key in c("1: SKIP", "2: ADD TO DICTIONARY", "3: TYPE MY OWN", "4: GOOGLE IT", "5: BAD WORD", "Q: dog", "W: dig")){
      expect_equal(lengths(regmatches(out, gregexpr(key, out, fixed = TRUE))), 1L, info = paste(key, "at width", w))
    }
  }

})

test_that("appropriate_answer(exit = TRUE) lets 'x' or a blank answer leave from the re-prompt", {

  choices <- c("SKIP", "ADD", "TYPE", "GOOGLE", "BAD", "dog")

  expect_equal(suppressMessages(appropriate_answer("9", choices, 5, exit = TRUE, reader = scripted_reader("x"))), 0)
  expect_equal(suppressMessages(appropriate_answer("9", choices, 5, exit = TRUE, reader = scripted_reader("X"))), 0)
  expect_equal(suppressMessages(appropriate_answer("9", choices, 5, exit = TRUE, reader = scripted_reader(""))), 0)

  # a valid answer after an invalid one still works, and without `exit` an
  # 'x' is rejected as before (the walkthrough / save menus)
  expect_equal(suppressMessages(appropriate_answer("9", choices, 5, exit = TRUE, reader = scripted_reader("3"))), 3)
  expect_error(suppressMessages(appropriate_answer("9", choices, 5, reader = scripted_reader(c("x")))), "exhausted")

})

test_that("drop_repeated_entry_words() also drops a repeat of only the END of an expanded entry", {

  # "cocker" had been expanded to "cocker spaniel" and merged with "english";
  # the "spaniel" the participant typed is left behind
  expect_identical(
    drop_repeated_entry_words(c("english cocker spaniel", "spaniel", "sapniel")),
    c("english cocker spaniel", "sapniel")
  )
  expect_identical(drop_repeated_entry_words(c("polar bear", "bear", "x")), c("polar bear", "x"))
  expect_identical(drop_repeated_entry_words(c("a b c", "b", "c", "d")), c("a b c", "d"))
  # a word that is not the end of the entry stays
  expect_identical(drop_repeated_entry_words(c("a b c", "b")), c("a b c", "b"))
  expect_identical(drop_repeated_entry_words(c("dog", "dog")), c("dog", "dog"))

})

test_that("phrase_covered() marks the words that sit inside a dictionary entry", {

  dict <- SemNetDictionaries::animals.dictionary

  expect_identical(phrase_covered(c("sting", "ray", "xx", "yy"), dict), c(TRUE, TRUE, FALSE, FALSE))
  # the longest entry covers all of its words
  expect_identical(phrase_covered(c("brown", "recluse", "spider"), dict), c(TRUE, TRUE, TRUE))
  expect_identical(phrase_covered(c("dog", "xx"), dict), c(FALSE, FALSE))
  expect_identical(phrase_covered("dog", dict), FALSE)
  expect_identical(phrase_covered(character(0), dict), logical(0))

})

test_that("segment_entries() splits words into the longest dictionary entries, or NULL when a word is not covered", {

  dict <- SemNetDictionaries::animals.dictionary

  expect_identical(segment_entries(c("sea", "slug"), dict), "sea slug")
  expect_identical(segment_entries(c("sea", "slug", "dog"), dict), c("sea slug", "dog"))
  expect_identical(segment_entries(c("dog", "sea", "slug", "cat"), dict), c("dog", "sea slug", "cat"))
  expect_identical(segment_entries(c("dog", "cat"), dict), c("dog", "cat"))
  expect_null(segment_entries(c("dog", "xqzvk"), dict))

})

test_that("split_valid_strings() splits a string of dictionary words unless the string is itself an entry", {

  dict <- SemNetDictionaries::animals.dictionary
  to <- list(a = "sea slug", b = c("marmoset otter", "cat"), c = "cat xqzvk", d = NA_character_, e = "sea slug dog", f = "dog")

  res <- split_valid_strings(to, dict)

  expect_identical(res$a, "sea slug")
  expect_identical(res$b, c("marmoset", "otter", "cat"))
  expect_identical(res$c, "cat xqzvk")
  expect_identical(res$d, NA_character_)
  expect_identical(res$e, c("sea slug", "dog"))
  expect_identical(res$f, "dog")
  expect_identical(names(res), names(to))

})

test_that("parallel_process() leaves the user's random seed alone", {

  set.seed(2026)
  before <- get(".Random.seed", envir = globalenv())

  parallel_process(3, function(i) i, cores = 1, progress = FALSE)
  parallel_process(3, function(i) i, cores = 1, progress = TRUE)

  expect_identical(get(".Random.seed", envir = globalenv()), before)

  # No seed to begin with: none is left behind
  rm(".Random.seed", envir = globalenv())
  parallel_process(3, function(i) i, cores = 1, progress = FALSE)
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))

})
