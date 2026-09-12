# SemNetCleaner 2.0.0

Full rebuild of the package's internals (see `REFACTOR_PLAN.md` for the
phase-by-phase development record). Every pre-2.0.0 exported name still
works -- see "Backward compatibility" below -- so existing scripts do not
need to change. The package had zero tests before this rebuild; it now has
an extensive `testthat` suite covering every exported and internal function,
including scripted-input tests that drive the full interactive spell-check
wizard end to end.

## New functions

* `qwerty_distance()` and `singularize()` -- both were, until this rebuild's
  own Phase 8 documentation audit, silently **never actually exported**: a
  roxygen doc-comment placement bug (see "Real bugs fixed" below) attached
  their `@export` tag to an unrelated private helper object placed between
  the doc comment and the function it was meant to document. Invisible
  throughout development, since `devtools::load_all()` exposes internal
  package objects the same as exported ones.
* `bad_response()`, `best_guess()`, `binary_to_responses()`,
  `qwerty_distance()`, `read_data()`, `responses_to_binary()` -- the
  snake_case replacements for `bad.response()`, `best.guess()`,
  `bin2resp()`, `qwerty.dist()`, `read.data()`, `resp2bin()` (see
  "Backward compatibility").

## Real bugs fixed

More than 60 confirmed correctness issues were found and fixed across every
phase of this rebuild; the full numbered list (with reproductions) lives in
`REFACTOR_PLAN.md`. Highlights:

* **`bad.response()` was defined twice** -- once as the real exported
  function, and, byte-identically, a second time inside
  `utils-SemNetCleaner.R` (marked `@noRd`). Alphabetical file collation
  meant the second, unreviewed copy silently won at load time. Now defined
  exactly once.
* **`correct.data()`'s Perseverations count was silently always 0** for any
  participant with no literal `"NA"`-string response -- the ordinary case
  for anyone with no bad responses. `correct.ord[-which(correct.ord ==
  "NA")]` (a vector minus an *empty* `which()` result) returns the vector
  **emptied out**, not unchanged -- R can't distinguish a "negative empty"
  index from a "positive empty" one. Confirmed and quantified against real
  bundled data: the original reports 3 perseverations total across 516
  participants; the correct total is 116, a ~38x undercount affecting 73 of
  516 participants (14%). This exact mechanism (`x[-which(cond)]` on an
  empty `which()`) produced 6 of the ~60 bugs found in this rebuild, in
  vectors, matrix columns, and lists alike.
* **A manual edit made during the automated-changes spreadsheet review step
  was silently discarded for fluency data**, in two compounding ways: (1)
  `corr_mat[spell_check$auto, ]` (no `drop = FALSE`) collapsed a
  single-response selection into a plain vector, losing the row identity a
  user's edit needs to land on; (2) even once that's fixed, the fluency
  branch never re-synced its own working copy of the correspondence matrix
  from the corrected one afterward (unlike the free-association branch,
  which already did). Confirmed directly: a "dogg" -> "zebra" edit showed up
  correctly in `$spellcheck$correspondence` but the final `$responses$clean`/
  `$responses$binary` still showed "dog".
* **`textcleaner(data, type = "fluency")`, the package's own default call
  path with no explicit `partBY =`, crashed outright** ("the condition has
  length > 1") on any current R version -- `partBY`'s own `c("row", "col")`
  default was never resolved via `match.arg()`. Every one of the function's
  own documented examples happened to always pass `partBY` explicitly.
* **`keepLength` was a structural no-op** for the fluency pipeline:
  `apply(data, 1:2, length)` measures each cell's own `length()` (always
  `1` for a single string), not its word count, so nothing was ever
  filtered for `keepLength >= 2`, and *everything* was filtered for
  `keepLength <= 1` once the word-count bug was fixed but a second,
  previously-masked off-by-one (`>= keepLength` instead of `> keepLength`)
  was still in place.
* **The manual spell-check wizard's GO-BACK/undo could silently discard
  every other already-recorded change in the entire session**, not just the
  one being undone -- the same "empty `which()`" class of bug as the
  perseveration count above, this time on a list.
* **`qwerty_distance()` and `singularize()` were silently never part of the
  package's public API** (see "New functions" above) -- a roxygen
  doc-comment attachment bug, not a logic bug, but one that would have meant
  `library(SemNetCleaner); qwerty_distance(...)` failing with "could not
  find function" for any real user, despite working fine in every phase's
  own `devtools::load_all()`-based dev testing.
* `convert2snafu()`'s single-dataset branch (`if(length(data.list) ==
  1){}else{...}`) was completely empty -- the function's own documented
  single-argument example silently wrote an empty, header-only CSV instead
  of the real data.
* Several `1:length()`/`1:nrow()`/`1:ncol()`-style ranges that silently ran
  backwards on degenerate (empty/size-1) inputs, individually audited
  (not blanket-converted) across every file -- see `REFACTOR_PLAN.md`'s bug
  #8 closing audit for the full, numbered list.

## Backward compatibility

Every pre-2.0.0 exported name is kept working via `R/legacy.R`, calling
straight into its new-named replacement:

| Old name | New name |
|---|---|
| `bad.response()` | `bad_response()` |
| `best.guess()` | `best_guess()` |
| `bin2resp()` | `binary_to_responses()` |
| `qwerty.dist()` | `qwerty_distance()` |
| `read.data()` | `read_data()` |
| `resp2bin()` | `responses_to_binary()` |

`textcleaner()`, `convert2snafu()`, `pluralize()`, and `singularize()` keep
their original names -- no wrapper needed.

## Shiny companion app

`automated_edit()` -- the manual spell-check review app launched mid-pipeline by
`correct_changes()`/`textcleaner()` -- is fully rebuilt (see `SHINY_PLAN.md` for the
development record). It now takes the data to review as a real R argument
(`automated_edit(automated)`) instead of round-tripping it through a
`tempdir()`/CSV file, which fixes a real, confirmed bug (a hardcoded Windows-style
path separator broke the file read on Linux/Mac) at the root instead of patching
the join, removes a concurrent-session file-collision risk, and makes the app's
server logic unit-testable via `shiny::testServer()`.

* **Real behavior change**: `correct_changes()` previously forked on OS for this
  step -- Linux used base R's lightweight `edit()`, every other OS launched the
  Shiny app. That fork is gone; every OS now defaults to the Shiny/`DT` review
  app. **Linux users who previously needed no extra packages for this step now
  need `shiny` and `DT` installed** (both remain `Suggests`-only, with a clear
  install-instruction error if missing) -- pass `edit_fn = edit` explicitly to
  keep the old, dependency-free behavior.
* A second real bug fixed: `onStop()` wrote a variable named `changes` into the
  caller's global R environment as a side effect of the app closing, redundant
  with (and riskier than) the return value already produced by the "Done"
  buttons. Removed entirely.
* The blank-cell -> `NA` normalization for a reviewed response now applies
  unconditionally (previously only on the non-Linux/Shiny branch), closing a
  code path that had no test coverage at all before this rebuild.

## Spell-correction enhancements

`best_guess()` gains a phonetic (Metaphone) disambiguation signal alongside
its existing QWERTY-keyboard tiebreak, closing a real gap: a word like
`"graple"` differs in length from both of its plausible corrections
(`"grape"`, `"grapple"`), so the existing same-length-only QWERTY check
couldn't disambiguate them, but Metaphone resolves it instantly. QWERTY and
phonetic are evaluated as peers -- agreement reinforces the answer,
disagreement falls through to manual review rather than arbitrarily picking
one (e.g. a `"gremble"` typo of `"grumble"` has QWERTY pick `"tremble"` while
Metaphone correctly picks `"grumble"`; neither is trusted over the other).
`@references` gains a Philips (1990) Metaphone citation. `hunspell` and
`phonics` move from `Suggests` to `Imports`, since both are reachable from
`textcleaner()`'s own default pipeline.

## Point-and-click wizard: `textcleaner_shiny()`

A new Shiny app wrapping the entire `textcleaner()` pipeline end to end --
upload, options, automated spell-check, manual spell-check wizard,
corrections review, download -- as a point-and-click alternative to the
interactive console wizard. Every console menu option is a real clickable
button, plus a free-text box for typing a custom word/string directly; the
"GOOGLE WORD"/"GOOGLE STRING"/"GOOGLE IT" options open an embedded in-app
search (DuckDuckGo -- a real Google results page cannot be embedded via
iframe at all) instead of a new browser window. Built directly on the
console wizard's own save/resume mechanism, so none of `spellcheck_menu()`'s
decision logic is reimplemented -- see `SHINY_PLAN.md` for the full design
record, including two real, additive-only fixes this required in
already-tested code: `spellcheck_dictionary()`'s saved resume state gains a
`$pending` field describing the next decision's render context, and a new
`build_answer_burst()` closes a genuine hang hazard in how many answers a
single button click must supply. `automated_edit()` (the automated-changes
review step) is reused here as an embedded, namespaced sub-module rather
than rebuilt, via `automated_edit_ui()`/`automated_edit_server()`'s new
`id =` argument. A new `prepare_automated_for_review()` closes a real bug
found via live browser testing (a "non-conformable arrays" crash reviewing
automated corrections) by extracting `correct_changes()`'s own automated-
matrix padding logic into a shared, exported function. `advance_wizard()`,
`build_answer_burst()`, `wizard_menu_labels()`, `wizard_response_letters()`,
and `make_shiny_search_fn()` are also newly exported -- every one of these,
`textcleaner_shiny()`'s own wizard module needs to call by name from outside
the package namespace. `promises` added to `Suggests`; minimum `shiny`
version raised to 1.8.0 (for `shiny::ExtendedTask`). The package's own
startup message now points new users at `textcleaner_shiny()`.

## Dependency changes

* **Dropped**: `pbapply`, `parallel` (both entirely unused once the merged
  wizard functions moved onto `parallel_process()`, a `future`/
  `future.apply`/`progressr`-based helper with guaranteed worker cleanup and
  a core-count floor); `easycsv` (`convert2snafu()`'s unconditional
  dependency on a `Suggests`-only package -- replaced with an explicit
  `path =` argument).
* **Moved to `Suggests`-only**, each guarded by `requireNamespace()` at its
  one real call site: `shiny`/`DT` (the deferred `AutomatedEdit()` companion
  app), `tcltk` (the Windows-only graphical progress bar during manual
  spell-checking, now falling back to the console progress bar if `tcltk`
  isn't available), `rstudioapi` (RStudio dark-mode highlight-color
  detection).
* **Unchanged**: `foreign`, `future`, `future.apply`, `progressr`,
  `R.matlab`, `readxl`, `searcher`, `stringi`, `stringdist`; `hunspell`
  (`Suggests` at this point in the rebuild -- moved to `Imports` later, see
  "Spell-correction enhancements" above), `htmlTable`, `knitr`, `markdown`,
  `rmarkdown`, `testthat` (`Suggests`).
* `NAMESPACE`'s whole-package `import(SemNetDictionaries)` is dropped
  entirely -- every call site already fully namespace-qualifies
  (`SemNetDictionaries::...`), so no `import`/`importFrom` was ever actually
  needed.

## Internal

* One file per exported function (function + its own `<name>_errors()`
  input validation), replacing the previous 7,491-line
  `utils-SemNetCleaner.R` (now fully deleted -- every one of its 39
  functions has a ported, individually tested replacement elsewhere, and
  nothing in the rebuilt codebase calls into it any longer).
* The ~80-85% duplication between the fluency and free-association code
  paths is collapsed into single, `type =`-dispatching functions:
  `auto_spellcheck()`, `spellcheck_dictionary()`, `textcleaner()`,
  `prep_spellcheck_dictionary()`. `correct_data()`/`correct_data_free()`
  stay separate -- confirmed genuinely different algorithms (the
  free-association variant does cue/ID sequence row-splicing fluency never
  needs).
* Every interactive/prompting function (the whole manual spell-check
  wizard, `textcleaner()`'s own narrative pauses, `correct_changes()`'s
  spreadsheet-editor step) now takes an injectable input reader
  (`reader = readline` / `edit_fn = edit`, defaulting to the real
  functions), the infrastructure that makes the wizard unit-testable for
  the first time.
* ~175 lines of confirmed dead code removed (`ql.dist()`, `permn()`,
  `update.monikers()`, several commented-out blocks), plus two further
  stale/shadowed duplicate definitions found and removed during Phase 1's
  own follow-up review (`seq_min_max()`, `moniker()`).
