# SemNetCleaner Shiny Companion: Rebuild (binding decision #4)

Picks up the one item deliberately deferred from `REFACTOR_PLAN.md`'s Phase 0/8:
`inst/Automated/app.R`, bug #11. Full rationale for this addendum lives in the approved plan
at `/home/alextops/.claude/plans/please-take-a-look-elegant-axolotl.md`'s "Addendum: Phase 9"
section. `REFACTOR_PLAN.md` itself is not rewritten — this document is the follow-up record,
same relationship `../SemNeT/SHINY_PLAN.md` has to `../SemNeT/REFACTOR_PLAN.md`.

## Context

The original `inst/Automated/app.R` is a single-purpose modal Shiny dialog, not a standalone
app: mid-pipeline in the interactive `textcleaner()` wizard, `correct_changes()` hands the
user a `DT`-table editor over the automated spell-check corrections (`from` + `to_1..to_n`
columns) so they can hand-correct anything the automated pass got wrong before it's folded
into the final cleaned data.

## Phase 9 — `automated_edit()` rebuild

| Item | Status | Notes |
|---|---|---|
| `inst/Automated/app.R` / `inst/Automated/` | Deleted | Nothing else lived in that directory. |
| `R/automated_edit.R` | Rebuilt | Closure-based (`automated` passed as a real argument), no `tempdir()`/CSV round-trip; `automated_edit_errors()`, `automated_edit_js()`, `automated_edit_ui()`, `automated_edit_server()` added alongside it. |
| `R/correct_changes.R` | Edited | OS fork removed (Linux `edit()` vs. non-Linux Shiny app -> one path for every OS); `write.csv()`/`tempdir()` step removed entirely; `edit_fn` default flipped `edit` -> `automated_edit`. |
| `R/textcleaner.R` | Edited | Matching `edit_fn` default flip + doc update. |
| `tests/testthat/test-automated_edit.R` | Rewritten | Validation tests + `shiny::testServer()` coverage of the server logic. |
| `tests/testthat/test-correct_changes.R` | Extended | Blank->NA regression test (previously untested code path), `edit_fn` default guard test. |
| `tests/testthat/test-textcleaner.R` | Extended | `edit_fn` default guard test. |
| `DESCRIPTION` / `NEWS.md` / `README.md` / `.Rbuildignore` / `REFACTOR_PLAN.md` | Updated | See below. |

### Bugs found and fixed (ported from the original, confirmed)

- **Bug #11a** (path join): the original's `app.R` read its input via `read.data(file =
  paste(tempdir(), "automated.csv", sep = "\\"))` inside a `reactive({...})` — a hardcoded
  Windows-style path separator, broken on Linux/Mac. Fixed by removing the whole
  `tempdir()`/CSV round-trip, not by patching the join: `automated` is now passed to
  `automated_edit()` as a real R argument, closed over directly by the server function.
- **Bug #11b** (global leak): `onStop(function(x){ changes <<- as.matrix(isolate(reactiveData())) })`
  wrote to a variable literally named `changes` in the caller's global environment on every
  session close — redundant with, and riskier than, the `stopApp()` return value both Done
  handlers already produced. Dropped entirely; the injectable `on_finish` (default
  `shiny::stopApp`) is the only path data leaves the app by now.
- Two structurally-identical `observeEvent(input$done/input$done2, {...})` handlers —
  consolidated into one shared `finish()` closure, bound twice (Shiny still requires one
  `observeEvent()` per distinct input trigger, but the body no longer needs duplicating).
- The DataTable keyboard-editing JS glue (`js <- c(...)`) was defined and referenced out of
  order in the original file (worked only by luck of lazy evaluation) — now
  `automated_edit_js()`, defined and called at the point of use.
- Two dead, never-finished blocks (an "Add Column" button pair, a hover-tooltip prototype),
  both already commented out and non-functional in the shipped original — dropped rather than
  finished (`correct_changes()` already pre-computes every `to_#` column the table will ever
  need, so dynamic column-adding was never actually necessary).
- Unconditional `library(DT)` despite `DT` being `Suggests`-only — replaced with a combined
  `requireNamespace()` guard (alongside `shiny`) and fully `DT::`-qualified calls.
- The blank-cell -> `NA` normalization (`ifelse(changes == "", NA, changes)`) used to run only
  on the non-Linux/Shiny branch of the now-removed OS fork — it had zero test coverage before
  this phase, since it only ever ran through the un-mockable original Shiny app. Now runs
  unconditionally and is covered by a regression test.

### Real, documented behavior change

`correct_changes()`'s OS fork (Linux -> base `edit()`, every other OS -> the Shiny app) is
gone. Every OS now defaults to `automated_edit()` (the rebuilt Shiny/DT review app) for the
interactive manual-review step. Linux users, who previously got a `shiny`/`DT`-free fallback,
now need those packages installed for the default path — both stay `Suggests`-only, guarded
by `requireNamespace()` with a clear install-instruction error, and `edit_fn = edit` remains
available as an explicit opt-out for the old, lightweight, dependency-free behavior.

### Bugs found via actual browser testing (chronological)

Live verification (2026-09-12): launched a real instance via `SemNetCleaner:::automated_edit(sample,
port = 7788, launch.browser = FALSE)` in a background `Rscript` process, driven with
Claude-in-Chrome against `http://127.0.0.1:7788`, result captured via `saveRDS()` after
`runApp()` unblocked (the only way to observe `stopApp()`'s return value from outside the
blocking process).

- Confirmed the app renders correctly: title, instructions, both "Done" buttons, and the
  `DT` table with the sample `from`/`to_1`/`to_2` data.
- Double-click-to-edit: edited `to_1` for "dog" from "dogg" to "VERIFIED" — flowed through
  correctly to the captured result.
- The `"NA"` sentinel: typed the literal string `"NA"` into `cat`'s `to_2` cell — captured as
  the string `"NA"` (`is.na()` is `FALSE` on it), not coerced to a real `NA`, matching the
  app's own on-screen instructions.
- ENTER-to-edit keyboard glue (`automated_edit_js()`, the piece most likely to regress from
  reordering it): selected the "bird" cell with a single click, pressed ENTER, confirmed it
  entered edit mode exactly like a double-click would — the JS ordering fix did not break
  this.
- **Top** "Done" button (`input$done`): clicked with no edits made in that pass — captured
  result was `identical()` to the original input matrix, including row names.
- **Bottom** "Done" button (`input$done2`): clicked after the "dog"/"cat" edits above —
  captured result correctly reflected both edits, confirming the shared `finish()` closure
  is reachable from both button bindings, not just one.
- Checked browser console messages and network requests during the whole interaction: no
  errors found.

**Result: no new bugs found.** Every interaction matched the expected/designed behavior; the
rebuild's fixes (closure-based data passing, shared `finish()` handler, reordered JS glue)
all held up under a real running session, not just under `shiny::testServer()`.

### Verification performed

- `devtools::test()`: 708/708 passing, 0 failures (6 warnings, all the same pre-existing
  `devtools::load_all()`-only artifact already documented in `REFACTOR_PLAN.md`'s Phase 4/6/8
  write-ups — confirmed absent under a real install by `R CMD check`'s own test run, same as
  every prior phase).
- `devtools::check(cran = FALSE)`: 0 errors, 0 warnings, 0 notes.
- Live browser pass: completed, see above — no bugs found.

## Phase 11 — `textcleaner_shiny()`, the full point-and-click wizard

**Status: `automated_edit()` above (Phase 9) is reused here as an embedded sub-component, not
rebuilt.** This is a new, much larger app: instead of one review dialog, it wraps the ENTIRE
`textcleaner()` pipeline (upload → options → automated spell-check → manual spell-check wizard
→ corrections review → download) end to end, as a point-and-click alternative to the
interactive console wizard.

### Core mechanism (the real design question this phase had to answer)

`spellcheck_dictionary()`'s existing save/resume `continue` mechanism (built during the
original 8-phase rebuild specifically to make the wizard scriptable) is genuinely complete
enough to drive a click-by-click UI without reimplementing any of the wizard's own decision
logic — confirmed by direct code reading, not assumed. Two real gaps were found and closed
along the way, both additive-only (no existing behavior/test changed):

1. **Render context was missing from the saved state.** `save_state()` (inside
   `spellcheck_dictionary()`) captured everything needed to *resume* the wizard, but nothing
   describing the *current pending decision* — the target word, the surrounding context, the
   `best_guess()` candidate list — that a UI needs to actually draw the next prompt/buttons.
   Closed by adding one new, additive `$pending` field (`context`, `check`, `possible`,
   `original`, `category`, `keep_strings`) to `save_state()`, populated explicitly at each of
   its 4 call sites in `R/spellcheck_dictionary.R` from values already computed once (not
   re-derived via `exists()`-sniffing the enclosing scope, which risked reading a stale
   variable left over from a previous response — see the file's own header comment). No
   existing caller reads `$pending`; `test-spellcheck_dictionary.R` gained 2 new regression
   tests confirming it matches exactly what the very next `spellcheck_menu()` call would have
   been given, for both the single-response and multi-word branches.
2. **A real infinite-loop hazard in how a button click's answers must be queued.** Confirmed
   via direct reading of `spellcheck_menu.R`/`helpers.R`: `spellcheck_menu()`'s *outer*
   "Selection" read is the only place an exhausted queue (`reader()` returning `""`) means
   STOP. `offer_add_missing_to_dictionary()`'s per-word yes/no prompt (`yes_no_menu()`) has no
   such guard — it re-prompts in an unguarded `while()` loop, so an under-supplied burst
   doesn't pause the wizard, it hangs the whole synchronous resume call. `build_answer_burst()`
   (`R/build_answer_burst.R`, package-level, tested) closes this by computing the exact number
   of trailing y/n answers a TYPE MY OWN WORD/STRING decision will need via the same
   `dictionary_contains()` check `offer_add_missing_to_dictionary()` itself uses — never a
   fixed-size guess.

Also confirmed and designed around: `walk_through(FALSE, reader = reader)` runs at the top of
**every** resume call (not just the first) whenever there is at least one manual response for
the whole session, and its own final line unconditionally consumes one `reader()` call whose
return value is discarded — `advance_wizard()` (`R/advance_wizard.R`) prepends one placeholder
`""` to every burst for exactly this reason, confirmed via direct reading of `walk_through.R`,
not assumed.

### Package-level additions (all tested via `devtools::test()`, not left to live-browser-only
coverage — this is real, safety-critical logic, not UI glue)

| File | Purpose |
|---|---|
| `R/read_uploaded_file.R` | Renames/copies a `shiny::fileInput()` upload to a path carrying its real extension before dispatching to `read_data()`; `.Rdata` loaded into a private environment, never `.GlobalEnv`. |
| `R/search_shiny.R` | `make_shiny_search_fn(store)` — a `search_fn`-shaped closure matching `spellcheck_menu()`'s real call contract, writing a DuckDuckGo HTML-results URL into a `reactiveVal` instead of calling `browseURL()`. |
| `R/finalize_textcleaner.R` | Pure code motion: `textcleaner()`'s own "back half" (corrections review → final cleaned/binary/behavioral build) extracted so the app can re-run it a second time with the user's real corrections-review edits. Byte-identical behavior, verified via the full existing `test-textcleaner.R` suite unchanged — including the error-path fallback return value (`spell_check`, not `res`), preserved exactly. |
| `R/build_answer_burst.R` | The exact queued-answer burst one button click needs (see hazard #2 above). |
| `R/advance_wizard.R` | Resumes `textcleaner(continue = ...)` with a one-shot queued reader built from a burst (see gap #2/walk-through placeholder above). |
| `R/wizard_menu_labels.R` | The exact, position-ordered menu labels `spellcheck_menu()` itself displays, so the UI can send the real displayed position as the raw answer and let `spellcheck_menu()`'s own internal `keep_strings` remap (5-vs-4 string options) do the rest — no remap logic duplicated in the app. |
| `R/automated_edit.R` (edited) | `automated_edit_ui(id = NULL)`/`automated_edit_server(automated, on_finish, id = NULL)` — `shiny::NS(NULL)` is the identity function, so every existing call/test is unchanged; supplying `id` wraps the server in `shiny::moduleServer()` so it can be embedded inside a bigger app. Promoted from `@noRd` to exported. |
| `R/helpers.R` (edited) | `parallel_process()` gains a nested-`multisession`-worker cap (`future::availableCores()`) and a `SemNetCleaner.progress_file`-driven per-completion progress channel — both ported from `../SemNeT/R/helpers.R`, needed because `progressr`'s own cross-process relay never reaches a `shiny::ExtendedTask`/`future_promise()` caller. |
| `R/textcleaner_shiny.R` | The exported launcher — guards `shiny`/`DT` exactly like `automated_edit()`, calls `shiny::runApp(system.file("Shiny", package = "SemNetCleaner"), ...)`. |

### `inst/Shiny/` app

```
inst/Shiny/
  global.R                 # future::plan(multisession), max_app_cores(), upload size limit
  ui.R                      # navbarPage, one tab per step
  server.R                  # one reactiveValues() `state` per session, tab-visibility
  R/
    mod_shared.R             # progress-file poller/status UI, ported from ../SemNeT
    mod_upload.R             # Step 1: file upload + full textcleaner() options form
    mod_auto_progress.R      # Step 2: ExtendedTask running textcleaner() with an always-"" reader
    mod_wizard.R              # Step 3: the manual review wizard -- UI glue only, see above
    mod_search_modal.R       # DuckDuckGo iframe modal, observes the shared search reactiveVal
    mod_corrections.R        # Step 4: embeds automated_edit_ui/server (namespaced), re-runs finalize_textcleaner()
    mod_export.R             # Step 5: downloadHandler()s built from state$result's real fields
```

Scope cut, deliberate: `add.path`'s interactive directory-picker mode (`"choose"`) is a plain
server-side text field instead. `$removed`/`$partChanges` (documented in `textcleaner()`'s own
`@return`) are not offered for download — confirmed via grep that neither is ever actually
populated anywhere in `textcleaner()`'s code, so the Export tab is built from the object's real
fields, not the aspirational doc.

### Testing

Package-level logic (`build_answer_burst()`, `advance_wizard()`, `wizard_menu_labels()`,
`read_uploaded_file()`, `make_shiny_search_fn()`, the `parallel_process()` additions,
`spellcheck_dictionary()`'s new `$pending` field) is covered by `devtools::test()` — including
full end-to-end wizard walkthroughs driven the exact same way the Shiny UI does (queued bursts
through `advance_wizard()`, real `textcleaner()`/`spellcheck_dictionary()` calls, real
`SemNetDictionaries::animals.dictionary` lookups), covering both `keep_strings` values and the
internal menu-position remap that only fires when `keep_strings = TRUE`. `inst/Shiny/`'s own
module files (UI glue, following `../SemNeT`'s own precedent for its Shiny app) are not
separately unit-tested — verified instead via the live browser pass below.

### Live browser verification

The Claude-in-Chrome extension was not available in this environment, so live verification
used headless Chrome directly via R's `chromote` package (already installed) driving the
system's real `google-chrome` binary — the same headless-Chrome-via-system-binary approach
used elsewhere for this kind of verification, just invoked from R instead of the extension.
`SemNetCleaner::textcleaner_shiny(port = 7799, launch.browser = FALSE)` launched in a
background `Rscript` process against a real, freshly-`devtools::install()`ed copy of the
package (not `devtools::load_all()` — see the first bug below for why that distinction
mattered), driven end to end: upload a 3-participant/3-response fluency CSV → set options →
Start Automated Spell-Check → Manual Review → Corrections Review → Export, using real file
upload (`DOM.setFileInputFiles`) and real button clicks, not simulated events.

**Bugs found via actual browser testing (chronological):**

1. **`future::multisession` workers don't see a `devtools::load_all()`-only package.** The
   automated-pass `ExtendedTask` failed with `unused arguments (reader = ..., edit_fn = ...,
   cores = ...)` — traced to a *stale, previously-installed* copy of `SemNetCleaner` in the
   user library predating even the `reader`/`edit_fn` parameters (from an earlier phase's own
   `devtools::install()`), which `future`'s worker processes load via a real `library()` call
   regardless of what the main process has `load_all()`ed. Not a code bug -- fixed by
   `devtools::install()`ing the current package before live-testing the app; documented here
   since anyone else live-testing this app needs the same real install, not just `load_all()`.
2. **Preview table rendered as raw, escaped HTML text instead of an actual table.**
   `mod_upload.R`'s `output$preview` called `shiny::renderTable(...)()` directly and embedded
   the result inline inside a `renderUI()`-built `tagList()` -- `renderTable()`'s return value
   is HTML source meant for a paired `tableOutput()`, not something safe to splice into
   another render's output directly. Fixed by splitting into a real `output$preview_table <-
   renderTable(...)` / `tableOutput()` pair.
3. **`wizard_response_letters(n)` returned `n` elements (padded with `NA`) for `n > 10`**
   instead of capping at 10, confirmed via a direct unit test once written -- caught before
   this ever reached the browser, but recorded here since it was found during this same
   phase's live-testing prep. Fixed via `seq_len(min(n, 10))`.
4. **Every helper `mod_wizard.R`/`mod_corrections.R`/`mod_auto_progress.R` need
   (`build_answer_burst()`, `advance_wizard()`, `wizard_menu_labels()`,
   `wizard_response_letters()`, `make_shiny_search_fn()`, `finalize_textcleaner()`) was
   unreachable at runtime: `could not find function "wizard_menu_labels"` (and equivalents).
   `inst/Shiny/`'s own files are sourced into the *global* environment by Shiny's own
   app-loading mechanism, not into the package namespace -- an unexported (`@noRd`) function
   is invisible to them by its bare name, exactly the same reachability issue Phase 9 already
   solved for `automated_edit_ui()`/`automated_edit_server()`. Fixed the same way: promoted
   all six from `@noRd` to fully exported, documented functions.
5. **KEEP ORIGINAL hung the whole resume call.** Confirmed via direct reproduction (not just
   observed in-browser): `spellcheck_menu()`'s KEEP ORIGINAL branch (context/multi-word only)
   has its own independent `dictionary_contains(original, full_dictionary)` check -- `original`
   there is the whole, un-split multi-word response text, essentially never itself a literal
   dictionary entry -- which reliably triggers one more `yes_no_menu()` prompt. `yes_no_menu()`
   re-prompts in an unguarded loop, so an under-supplied burst here doesn't pause the wizard,
   it hangs the synchronous resume call indefinitely. Fixed by adding `build_answer_burst()`'s
   `original_check` parameter (see that file's own header) and having `mod_wizard.R` pass
   `pending$original` whenever the button being sent is KEEP ORIGINAL.
6. **Corrections Review crashed with "non-conformable arrays."** `mod_corrections.R` rendered
   the *raw*, un-padded `res$spellcheck$automated` for editing, but `correct_changes()`
   internally pads its own copy (enough `"to_#"` columns for the widest multi-word `"from"`
   entry) before ever calling `edit_fn` -- handing back the narrower, un-padded shape from the
   review step crashed the moment `finalize_textcleaner()`'s second call compared the two.
   Fixed by extracting that padding logic out of `correct_changes()` into a new, exported
   `prepare_automated_for_review()`, used by both `correct_changes()` (replacing its old inline
   block, byte-identical behavior) and `mod_corrections.R` (so the review table's shape always
   matches what `correct_changes()` will expect back).

After fixes 1-6, a full session completed cleanly through every tab (Upload & Options →
Automated Spell-Check → Manual Review → Corrections Review → Export) with **zero JS
exceptions** thrown during the whole interaction (checked via `Runtime.exceptionThrown`), a
real SKIP decision correctly resolving the one ambiguous response ("catdog"), and the Export
tab rendering all 5 real download buttons. A live in-browser cell-edit interaction (double-
click a `DT` cell, type a replacement) was attempted but the test script's own cell-locator
query didn't find the target row (a test-script limitation, not investigated further) --
the underlying mechanism was instead verified directly in R: `prepare_automated_for_review()`'s
output, hand-edited, round-tripped correctly through a second `finalize_textcleaner()` call
end to end (a "cow" → "puppy" edit landed correctly in the final `$responses$clean`), and
`automated_edit_server()`'s own cell-editing JS (`automated_edit_js()`) was already verified
live in Phase 9 and is unchanged here except for the `id = NULL` namespacing refactor, which
is covered by `shiny::testServer()` regression tests instead.

### Verification performed

- `devtools::test()`: 812 passing, 0 failures (6 pre-existing warnings, all the same
  `future.apply`/`devtools::load_all()`-only artifact already documented at Phase 4/8/10,
  unrelated to this phase).
- `devtools::check(cran = FALSE)`: 0 errors, 0 warnings, 0 notes.
- Live browser pass: completed after fixing the 6 bugs above, see the log there -- zero JS
  exceptions on the final clean run, full Upload → Export walkthrough confirmed working.

## Post-Phase-11 addendum — "Save to R Environment" + `SemNeTShiny()` hand-off

Per direct request, two related additions:

1. **`mod_export.R` gains a "Save to R Environment" control**: an object-name text field
   (default `textcleaner_result`) + an action button that `assign()`s `state$result` directly
   into `.GlobalEnv`, alongside the existing download buttons (never a replacement for them --
   only meaningful when the app runs in the same R process as the user's own console, the
   ordinary local/RStudio way of launching it). A basic R-name validity check
   (`^[a-zA-Z.][a-zA-Z0-9._]*$`) guards the assignment.
2. **`SemNeT::SemNeTShiny()`'s own Load Data tab** (a separate, sibling package) is extended to
   recognize any `textcleaner`-classed object already sitting in `.GlobalEnv` as an alternative
   data source alongside its existing file-upload widget -- not a replacement, a second option,
   per direct request ("separate from browsing the computer"). See `../SemNeT/SHINY_PLAN.md`
   for that package's own record of the change (the shared `.GlobalEnv` -- rather than any
   file/temp-path/`options()` signal -- is the whole hand-off mechanism: both apps are launched
   sequentially from the same interactive session in the ordinary case, so "Save to R
   Environment" here is literally the same action `SemNeTShiny()` later looks for).
