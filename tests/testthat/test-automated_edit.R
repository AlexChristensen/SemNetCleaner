test_that("automated_edit_errors() rejects malformed input", {

  expect_error(automated_edit_errors(c("a", "b")), "matrix")

  no_from <- matrix("x", nrow = 1, dimnames = list("1", c("to_1")))
  expect_error(automated_edit_errors(no_from), '"from"')

  no_to <- matrix("x", nrow = 1, dimnames = list("1", c("from")))
  expect_error(automated_edit_errors(no_to), '"to_#"')

  zero_row <- matrix("x", nrow = 0, ncol = 2, dimnames = list(NULL, c("from", "to_1")))
  expect_error(automated_edit_errors(zero_row), "zero rows")

})

test_that("automated_edit_errors() accepts a well-formed matrix or data.frame", {

  m <- matrix(c("dog", "dogg"), nrow = 1, dimnames = list("1", c("from", "to_1")))
  expect_no_error(automated_edit_errors(m))
  expect_no_error(automated_edit_errors(as.data.frame(m, stringsAsFactors = FALSE)))

})

test_that("automated_edit() guards its shiny/DT dependency and launches shiny::runApp() (static check)", {

  # automated_edit() launches a real Shiny app -- actually invoking it
  # would hang the test suite waiting on a GUI event loop, so this checks
  # the guard/launch statically instead of calling the function. Mocking
  # requireNamespace() would require a package-level `requireNamespace <-
  # NULL` binding just to satisfy testthat's namespace-locking requirement
  # for base-function mocks -- not worth it for one guard check. The real
  # server logic (automated_edit_server()) IS driven end to end below, via
  # shiny::testServer() -- that's the part actually worth testing directly.
  body_text <- paste(deparse(body(automated_edit)), collapse = "\n")

  expect_true(grepl("requireNamespace", body_text, fixed = TRUE))
  expect_true(grepl('"shiny"', body_text, fixed = TRUE))
  expect_true(grepl('"DT"', body_text, fixed = TRUE))
  expect_true(grepl("shiny::runApp", body_text, fixed = TRUE))
  expect_true(grepl("shiny::shinyApp", body_text, fixed = TRUE))

})

# Every automated_edit_server() test below needs a real shiny/DT install
# (both Suggests-only) to actually construct/exercise the reactive graph.
skip_if_not_installed("shiny")
skip_if_not_installed("DT")

sample_automated <- matrix(
  c("dog", "dogg", NA, "cat", "catt", NA),
  nrow = 2, byrow = TRUE,
  dimnames = list(c("1", "2"), c("from", "to_1", "to_2"))
)

test_that("automated_edit_server(): an untouched review round-trips identically, including row names (real bug fix regression guard)", {

  # Real bug fixed (see correct_changes.R's own header): a collapsed
  # single-row selection losing its row name upstream is exactly what
  # correct_changes()'s `correspondence[row.names(automated), ] <- changes`
  # depends on downstream -- this server must preserve row identity
  # through the full reactiveVal()/data.frame round-trip untouched.
  captured <- NULL
  spy <- function(x){ captured <<- x }

  shiny::testServer(automated_edit_server(sample_automated, on_finish = spy), {
    session$setInputs(done = 1)
  })

  expect_equal(row.names(captured), row.names(sample_automated))
  expect_equal(unname(captured), unname(sample_automated))

})

test_that("automated_edit_server(): a cell edit flows through to the captured result", {

  captured <- NULL
  spy <- function(x){ captured <<- x }

  shiny::testServer(automated_edit_server(sample_automated, on_finish = spy), {
    session$setInputs(x_cell_edit = list(row = 1, col = 2, value = "zebra"))
    session$setInputs(done = 1)
  })

  expect_equal(unname(captured[1, "to_1"]), "zebra")
  expect_equal(unname(captured[2, "to_1"]), "catt")

})

test_that("automated_edit_server(): the second ('done2') button reaches the same shared finish() closure", {

  captured <- NULL
  spy <- function(x){ captured <<- x }

  shiny::testServer(automated_edit_server(sample_automated, on_finish = spy), {
    session$setInputs(x_cell_edit = list(row = 2, col = 3, value = "meow"))
    session$setInputs(done2 = 1)
  })

  expect_equal(unname(captured[2, "to_2"]), "meow")

})

test_that("automated_edit_server(): typing the literal \"NA\" sentinel is captured as the string, not coerced to real NA", {

  captured <- NULL
  spy <- function(x){ captured <<- x }

  shiny::testServer(automated_edit_server(sample_automated, on_finish = spy), {
    session$setInputs(x_cell_edit = list(row = 1, col = 2, value = "NA"))
    session$setInputs(done = 1)
  })

  expect_identical(unname(captured[1, "to_1"]), "NA")
  expect_false(is.na(captured[1, "to_1"]))

})

test_that("automated_edit_server(): the reactive table's shape tracks the input matrix", {

  captured <- NULL
  spy <- function(x){ captured <<- x }

  shiny::testServer(automated_edit_server(sample_automated, on_finish = spy), {
    session$setInputs(done = 1)
  })

  expect_equal(dim(captured), dim(sample_automated))
  expect_equal(colnames(captured), colnames(sample_automated))

})

# Phase 11: `id = NULL` module namespacing (embedding automated_edit as one
# step of the larger textcleaner_shiny() wizard) -- `shiny::NS(NULL)` is the
# identity function, so every regression test above (all implicitly
# `id = NULL`) already proves zero behavior change for the standalone
# launcher. These add coverage for the namespaced case itself.

test_that("automated_edit_ui(id = NULL) keeps the original, unprefixed input/output IDs (regression guard)", {

  ui_html <- as.character(automated_edit_ui())

  expect_true(grepl('id="done"', ui_html, fixed = TRUE))
  expect_true(grepl('id="done2"', ui_html, fixed = TRUE))
  expect_true(grepl('id="x"', ui_html, fixed = TRUE))

})

test_that("automated_edit_ui(id = ...) prefixes every input/output ID with the namespace (no cross-talk risk)", {

  ui_html <- as.character(automated_edit_ui("mod1"))

  expect_true(grepl('id="mod1-done"', ui_html, fixed = TRUE))
  expect_true(grepl('id="mod1-done2"', ui_html, fixed = TRUE))
  expect_true(grepl('id="mod1-x"', ui_html, fixed = TRUE))
  # The raw, unprefixed IDs must not appear at all -- two embedded
  # instances (e.g. "mod1"/"mod2") sharing a raw "done"/"x" ID would be
  # exactly the cross-talk this namespacing exists to prevent.
  expect_false(grepl('id="done"', ui_html, fixed = TRUE))
  expect_false(grepl('id="x"', ui_html, fixed = TRUE))

})

test_that("automated_edit_ui() for two different IDs produces fully disjoint ID sets (no cross-talk between two embedded instances)", {

  ui_mod1 <- as.character(automated_edit_ui("mod1"))
  ui_mod2 <- as.character(automated_edit_ui("mod2"))

  expect_false(grepl('id="mod2-', ui_mod1, fixed = TRUE))
  expect_false(grepl('id="mod1-', ui_mod2, fixed = TRUE))

})

test_that("automated_edit_server(id = ...): a namespaced module instance still round-trips a cell edit correctly", {

  captured <- NULL
  spy <- function(x){ captured <<- x }

  # shiny::testServer() only auto-detects a module when the tested
  # function's FIRST formal is literally named `id` (`shiny:::isModuleServer()`)
  # -- automated_edit_server()'s real signature is `(automated, on_finish, id)`,
  # matching its own standalone/positional calling convention (called
  # elsewhere as `automated_edit_server(automated)`), not shiny's module-
  # testing convention. This thin, test-only wrapper just re-shapes the call
  # so testServer() recognizes it as a module; it still exercises the real
  # automated_edit_server()/moduleServer() code path unchanged.
  module_wrapper <- function(id, automated, on_finish){
    automated_edit_server(automated, on_finish = on_finish, id = id)
  }

  shiny::testServer(
    module_wrapper,
    args = list(id = "mod1", automated = sample_automated, on_finish = spy),
    expr = {
      session$setInputs(x_cell_edit = list(row = 1, col = 2, value = "zebra"))
      session$setInputs(done = 1)
    }
  )

  expect_equal(unname(captured[1, "to_1"]), "zebra")
  expect_equal(row.names(captured), row.names(sample_automated))

})

test_that("automated_edit_server(): an edit is recorded without redrawing the table (the user keeps their place)", {

  captured <- NULL
  spy <- function(x){ captured <<- x }

  shiny::testServer(automated_edit_server(sample_automated, on_finish = spy), {

    before <- output$x

    # the row and column of the edited cell (column 2 is "to_1")
    session$setInputs(x_cell_edit = list(row = 1, col = 2, value = "dog"))

    # the rendered table is the same object: nothing was redrawn
    expect_identical(output$x, before)

    session$setInputs(done = 1)

  })

  # ... but the edit was recorded
  expect_equal(unname(captured[1, 2]), "dog")

})

test_that("automated_edit_server(): the table is processed in the browser, not through server-side requests", {

  body_text <- paste(deparse(body(automated_edit_server)), collapse = "\n")

  expect_true(grepl("server = FALSE", body_text, fixed = TRUE))
  expect_false(grepl("replaceData", body_text, fixed = TRUE))

})
