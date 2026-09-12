#' @noRd
# Up-front validation for `automated_edit()`.
# Updated 12.09.2026
automated_edit_errors <- function(automated)
{

  object_error(automated, c("matrix", "data.frame"), "automated_edit")

  if(!"from" %in% colnames(automated)){
    stop("In `automated_edit()`: `automated` must have a \"from\" column.", call. = FALSE)
  }

  if(!any(grepl("^to_", colnames(automated)))){
    stop("In `automated_edit()`: `automated` must have at least one \"to_#\" column.", call. = FALSE)
  }

  if(nrow(automated) == 0){
    stop("In `automated_edit()`: `automated` has zero rows to review.", call. = FALSE)
  }

}

#' @noRd
# Keyboard-editing JS glue for the review table (ENTER-to-edit, Tab/arrow-
# key navigation blurring the active cell). Ported from `app.R`'s own `js`
# vector. Real bug fixed: the original referenced `js` (via `JS(js)`
# inside `render_dt()`) *before* `js` was itself defined later in the same
# file -- it only ever worked because `renderDT()`'s callback closure
# doesn't force its arguments until the app actually runs, by which point
# top-level `source()`-ing of the whole script had already reached the
# later `js <- c(...)` assignment. A function is defined and called at the
# point of use instead, so this can't silently depend on ordering again.
# Updated 12.09.2026
automated_edit_js <- function()
{

  DT::JS(
    "table.on('key', function(e, datatable, key, cell, originalEvent){",
    "  var targetName = originalEvent.target.localName;",
    "  if(key == 13 && targetName == 'body'){",
    "    $(cell.node()).trigger('dblclick.dt');",
    "  }",
    "});",
    "table.on('keydown', function(e){",
    "  if(e.target.localName == 'input' && [9,13,37,38,39,40].indexOf(e.keyCode) > -1){",
    "    $(e.target).trigger('blur');",
    "  }",
    "});"
  )

}

#' Automated Spell-Check Review App UI
#'
#' @description The UI half of the review app used to hand-correct
#' automated spell-check changes mid-\code{\link{textcleaner}} pipeline
#' (see \code{automated_edit()}). Exposed as a reusable, namespaceable
#' building block (Phase 11) so a larger app (\code{textcleaner_shiny()})
#' can embed it as one step of a bigger wizard instead of only launching it
#' standalone.
#'
#' @param id Character or \code{NULL}.
#' A \code{shiny} module namespace ID. Defaults to \code{NULL}, matching
#' this function's original, non-namespaced behavior exactly (
#' \code{shiny::NS(NULL)} is the identity function) -- pass a real ID only
#' when embedding this UI as one module inside a bigger app, alongside the
#' matching \code{id} on \code{\link{automated_edit_server}}
#'
#' @return A \code{shiny} UI definition
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# The review app's UI. Same visual structure as the original `app.R`'s
# `ui`/`dt_output()`, minus two dead, never-finished blocks: a top/bottom
# "Add Column" button pair (`correct_changes()` already pre-computes every
# `to_#` column the review table will ever need, so dynamic column-adding
# was never actually necessary) and a commented-out mouse-hover tooltip
# prototype. `id = NULL` namespacing added (Phase 11): every `inputId`/
# `outputId` wrapped in `ns()` (`shiny::NS(id)`), which is the identity
# function when `id` is `NULL` -- zero behavior change for the standalone
# launcher or Phase 9's own tests.
# Updated 12.09.2026
automated_edit_ui <- function(id = NULL)
{

  ns <- shiny::NS(id)

  shiny::fluidPage(

    shiny::tags$style('#mydiv {font-family:"Lucida Console";}'),

    title = "Edit Data",

    shiny::br(),
    shiny::fluidRow(shiny::column(
      12, shiny::actionButton(inputId = ns("done"), "Done"), align = "right"
    )),

    shiny::fluidRow(shiny::column(
      12,
      shiny::HTML("<h1><span id = 'mydiv'>textcleaner</span>'s Auto-correct Check</h1>"),
      shiny::HTML("<h4>Press <span id = 'mydiv'>ENTER</span> or double-click to edit responses.
        </br>Arrow keys to move between cells."),
      shiny::HTML("<h5><em>Additional cells are provided to separate multiple responses
        (e.g., \"dog cat bird\" to \"dog\" \"cat\" \"bird\")</em></h5>"),
      shiny::hr(),
      DT::DTOutput(ns("x")), align = "center"
    )),

    shiny::fluidRow(shiny::column(
      12, shiny::actionButton(inputId = ns("done2"), "Done"), align = "right"
    )),
    shiny::br(), shiny::br()

  )

}

#' Automated Spell-Check Review App Server
#'
#' @description The server half of the review app used to hand-correct
#' automated spell-check changes mid-\code{\link{textcleaner}} pipeline
#' (see \code{automated_edit()}). Exposed as a reusable, namespaceable
#' building block (Phase 11) so a larger app (\code{textcleaner_shiny()})
#' can embed it as one module of a bigger wizard instead of only launching
#' it standalone.
#'
#' @param automated Matrix or data frame.
#' The automated spell-check changes to review -- a \code{"from"} column
#' plus one or more \code{"to_#"} columns. Closed over directly by the
#' returned server function; no file round-trip involved
#'
#' @param on_finish Function.
#' Called with the edited matrix once either "Done" button is pressed.
#' Defaults to \code{shiny::stopApp} for real standalone use; the injection
#' point \code{shiny::testServer()} tests (and a parent module, via its own
#' \code{on_finish}) use to capture the produced matrix directly instead of
#' depending on \code{stopApp()}'s own behavior
#'
#' @param id Character or \code{NULL}.
#' A \code{shiny} module namespace ID, matching \code{\link{automated_edit_ui}}'s
#' own \code{id}. Defaults to \code{NULL} (no namespacing, this function's
#' original behavior) -- when supplied, the server body is wrapped in
#' \code{shiny::moduleServer(id, ...)}
#'
#' @return A \code{function(input, output, session)} suitable for
#' \code{shiny::shinyApp()}'s \code{server} argument (when \code{id} is
#' \code{NULL}), or a module server usable inside a larger app (when
#' \code{id} is supplied)
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Server factory: `automated` is closed over directly by the returned
# `function(input, output, session)` -- the app's only real input, no
# `tempdir()`/CSV round-trip at all (real bug fixed, see `automated_edit()`
# below for the full explanation). `on_finish` defaults to `shiny::stopApp`
# for real interactive use, but is the injection point `shiny::testServer()`
# tests use to capture the produced matrix directly. `id = NULL` namespacing
# added (Phase 11): when `id` is supplied, the whole body runs inside
# `shiny::moduleServer(id, ...)`, so two embedded instances (e.g. two
# concurrent app sessions) never cross-talk over shared input/output IDs.
# `shiny::moduleServer()` itself already namespaces `output$x` writes and
# `input$x_cell_edit`/`input$done`/`input$done2` reads correctly with no
# further `ns()` calls needed inside the body -- only the UI's own
# `inputId`/`outputId` strings need explicit `ns()` wrapping.
# Updated 12.09.2026
automated_edit_server <- function(automated, on_finish = shiny::stopApp, id = NULL)
{

  server_fn <- function(input, output, session){

    reactive_data <- shiny::reactiveVal(as.data.frame(automated, stringsAsFactors = FALSE))

    output$x <- DT::renderDT(
      {
        reactive_data()
      },
      selection = "none", server = TRUE,
      editable = list(target = "cell", disable = list(columns = 1)),
      options = list(
        orderFixed = c(1, "asc"),
        keys = TRUE, paging = FALSE,
        scrollX = TRUE, autoWidth = FALSE, searching = FALSE,
        columns.width = "1em"
      ),
      callback = automated_edit_js(),
      extensions = "KeyTable"
    )

    proxy <- DT::dataTableProxy("x")

    shiny::observeEvent(input$x_cell_edit, {
      info <- input$x_cell_edit
      new_data <- reactive_data()
      new_data[info$row, info$col] <- suppressWarnings(
        DT::coerceValue(info$value, new_data[info$row, info$col])
      )
      reactive_data(new_data)
      DT::replaceData(proxy, reactive_data(), resetPaging = FALSE)
    })

    # Real bug fixed: the original bound this exact same body to BOTH
    # `input$done` and `input$done2` via two separate, copy-pasted
    # `observeEvent()` calls. Shiny requires one `observeEvent()` per
    # distinct input trigger (two buttons can't share one input ID), but
    # the body itself doesn't need to be duplicated -- one shared closure,
    # bound twice.
    finish <- function(){
      on_finish(as.matrix(reactive_data()))
    }

    shiny::observeEvent(input$done, finish())
    shiny::observeEvent(input$done2, finish())

  }

  # `id = NULL`: return the plain `function(input, output, session)`, for
  # `shiny::shinyApp()`'s own `server` argument -- this function's original
  # (pre-Phase-11) contract, unchanged. `id` supplied: call
  # `shiny::moduleServer()` directly instead of returning a function to call
  # later -- matching the standard shiny module convention (`moduleServer()`
  # mounts the module immediately, against the caller's own ambient reactive
  # domain, rather than being deferred like a plain server function) -- so
  # this must itself be called from inside a live server/module body, not
  # stored and invoked later.
  if(is.null(id)){
    return(server_fn)
  }

  shiny::moduleServer(id, server_fn)

}

#' @noRd
# Launches the review app for spreadsheet-style correction of automated
# spell-check changes -- the manual-review step of the `textcleaner()`
# wizard, driven by `correct_changes()`'s `edit_fn` parameter (which
# defaults to this function; pass `edit_fn = edit` there for the
# lightweight, shiny/DT-free base-R editor instead).
#
# Ported from `AutomatedEdit()`. The app itself was out of scope for the
# main rebuild (binding decision #4); this closes that out. Two real bugs
# confirmed and fixed:
#
# Bug (path join): the original's `app.R` read its input via
# `read.data(file = paste(tempdir(), "automated.csv", sep = "\\"))` inside
# a `reactive({...})` -- a hardcoded Windows-style path separator, broken
# on Linux/Mac (a literal backslash has no path-separator meaning on
# Unix). Rather than patch the join, the whole `tempdir()`/CSV round-trip
# is removed: `automated` is now passed in as a real R argument and closed
# over directly by `automated_edit_server()`, which also removes the
# concurrent-session collision risk of two callers racing on the same
# fixed filename, and makes the server logic unit-testable via
# `shiny::testServer()` with no file setup at all.
#
# Bug (global leak): the original's `onStop(function(x){ changes <<- ... })`
# wrote to a variable literally named `changes` in the caller's global
# environment on every session close (Done button OR the user just
# closing the window) -- entirely redundant with, and riskier than, the
# `stopApp()` return value both Done handlers already produce. Dropped
# entirely; `stopApp()`'s return value (via the injectable `on_finish`) is
# the only path data leaves this app by.
#
# Also fixed: `DT` was `library()`-attached unconditionally despite being
# a `Suggests`-only dependency (confirmed nothing in `DESCRIPTION`
# guarantees it's installed) -- now `requireNamespace()`-guarded exactly
# like `shiny`, with every call fully `DT::`-qualified.
# Updated 12.09.2026
automated_edit <- function(automated, ...)
{

  automated_edit_errors(automated)

  required_packages <- c("shiny", "DT")
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

  if(length(missing_packages) > 0){
    stop(
      paste0(
        "In `automated_edit()`: the following packages are required but aren't installed: ",
        paste0(missing_packages, collapse = ", "), ".\n",
        "Install them with `install.packages(c(", paste0("\"", missing_packages, "\"", collapse = ", "), "))`."
      ),
      call. = FALSE
    )
  }

  shiny::runApp(
    shiny::shinyApp(ui = automated_edit_ui(), server = automated_edit_server(automated)),
    ...
  )

}
