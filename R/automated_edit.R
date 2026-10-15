#' @noRd
# Errors for the review app ----
# Up-front validation for `automated_edit()`.
# Updated 14.10.2026
automated_edit_errors <- function(
    automated # table of automated corrections
)
{

  # Check input type
  object_error(automated, c("matrix", "data.frame"), "automated_edit")

  # Check for the original responses column
  if(!"from" %in% colnames(automated)){

    # Send error
    stop("In `automated_edit()`: `automated` must have a \"from\" column.", call. = FALSE)

  }

  # Check for correction columns
  if(!any(grepl("^to_", colnames(automated)))){

    # Send error
    stop("In `automated_edit()`: `automated` must have at least one \"to_#\" column.", call. = FALSE)

  }

  # Check for responses to review
  if(nrow(automated) == 0){

    # Send error
    stop("In `automated_edit()`: `automated` has zero rows to review.", call. = FALSE)

  }

}

#' @noRd
# JavaScript for the review app ----
# JavaScript for the review table: ENTER starts editing a cell and Tab or the arrow keys
# leave it. It is a function (not an object defined later in the file) so that it does
# not depend on the order of the file
# Updated 14.10.2026
automated_edit_js <- function()
{

  # Create JavaScript for keyboard editing
  return(DT::JS(
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
  ))

}

#' Automated Spell-Check Review App UI
#'
#' @description The UI half of the review app used to hand-correct
#' automated spell-check changes mid-\code{\link{textcleaner}} pipeline
#' (see \code{automated_edit()}). Exposed as a reusable, namespaceable
#' building block so a larger app (\code{textcleaner_shiny()})
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
#' @keywords internal
# User interface of the review app ----
# The review app's UI. Every `inputId` and `outputId` goes through `ns()` (`shiny::NS(id)`,
# which does nothing when `id` is `NULL`), so the UI can be embedded as a module
# Updated 14.10.2026
automated_edit_ui <- function(
    id = NULL # namespace of the module
)
{

  # Create namespace function
  ns <- shiny::NS(id)

  # Create page
  return(shiny::fluidPage(

    # Set font for highlighted text
    shiny::tags$style('#mydiv {font-family:"Lucida Console";}'),

    # Set title
    title = "Edit Data",

    # Add spacing
    shiny::br(),
    # Add top Done button
    shiny::fluidRow(shiny::column(
      12, shiny::actionButton(inputId = ns("done"), "Done"), align = "right"
    )),

    shiny::fluidRow(shiny::column(
      12,
      # Add heading
      shiny::HTML("<h1><span id = 'mydiv'>textcleaner</span>'s Auto-correct Check</h1>"),
      # Add instructions
      shiny::HTML("<h4>Press <span id = 'mydiv'>ENTER</span> or double-click to edit responses.
        </br>Arrow keys to move between cells."),
      # Add note about extra cells
      shiny::HTML("<h5><em>Additional cells are provided to separate multiple responses
        (e.g., \"dog cat bird\" to \"dog\" \"cat\" \"bird\")</em></h5>"),
      # Add divider
      shiny::hr(),
      # Add table
      DT::DTOutput(ns("x")), align = "center"
    )),

    # Add bottom Done button
    shiny::fluidRow(shiny::column(
      12, shiny::actionButton(inputId = ns("done2"), "Done"), align = "right"
    )),
    # Add spacing
    shiny::br(), shiny::br()

  ))

}

#' Automated Spell-Check Review App Server
#'
#' @description The server half of the review app used to hand-correct
#' automated spell-check changes mid-\code{\link{textcleaner}} pipeline
#' (see \code{automated_edit()}). Exposed as a reusable, namespaceable
#' building block so a larger app (\code{textcleaner_shiny()})
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
#' @keywords internal
# Server of the review app ----
# Server factory: `automated` is closed over directly (there is no file round trip) and
# `on_finish` defaults to `shiny::stopApp` (tests use it to capture the produced matrix).
# With an `id`, the body runs inside `shiny::moduleServer()`, so embedded instances never
# share input or output IDs
# Updated 14.10.2026
automated_edit_server <- function(
    automated, # table of automated corrections
    on_finish = shiny::stopApp, # function called with the edited table
    id = NULL # namespace of the module
)
{

  # Create server function
  server_fn <- function(input, output, session){

    # Store the editable table
    reactive_data <- shiny::reactiveVal(as.data.frame(automated, stringsAsFactors = FALSE))

    # Render editable table once: the data are read without a reactive dependency
    # (`shiny::isolate()`), and the table is processed in the browser (`server = FALSE`),
    # so an edit does not redraw the table and the user keeps their place; the browser
    # table holds the edited value and the server only records it
    output$x <- DT::renderDT(
      {
        # Return the starting table
        shiny::isolate(reactive_data())
      },
      selection = "none", server = FALSE,
      editable = list(target = "cell", disable = list(columns = 1)),
      options = list(
        orderFixed = c(1, "asc"),
        keys = TRUE, paging = FALSE,
        scrollX = TRUE, autoWidth = FALSE, searching = FALSE,
        columns.width = "1em"
      ),
      # Add keyboard editing
      callback = automated_edit_js(),
      # Add keyboard navigation extension
      extensions = "KeyTable"
    )

    # Record the edit when a cell is edited
    shiny::observeEvent(input$x_cell_edit, {
      # Get edited cell
      info <- input$x_cell_edit
      # Get current table
      new_data <- reactive_data()
      # Replace edited value
      new_data[info$row, info$col] <- suppressWarnings(
        DT::coerceValue(info$value, new_data[info$row, info$col])
      )
      # Store updated table (the table in the browser already shows the edit)
      reactive_data(new_data)
    })

    # One shared closure for both Done buttons (each button needs its own `observeEvent()`)
    finish <- function(){

      # Return edited table
      on_finish(as.matrix(reactive_data()))

    }

    # Finish when the top Done button is pressed
    shiny::observeEvent(input$done, finish())

    # Finish when the bottom Done button is pressed
    shiny::observeEvent(input$done2, finish())

  }

  # Without `id`, return the plain `function(input, output, session)` for `shiny::shinyApp()`;
  # with `id`, mount the module right away with `shiny::moduleServer()`, which must be called
  # from inside a live server or module body
  if(is.null(id)){

    # Return server function
    return(server_fn)

  }

  # Mount server as a module
  return(shiny::moduleServer(id, server_fn))

}

#' @noRd
# Automated edit ----
# Launches the review app for spreadsheet-style correction of the automated spell-check
# changes: the manual-review step of the `textcleaner()` wizard, used through
# `correct_changes()`'s `edit_fn` (pass `edit_fn = edit` there for the lightweight base R
# editor, which needs no `shiny` or `DT`)
#
# `automated` is passed as an argument and closed over by `automated_edit_server()`, so there
# is no temporary file and nothing is left in the global environment; the table leaves the
# app only as the value of `shiny::stopApp()`. `shiny` and `DT` are `Suggests` and checked
# with `requireNamespace()`
# Updated 14.10.2026
automated_edit <- function(
    automated, # table of automated corrections
    ... # arguments passed to shiny::runApp
)
{

  # Check for errors
  automated_edit_errors(automated)

  # Set required packages
  required_packages <- c("shiny", "DT")

  # Find missing packages
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

  # Check for missing packages
  if(length(missing_packages) > 0){

    # Send error
    stop(
      paste0(
        "In `automated_edit()`: the following packages are required but aren't installed: ",
        paste0(missing_packages, collapse = ", "), ".\n",
        "Install them with `install.packages(c(", paste0("\"", missing_packages, "\"", collapse = ", "), "))`."
      ),
      call. = FALSE
    )

  }

  # Run the review app
  shiny::runApp(
    shiny::shinyApp(ui = automated_edit_ui(), server = automated_edit_server(automated)),
    ...
  )

}
