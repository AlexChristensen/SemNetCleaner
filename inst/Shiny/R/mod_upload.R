# Step 1: file upload + the full textcleaner() options form. `add.path`'s
# interactive directory-picker mode ("choose") is a deliberate scope cut --
# a plain server-side path-string field instead (see SHINY_PLAN.md).

mod_upload_ui <- function(id)
{

  ns <- shiny::NS(id)

  shiny::sidebarLayout(

    shiny::sidebarPanel(

      shiny::fileInput(ns("file"), "Upload response data", accept = c(
        ".csv", ".txt", ".xlsx", ".xls", ".sav", ".rds", ".Rdata", ".mat", ".dat"
      )),

      shiny::radioButtons(
        ns("type"), "Task type",
        choices = c("Verbal fluency" = "fluency", "Free association" = "free"),
        selected = "fluency"
      ),

      shiny::selectizeInput(
        ns("dictionary"), "Dictionary", multiple = TRUE,
        choices = SemNetCleaner:::get_available_dictionaries(),
        selected = "general"
      ),

      shiny::radioButtons(ns("spelling"), "Spelling", choices = c("US", "UK"), selected = "US"),

      shiny::conditionalPanel(
        condition = paste0("input['", ns("type"), "'] == 'fluency'"),
        shiny::radioButtons(
          ns("partBY"), "Participants are organized by",
          choices = c("Row" = "row", "Column" = "col"), selected = "row"
        )
      ),

      shiny::conditionalPanel(
        condition = paste0("input['", ns("type"), "'] == 'free'"),
        shiny::checkboxInput(ns("keepCue"), "Keep cue words in responses", value = FALSE)
      ),

      shiny::textInput(ns("miss"), "Missing data value", value = "99"),
      shiny::checkboxInput(ns("keepStrings"), "Keep multi-word responses as strings", value = FALSE),
      shiny::textInput(ns("allowPunctuations"), "Punctuation to allow", value = "-"),
      shiny::checkboxInput(ns("allowNumbers"), "Allow numbers", value = FALSE),
      shiny::checkboxInput(ns("lowercase"), "Convert to lowercase", value = TRUE),
      shiny::numericInput(ns("keepLength"), "Max words per response (blank = no limit)", value = NA, min = 1),
      shiny::textInput(ns("add_path"), "Additional dictionary path (optional)", value = ""),
      shiny::numericInput(ns("cores"), "Cores", value = 1, min = 1, max = max_app_cores()),

      shiny::actionButton(ns("start"), "Start Automated Spell-Check", class = "btn-primary")

    ),

    shiny::mainPanel(
      shiny::uiOutput(ns("preview_summary")),
      shiny::tableOutput(ns("preview_table"))
    )

  )

}

mod_upload_server <- function(id, state)
{

  shiny::moduleServer(id, function(input, output, session){

    uploaded <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$file, {

      result <- tryCatch(
        read_uploaded_file(input$file$datapath, input$file$name),
        error = function(e){
          shiny::showNotification(paste("Could not read file:", conditionMessage(e)), type = "error")
          NULL
        }
      )

      uploaded(result)

    })

    output$preview_summary <- shiny::renderUI({

      data <- uploaded()

      if(is.null(data)){
        return(shiny::tags$p(shiny::tags$em("Upload a file to preview it here.")))
      }

      shiny::tags$p(paste0("Rows: ", nrow(data), ", Columns: ", ncol(data)))

    })

    # A separate renderTable()/tableOutput() pair, not renderTable() called
    # and embedded inline inside renderUI() -- real bug found via live
    # verification: renderTable()'s return value is a raw HTML string
    # meant for tableOutput() to display; calling it directly inside a
    # tagList() just prints that HTML source as escaped, literal text
    # instead of rendering an actual table.
    output$preview_table <- shiny::renderTable({
      shiny::req(uploaded())
      utils::head(uploaded())
    })

    shiny::observeEvent(input$start, {

      shiny::req(uploaded())

      keep_length <- if(is.na(input$keepLength)) NULL else input$keepLength

      state$data <- uploaded()
      state$options <- list(
        type = input$type,
        dictionary = if(length(input$dictionary) == 0) NULL else input$dictionary,
        spelling = input$spelling,
        partBY = input$partBY,
        keepCue = isTRUE(input$keepCue),
        miss = suppressWarnings(as.numeric(input$miss)),
        keepStrings = isTRUE(input$keepStrings),
        allowPunctuations = input$allowPunctuations,
        allowNumbers = isTRUE(input$allowNumbers),
        lowercase = isTRUE(input$lowercase),
        keepLength = keep_length,
        add.path = if(nzchar(input$add_path)) input$add_path else NULL,
        cores = input$cores
      )
      state$step <- "auto_progress"

    })

  })

}
