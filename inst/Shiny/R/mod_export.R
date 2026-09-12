# Step 5: download the finished textcleaner() result and its real,
# populated fields. $removed/$partChanges are NOT offered here -- confirmed
# via direct grep (see SHINY_PLAN.md) that neither is ever actually
# populated anywhere in textcleaner()'s own code, despite being documented
# in its @return -- built from state$result's real fields instead.
#
# "Save to R Environment" (per direct request) assigns the full result
# directly into .GlobalEnv, for the common case where textcleaner_shiny()
# is launched from the same interactive R/RStudio session the user will
# keep working in -- skips the download-then-readRDS() round trip. Only
# meaningful when the app runs in the SAME R process as the console (the
# ordinary local/RStudio way of launching it); on a remote/deployed Shiny
# session there is no shared console to assign into, so this is presented
# as an addition to the existing downloads, never a replacement for them.
# This is also the hand-off mechanism `SemNeTShiny()` (a sibling package)
# looks for -- it scans .GlobalEnv on startup for any `textcleaner`-classed
# object and offers to load it directly, instead of only a file upload.

mod_export_ui <- function(id)
{

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("summary")),
    shiny::downloadButton(ns("dl_full"), "Full result (.rds)"),
    shiny::downloadButton(ns("dl_clean"), "Clean responses (.csv)"),
    shiny::downloadButton(ns("dl_binary"), "Binary/frequency matrix (.csv)"),
    shiny::downloadButton(ns("dl_behavioral"), "Behavioral summary (.csv)"),
    shiny::downloadButton(ns("dl_correspondence"), "Correspondence table (.csv)"),
    shiny::tags$hr(),
    shiny::textInput(ns("env_name"), "Object name", value = "textcleaner_result"),
    shiny::actionButton(ns("save_to_env"), "Save to R Environment", class = "btn-primary"),
    shiny::uiOutput(ns("save_status"))
  )

}

mod_export_server <- function(id, state)
{

  shiny::moduleServer(id, function(input, output, session){

    output$summary <- shiny::renderUI({
      shiny::req(state$result)
      shiny::tags$p("Preprocessing complete. Download your results below.")
    })

    shiny::observeEvent(input$save_to_env, {

      shiny::req(state$result)

      name <- trimws(input$env_name)

      if(!nzchar(name) || !grepl("^[a-zA-Z.][a-zA-Z0-9._]*$", name)){
        output$save_status <- shiny::renderUI(
          shiny::tags$p(style = "color: #a94442;", "Not a valid R object name.")
        )
        return(invisible(NULL))
      }

      assign(name, state$result, envir = .GlobalEnv)

      output$save_status <- shiny::renderUI(
        shiny::tags$p(style = "color: #3c763d;", paste0(
          "Saved to your R session as `", name, "` (only works when this app is ",
          "running in the same R process as your console)."
        ))
      )

    })

    output$dl_full <- shiny::downloadHandler(
      filename = function() "textcleaner_result.rds",
      content = function(file){ saveRDS(state$result, file) }
    )

    output$dl_clean <- shiny::downloadHandler(
      filename = function() "clean_responses.csv",
      content = function(file){
        utils::write.csv(state$result$responses$clean, file, row.names = TRUE)
      }
    )

    output$dl_binary <- shiny::downloadHandler(
      filename = function() "binary_matrix.csv",
      content = function(file){
        shiny::req(state$result$responses$binary)
        utils::write.csv(state$result$responses$binary, file, row.names = TRUE)
      }
    )

    output$dl_behavioral <- shiny::downloadHandler(
      filename = function() "behavioral_summary.csv",
      content = function(file){
        utils::write.csv(state$result$behavioral, file, row.names = TRUE)
      }
    )

    output$dl_correspondence <- shiny::downloadHandler(
      filename = function() "correspondence.csv",
      content = function(file){
        utils::write.csv(state$result$spellcheck$correspondence, file, row.names = TRUE)
      }
    )

  })

}
