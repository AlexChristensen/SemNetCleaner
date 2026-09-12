# Step 2: runs the automated spell-check pass (the real, parallelizable
# long-running part of textcleaner()'s pipeline) via shiny::ExtendedTask,
# then hands off to the manual wizard (mod_wizard.R) if anything needs it,
# or straight to corrections review / export if not.
#
# The `reader = function(prompt = "") ""` used for this first call is what
# makes this step possible without reimplementing textcleaner()'s own data-
# prep + auto_spellcheck() + spellcheck_dictionary() dispatch a second time:
# an always-empty reader lets the REAL, already-tested pipeline run in full
# up through the automated pass, then pause at the very first manual
# decision (if any) via the exact same STOP mechanism a real user hitting
# Enter on an empty prompt would trigger -- see advance_wizard.R's own
# header comment for the mechanics of why this is safe.

mod_auto_progress_ui <- function(id)
{

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("status")),
    shiny::uiOutput(ns("continue_button"))
  )

}

mod_auto_progress_server <- function(id, state)
{

  shiny::moduleServer(id, function(input, output, session){

    progress_file <- shiny::reactiveVal(NULL)
    progress <- progress_poller(session, progress_file)

    task <- shiny::ExtendedTask$new(function(data, options, progress_file){
      promises::future_promise({

        options(SemNetCleaner.progress_file = progress_file)

        do.call(
          textcleaner,
          c(
            list(
              data = data, continue = NULL,
              reader = function(prompt = "") "",
              edit_fn = function(x) x
            ),
            options
          )
        )

      }, seed = TRUE)
    })

    shiny::observeEvent(state$step, {

      if(identical(state$step, "auto_progress")){
        progress_file(new_progress_file())
        task$invoke(state$data, state$options, progress_file())
      }

    })

    shiny::observe({
      if(task$status() == "success"){

        result <- task$result()

        state$continue <- result

        if(isTRUE(result$stop)){
          state$step <- "wizard"
        }else{
          state$result <- result
          state$step <- if(length(result$spellcheck$automated) != 0) "corrections" else "export"
        }

      }
    })

    output$status <- shiny::renderUI(progress_status_ui(task, progress))

  })

}
