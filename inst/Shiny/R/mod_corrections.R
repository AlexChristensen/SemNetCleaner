# Step 4: embeds the already-built, already-tested automated_edit_ui()/
# automated_edit_server() (Phase 9) as a namespaced sub-module (Phase 11's
# own id = NULL refactor) for spreadsheet-style review of the automated
# spell-check corrections, then re-derives the final cleaned/binary/
# behavioral output from the real edits via finalize_textcleaner() -- the
# exact same tested logic a normal console run uses, not a reimplementation.

mod_corrections_ui <- function(id)
{

  ns <- shiny::NS(id)

  shiny::tagList(
    automated_edit_ui(ns("review"))
  )

}

mod_corrections_server <- function(id, state)
{

  shiny::moduleServer(id, function(input, output, session){

    # shiny::moduleServer() (inside automated_edit_server(id = "review"))
    # can only ever be called ONCE for a given id in a session's lifetime --
    # guarded so a stray re-fire of this observer can't attempt a second
    # mount (state$step only ever moves forward to "corrections" once in
    # the intended flow, but this guard makes that a hard guarantee, not an
    # assumption)
    mounted <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(state$step, {

      if(identical(state$step, "corrections") && !mounted()){

        mounted(TRUE)

        # prepare_automated_for_review() (not the raw
        # state$result$spellcheck$automated) -- the padded, "to_#"-complete
        # shape correct_changes() itself expects back from edit_fn. Real bug
        # found via live verification: rendering the raw, un-padded matrix
        # here produced a NARROWER table than finalize_textcleaner()'s own
        # second call (below) hands to correct_changes() internally, which
        # errors with "non-conformable arrays" comparing the two.
        automated_edit_server(
          as.data.frame(prepare_automated_for_review(state$result), stringsAsFactors = FALSE),
          on_finish = function(edited){

            type <- state$options$type

            data_baseline <- if(type == "fluency"){
              state$result$responses$original
            }else{
              state$result$data$original
            }

            ids <- if(type == "fluency"){
              row.names(data_baseline)
            }else{
              unique(data_baseline[, "ID"])
            }

            final <- finalize_textcleaner(
              state$result, data_baseline, type, ids,
              reader = function(prompt = "") "",
              edit_fn = function(x) edited
            )

            state$result <- final
            state$step <- "export"

          },
          id = "review"
        )

      }

    })

  })

}
