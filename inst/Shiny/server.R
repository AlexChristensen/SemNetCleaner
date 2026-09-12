# Top-level server: one reactiveValues() per session (`state`) replaces the
# console wizard's own save/resume `continue` object as the thing every
# module reads/writes -- nothing here is shared across sessions.

server <- function(input, output, session)
{

  state <- shiny::reactiveValues(
    data = NULL,       # uploaded data (data.frame)
    options = NULL,    # named list of textcleaner() options from mod_upload
    continue = NULL,   # current textcleaner()/spellcheck_dictionary() return value
    result = NULL,     # the finished (or re-finalized) textcleaner() result
    step = "upload"    # "upload" | "auto_progress" | "wizard" | "corrections" | "export"
  )

  search_store <- shiny::reactiveVal(NULL)

  tab_for_step <- c(
    upload = "Upload & Options", auto_progress = "Automated Spell-Check",
    wizard = "Manual Review", corrections = "Corrections Review", export = "Export"
  )

  for(tab in tab_for_step[-1]){
    shiny::hideTab(inputId = "tabs", target = tab)
  }

  shiny::observeEvent(state$step, {

    target <- tab_for_step[[state$step]]
    shiny::req(target)

    shiny::showTab(inputId = "tabs", target = target)
    shiny::updateTabsetPanel(session, "tabs", selected = target)

  })

  mod_upload_server("upload", state)
  mod_auto_progress_server("auto", state)
  mod_wizard_server("wizard", state, search_store)
  mod_corrections_server("corrections", state)
  mod_export_server("export", state)
  mod_search_modal_server("search_modal", search_store)

}
