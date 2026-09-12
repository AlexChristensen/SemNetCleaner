# GOOGLE WORD/STRING/IT, embedded in-app. spellcheck_menu()'s real Google
# option just calls utils::browseURL() (confirmed via
# searcher:::search_google()/searcher:::browse_url()) -- a real Google
# results page also cannot be iframed at all (blocked via response
# headers). make_shiny_search_fn() (search_shiny.R) writes a DuckDuckGo HTML
# results URL into `search_store` instead of opening a browser window; this
# module renders it in a modal iframe and observes the store to know when
# to open.

mod_search_modal_ui <- function(id)
{
  NULL # the modal itself is created by shiny::showModal(), not static UI
}

mod_search_modal_server <- function(id, search_store)
{

  shiny::moduleServer(id, function(input, output, session){

    shiny::observeEvent(search_store(), {

      info <- search_store()
      shiny::req(info)

      shiny::showModal(shiny::modalDialog(
        title = paste0("Search: ", info$query),
        shiny::tags$iframe(
          src = info$url, style = "width: 100%; height: 70vh; border: none;"
        ),
        size = "l", easyClose = TRUE, footer = shiny::modalButton("Close")
      ))

    })

  })

}
