library(shiny)
library(editData)

ui <- fluidPage(
  h2("Preprocessing Check"),
  editableDTUI("table1"),
)
server <- function(input, output) {
  
  # Get 'automated' variable
  automated = reactive({
    as.data.frame(get("automated", envir = globalenv()))
  })
  
  # Call GUI
  df = callModule(editableDT, "table1", data = automated)
  
  observeEvent(input$done, {
    result=df()
    stopApp(invisible(result))
  })
  
  observeEvent(input$cancel, {
    result=df()
    stopApp(invisible(result))
  })

}
shinyApp(ui, server)