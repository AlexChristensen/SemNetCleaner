library(shiny)
library(editData)
library(miniUI)

ui <- fluidPage(
  gadgetTitleBar("Preprocessing Check"),
  editableDTUI("table1"),
)
server <- function(input, output) {
  
  # Get 'automated' variable
  automated = reactive({
    #as.data.frame(get("automated", envir = environment()))
    DIR <- tempdir()
    PATH <- paste(DIR, "automated.csv", sep = "\\")
    read.data(file = PATH)
  })
  
  # Call GUI
  df = callModule(editableDT, "table1", data = automated)
  
  # Check for 'done' button press
  observeEvent(input$done, {
    stopApp(df())
  })
  
  # Check for 'cancel' button press
  observeEvent(input$cancel, {
    stopApp()
  })
  
}
shinyApp(ui, server)