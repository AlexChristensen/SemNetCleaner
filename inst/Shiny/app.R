suppressMessages(library(shiny))
suppressMessages(library(DT))

dt_output = function(title, id) {
  fluidRow(column(
    12, h1(title)),
    hr(), DTOutput(id), align = "center"
  )
}

render_dt = function(data, editable = "cell", server = TRUE, ...) {
  renderDT(data, selection = "none", server = server, editable = editable, ...)
}

ui = fluidPage(
  title = "Edit Data",
  
  br(),
  fluidRow(
    column(
      12,
      actionButton(
        inputId = "done", "Done"
      ), align = "right"
    )
  ),
  
  #actionButton(
  #  inputId = "addColumn", "Add Column"
  #),
  
  dt_output("Double-click to edit table cells", id = "x"),
  
  #actionButton(
  #  inputId = "addColumn2", "Add Column"
  #),
  
  fluidRow(
    column(
      12,
      actionButton(
        inputId = "done2", "Done"
      ), align = "right"
    )
  ),
  br(), br()
)

server <- function(input, output, session) {
  
  # Get 'automated' data
  data = reactive({
    DIR <- tempdir() # path to temporary directory
    PATH <- paste(DIR, "automated.csv", sep = "\\") # path to "automated.csv"
    read.data(file = PATH) # read in data
  })
  
  # Set up reactive value
  reactiveData = reactiveVal()
  
  # Observe data
  observeEvent(data(),{
    reactiveData(data())
  })
  
  # Setup table
  output$x = render_dt({
      reactiveData()
  }, list(
    target = "cell",
    disable = list(columns = 1)
  ))
  
  # Edit a single cell
  proxy = dataTableProxy("x")
  observeEvent(input$x_cell_edit, {
    info = input$x_cell_edit
    newData <- reactiveData()
    newData[info$row, info$col] <- suppressWarnings(
      coerceValue(info$value, newData[info$row, info$col])
    )
    reactiveData(newData)
    replaceData(proxy, reactiveData(), resetPaging = FALSE)
  })
  
  # Add a column (top button)
  # observeEvent(input$addColumn,{
  #   newData <- reactiveData()
  #   newData[[paste("to", ncol(newData), sep = "_")]] <- vector("character", length = nrow(newData))
  #   reactiveData(newData)
  #   replaceData(proxy, reactiveData(), resetPaging = FALSE)
  #   output$x = render_dt({
  #     reactiveData()
  #   }, list(
  #     target = "cell",
  #     disable = list(columns = 1),
  #     scrollX = TRUE
  #   ))
  # })
  
  # Add a column (bottom button)
  # observeEvent(input$addColumn2,{
  #   newData <- reactiveData()
  #   newData[[paste("to", ncol(newData), sep = "_")]] <- vector("character", length = nrow(newData))
  #   reactiveData(newData)
  #   replaceData(proxy, reactiveData(), resetPaging = FALSE)
  #   output$x = render_dt({
  #     reactiveData()
  #   }, list(
  #     target = "cell",
  #     disable = list(columns = 1)
  #   ))
  # })
  
  # Check for 'finish' button press (top button)
  observeEvent(input$done, {
    stopApp(reactiveData())
  })
  
  # Check for 'finish' button press (bottom button)
  observeEvent(input$done2, {
    stopApp(reactiveData())
  })
  
}

shinyApp(ui, server)