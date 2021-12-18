# Load packages
suppressMessages(library(shiny))
suppressMessages(library(DT))

# datatable output function
dt_output = function(title, instruct, spaces, id) {
  fluidRow(column(
    12, HTML(title)),
    HTML(instruct),
    HTML(spaces),
    hr(),
    DTOutput(id), align = "center"
  )
}

# Render datatable function
render_dt = function(data, editable = "cell", server = TRUE, ...) {
  renderDT(
    data, selection = "none", server = server, editable = editable,
    options = list(
      orderFixed = c(1, "asc"),
      keys = TRUE, paging = FALSE,
      scrollX = TRUE, autoWidth = FALSE, searching = FALSE,
      columns.width = "1em"
    ), callback = JS(js),
    extensions = "KeyTable", ...
  )
}

# User interface
ui = fluidPage(
  
  # Font
  tags$style('
    #mydiv {font-family:"Lucida Console";}
  '),
  
  title = "Edit Data",
  
  # Done (top button)
  br(),
  fluidRow(
    column(
      12,
      actionButton(
        inputId = "done", "Done"
      ), align = "right"
    )
  ),
  
  # actionButton(
  #  inputId = "addColumn", "Add Column"
  # ),
  
  dt_output(
    "<h1><span id = 'mydiv'>textcleaner</span>'s Auto-correct Check</h1>",
    "<h4>Press <span id = 'mydiv'>ENTER</span> or double-click to edit responses.
    </br>Arrow keys to move between cells.",
    "<h5><em>Additional cells are provided to separate multiple responses
    (e.g., \"dog cat bird\" to \"dog\" \"cat\" \"bird\")</em></h5>",
    id = "x"
  ),
  
  #actionButton(
  #  inputId = "addColumn2", "Add Column"
  #),
  
  # Done (bottom button)
  fluidRow(
    column(
      12,
      actionButton(
        inputId = "done2", "Done"
      ), align = "right"
    )
  ),
  br(), br(),
  
  # Tooltip implementation
  # dataTableOutput("tableWithHoverData")
)

# Tab to move down
js <- c(
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
  # Tooltip implementation
  # ,"
  #                             table.on('mouseenter', 'tbody td', function() {
  #                               var column = $(this).index();
  #                               var row = $(this).parent().index();
  # 
  #                               var dataFromOtherTable = $('#tableWithHoverData').find('tbody tr').eq(row).find('td').eq(column).text();
  # 
  #                               this.setAttribute('title', dataFromOtherTable);
  #                             });
  # 
  #                             return table;
  #                             "
)

# Server
server <- function(input, output, session) {
  
  # Get 'automated' data
  data = reactive({
    DIR <- tempdir() # path to temporary directory
    PATH <- paste(DIR, "automated.csv", sep = "\\") # path to "automated.csv"
    read.data(file = PATH) # read in data
  })
  
  # Tooltip implementation
  # table2 <- data.frame(
  #   row = c(1:2),
  #   best_guesses = c("facade, aloadae, faade, aefaldy, afaced, affable, affaire, affaite, afflate, aggrade",
  #                    "something, something")
  # )
  
  # Set up reactive value
  reactiveData = reactiveVal()
  
  # Observe data
  observeEvent(data(),{
    reactiveData(data())
  })
  
  # Tooltip implementation
  # Observe hover
  # observeEvent(input$hoveredCellInfo, {
  #   info <- input$hoveredCellInfo
  #   content <- as.character(table2[info$row, 1])
  # })
  
  # Setup table
  output$x = render_dt({
      
    # Tooltip implementation
      # output$tableWithHoverData <- renderDataTable({
      #   datatable(table2, rownames = FALSE)
      # })
      
      data()
    
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
  
  # # Add a column (top button)
  # observeEvent(input$addColumn,{
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
  
  # # Add a column (bottom button)
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
  
  # Check for close out
  onStop(function(x){
    changes <<- as.matrix(isolate(reactiveData()))
    return(changes)
  })
  
}

# Run app
shinyApp(ui, server)