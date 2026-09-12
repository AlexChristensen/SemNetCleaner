# Top-level UI: one navbarPage tab per step. Tabs beyond "Upload" start
# hidden and are revealed by server.R as `state$step` advances.

ui <- shiny::navbarPage(

  title = "textcleaner",
  id = "tabs",

  shiny::tabPanel("Upload & Options", mod_upload_ui("upload")),
  shiny::tabPanel("Automated Spell-Check", mod_auto_progress_ui("auto")),
  shiny::tabPanel("Manual Review", mod_wizard_ui("wizard")),
  shiny::tabPanel("Corrections Review", mod_corrections_ui("corrections")),
  shiny::tabPanel("Export", mod_export_ui("export"))

)
