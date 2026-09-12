# Step 3: the manual spell-check wizard -- the highest-risk, most novel
# module. Every button click resumes the REAL textcleaner()/
# spellcheck_dictionary() wizard by exactly one decision, via
# advance_wizard() + build_answer_burst() (both package-level, tested
# directly in test-advance_wizard.R/test-build_answer_burst.R/
# test-wizard_menu_labels.R) -- this module is UI glue only; it never
# reimplements spellcheck_menu()'s own decision logic.

mod_wizard_ui <- function(id)
{

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("progress")),
    shiny::uiOutput(ns("decision"))
  )

}

mod_wizard_server <- function(id, state, search_store)
{

  shiny::moduleServer(id, function(input, output, session){

    ns <- session$ns

    # Which TYPE MY OWN [WORD]/TYPE MY OWN STRING button was last clicked
    # (its real, displayed menu position -- see wizard_menu_labels.R), and
    # whether that form should currently be shown. Session-local UI state
    # only; never itself sent as an answer until Submit is clicked.
    type_own_position <- shiny::reactiveVal(NULL)

    advance <- function(answers){

      result <- suppressMessages(advance_wizard(
        state$continue, answers,
        search_fn = make_shiny_search_fn(search_store), cores = state$options$cores
      ))

      type_own_position(NULL)
      state$continue <- result

      if(!isTRUE(result$stop)){
        state$result <- result
        state$step <- if(length(result$spellcheck$automated) != 0) "corrections" else "export"
      }

    }

    output$progress <- shiny::renderUI({

      shiny::req(state$continue, isTRUE(state$continue$stop))

      total <- length(state$continue$ind)
      done <- state$continue$main_count - 1

      shiny::tags$p(paste0("Reviewing response ", min(done + 1, total), " of ", total, "."))

    })

    output$decision <- shiny::renderUI({

      shiny::req(state$continue, isTRUE(state$continue$stop))

      pending <- state$continue$pending
      shiny::req(pending)

      labels <- wizard_menu_labels(pending$context, pending$keep_strings)
      response_letters <- wizard_response_letters(length(pending$possible))

      display_string <- if(is.null(pending$context)){
        pending$original
      }else{
        paste(pending$context, collapse = " ")
      }

      target_display <- if(is.null(pending$context)) pending$check else pending$context[pending$check]

      type_own_label <- if(is.null(pending$context)) "TYPE MY OWN" else "TYPE MY OWN WORD"
      type_own_string_label <- if(!is.null(pending$context)) "TYPE MY OWN STRING" else NULL

      plain_labels <- setdiff(labels, c(type_own_label, type_own_string_label))

      shiny::tagList(

        shiny::tags$p(shiny::tags$strong("Original: "), pending$original),
        shiny::tags$p(shiny::tags$strong("Current: "), display_string),
        shiny::tags$p(shiny::tags$strong("Target word: "),
                      shiny::tags$span(style = "color: #d9534f;", target_display)),

        shiny::tags$div(
          lapply(plain_labels, function(label){
            shiny::actionButton(ns(paste0("opt_", make.names(label))), label)
          })
        ),

        shiny::tags$div(
          shiny::actionButton(ns("opt_type_own"), type_own_label),
          if(!is.null(type_own_string_label)){
            shiny::actionButton(ns("opt_type_own_string"), type_own_string_label)
          }
        ),

        if(!is.null(type_own_position())){
          shiny::tagList(
            shiny::textInput(ns("typed_text"), "Type replacement(s), comma-separated"),
            shiny::checkboxInput(ns("add_to_dictionary"), "Add new word(s) to the dictionary", value = FALSE),
            shiny::actionButton(ns("submit_typed"), "Submit", class = "btn-primary")
          )
        },

        if(length(pending$possible) > 0){
          shiny::tagList(
            shiny::tags$p(shiny::tags$strong("Suggestions:")),
            shiny::tags$div(
              lapply(seq_along(pending$possible), function(i){
                shiny::actionButton(
                  ns(paste0("suggestion_", response_letters[i])),
                  paste0(response_letters[i], ": ", pending$possible[i])
                )
              })
            )
          )
        },

        shiny::tags$div(
          shiny::actionButton(ns("go_back"), "GO BACK"),
          shiny::actionButton(ns("help"), "HELP")
        )

      )

    })

    # One observer per plain (non-TYPE-MY-OWN) meta-option label -- every
    # real spellcheck_menu() layout uses a subset of these same 9 labels
    # (10 in the single-response case, "SKIP" through "BAD WORD"), each
    # sending its own real, currently-displayed menu position (see
    # wizard_menu_labels.R's own header for why the raw displayed position,
    # not a hardcoded number, is what's safe to send here)
    all_plain_labels <- c(
      "SKIP", "ADD TO DICTIONARY", "TYPE MY OWN", "GOOGLE IT", "BAD WORD",
      "SKIP WORD", "ADD WORD TO DICTIONARY", "TYPE MY OWN WORD", "GOOGLE WORD",
      "KEEP ORIGINAL", "KEEP AUTO-CORRECT", "GOOGLE STRING", "BAD STRING"
    )

    lapply(all_plain_labels, function(label){
      shiny::observeEvent(input[[paste0("opt_", make.names(label))]], {

        pending <- state$continue$pending
        shiny::req(pending)

        labels <- wizard_menu_labels(pending$context, pending$keep_strings)
        position <- which(labels == label)
        shiny::req(length(position) == 1)

        # KEEP ORIGINAL has its own independent dictionary-membership check
        # inside spellcheck_menu() (the whole, un-split original response
        # text is essentially never itself a dictionary entry), which can
        # trigger one more yes/no prompt -- see build_answer_burst.R's own
        # header for the real hang this closes. Every other meta-option
        # never reaches that check, so `original_check` is only ever passed
        # here.
        original_check <- if(label == "KEEP ORIGINAL") pending$original else NULL

        advance(build_answer_burst(
          position, original_check = original_check,
          full_dictionary = state$continue$full_dictionary
        ))

      }, ignoreInit = TRUE)
    })

    shiny::observeEvent(input$opt_type_own, {
      pending <- state$continue$pending
      shiny::req(pending)
      labels <- wizard_menu_labels(pending$context, pending$keep_strings)
      target <- if(is.null(pending$context)) "TYPE MY OWN" else "TYPE MY OWN WORD"
      type_own_position(which(labels == target))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$opt_type_own_string, {
      pending <- state$continue$pending
      shiny::req(pending)
      labels <- wizard_menu_labels(pending$context, pending$keep_strings)
      type_own_position(which(labels == "TYPE MY OWN STRING"))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$submit_typed, {

      shiny::req(type_own_position(), nzchar(input$typed_text))

      advance(build_answer_burst(
        type_own_position(), typed_text = input$typed_text,
        add_to_dictionary = isTRUE(input$add_to_dictionary),
        full_dictionary = state$continue$full_dictionary
      ))

    })

    shiny::observeEvent(input$go_back, {
      advance(build_answer_burst("b"))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$help, {
      shiny::showModal(shiny::modalDialog(
        title = "Help",
        "Choose an option to correct this response, click a suggestion to accept it, or GO BACK to revert your previous decision.",
        easyClose = TRUE
      ))
    })

    lapply(c("Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P"), function(letter){
      shiny::observeEvent(input[[paste0("suggestion_", letter)]], {
        advance(build_answer_burst(letter))
      }, ignoreInit = TRUE)
    })

  })

}
