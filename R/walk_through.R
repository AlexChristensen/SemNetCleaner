#' @noRd
# Walk through ----
# First-time-user walkthrough of the manual spell-check menu, shown before the manual
# spell-check starts; `reader` makes the prompts scriptable
# `help_opened`: the help window was just opened (see spellcheck_dictionary())
# Updated 14.10.2026
walk_through <- function(
    walkthrough, # TRUE, FALSE, or NULL to ask
    help_opened = FALSE, # whether the help window was opened
    reader = readline # function used to read answers
)
{

  # Do walkthrough?
  do_it <- FALSE

  # Check if NULL
  if(is.null(walkthrough)){

    # Ask user
    custom_menu(
      choices = c("Yes", "No"),
      title = c(paste(
        "\nBefore starting the manual spell-check, would you like to do a walkthrough?",
        color_text("\n(Recommended for first time users)", defaults = "message")
      )),
      default = 2, dictionary = TRUE
    )

    # Get response
    ans <- read_input("Selection: ", reader = reader)

    # Make sure answer is appropriate
    ans <- appropriate_answer(ans, choices = 1:2, default = 2, dictionary = TRUE, reader = reader)

    # Check for walkthrough request
    if(ans == 1){

      # Do the walkthrough
      do_it <- TRUE

    }else{ # Skip the walkthrough

      # Let user know
      message("\nWalkthrough skipped.")

    }

  }else if(walkthrough){ # Walkthrough requested

    do_it <- TRUE

  }

  # If do the walkthrough is TRUE, then do it
  if(do_it){

    # Introduction to walkthrough
    cat(color_text("\nWelcome to the manual spell-check walkthrough for `textcleaner`!", defaults = "message"))

    # Explain the purpose of the manual spell-check
    cat(color_text("\n\nThe manual spell-check process is designed to allow you to", defaults = "message"))
    cat(color_text("\nhave maximum control over the responses that need spell-checking.", defaults = "message"))
    cat(color_text("\nThis walkthrough will guide you through each response option", defaults = "message"))
    cat(color_text("\nin the manual spell-check menu by using examples of responses", defaults = "message"))
    cat(color_text("\nyou might encounter.", defaults = "message"))

    # Explain the types of responses
    cat(color_text("\n\nThere are generally two types of responses you might encounter:", defaults = "message"))
    cat(color_text("\na single word or multiple word response. This latter type is a", defaults = "message"))
    cat(color_text("\nbit tricky because the response could be a single response or", defaults = "message"))
    cat(color_text("\nit could be multiple responses entered as though it were a", defaults = "message"))
    cat(color_text("\nsingle response.", defaults = "message"))

    # Explain why intervention is necessary
    cat(color_text("\n\nBecause `textcleaner` spell-checks each word individually, your", defaults = "message"))
    cat(color_text("\nintervention is necessary to determine how to properly split", defaults = "message"))
    cat(color_text("\nthese responses. By demonstrating how to correct this type of", defaults = "message"))
    cat(color_text("\nresponse, you'll be prepared to correct single responses as well.", defaults = "message"))

    # Explain the example
    cat(color_text("\n\nThis is because these multiple word responses can be checked", defaults = "message"))
    cat(color_text("\nindividually or across all words in the response. Therefore, all", defaults = "message"))
    cat(color_text("\nexplanation for how to use the manual spell-check options of", defaults = "message"))
    cat(color_text("\n`textcleaner` are discussed in this single example.", defaults = "message"))

    # Explain where the walkthrough begins
    cat(color_text("\n\nThese multiple word responses are where the `textcleaner`", defaults = "message"))
    cat(color_text("\nfunction starts and our walkthrough begins.\n\n", defaults = "message"))

    # Wait for user
    read_input("Press ENTER to continue...", reader = reader)

    # Introduction to multiple word response
    cat(color_text("\nThe multiple word response we will use for our example was given", defaults = "message"))
    cat(color_text("\nby an actual participant:", defaults = "message"))

    # Print original response
    cat("\n\n'turtle catdog elephant fish bird squiral rabbit fox deer monkey giraff'\n")

    # Explain automated spell-check
    cat(color_text("\nWhen this multiple word response was passed through the automated", defaults = "message"))
    cat(color_text("\nspell-check, it was changed to:", defaults = "message"))

    # Print automated response
    cat("\n\n'turtle' 'catdog' 'elephant' 'fish' 'bird' 'squirrel' 'rabbit' 'fox' 'deer' 'monkey' 'giraffe'\n")

    # Explain automated changes
    cat(color_text("\nThe automated spell-check handled a few errors in the original", defaults = "message"))
    cat(color_text("\nresponse. First, the words in the response were separated into", defaults = "message"))
    cat(color_text("\nindividual responses. Second, the responses 'squiral' and 'giraff'", defaults = "message"))
    cat(color_text("\nwere automatically corrected to 'squirrel' and 'giraffe',", defaults = "message"))
    cat(color_text("\nrespectively. The word 'catdog', however, was not successfully", defaults = "message"))
    cat(color_text("\nseparated and is passed on to you to be manually spell-checked.\n\n", defaults = "message"))

    # Wait for user
    read_input("Press ENTER to continue...", reader = reader)

    # Introduce the menu
    cat(color_text("\nBelow is the interactive menu that will appear when manually spell-checking responses:\n", defaults = "message"))

    # Pause to read
    pause(5)

    #-------------------------#
    ## Begin example of menu ##
    #-------------------------#

    # Set original response
    original <- paste("turtle catdog elephant fish bird squiral rabbit fox deer monkey giraff")

    # Set auto-corrected words
    context <- paste("turtle", "catdog", "elephant", "fish", "bird",  "squirrel", "rabbit", "fox", "deer", "monkey", "giraffe")

    # Split into words
    context <- unlist(strsplit(context, split = " "))

    # Set target word
    check <- 2

    # Build menu title
    title <- paste(
      paste("\nOriginal string: ", "'", original, "'", sep = ""),
      paste("Auto-corrected string: ", paste("'", context, "'", sep = "", collapse = " "), sep = ""),
      paste("Target word: ", paste("'", color_text(context[check], defaults = "highlight"), "'", sep = ""), sep = ""),
      sep = "\n\n"
    )

    # Set word options
    word <- c("SKIP WORD", "ADD WORD TO DICTIONARY", "TYPE MY OWN WORD", "GOOGLE WORD", "BAD WORD")

    # Set string options
    string <- c("KEEP ORIGINAL", "KEEP AUTO-CORRECT", "TYPE MY OWN STRING", "GOOGLE STRING", "BAD STRING")

    # Get response suggestions
    resp <- best_guess(word = "catdog", full_dictionary = SemNetDictionaries::animals.dictionary, dictionary = "animals")

    # Combine menu options
    choices <- c(word, string, resp)

    # Print example menu
    custom_menu(choices = choices, title = title, default = 10)

    # Print navigation keys
    message("Press 'B' to GO BACK, 'H' for HELP, or 'X' to EXIT.\n")

    # Print selection prompt
    cat("Selection (accepts lowercase):\n\n")

    #-----------------------#
    ## End example of menu ##
    #-----------------------#

    # Wait for user
    read_input("Press ENTER to continue...\n(make sure to expand R's Console vertically to see the full menu)", reader = reader)

    # First three lines
    cat(paste(
      color_text("\nAs you can see, the interactive menu contains", defaults = "message"),
      color_text(style_text("a lot", "italics"), defaults = "message"),
      color_text("of information.", defaults = "message")
    ))

    # Explain the first three lines
    cat(color_text("\nThe purpose of this walkthrough is to get familiar with this menu", defaults = "message"))
    cat(color_text("\nby breaking it down into digestible pieces. We'll start with the", defaults = "message"))
    cat(color_text("\nfirst three lines:\n", defaults = "message"))

    # Print title
    cat(title)

    # Print line break
    line_break()

    # Explain original string
    cat(paste("\n", "Original string:\n",
              color_text(paste(" ", text_symbol("bullet"), " Refers to the original string that the participant typed", sep = ""), defaults = "message"),
              "\n",
              sep = "")
    )

    # Explain auto-corrected string
    cat(paste("\n", "Auto-corrected string:\n",
              color_text(paste(" ", text_symbol("bullet"), " Refers to the string that the automated spell-check corrected to", sep = ""), defaults = "message"),
              "\n",
              sep = "")
    )

    # Explain target word
    cat(paste("\n", "Target word: '", color_text("catdog", defaults = "highlight"), "'\n",
              paste(
                color_text(paste(" ", text_symbol("bullet"), " Refers to the ", sep = ""), defaults = "message"),
                style_text(color_text("target", defaults = "message"), defaults = "italics"),
                color_text(paste(" word to be manually spell-checked", sep = ""), defaults = "message"),
                sep = ""
              ),
              "\n\n",
              sep = "")
    )

    # Wait for user
    read_input("Press ENTER to continue...", reader = reader)

    # Explain the context lines
    cat(color_text("\nThese first three lines provide you with the full context of the", defaults = "message"))
    cat(color_text("\nparticipant's response to help you make a more informed decision.", defaults = "message"))
    cat(paste(color_text("\nThe ", defaults = "message"), "Auto-corrected string:",
              color_text(" is the current state of the response.", defaults = "message"),
              sep = "")
    )

    # Explain the current state
    cat(color_text("\nThat is, if you do nothing to manually spell-check the response,", defaults = "message"))
    cat(color_text("\nthen these are the responses that will be retained. In our example,", defaults = "message"))
    cat(paste(color_text("\nwe want to change the target word: ", defaults = "message"),
              "'", color_text("catdog", defaults = "highlight"), "'",
              color_text(".", defaults = "message"),
              sep = "")
    )

    # Introduce the options
    cat(color_text("\n\nNext, we'll move on to the possible options we have to", defaults = "message"))
    cat(color_text("\ncorrect this response.\n\n", defaults = "message"))

    # Wait for user
    read_input("Press ENTER to continue...", reader = reader)

    # Word options
    cat(color_text("\nThe first set of options (i.e., ", defaults = "message"),
        style_text("Word options", defaults = "underline"),
        color_text(") we have are", defaults = "message"),
        sep = ""
    )

    # Explain word options
    cat(color_text("\nto correct the target word:", defaults = "message"),
        paste("'", color_text("catdog", defaults = "highlight"), "'",
              color_text(". These options allow", defaults = "message"),
              sep = "")
    )

    cat(color_text("\nyou to make a decision about how to handle this single", defaults = "message"))
    cat(color_text("\nword in the response -- that is, these options will", defaults = "message"),
        color_text(style_text("\nonly", "italics"), defaults = "message"),
        color_text("affect the target word. There are five options:", defaults = "message")
    )

    # Set numbered word options
    word_set <- paste(1:5, ": ", word, sep = "")

    # Print word options
    cat(paste(style_text("\n\nWord options\n", defaults = "underline"),
              paste0(word_set, c(rep.int("  ", min(20, 5) - 1L)), sep = "", collapse = "")))

    # Print line break
    line_break()

    # Explain SKIP WORD
    cat(paste("\n", "1: SKIP WORD\n",
              color_text(paste(" ", text_symbol("bullet"), ' Keeps word "as is" and moves on to next word to be spell-checked', sep = ""), defaults = "message"),
              "\n",
              sep = "")
    )

    # Explain ADD WORD TO DICTIONARY
    cat(paste("\n", "2: ADD WORD TO DICTIONARY\n",
              color_text(paste(" ", text_symbol("bullet"), " Adds word to dictionary for future spell-checking", sep = ""), defaults = "message"),
              "\n",
              sep = "")
    )

    # Explain TYPE MY OWN WORD
    cat(paste("\n", "3: TYPE MY OWN WORD\n",
              color_text(paste(" ", text_symbol("bullet"), " Allows you to type your own correction for the word", sep = ""), defaults = "message"),
              "\n",
              sep = "")
    )

    # Explain GOOGLE WORD
    cat(paste("\n", "4: GOOGLE WORD\n",
              color_text(paste(" ", text_symbol("bullet"), ' Opens your default browser and "Googles" the word', sep = ""), defaults = "message"),
              "\n",
              sep = "")
    )

    # Explain BAD WORD
    cat(paste("\n", "5: BAD WORD\n",
              color_text(paste(" ", text_symbol("bullet"), " Marks the word as an inappropriate response (NA)", sep = ""), defaults = "message"),
              "\n\n",
              sep = "")
    )

    # Wait for user
    read_input("Press ENTER to continue...", reader = reader)

    # Introduce string options
    cat(color_text("\nThese options only affect the target word. But what if you want", defaults = "message"))
    cat(color_text("\nto change multiple words or the", defaults = "message"),
        color_text(style_text("entire", defaults = "italics"), defaults = "message"),
        color_text("string of responses? We", defaults = "message"))

    cat(color_text("\nmove to those options next.\n\n", defaults = "message"))

    # Wait for user
    read_input("Press ENTER to continue...", reader = reader)

    # String options
    cat(color_text("\nThe next set of options (i.e., ", defaults = "message"),
        style_text("String options", defaults = "underline"),
        color_text(") are able to", defaults = "message"),
        sep = "")

    # Explain string options
    cat(color_text("\ncorrect the entire string of responses. These options will", defaults = "message"))
    cat(color_text("\naffect the entire string rather than just the target word.", defaults = "message"))
    cat(color_text("\nThere are also five options:", defaults = "message"))

    # Set numbered string options
    string_set <- paste(6:10, ": ", string, sep = "")

    # Print string options
    cat(paste(style_text("\n\nString options\n", defaults = "underline"),
              paste0(string_set, c(rep.int("  ", min(20, 5) - 1L)), sep = "", collapse = "")))

    # Print line break
    line_break()

    # Explain KEEP ORIGINAL
    cat(paste("\n", "6: KEEP ORIGINAL\n",
              paste(color_text(paste(" ", text_symbol("bullet"), " Reverts the string back to the", sep = ""), defaults = "message"),
                    "Original string:",
                    color_text("the participant provided.", defaults = "message")),
              "\n", sep = ""
    )
    )

    # Explain KEEP AUTO-CORRECT
    cat(paste("\n", "7: KEEP AUTO-CORRECT\n",
              paste(color_text(paste(" ", text_symbol("bullet"), " Keeps the string 'as is' with the", sep = ""), defaults = "message"),
                    "Auto-corrected string:",
                    color_text("provided by the automated spell-check.", defaults = "message")),
              "\n", sep = ""
    )
    )

    # Explain TYPE MY OWN STRING
    cat(paste("\n", "8: TYPE MY OWN STRING\n",
              paste(color_text(paste(" ", text_symbol("bullet"), " Allows you to type your own correction for the", sep = ""), defaults = "message"),
                    "Original string:",
                    color_text("the participant provided.", defaults = "message")),
              "\n", sep = ""
    )
    )

    # Explain GOOGLE STRING
    cat(paste("\n", "9: GOOGLE STRING\n",
              paste(color_text(paste(" ", text_symbol("bullet"), " Opens your default browser and 'Googles' the", sep = ""), defaults = "message"),
                    "Original string:",
                    color_text("the participant provided.", defaults = "message")),
              "\n", sep = ""
    )
    )

    # Explain BAD STRING
    cat(paste("\n", "10: BAD STRING\n",
              color_text(paste(" ", text_symbol("bullet"), " Marks the entire string as an inappropriate response (NA)", sep = ""), defaults = "message"),
              "\n\n",
              sep = ""
    )
    )

    # Wait for user
    read_input("Press ENTER to continue...", reader = reader)

    # Explain when to use string options
    cat(color_text("\nThese options only affect the entire string. They allow you to", defaults = "message"))
    cat(color_text("\nlook beyond the target word and to make a decision about the", defaults = "message"))
    cat(color_text("\nwhole string. These options are most useful when considering", defaults = "message"))
    cat(color_text("\na multiple word response as a single response (rather than", defaults = "message"))
    cat(color_text("\nmultiple individual responses).\n\n", defaults = "message"))

    # Wait for user
    read_input("Press ENTER to continue...", reader = reader)

    # Response options
    cat(color_text("\nThe final set of options (i.e., ", defaults = "message"),
        style_text("Response options", defaults = "underline"),
        color_text(") only affect the", defaults = "message"),
        sep = "")

    # Explain response options
    cat(color_text("\ntarget word. These options are `textcleaner`'s best guess for", defaults = "message"))
    cat(color_text("\nwhat the response might be. These response options offer quick", defaults = "message"))
    cat(color_text("\ncorrections or potential directions for what the user meant to", defaults = "message"))
    cat(color_text("\nBelow the", defaults = "message"),
        style_text("Response options", defaults = "underline"),
        color_text("are two convenience options and an ", defaults = "message")
    )

    cat(color_text("\ninput for your selection.", defaults = "message"))

    # Set lettered response options
    resp_set <- paste(c("Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P"), ": ", resp, sep = "")

    # Print response options
    cat(paste(style_text("\n\nResponse options\n", defaults = "underline"),
              paste0(resp_set, c(rep.int("  ", min(20, 5) - 1L)), sep = "", collapse = "")))

    # Print navigation keys
    cat(color_text("\n\nPress 'B' to GO BACK, 'H' for HELP, or 'X' to EXIT.\n", defaults = "message"))

    # Print selection prompt
    cat("\nSelection (accepts lowercase): ")

    # Print line break
    line_break()

    # Explain response options
    cat(paste(style_text("\nResponse options\n", defaults = "underline"),
              paste(color_text(paste(" ", text_symbol("bullet"), " Potential options based on `textcleaner`'s best guess (letters correspond to the response)", sep = ""), defaults = "message")),
              "\n", sep = ""
    )
    )

    # Explain GO BACK
    cat(paste("\n'B'\n",
              paste(color_text(paste(" ", text_symbol("bullet"), " Takes you back one decision (use if you make a mistake). From the first decision of a response it returns to the PREVIOUS response and asks about it again from its first word", sep = ""), defaults = "message")),
              "\n", sep = ""
    )
    )

    # Explain HELP
    cat(paste("\n'H'\n",
              paste(color_text(paste(" ", text_symbol("bullet"), " Shows the help: a legend of every option (in a separate window you can keep beside the console, when R is interactive)", sep = ""), defaults = "message")),
              "\n", sep = ""
    )
    )

    # Explain EXIT
    cat(paste("\n'X'\n",
              paste(color_text(paste(" ", text_symbol("bullet"), " Exits textcleaner completely but saves your output", sep = ""), defaults = "message")),
              "\n", sep = ""
    )
    )

    # Explain selection
    cat(paste("\nSelection (accepts lowercase): \n",
              paste(color_text(paste(" ", text_symbol("bullet"), " Where you input the option you select", sep = ""), defaults = "message")),
              "\n\n", sep = ""
    )
    )

    # Wait for user
    read_input("Press ENTER to continue...", reader = reader)

    # Summarize the options
    cat(color_text("\nThese make up all the options you'll use in `textcleaner`. They", defaults = "message"))
    cat(color_text("\nenable total control over correcting the response(s). When there", defaults = "message"))
    cat(color_text("\nis only a single word response, then the 'String options' will", defaults = "message"))
    cat(color_text("\nnot be available (because they are no longer necessary).", defaults = "message"))

    # Explain the keepStrings menu
    cat(color_text("\n\nThe example above is the menu for `keepStrings = FALSE`. With", defaults = "message"))
    cat(color_text("\n`keepStrings = TRUE` there is no KEEP AUTO-CORRECT option, so the", defaults = "message"))
    cat(color_text("\nstring options after KEEP ORIGINAL are numbered one lower. The", defaults = "message"))
    cat(color_text("\nmenu always shows the number to type.", defaults = "message"))

    # Conclude the walkthrough
    cat(color_text("\n\nThis concludes the walkthrough of the manual spell-check process", defaults = "message"))
    cat(color_text("\nin `textcleaner`. You're now prepared to work through your data.\n", defaults = "message"))

  }

  # Check whether the help window is open
  if(help_opened){

    # Tell user about the help window
    cat(color_text("\nThe help is open in a separate window. Keep it beside the console while", defaults = "message"))
    cat(color_text("\nyou work, or close it. Press 'H' at any decision to open it again.\n", defaults = "message"))

  }

  # Print line break
  cat("\n")

  # Wait for user
  read_input("Press ENTER to start manual spell-check...", reader = reader)

}
