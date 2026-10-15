#' @noRd
# Manual spell-check help ----
# Prints the manual spell-check help screen: a legend of every menu option, shown
# mid-menu when the user presses H and the help window is not used
# Updated 14.10.2026
textcleaner_help <- function(
    check, # target word (or position of the word in the string)
    context, # words of the auto-corrected string or NULL
    original, # original string
    possible, # suggested corrections
    reader = readline, # function used to read answers
    keep_strings = FALSE, # whether strings are kept
    window = help_window_enabled(), # open the help in a separate window
    open_fn = utils::browseURL # function used to open the window
)
{

  # In a separate window the legend can stay beside the console while you
  # work (see help_window.R); the console legend below is the fallback
  if(isTRUE(window) && open_help_window(keep_strings, open_fn)){

    # Let user know
    message(
      "\nThe help is open in a separate window. Keep it beside the console, ",
      "and press 'H' again to open it again. (To print the help here instead: ",
      "options(SemNetCleaner.help_window = FALSE))"
    )

    # Return silently
    return(invisible(NULL))

  }

  # Print line break
  line_break(breaks = "\n\n")

  # Set help title art
  help_art <- c(" _            _       _                              _          _        ",
                "| |_ _____  _| |_ ___| | ___  __ _ _ __   ___ _ __  | |__   ___| |_ __   ",
                "| __/ _ \\ \\/ / __/ __| |/ _ \\/ _` | '_ \\ / _ \\ '__| | '_ \\ / _ \\ | '_ \\  ",
                "| ||  __/>  <| || (__| |  __/ (_| | | | |  __/ |    | | | |  __/ | |_) | ",
                " \\__\\___/_/\\_\\\\__\\___|_|\\___|\\__,_|_| |_|\\___|_|    |_| |_|\\___|_| .__/  ",
                "                                                                 |_| ")

  # Print help title
  cat(help_art, sep = "\n")

  # Print line break
  line_break(breaks = "")

  # Pause to read
  pause(2)

  # Check for multi-word menu
  if(!is.null(context)){

    # Explain original string
    cat(paste("\n", "Original string:\n\n",
              paste("'", original, "'", sep = ""), "\n\n",
              color_text(paste(" ", text_symbol("bullet"), " Refers to the original string that the participant typed", sep = ""), defaults = "message"),
              "\n",
              sep = "")
    )

    # only the `keep_strings = FALSE` menu shows the auto-corrected string
    if(!keep_strings){

      # Explain auto-corrected string
      cat(paste("\n", "Auto-corrected string:\n\n",
                paste("'", context, "'", sep = "", collapse = " "), "\n\n",
                color_text(paste(" ", text_symbol("bullet"), " Refers to the auto-corrected string that the automated spell-check derived", sep = ""), defaults = "message"),
                "\n",
                sep = "")
      )

    }

    # Choices for spell-check
    word <- c("SKIP WORD", "ADD WORD TO DICTIONARY", "TYPE MY OWN WORD", "GOOGLE WORD", "BAD WORD")

    # Set string options
    string <- if(keep_strings){

      c("KEEP ORIGINAL", "TYPE MY OWN STRING", "GOOGLE STRING", "BAD STRING") # Without KEEP AUTO-CORRECT

    }else{

      c("KEEP ORIGINAL", "KEEP AUTO-CORRECT", "TYPE MY OWN STRING", "GOOGLE STRING", "BAD STRING") # With KEEP AUTO-CORRECT

    }

    # Combine menu options
    choices <- c(word, string, possible)

    # Set target word
    check <- context[check]

  }else{ # Single-response menu

    # Choices for spell-check
    choices <- c("SKIP", "ADD TO DICTIONARY", "TYPE MY OWN", "GOOGLE IT", "BAD WORD", possible)

  }

  # Explain target word
  cat(paste("\n", "Target word: '", color_text(check, defaults = "highlight"), "'\n",
            paste(
              color_text(paste(" ", text_symbol("bullet"), " Refers to the ", sep = ""), defaults = "message"),
              color_text("target", defaults = "message"),
              color_text(paste(" word to be manually spell-checked", sep = ""), defaults = "message"),
              sep = ""
            ),
            sep = "")
  )

  # Print line break
  line_break(breaks = "\n\n")

  # Explain SKIP WORD
  cat(paste("\n", "1: SKIP WORD\n",
            color_text(paste(" ", text_symbol("bullet"), " Keeps the target word 'as is' and moves on to next word to be spell-checked", sep = ""), defaults = "message"),
            "\n",
            sep = "")
  )

  # Explain ADD WORD TO DICTIONARY
  cat(paste("\n", "2: ADD WORD TO DICTIONARY\n",
            color_text(paste(" ", text_symbol("bullet"), " Adds the target word to dictionary for future spell-checking", sep = ""), defaults = "message"),
            "\n",
            sep = "")
  )

  # Explain TYPE MY OWN WORD
  cat(paste("\n", "3: TYPE MY OWN WORD\n",
            color_text(paste(" ", text_symbol("bullet"), " Allows you to type your own correction for the target word", sep = ""), defaults = "message"),
            "\n",
            sep = "")
  )

  # Explain GOOGLE WORD
  cat(paste("\n", "4: GOOGLE WORD\n",
            color_text(paste(" ", text_symbol("bullet"), " Opens your default browser and 'Googles' the target word", sep = ""), defaults = "message"),
            "\n",
            sep = "")
  )

  # Explain BAD WORD
  cat(paste("\n", "5: BAD WORD\n",
            color_text(paste(" ", text_symbol("bullet"), " Marks the target word as an inappropriate response (NA)", sep = ""), defaults = "message"),
            sep = "")
  )

  # Print line break
  line_break(breaks = "\n\n")

  # Check for multi-word menu
  if(!is.null(context)){

    # The string options are numbered differently when `keep_strings` is `TRUE`: there is no
    # KEEP AUTO-CORRECT, so there are 4 options instead of 5
    string_entries <- list(
      c("KEEP ORIGINAL", " Reverts the string back to the", "Original string:", "the participant provided"),
      c("KEEP AUTO-CORRECT", " Keeps the string 'as is' with the", "Auto-correct string:", "provided by the automated spell-check"),
      c("TYPE MY OWN STRING", " Allows you to type your own correction for the", "Original string:", "the participant provided"),
      c("GOOGLE STRING", " Opens your default browser and 'Googles' the", "Original string:", "the participant provided"),
      c("BAD STRING", NA, NA, NA)
    )

    # Remove KEEP AUTO-CORRECT when strings are kept
    if(keep_strings){

      # Drop KEEP AUTO-CORRECT
      string_entries <- string_entries[-2]

    }

    # Loop over string options
    for(i in seq_along(string_entries)){

      # Get option
      entry <- string_entries[[i]]

      # Get option number
      number <- 5 + i

      # Check for BAD STRING
      if(entry[1] == "BAD STRING"){

        # Explain BAD STRING
        cat(paste("\n", number, ": ", entry[1], "\n",
                  color_text(paste(" ", text_symbol("bullet"), " Marks the entire string as an inappropriate response (NA)", sep = ""), defaults = "message"),
                  sep = ""))

      }else{ # Other string options

        # Explain string option
        cat(paste("\n", number, ": ", entry[1], "\n",
                  paste(color_text(paste(" ", text_symbol("bullet"), entry[2], sep = ""), defaults = "message"),
                        entry[3],
                        color_text(entry[4], defaults = "message")),
                  "\n", sep = ""))

      }

    }

    # Print line break
    line_break(breaks = "\n\n")

    # Explain response options
    cat(paste("\n", style_text("Response options\n", defaults = "underline"),
              custom_menu(choices = choices, title = NULL, default = if(keep_strings) 9 else 10, help = TRUE), "\n",
              paste(color_text(paste(" ", text_symbol("bullet"), " Potential options based on `textcleaner`'s best guess for the target word (letters correspond to the response)", sep = ""), defaults = "message")),
              "\n", sep = ""
    )
    )

  }else{ # Single-response menu

    # Explain response options
    cat(paste("\n", style_text("Response options\n", defaults = "underline"),
              custom_menu(choices = choices, title = NULL, default = 5, help = TRUE), "\n",
              paste(color_text(paste(" ", text_symbol("bullet"), " Potential options based on `textcleaner`'s best guess for the target word (letters correspond to the response)", sep = ""), defaults = "message")),
              "\n", sep = ""
    )
    )

  }

  # Explain GO BACK
  cat(paste("\n'B'\n",
            paste(color_text(paste(" ", text_symbol("bullet"), " Takes you back one decision (use if you make a mistake). From the first decision of a response it returns to the PREVIOUS response and asks about it again from its first word", sep = ""), defaults = "message")),
            "\n", sep = ""
  )
  )

  # Explain HELP
  cat(paste("\n'H'\n",
            paste(color_text(paste(" ", text_symbol("bullet"), " Outputs the information you see here. For other help information, see `?textcleaner`", sep = ""), defaults = "message")),
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
  read_input("Press ENTER to get back to manual spell-check...", reader = reader)

}
