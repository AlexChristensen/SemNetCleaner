#' @noRd
# Prints the manual spell-check 'H'/HELP screen: a legend for every menu
# option (word/string/response options plus B/H/esc), shown mid-menu when
# the user requests help. Ported from `textcleaner.help()`. Rebuilt on
# `read_input()` (binding decision #3) for its one `readline()` pause.
# Bug #12 (custom_menu(help = TRUE) crashing on a short `possible` guess
# vector -- see helpers.R) is confirmed reachable from here: the
# `context`-branch call below is exactly `custom_menu(choices, default = 10,
# help = TRUE)` with `choices` built from 10 fixed items plus a
# variable-length `possible`, so any word with fewer than 5 guesses hit it.
# Updated 09.09.2026
textcleaner_help <- function(check, context, original, possible, reader = readline)
{

  line_break(breaks = "\n\n")

  help_art <- c(" _            _       _                              _          _        ",
                "| |_ _____  _| |_ ___| | ___  __ _ _ __   ___ _ __  | |__   ___| |_ __   ",
                "| __/ _ \\ \\/ / __/ __| |/ _ \\/ _` | '_ \\ / _ \\ '__| | '_ \\ / _ \\ | '_ \\  ",
                "| ||  __/>  <| || (__| |  __/ (_| | | | |  __/ |    | | | |  __/ | |_) | ",
                " \\__\\___/_/\\_\\\\__\\___|_|\\___|\\__,_|_| |_|\\___|_|    |_| |_|\\___|_| .__/  ",
                "                                                                 |_| ")

  cat(help_art, sep = "\n")

  line_break(breaks = "")

  Sys.sleep(2)

  if(!is.null(context)){

    cat(paste("\n", "Original string:\n\n",
              paste("'", original, "'", sep = ""), "\n\n",
              color_text(paste(" ", text_symbol("bullet"), " Refers to the original string that the participant typed", sep = ""), defaults = "message"),
              "\n",
              sep = "")
    )

    cat(paste("\n", "Auto-corrected string:\n\n",
              paste("'", context, "'", sep = "", collapse = " "), "\n\n",
              color_text(paste(" ", text_symbol("bullet"), " Refers to the auto-corrected string that the automated spell-check derived", sep = ""), defaults = "message"),
              "\n",
              sep = "")
    )

    # Choices for spell-check
    word <- c("SKIP WORD", "ADD WORD TO DICTIONARY", "TYPE MY OWN WORD", "GOOGLE WORD", "BAD WORD")
    string <- c("KEEP ORIGINAL", "KEEP AUTO-CORRECT", "TYPE MY OWN STRING", "GOOGLE STRING", "BAD STRING")

    choices <- c(word, string, possible)

    check <- context[check]

  }else{

    # Choices for spell-check
    choices <- c("SKIP", "ADD TO DICTIONARY", "TYPE MY OWN", "GOOGLE IT", "BAD WORD", possible)

  }

  cat(paste("\n", "Target word: '", color_text(check, defaults = "highlight"), "'\n",
            paste(
              color_text(paste(" ", text_symbol("bullet"), " Refers to the ", sep = ""), defaults = "message"),
              color_text("target", defaults = "message"),
              color_text(paste(" word to be manually spell-checked", sep = ""), defaults = "message"),
              sep = ""
            ),
            sep = "")
  )

  line_break(breaks = "\n\n")

  cat(paste("\n", "1: SKIP WORD\n",
            color_text(paste(" ", text_symbol("bullet"), " Keeps the target word 'as is' and moves on to next word to be spell-checked", sep = ""), defaults = "message"),
            "\n",
            sep = "")
  )

  cat(paste("\n", "2: ADD WORD TO DICTIONARY\n",
            color_text(paste(" ", text_symbol("bullet"), " Adds the target word to dictionary for future spell-checking", sep = ""), defaults = "message"),
            "\n",
            sep = "")
  )

  cat(paste("\n", "3: TYPE MY OWN WORD\n",
            color_text(paste(" ", text_symbol("bullet"), " Allows you to type your own correction for the target word", sep = ""), defaults = "message"),
            "\n",
            sep = "")
  )

  cat(paste("\n", "4: GOOGLE WORD\n",
            color_text(paste(" ", text_symbol("bullet"), " Opens your default browser and 'Googles' the target word", sep = ""), defaults = "message"),
            "\n",
            sep = "")
  )

  cat(paste("\n", "5: BAD WORD\n",
            color_text(paste(" ", text_symbol("bullet"), " Marks the target word as an inappropriate response (NA)", sep = ""), defaults = "message"),
            sep = "")
  )

  line_break(breaks = "\n\n")

  if(!is.null(context)){

    cat(paste("\n", "6: KEEP ORIGINAL\n",
              paste(color_text(paste(" ", text_symbol("bullet"), " Reverts the string back to the", sep = ""), defaults = "message"),
                    "Original string:",
                    color_text("the participant provided", defaults = "message")),
              "\n", sep = ""
    )
    )

    cat(paste("\n", "7: KEEP AUTO-CORRECT\n",
              paste(color_text(paste(" ", text_symbol("bullet"), " Keeps the string 'as is' with the", sep = ""), defaults = "message"),
                    "Auto-correct string:",
                    color_text("provided by the automated spell-check", defaults = "message")),
              "\n", sep = ""
    )
    )

    cat(paste("\n", "8: TYPE MY OWN STRING\n",
              paste(color_text(paste(" ", text_symbol("bullet"), " Allows you to type your own correction for the", sep = ""), defaults = "message"),
                    "Original string:",
                    color_text("the participant provided", defaults = "message")),
              "\n", sep = ""
    )
    )

    cat(paste("\n", "9: GOOGLE STRING\n",
              paste(color_text(paste(" ", text_symbol("bullet"), " Opens your default browser and 'Googles' the", sep = ""), defaults = "message"),
                    "Original string:",
                    color_text("the participant provided", defaults = "message")),
              "\n", sep = ""
    )
    )

    cat(paste("\n", "10: BAD STRING\n",
              color_text(paste(" ", text_symbol("bullet"), " Marks the entire string as an inappropriate response (NA)", sep = ""), defaults = "message"),
              sep = ""
    )
    )

    line_break(breaks = "\n\n")

    cat(paste("\n", style_text("Response options\n", defaults = "underline"),
              custom_menu(choices = choices, title = NULL, default = 10, help = TRUE), "\n",
              paste(color_text(paste(" ", text_symbol("bullet"), " Potential options based on `textcleaner`'s best guess for the target word (letters correspond to the response)", sep = ""), defaults = "message")),
              "\n", sep = ""
    )
    )

  }else{

    cat(paste("\n", style_text("Response options\n", defaults = "underline"),
              custom_menu(choices = choices, title = NULL, default = 5, help = TRUE), "\n",
              paste(color_text(paste(" ", text_symbol("bullet"), " Potential options based on `textcleaner`'s best guess for the target word (letters correspond to the response)", sep = ""), defaults = "message")),
              "\n", sep = ""
    )
    )

  }

  cat(paste("\n'B'\n",
            paste(color_text(paste(" ", text_symbol("bullet"), " Takes you back to the previous response (use if you make a mistake or just want to go back)", sep = ""), defaults = "message")),
            "\n", sep = ""
  )
  )

  cat(paste("\n'H'\n",
            paste(color_text(paste(" ", text_symbol("bullet"), " Outputs the information you see here. For other help information, see `?textcleaner`", sep = ""), defaults = "message")),
            "\n", sep = ""
  )
  )

  cat(paste("\n'X'\n",
            paste(color_text(paste(" ", text_symbol("bullet"), " Exits textcleaner completely but saves your output", sep = ""), defaults = "message")),
            "\n", sep = ""
  )
  )

  cat(paste("\nSelection (accepts lowercase): \n",
            paste(color_text(paste(" ", text_symbol("bullet"), " Where you input the option you select", sep = ""), defaults = "message")),
            "\n\n", sep = ""
  )
  )

  read_input("Press ENTER to get back to manual spell-check...", reader = reader)

}
