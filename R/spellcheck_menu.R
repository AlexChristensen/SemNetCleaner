#' @noRd
# Describe word change ----
# Emits the "Response was <verb>: '...'" message for a word option of a multi-word
# response, showing the response with the target word replaced and in bold
# Updated 14.10.2026
describe_word_change <- function(
    context, # words of the response
    check, # positions of the target word
    verb, # description of the decision
    replacement # word shown in place of the target
)
{

  # `check` may hold several positions (a response that repeats the unknown word is decided
  # once for all of its occurrences); every position is replaced
  shown <- context

  # Style the replacement
  shown[check] <- style_text(replacement, defaults = "bold")

  # Let user know the change
  message(paste("\nResponse was ", verb, ": '", paste(shown, collapse = " "), "'", sep = ""))

}

#' @noRd
# Parse typed words ----
# Splits a "TYPE MY OWN" entry on commas and drops blanks; empty input returns
# `character(0)` so that the caller can return to the menu
# Updated 14.10.2026
parse_typed_words <- function(
    input # text typed by the user
)
{

  # Split on commas
  words <- trimws(unlist(strsplit(input, split = ",")))

  # Drop blank words
  return(words[nzchar(words)])

}

#' @noRd
# Normalize typed words ----
# Runs typed replacement words through the same cleaning as the rest of the data
# (`normalize_fn`, built by `textcleaner()` from `allowPunctuations` and `allowNumbers`, plus
# lowercasing); the "NA" bad-response marker is left alone and words that normalize away
# (e.g., "123") are dropped
# Updated 14.10.2026
normalize_typed_words <- function(
    words, # words typed by the user
    normalize_fn = NULL # function used to clean words
)
{

  # Check for nothing to normalize
  if(is.null(normalize_fn) || length(words) == 0){

    # Return words as typed
    return(words)

  }

  # Find bad-response markers
  marker <- words == "NA"

  # Normalize all other words
  words[!marker] <- normalize_fn(words[!marker])

  # Drop words that normalize away
  return(words[!is.na(words) & nzchar(words)])

}

#' @noRd
# Offer add missing to dictionary ----
# For every word of `words` that is not in `full_dictionary`, asks whether it should be
# added and adds it on "Yes"
# Updated 14.10.2026
offer_add_missing_to_dictionary <- function(
    words, # words typed by the user
    full_dictionary, # dictionary used to check words
    reader = readline # function used to read answers
)
{

  # Check which words are in the dictionary
  in_dictionary <- dictionary_contains(words, full_dictionary)

  # Check for missing words
  if(!all(in_dictionary)){

    # Get missing words
    non_dict <- which(!in_dictionary)

    # Loop over missing words
    for(i in seq_along(non_dict)){

      # Let user know
      message(paste("\n'", words[non_dict[i]], "'", sep = ""), " was not found in the dictionary.\n")

      # Ask whether to add the word
      dict_ans <- yes_no_menu(
        paste("Should ", paste("'", words[non_dict[i]], "'", sep = ""), " be added to the dictionary", sep = ""),
        reader = reader
      )

      # Check for yes
      if(dict_ans == 1){

        # Add word to dictionary
        full_dictionary <- SemNetDictionaries::append.dictionary(
          words[non_dict[i]], full_dictionary,
          dictionary.name = "full", save.location = "envir", textcleaner = TRUE
        )

      }

    }

  }

  # Return updated dictionary
  return(full_dictionary)

}

#' @noRd
# Manual spell-check menu ----
# The manual spell-check decision menu, called once per word of a multi-word response
# (`context`) or once per response (single-response menu) from `spellcheck_dictionary()`.
# `reader` and `search_fn` can be replaced so that the menu can be scripted
# Updated 14.10.2026
spellcheck_menu <- function(
    check, # target word (or its positions in the response)
    context = NULL, # words of the response or NULL
    possible, # suggested corrections
    original, # original response
    current_index, # index of the current response
    changes, # changes made so far
    full_dictionary, # dictionary used to check words
    category, # category used in web searches
    dictionary, # names of dictionaries requested
    keep_strings, # whether strings are kept
    go_back_count, # number of decisions made
    reader = readline, # function used to read answers
    search_fn = searcher::search_site, # function used to search the web
    normalize_fn = NULL # function used to clean typed words
)
{

  # Set answer (30 returns to the menu)
  ans <- 30

  # Initialize go back
  go_back <- FALSE

  # Check if word is already in dictionary (due to dictionary updates)
  if(!is.null(context)){

    # Use words at the target positions
    dict_check <- unique(context[check])

  }else{ # Single response

    # Use the target word
    dict_check <- check

  }

  # Check for word in dictionary
  if(dictionary_contains(dict_check, full_dictionary)){

    # Check for multi-word response
    if(!is.null(context)){

      # Let user know word was kept
      message(paste("\nResponse '",
                    style_text(dict_check, defaults = "bold"), "' in '", style_text(paste(context, collapse = " "), defaults = "bold"),
                    "' was KEPT AS IS based on a previous manual spell-check decision",
                    sep = ""))

    }else{ # Single response

      # Let user know word was kept
      message(paste("\nResponse '",
                    style_text(dict_check, defaults = "bold"),
                    "' was KEPT AS IS based on a previous manual spell-check decision",
                    sep = ""))

    }

    # Initialize results
    res <- list()

    # Set go back
    res$go_back <- go_back

    # Set target
    res$target <- if(is.null(context)) dict_check else context

    # Pass along changes
    res$changes <- changes

    # Pass along dictionary
    res$full_dictionary <- full_dictionary

    # Set end of response
    res$end <- if(is.null(context)) NULL else FALSE

    # Count decision
    res$go_back_count <- go_back_count + 1

    # Pause to read (unless general dictionary)
    if(!"general" %in% dictionary){

      # Pause to read
      pause(1)

    }

    # Return results
    return(res)

  }

  # Set up based on context (multiple responses)
  if(!is.null(context)){

    # Initialize end of response
    end <- FALSE

    # Loop until a final decision
    while(ans == 30){

      # The word being decided. `check` holds every position it occupies in
      # the response (a repeated word, "gren gren gren", is decided ONCE, for
      # all of its occurrences), so the word itself is the first of them
      target_word <- context[check[1]]

      # Check for kept strings
      if(keep_strings){

        # Build title without auto-corrected string
        title <- paste(paste("\nOriginal string: ", "'", original, "'", sep = ""),
                       paste("Target word: ", paste("'", color_text(target_word, defaults = "highlight"), "'", sep = ""), sep = ""),
                       sep = "\n\n")

      }else{ # Strings are split

        # Build title with auto-corrected string
        title <- paste(paste("\nOriginal string: ", "'", original, "'", sep = ""),
                       paste("Auto-corrected string: ", paste("'", context, "'", sep = "", collapse = " "), sep = ""),
                       paste("Target word: ", paste("'", color_text(target_word, defaults = "highlight"), "'", sep = ""), sep = ""),
                       sep = "\n\n")

      }

      # Set word options
      word <- c("SKIP WORD", "ADD WORD TO DICTIONARY", "TYPE MY OWN WORD", "GOOGLE WORD", "BAD WORD")

      # Check for kept strings
      if(keep_strings){

        # Set string options without auto-correct
        string <- c("KEEP ORIGINAL", "TYPE MY OWN STRING", "GOOGLE STRING", "BAD STRING")

      }else{ # Strings are split

        # Set string options
        string <- c("KEEP ORIGINAL", "KEEP AUTO-CORRECT", "TYPE MY OWN STRING", "GOOGLE STRING", "BAD STRING")

      }

      # Combine menu options
      choices <- c(word, string, possible)

      # Set default option
      default <- length(c(word, string))

      # Print menu
      custom_menu(choices = choices, title = title, default = default)

      # Print navigation keys
      message("Press 'B' to GO BACK, 'H' for HELP, or 'X' to EXIT.\n")

      # Read selection
      ans <- trimws(read_input("Selection (accepts lowercase): ", reader = reader))

      # Check for exit
      if(tolower(ans) == "x" || ans == ""){

        return("STOP")

      }

      # Check selection
      ans <- appropriate_answer(answer = ans, choices = choices, default = default, exit = TRUE, reader = reader)

      # Check for exit
      if(ans == 0){

        return("STOP")

      }

      # GO BACK and HELP are identified by their position right after `appropriate_answer()`
      # (`length(choices) + 1` and `+ 2`), before the `keep_strings` renumbering below, because
      # the renumbering moves other options onto those positions
      navigation <- if(ans == length(choices) + 1){

        # Set navigation to go back
        "back"

      }else if(ans == length(choices) + 2){

        # Set navigation to help
        "help"

      }else{

        # Set no navigation
        "none"

      }

      # Renumber string options when strings are kept
      if(keep_strings && navigation == "none"){

        # Check for string option
        if(ans >= 7 && ans <= length(choices)){

          # Move past the removed option
          ans <- ans + 1

        }

      }

      if(navigation == "back"){ # GO BACK

        # Flag go back
        go_back <- TRUE

      }else if(navigation == "help"){ # HELP

        # Show help
        textcleaner_help(check, context, original, possible, reader = reader, keep_strings = keep_strings)

        # Return to menu
        ans <- 30

      }else if(ans == 1){ # SKIP WORD

        # Check for kept strings
        if(keep_strings){

          # Show skipped word within string
          describe_word_change(context, check, "SKIPPED", target_word)

        }else{ # Strings are split

          # Show skipped word
          message(paste("\nResponse was SKIPPED:", paste("'", target_word, "'", sep = "")))

        }

      }else if(ans == 2){ # ADD WORD TO DICTIONARY

        # Add word to dictionary
        full_dictionary <- SemNetDictionaries::append.dictionary(target_word,
                                                                  full_dictionary,
                                                                  dictionary.name = "full",
                                                                  save.location = "envir", textcleaner = TRUE)

        # Check for kept strings
        if(keep_strings){

          # Show kept word within string
          describe_word_change(context, check, "KEPT AS", target_word)

        }

      }else if(ans == 3){ # TYPE MY OWN WORD

        # Explain how to go back
        message("\nType '30' (no quotations) to go back to the other response options\n")

        # Read typed words
        tmo <- read_input("Use commas for multiple words (dog, fish, etc.): ", reader = reader)

        # Parse and normalize typed words
        tmo_split <- normalize_typed_words(parse_typed_words(tmo), normalize_fn)

        # Check for typed words
        if(tmo != "30" && length(tmo_split) > 0){

          # Create change matrix
          change_mat <- t(as.matrix(c(target_word, tmo_split)))

          # Name columns
          colnames(change_mat) <- c("from", paste("to", 1:(ncol(change_mat) - 1), sep = "_"))

          # Record change
          changes <- change_format(change_list = changes,
                                   change_matrix = change_mat,
                                   current_index = current_index)

          # Offer to add typed words to dictionary
          full_dictionary <- offer_add_missing_to_dictionary(tmo_split, full_dictionary, reader = reader)

          # Check for kept strings
          if(keep_strings){

            # Show changed word within string
            describe_word_change(context, check, "CHANGED TO", paste(tmo_split, collapse = " "))

          }else{ # Strings are split

            # Show changed word
            message(paste("\nResponse was CHANGED TO:", paste("'", tmo_split, "'", sep = "", collapse = " ")))

          }

          # every occurrence of the word is replaced (`[[<-` with several
          # positions would index recursively instead)
          context_change <- as.list(context)

          # Replace every occurrence of the word
          context_change[check] <- list(tmo_split)

          # Flatten words
          context <- unlist(context_change)

        }else{ # No typed words

          # Let user know when nothing was entered
          if(tmo != "30"){

            message("\nNo response was entered. Returning to the response options.")

          }

          # Return to menu
          ans <- 30

        }

      }else if(ans == 4){ # GOOGLE WORD

        # Search the web
        search_fn(paste(category, " '", target_word, "'", sep = "", collapse = ""),
                  site = "google", rlang = FALSE)

        # Return to menu
        ans <- 30

      }else if(ans == 5){ # BAD WORD

        # Create change matrix
        change_mat <- t(as.matrix(c(target_word, NA)))

        # Name columns
        colnames(change_mat) <- c("from", paste("to", 1:(ncol(change_mat) - 1), sep = "_"))

        # Record change
        changes <- change_format(change_list = changes,
                                 change_matrix = change_mat,
                                 current_index = current_index)

        # Check for kept strings
        if(keep_strings){

          # Show bad word within string
          describe_word_change(context, check, "CHANGED TO", NA)

        }else{ # Strings are split

          # Show bad word
          message(paste("\nResponse was CHANGED TO:", paste(NA, sep = "", collapse = " ")))

        }

        # Mark target word as bad
        context[check] <- "NA"

      }else if(ans == 6){ # KEEP ORIGINAL

        # Let user know
        message(paste("\nString was REVERTED TO ORIGINAL:", paste("'", original, "'", sep = "")))

        # Revert to original string
        context <- original

        # End response
        end <- TRUE

        # Check whether original is in dictionary
        if(!dictionary_contains(original, full_dictionary)){

          # Let user know
          message(paste("\n'", original, "'", sep = ""),
                  " was not found in the dictionary.\n")

          # Ask whether to add original
          dict_ans <- yes_no_menu(
            paste("Should ",
                  paste("'", original, "'", sep = ""),
                  " be added to the dictionary", sep = ""),
            reader = reader
          )

          # Check for yes
          if(dict_ans == 1){

            # Add original to dictionary
            full_dictionary <- SemNetDictionaries::append.dictionary(original,
                                                                     full_dictionary,
                                                                     dictionary.name = "full",
                                                                     save.location = "envir", textcleaner = TRUE)

          }

        }

      }else if(ans == 7){ # KEEP AUTO-CORRECT

        # Let user know
        message(paste("\nString was KEPT AS AUTO-CORRECT:", paste("'", context, "'", sep = "", collapse = " ")))

        # Keep auto-corrected string
        context <- context

        # End response
        end <- TRUE

      }else if(ans == 8){ # TYPE MY OWN STRING

        # Explain how to go back
        message("\nType '30' (no quotations) to go back to the other response options\n")

        # Read typed string
        ams <- read_input("Use commas for multiple words (dog, fish, etc.): ", reader = reader)

        # Parse and normalize typed words
        ams_split <- normalize_typed_words(parse_typed_words(ams), normalize_fn)

        # Check for typed words
        if(ams != "30" && length(ams_split) > 0){

          # Create change matrix
          change_mat <- t(as.matrix(c(paste(context, collapse = " "), ams_split)))

          # Name columns
          colnames(change_mat) <- c("from", paste("to", 1:(ncol(change_mat) - 1), sep = "_"))

          # Record change
          changes <- change_format(change_list = changes,
                                   change_matrix = change_mat,
                                   current_index = current_index)

          # Offer to add typed words to dictionary
          full_dictionary <- offer_add_missing_to_dictionary(ams_split, full_dictionary, reader = reader)

          # Let user know
          message(paste("\nString was CHANGED TO:", paste("'", ams_split, "'", sep = "", collapse = " ")))

          # Replace string
          context <- ams_split

          # End response
          end <- TRUE

        }else{ # No typed words

          # Let user know when nothing was entered
          if(ams != "30"){

            message("\nNo response was entered. Returning to the response options.")

          }

          # Return to menu
          ans <- 30

        }

      }else if(ans == 9){ # GOOGLE STRING

        # Search the web
        search_fn(paste(category, " '", original, "'", sep = "", collapse = ""),
                  site = "google", rlang = FALSE)

        # Return to menu
        ans <- 30

      }else if(ans == 10){ # BAD STRING

        # Create change matrix
        change_mat <- cbind(context, rep(NA, length(context)))

        # Name columns
        colnames(change_mat) <- c("from", paste("to", 1:(ncol(change_mat) - 1), sep = "_"))

        # Record change
        changes <- change_format(change_list = changes,
                                 change_matrix = change_mat,
                                 current_index = current_index)

        # Let user know
        message(paste("\nALL responses were CHANGED TO:", paste(NA, sep = "", collapse = " ")))

        # Mark all words as bad
        context <- rep("NA", length(context))

        # End response
        end <- TRUE

      }else{ # RESPONSE OPTION

        # Create change matrix
        change_mat <- t(as.matrix(c(target_word, possible[ans - 10])))

        # Name columns
        colnames(change_mat) <- c("from", paste("to", 1:(ncol(change_mat) - 1), sep = "_"))

        # Record change
        changes <- change_format(change_list = changes,
                                 change_matrix = change_mat,
                                 current_index = current_index)

        # Check for kept strings
        if(keep_strings){

          # Show changed word within string
          describe_word_change(context, check, "CHANGED TO", possible[ans - 10])

        }else{ # Strings are split

          # Show changed word
          message(paste("\nResponse was CHANGED TO:", paste("'", possible[ans - 10], "'", sep = "")))

        }

        # Replace target word
        context[check] <- possible[ans - 10]

      }

    }

    # Initialize results
    res <- list()

    # Set go back
    res$go_back <- go_back

    # Set response
    res$target <- context

    # Pass along changes
    res$changes <- changes

    # Pass along dictionary
    res$full_dictionary <- full_dictionary

    # Set end of response
    res$end <- end

    # Count decisions
    res$go_back_count <- go_back_count

    # Pause to read (unless general dictionary)
    if(!"general" %in% dictionary){

      # Pause to read
      pause(1)

    }

    # Return results
    return(res)

  }else{

    # Single response
    # Loop until a final decision
    while(ans == 30){

      # Build title
      title <- paste(paste("\nTarget word: ", paste("'", color_text(check, defaults = "highlight"), "'", sep = ""), sep = ""))

      # Set menu options
      choices <- c("SKIP", "ADD TO DICTIONARY", "TYPE MY OWN", "GOOGLE IT", "BAD WORD", possible)

      # Print menu
      custom_menu(choices = choices, title = title, default = 5)

      # Print navigation keys
      message("Press 'B' to GO BACK, 'H' for HELP, or 'X' to EXIT.\n")

      # Read selection
      ans <- trimws(read_input("Selection (accepts lowercase): ", reader = reader))

      # Check for exit
      if(tolower(ans) == "x" || ans == ""){

        return("STOP")

      }

      # Check selection
      ans <- appropriate_answer(answer = ans, choices = choices, default = 5, exit = TRUE, reader = reader)

      # Check for exit
      if(ans == 0){

        return("STOP")

      }

      if(ans == 1){ # SKIP

        # Let user know
        message(paste("\nResponse was SKIPPED:", paste("'", check, "'", sep = "")))

      }else if(ans == 2){ # ADD TO DICTIONARY

        # Add word to dictionary
        full_dictionary <- SemNetDictionaries::append.dictionary(check,
                                                                  full_dictionary,
                                                                  dictionary.name = "full",
                                                                  save.location = "envir", textcleaner = TRUE)

      }else if(ans == 3){ # TYPE MY OWN

        # Explain how to go back
        message("\nType '30' (no quotations) to go back to the other response options\n")

        # Read typed words
        tmo <- read_input("Use commas for multiple words (dog, fish, etc.): ", reader = reader)

        # Parse and normalize typed words
        tmo_split <- normalize_typed_words(parse_typed_words(tmo), normalize_fn)

        # Check for typed words
        if(tmo != "30" && length(tmo_split) > 0){

          # Create change matrix
          change_mat <- t(as.matrix(c(check, tmo_split)))

          # Name columns
          colnames(change_mat) <- c("from", paste("to", 1:(ncol(change_mat) - 1), sep = "_"))

          # Record change
          changes <- change_format(change_list = changes,
                                   change_matrix = change_mat,
                                   current_index = current_index)

          # Offer to add typed words to dictionary
          full_dictionary <- offer_add_missing_to_dictionary(tmo_split, full_dictionary, reader = reader)

          # Let user know
          message(paste("\nResponse was CHANGED TO:", paste("'", tmo_split, "'", sep = "", collapse = " ")))

          # Replace response
          check <- tmo_split

        }else{ # No typed words

          # Let user know when nothing was entered
          if(tmo != "30"){

            message("\nNo response was entered. Returning to the response options.")

          }

          # Return to menu
          ans <- 30

        }

      }else if(ans == 4){ # GOOGLE IT

        # Search the web
        search_fn(paste(category, " '", check, "'", sep = "", collapse = ""),
                  site = "google", rlang = FALSE)

        # Return to menu
        ans <- 30

      }else if(ans == 5){ # BAD RESPONSE

        # Create change matrix
        change_mat <- t(as.matrix(c(check, NA)))

        # Name columns
        colnames(change_mat) <- c("from", paste("to", 1:(ncol(change_mat) - 1), sep = "_"))

        # Record change
        changes <- change_format(change_list = changes,
                                 change_matrix = change_mat,
                                 current_index = current_index)

        # Let user know
        message(paste("\nResponse was CHANGED TO:", paste(NA, sep = "", collapse = " ")))

        # Mark response as bad
        check <- "NA"

      }else if(ans == length(choices) + 1){ # GO BACK

        # Flag go back
        go_back <- TRUE

      }else if(ans == length(choices) + 2){ # HELP

        # Show help
        textcleaner_help(check, context, original, possible, reader = reader, keep_strings = keep_strings)

        # Return to menu
        ans <- 30

      }else{ # RESPONSE OPTION

        # Create change matrix
        change_mat <- t(as.matrix(c(check, possible[ans - 5])))

        # Name columns
        colnames(change_mat) <- c("from", paste("to", 1:(ncol(change_mat) - 1), sep = "_"))

        # Record change
        changes <- change_format(change_list = changes,
                                 change_matrix = change_mat,
                                 current_index = current_index)

        # Let user know
        message(paste("\nResponse was CHANGED TO:", paste("'", possible[ans - 5], "'", sep = "")))

        # Replace response
        check <- possible[ans - 5]

      }

    }

    # Initialize results
    res <- list()

    # Set go back
    res$go_back <- go_back

    # Set response
    res$target <- check

    # Pass along changes
    res$changes <- changes

    # Pass along dictionary
    res$full_dictionary <- full_dictionary

    # Count decisions
    res$go_back_count <- go_back_count

    # Pause to read (unless general dictionary)
    if(!"general" %in% dictionary){

      # Pause to read
      pause(1)

    }

    # Return results
    return(res)

  }

}
