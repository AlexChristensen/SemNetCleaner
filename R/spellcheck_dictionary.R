#' @noRd
#' @importFrom utils setTxtProgressBar txtProgressBar
# Manual spell-check wizard ----
# The manual spell-check wizard for both tasks (`type = "fluency"` or `"free"`): runs
# `auto_spellcheck()`, then drives `spellcheck_menu()` once per word of a multi-word
# response, or once per response, for everything the automated pass could not resolve. It
# keeps the state needed to save, resume (`continue`), and go back
#
# The tasks differ in a few places: free association always uses the "cocaspell"
# dictionary and the "define" search category (fluency detects the category from the
# dictionary names), and it tracks `auto_ind`, the automatically corrected responses,
# through the whole function. The ad hoc moniker check after the manual spell-check is
# shared (`parallel_process()`; `cores = 1` is serial)
#
# Undoing a decision (GO BACK) removes the entry of that response from `changes` with a
# positive filter, because `changes[-which(...)]` would empty the whole list when nothing
# matches
# Updated 14.10.2026
spellcheck_dictionary <- function(
    uniq_resp = NULL, # unique responses to spell-check
    dictionary = NULL, # dictionary names or a vector of words
    spelling = NULL, # "UK" or "US" spelling
    add_path = NULL, # directory with custom dictionaries
    keep_strings = NULL, # keep multi-word responses as strings
    keep_length = NULL, # maximum number of words in a response
    keep_cue = FALSE, # keep cue words in responses
    data = NULL, # data of responses
    continue = NULL, # unfinished result to continue
    type = c("fluency", "free"), # type of semantic task
    reader = readline, # function used to read answers
    search_fn = searcher::search_site, # function used to search the web
    cores = 1, # number of cores
    normalize_fn = NULL, # function used to clean typed words
    open_fn = utils::browseURL # function used to open the help window
)
{

  # Match type
  type <- match.arg(type)

  # Continuation check
  if(is.null(continue)){

    # Check for free association
    if(type == "free"){

      # Use default dictionary
      if(is.null(dictionary)){

        dictionary <- "cocaspell"

      }

      # Set category for web searches
      category <- "define"

      # Load free association dictionary
      full_dictionary <- SemNetDictionaries::load.dictionaries("cocaspell")

    }else{ # Fluency

      # Get available dictionaries
      categories <- get_available_dictionaries()

      # Check for known dictionaries
      if(any(dictionary %in% categories)){

        # Get requested dictionaries
        target <- dictionary[na.omit(match(categories, dictionary))]

        # Set category for web searches (first match wins)
        if("general" %in% target){

          category <- "dictionary"

        }else if("fruits" %in% target && "vegetables" %in% target){

          category <- "food" # Fruits and vegetables

        }else if("fruits" %in% target && !"vegetables" %in% target){

          category <- "fruits" # Fruits only

        }else if(!"fruits" %in% target && "vegetables" %in% target){

          category <- "vegetables" # Vegetables only

        }else if("jobs" %in% target){

          category <- "jobs" # Jobs

        }else if("hot" %in% target || "good" %in% target){

          category <- "synonym" # Synonyms

        }else if("animals" %in% target){

          category <- "animals" # Animals

        }else{

          category <- "define" # Other dictionaries

        }

      }else{

        category <- "define" # Use generic category

      }

      # Hold the warning that newer 'SemNetDictionaries' gives for a dictionary it cannot find
      not_found <- NULL

      # Load dictionaries
      full_dictionary <- withCallingHandlers(
        SemNetDictionaries::load.dictionaries(dictionary, add.path = add_path),
        semnet_dictionary_not_found = function(condition){

          # Store the warning
          not_found <<- condition

          # Keep it from printing here
          invokeRestart("muffleWarning")

        }
      )

      # `load.dictionaries()` leaves out a dictionary it cannot find and returns an empty
      # result when none are found
      if(length(full_dictionary) == 0){

        # Send error
        stop(
          "No words were loaded for dictionary ", paste0("'", dictionary, "'", collapse = ", "),
          if(is.null(add_path)) "" else paste0(" (add.path = '", add_path, "')"),
          ". Check the spelling of the name. A custom dictionary must be saved as ",
          "'<name>.dictionary.rds' (see SemNetDictionaries::append.dictionary()) in the ",
          "working directory or in 'add.path'.",
          call. = FALSE
        )

      }

      # Some dictionaries were found, so show which ones were left out
      if(!is.null(not_found)){

        # Send warning
        warning(conditionMessage(not_found), call. = FALSE)

      }

    }

    # Let user know
    message(paste("\nConverting dictionary to '", spelling, "' spelling...", sep = ""), appendLF = FALSE)

    # Convert dictionary to requested spelling
    full_dictionary <- brit_us_conversion_vector(full_dictionary, spelling = spelling, dictionary = TRUE)

    # Let user know when done
    message("done")

    # Keep original dictionary
    orig_dictionary <- full_dictionary

    # Get responses as a list
    from <- as.list(uniq_resp)

    # Check for free association
    if(type == "free"){

      # Name responses
      names(from) <- formatC(
        seq_along(from), width = nchar(as.character(length(from))), format = "d", flag = "0"
      )

    }

    # Copy responses for corrections
    to <- from

    # Run automated spell-check
    initial <- try(
      auto_spellcheck(
        check = from, full_dictionary = full_dictionary, dictionary = dictionary,
        spelling = spelling, keep_strings = keep_strings, type = type,
        keep_length = keep_length, cores = cores
      ),
      silent = TRUE
    )

    # Re-raise the error so that `textcleaner()` reports it once (carrying on would fail on
    # `initial$manual`)
    if(inherits(initial, "try-error")){

      # Send error
      stop(
        "auto_spellcheck failed: ", conditionMessage(attr(initial, "condition")),
        call. = FALSE
      )

    }

    # Get responses needing manual spell-check
    ind <- initial$manual

    # Get automatically corrected responses
    if(type == "free"){

      auto_ind <- initial$auto

    }

    # Get corrected responses
    to <- initial$to

    # Keep automated corrections
    initial_to <- to

    # Check for kept strings
    if(keep_strings){

      # Find responses with multiple words
      multi_ind <- which(lapply(to[ind], function(x){

        # Count words in the string
        length(unlist(strsplit(x, split = " ")))

      }) >= 2)

    }else{ # Strings are split

      # Find responses with multiple words
      multi_ind <- which(lapply(to[ind], length) >= 2)

    }

    # Find single-word responses
    single_ind <- setdiff(ind, as.numeric(names(multi_ind)))

    # Order multi-word responses first
    ind <- c(as.numeric(names(multi_ind)), single_ind)

    # Initialize changes
    changes <- list()

    # Initialize response counter
    main_count <- 1

    # Initialize go back counter
    go_back_count <- 1

    # Initialize go back reset
    go_back_reset <- FALSE

    # Check for fluency
    if(type == "fluency"){

      # Add artificial pause for smoother feel
      pause(2)

    }

  }else{ # Continue unfinished session

    # Restore type of data
    type <- continue$type

    # Restore names of dictionaries
    dictionary <- continue$dictionary

    # Restore dictionary
    full_dictionary <- continue$full_dictionary

    # Restore original dictionary
    orig_dictionary <- continue$orig_dictionary

    # Restore spelling
    spelling <- continue$spelling

    # Restore category
    category <- continue$category

    # Restore original responses
    from <- continue$from

    # Restore corrected responses
    to <- continue$to

    # Restore automated results
    initial <- continue$initial

    # Restore automated corrections
    initial_to <- continue$initial_to

    # Restore responses for manual spell-check
    ind <- continue$ind

    # Restore automated responses
    if(type == "free"){

      auto_ind <- continue$auto_ind

    }

    # Restore changes
    changes <- continue$changes

    # Restore position in responses
    main_count <- continue$main_count

    # Restore go back counter
    go_back_count <- continue$go_back_count

    # Restore go back reset
    go_back_reset <- continue$go_back_reset

    # Restore position in multi-word response
    if(!is.null(continue$multi_count)){

      multi_count <- continue$multi_count

    }

    # Restore keep strings setting
    keep_strings <- continue$keep_strings

    # Restore data
    data <- continue$data

  }

  # Check if manual spell-check is necessary
  if(length(ind) != 0){

    # The help opens in its own window as soon as the manual process starts
    # (on every start and every `continue =`; close it freely, 'H' opens it
    # again). Only when the window is enabled, see `help_window_enabled()`
    help_opened <- help_window_enabled() && open_help_window(keep_strings, open_fn)

    # Walk through the manual spell-check
    walk_through(FALSE, help_opened = help_opened, reader = reader)

    # Check for Windows with tcltk
    if(Sys.info()["sysname"] == "Windows" && requireNamespace("tcltk", quietly = TRUE)){

      # Create graphical progress bar
      pb <- tcltk::tkProgressBar(title = "R progress bar", label = "Spell-check progress",
                                 min = 0, max = length(ind), initial = 0, width = 300)

      # Show progress bar
      invisible(tcltk::getTkProgressBar(pb))

    }else{

      pb <- txtProgressBar(min = 0, max = length(ind), style = 3) # Create text progress bar

    }

    # Print line break
    line_break()

  }

  # `pending` is the render context of the decision the wizard was about to ask about when it
  # stopped (`context`, `check`, `possible`, `original`, `category`, `keep_strings`), exactly
  # the arguments of the next `spellcheck_menu()` call. Each call site passes it explicitly
  # because `target`, `check_words`, and `multi_count` persist, unset, across iterations. The
  # internal Shiny wizard uses it to show the next prompt
  save_state <- function(
    stop_target = NULL, # word or string being decided
    pending = NULL # context of the pending decision
  ){

    # Initialize state
    res <- list()

    # Save type of data
    res$type <- type

    # Save names of dictionaries
    res$dictionary <- dictionary

    # Save dictionary
    res$full_dictionary <- full_dictionary

    # Save original dictionary
    res$orig_dictionary <- orig_dictionary

    # Save spelling
    res$spelling <- spelling

    # Save category
    res$category <- category

    # Save original responses
    res$from <- from

    # Save corrected responses
    res$to <- to

    # Save automated results
    res$initial <- initial

    # Save automated corrections
    res$initial_to <- initial_to

    # Save responses for manual spell-check
    res$ind <- ind

    # Save automated responses
    if(type == "free"){

      res$auto_ind <- auto_ind

    }

    # Save changes
    res$changes <- changes

    # Save position in responses
    res$main_count <- main_count

    # Save go back counter
    res$go_back_count <- go_back_count

    # Save go back reset
    res$go_back_reset <- go_back_reset

    # Only when stopping inside a multi-word response (`pending$context` is set):
    # `multi_count` can be left over from an earlier multi-word response, and resuming from a
    # single-word decision with it would treat the next multi-word response as half done
    if(!is.null(pending) && !is.null(pending$context) && exists("multi_count")){

      # Save position in multi-word response
      res$multi_count <- multi_count

      # `target` (the working copy of the multi-word response) and `target_history` (its undo
      # stack) are only folded into `to[[i]]` when the whole response is finished, so they are
      # saved and restored together with `multi_count`; otherwise a resume would return the
      # earlier words to their automated form and GO BACK would fail
      res$wip_target <- target

      # Save undo history
      if(exists("target_history")){

        res$wip_target_history <- target_history

      }

    }

    # Save data
    res$data <- data

    # Save keep strings setting
    res$keep_strings <- keep_strings

    # Flag as unfinished
    res$stop <- TRUE

    # Save word or string being decided
    if(!is.null(stop_target)){

      res$target <- stop_target

    }

    # Save pending decision
    if(!is.null(pending)){

      res$pending <- pending

    }

    # Set class
    class(res) <- "textcleaner"

    # Return state
    res

  }

  # Loop through for manual spell-check
  while(main_count != (length(ind) + 1)){

    # Get index of response
    i <- ind[main_count]

    # Get corrected response
    target <- to[[i]]

    # Check for kept strings
    if(keep_strings){

      # Split into words
      target <- unlist(strsplit(target, split = " "))

      # Check for free association
      if(type == "free"){

        # Remove blank words
        target <- na.omit(ifelse(target == "", NA, target))

      }

    }else{ # Strings are split

      # With keepStrings = FALSE strings are separated: an unresolved multi-word string ("dog
      # xqzvk") is asked about word by word, so BAD WORD drops only the bad word (KEEP ORIGINAL
      # and BAD STRING remain for whole-string answers; `split_unknown_strings()` separates a
      # kept original string at the end)
      target <- unlist(lapply(target, function(piece){

        # Check for multi-word piece not in the dictionary
        if(!is.na(piece) && grepl(" ", piece, fixed = TRUE) && !dictionary_contains(piece, full_dictionary)){

          # Split piece into words
          words <- strsplit(trimws(piece), "[[:space:]]+")[[1]]

          # Drop blank words
          words[nzchar(words)]

        }else{ # Keep piece as is

          # Keep piece as is
          piece

        }

      }))

    }

    # Check for multiple words
    if(length(target) > 1){

      # Remove punctuation
      target_punct <- gsub("[^[:alnum:][:space:]]", "", target)

      # Check for free association with empty words
      if(type == "free" && any(target_punct == "")){

        # Remove empty words
        target <- target[-which(target_punct == "")]

        # Remove empty words from punctuation-free version
        target_punct <- na.omit(ifelse(target_punct == "", NA, target_punct))

        # Find words to check (not in dictionary or a phrase)
        check_words <- target[which(!target_punct %in% full_dictionary & !phrase_covered(target_punct, full_dictionary))]

        # Check for no words to check
        if(length(check_words) == 0){

          # Combine into one string
          target <- paste(target, collapse = " ")

          # Check combined string
          check_words <- target[!target %in% full_dictionary]

        }

      }else{ # No empty words

        # Find words to check (not in dictionary or a phrase)
        check_words <- target[which(!target_punct %in% full_dictionary & !phrase_covered(target_punct, full_dictionary))]

        # Check for free association with nothing to check
        if(type == "free" && length(check_words) == 0){

          # Combine into one string
          single_string <- paste(target_punct, collapse = " ")

          # Check whether string was already decided
          if(!single_string %in% unlist(to[1:i])){

            # Check the whole string
            check_words <- target

          }

        }

      }

      # A word repeated within the response ("gren gren gren") is decided once for all of its
      # occurrences (the menu replaces every position in `menu_check`)
      check_words <- unique(check_words)

      # Check for starting from the first word
      if(is.null(continue$multi_count)){

        # Initialize word counter
        multi_count <- 1

        # One entry per word position: `target_history[[k]]` holds `target` as it stood right before
        # the decision on word k, so that GO BACK can undo any number of consecutive decisions
        target_history <- list()

      }else{ # Continue within a response

        # Restore the in-progress working copy (`target`) and the undo history of this response,
        # saved together with `multi_count` (only for the response that was stopped)
        if(!is.null(continue$wip_target)){

          target <- continue$wip_target

        }

        # Restore undo history
        target_history <- if(!is.null(continue$wip_target_history)) continue$wip_target_history else list()

        # Clear saved position
        continue$multi_count <- NULL

      }

      # Check for no words to check
      if(length(check_words) == 0){

        # Initialize result
        result <- list()

        # Count decision
        result$go_back_count <- go_back_count + 1

        # Check for kept strings
        if(keep_strings){

          # Let user know
          message(paste("\nResponse '",
                        style_text(paste(target, collapse = " "), defaults = "bold"),
                        "' was KEPT AS IS based on a previous manual spell-check decision",
                        sep = ""))

        }else{ # Strings are split

          # Let user know
          message(paste("\nResponse '",
                        style_text(target, defaults = "bold"),
                        "' was KEPT AS IS based on a previous manual spell-check decision",
                        sep = ""))

        }

        # Check whether go back counter changed
        if(result$go_back_count == go_back_count){

          # Check for go back reset
          if(go_back_reset){

            # Reset go back counter
            go_back_count <- 1

          }else{ # Go back not yet reset

            # Flag go back reset
            go_back_reset <- TRUE

          }

        }else{ # Counter changed

          # Update go back counter
          go_back_count <- result$go_back_count

          # Clear go back reset
          go_back_reset <- FALSE

        }

        # Print line break
        line_break()

      }

      # Loop through words that need to be checked
      while(multi_count != (length(check_words) + 1)){

        # Computed once and reused for the `spellcheck_menu()` call and for the `pending` context
        # saved on STOP or error
        menu_check <- which(check_words[multi_count] == target)

        # Get suggestions
        menu_possible <- best_guess(
          unique(target[menu_check]),
          full_dictionary = full_dictionary, dictionary = dictionary
        )

        # Set original string
        menu_original <- if(keep_strings) paste(target, collapse = " ") else from[[i]]

        # Store context of the pending decision
        pending <- list(
          context = target, check = menu_check, possible = menu_possible,
          original = menu_original, category = category, keep_strings = keep_strings
        )

        # Ask for decision
        result <- try(
          spellcheck_menu(
            check = menu_check, context = target, possible = menu_possible,
            original = menu_original,
            current_index = i, changes = changes, full_dictionary = full_dictionary,
            category = category, dictionary = dictionary, keep_strings = keep_strings,
            go_back_count = go_back_count, reader = reader, search_fn = search_fn,
            normalize_fn = normalize_fn
          ),
          silent = TRUE
        )

        # Print line break
        line_break()

        # Check for exit
        if("STOP" %in% result){

          # Let user know
          message("\nUser stopped. Saving progress...\n")

          # Save state
          res <- save_state(pending = pending)

          # Close progress bar
          close(pb)

          # Pause to read (free association)
          if(type == "free"){

            pause(2)

          }

          # Return state
          return(res)

        }else if(inherits(result, "try-error")){ # Error in menu

          # Report error
          error_report(result, "spellcheck_menu", "textcleaner")

          # Save state
          res <- save_state(target, pending = pending)

          # Close progress bar
          close(pb)

          # Let user know
          message("\nSaving progress...\n")

          # Pause to read
          pause(2)

          # Return state
          return(res)

        }

        # Check whether go back counter changed
        if(result$go_back_count == go_back_count){

          # Check for go back reset
          if(go_back_reset){

            # Reset go back counter
            go_back_count <- 1

          }else{ # Go back not yet reset

            # Flag go back reset
            go_back_reset <- TRUE

          }

        }else{ # Counter changed

          # Update go back counter
          go_back_count <- result$go_back_count

          # Clear go back reset
          go_back_reset <- FALSE

        }

        # Check for go back
        if(result$go_back){

          # Check for first word of response
          if(multi_count == 1){

            # Check for a previous response
            if(main_count != 1){

              # Tell user which response is reopened
              announce_go_back_to_previous(from[[ind[main_count - go_back_count]]])

              # Restore automated correction
              to[[i]] <- initial_to[[i]]

              # Get previous response
              prev_resp <- initial_to[[ind[main_count - go_back_count]]]

              # Split previous response into words
              prev_resp_split <- unlist(strsplit(prev_resp, split = " "))

              # Check for added dictionary words
              if(any(!prev_resp_split %in% full_dictionary == prev_resp_split %in% orig_dictionary)){

                # Find added words
                target_rms <- which(!prev_resp_split %in% full_dictionary == prev_resp_split %in% orig_dictionary)

                # Find positions in dictionary
                ind_rms <- match(prev_resp_split[target_rms], full_dictionary)

                # Remove added words
                full_dictionary <- full_dictionary[-ind_rms]

              }

              # Restore previous response
              to[[ind[main_count - go_back_count]]] <- prev_resp

              # Remove previous changes
              changes <- changes[names(changes) != paste(ind[main_count - go_back_count])]

              # Move back to previous response
              main_count <- main_count - (go_back_count + 1)

              # Reset go back counter
              go_back_count <- 1

              # Leave word loop
              break

            }else{ # First response

              # Let user know
              message("\nThis is the first response. 'GO BACK' is not available.")

              # Print line break
              line_break()

              # Pause to read
              pause(0.5)

            }

          }else{ # Not first word

            # Read the previous state from the undo stack at position `multi_count - 1` (this also works
            # for consecutive GO BACKs)
            prior_target <- target_history[[multi_count - 1]]

            # Restore target
            target <- prior_target

            # Check for added dictionary words
            if(any(prior_target %in% full_dictionary & !prior_target %in% orig_dictionary)){

              # Find added words
              target_rms <- which(prior_target %in% full_dictionary & !prior_target %in% orig_dictionary)

              # Find positions in dictionary
              ind_rms <- match(prior_target[target_rms], full_dictionary)

              # Remove added words
              full_dictionary <- full_dictionary[-ind_rms]

            }

            # Update the entry of this response in `changes` in place, only when one exists (a response
            # decided with SKIP or KEEP AUTO-CORRECT has no entry)
            change_idx <- which(names(changes) == paste(ind[main_count]))

            # Check for recorded changes
            if(length(change_idx) != 0){

              # Remove last change
              changes[[change_idx]] <- changes[[change_idx]][-(multi_count - 1), , drop = FALSE]

            }

            # Move back one word
            multi_count <- multi_count - 1

          }

        }else{ # Decision made

          # Push the state before the decision onto the undo stack
          target_history[[multi_count]] <- target

          # `result$target` is the new full set of words of the response (not just the word at
          # `multi_count`)
          target <- result$target

          # Store changes
          changes <- result$changes

          # Store dictionary
          full_dictionary <- result$full_dictionary

          # Check for all bad words
          if(all(target == "NA")){

            # End response
            result$end <- TRUE

          }

          # Check for end of response
          if(result$end){

            # Skip remaining words
            multi_count <- length(check_words) + 1

          }else{

            multi_count <- multi_count + 1 # Move to next word

          }

        }

      }

      # Check for kept strings
      if(keep_strings){

        # A word marked bad (BAD WORD) is dropped from a kept string and the rest is kept ("dog
        # xqzvk" becomes "dog"); one "NA" marker stays beside it so that the bad word still counts
        # as an intrusion
        bad_words <- target == "NA"

        # Check for bad words
        if(any(bad_words)){

          # Drop bad words
          kept_words <- target[!bad_words]

          # Keep one marker beside the rest
          target <- if(length(kept_words) > 0) c(paste(kept_words, collapse = " "), "NA") else "NA"

        }else{ # No bad words

          # Join words into a string
          target <- paste(target, collapse = " ")

        }

      }

      # Store corrected response
      to[[i]] <- target

    }else{

      # Single response, wrapped in a `repeat` so that a denied GO BACK (first response) asks
      # again instead of moving on
      repeat{

        # Get suggestions
        menu_possible <- best_guess(target, full_dictionary = full_dictionary, dictionary)

        # Same precomputation as the multi-word branch; `context = NULL` because there is no
        # surrounding string
        pending <- list(
          context = NULL, check = target, possible = menu_possible,
          original = from[[i]], category = category, keep_strings = keep_strings
        )

        # Ask for decision
        result <- try(
          spellcheck_menu(
            check = target, context = NULL, possible = menu_possible,
            original = from[[i]], current_index = i, changes = changes,
            full_dictionary = full_dictionary, category = category, dictionary = dictionary,
            keep_strings = keep_strings, go_back_count = go_back_count,
            reader = reader, search_fn = search_fn, normalize_fn = normalize_fn
          ),
          silent = TRUE
        )

        # Print line break
        line_break()

        # Check for exit
        if("STOP" %in% result){

          # Let user know
          message("\nUser stopped. Saving progress...\n")

          # Save state
          res <- save_state(pending = pending)

          # Close progress bar
          close(pb)

          # Pause to read
          pause(2)

          # Return state
          return(res)

        }else if(inherits(result, "try-error")){ # Error in menu

          # Report error
          error_report(result, "spellcheck_menu", "textcleaner")

          # Save state
          res <- save_state(target, pending = pending)

          # Close progress bar
          close(pb)

          # Let user know
          message("\nSaving progress...\n")

          # Pause to read
          pause(2)

          # Return state
          return(res)

        }

        # Check whether go back counter changed
        if(result$go_back_count == go_back_count){

          # Check for go back reset
          if(go_back_reset){

            # Reset go back counter
            go_back_count <- 1

          }else{ # Go back not yet reset

            # Flag go back reset
            go_back_reset <- TRUE

          }

        }else{ # Counter changed

          # Update go back counter
          go_back_count <- result$go_back_count

          # Clear go back reset
          go_back_reset <- FALSE

        }

        # Check for go back
        if(result$go_back){

          # Check for a previous response
          if(main_count != 1){

            # Tell user which response is reopened
            announce_go_back_to_previous(from[[ind[main_count - go_back_count]]])

            # Restore automated correction
            to[[i]] <- initial_to[[i]]

            # Get previous response
            prev_resp <- initial_to[[ind[main_count - go_back_count]]]

            # Split previous response into words
            prev_resp_split <- unlist(strsplit(prev_resp, split = " "))

            # Check for added dictionary words
            if(any(!prev_resp_split %in% full_dictionary == prev_resp_split %in% orig_dictionary)){

              # Find added words
              target_rms <- which(!prev_resp_split %in% full_dictionary == prev_resp_split %in% orig_dictionary)

              # Find positions in dictionary
              ind_rms <- match(prev_resp_split[target_rms], full_dictionary)

              # Remove added words
              full_dictionary <- full_dictionary[-ind_rms]

            }

            # Restore previous response
            to[[ind[main_count - go_back_count]]] <- prev_resp

            # Remove previous changes
            changes <- changes[names(changes) != paste(ind[main_count - go_back_count])]

            # Move back to previous response
            main_count <- main_count - (go_back_count + 1)

            # Reset go back counter
            go_back_count <- 1

            # Leave decision loop
            break

          }else{ # First response

            # Let user know
            message("This is the first response. 'GO BACK' is not available.")

            # Print line break
            line_break()

            # Pause to read
            pause(0.5)

          }

        }else{ # Decision made

          # Store response
          to[[i]] <- result$target

          # Store changes
          changes <- result$changes

          # Store dictionary
          full_dictionary <- result$full_dictionary

          # Leave decision loop
          break

        }

      }

    }

    # Check for Windows with tcltk
    if(Sys.info()["sysname"] == "Windows" && requireNamespace("tcltk", quietly = TRUE)){

      # Compute percent done
      percent <- floor((main_count / length(ind)) * 100)

      # Create progress label
      info <- suppressWarnings(sprintf(paste(main_count, "of", length(ind), "responses done"), percent))

      # Update graphical progress bar
      tcltk::setTkProgressBar(pb, main_count, sprintf("Spell-check Progress (%s)", info), info)

    }else{ # Text progress bar

      # Print blank line
      cat("\n")

      # Update progress bar
      setTxtProgressBar(pb, main_count)

      # Pause for smoother feel
      pause(0.10)

      # Print blank line
      cat("\n")

    }

    # Move to next response
    main_count <- main_count + 1

  }

  # Check for progress bar
  if(main_count != 1){

    # Close progress bar
    close(pb)

  }

  # Initialize final results
  final_res <- list()

  # Update dictionary
  if(length(orig_dictionary) != length(full_dictionary)){

    # Set options for saving
    choices <- c("In my results",
                 "In my working directory",
                 "I'd like to choose the directory",
                 "Don't save it")

    # Set menu title
    title <- "\nWhere would you like to save your additional dictionary entries?"

    # Print menu
    custom_menu(choices = choices, title = title, default = 4, dictionary = TRUE)

    # Read selection
    ans <- read_input("Selection: ", reader = reader)

    # Check selection
    ans <- appropriate_answer(answer = ans, choices = choices, default = 4, dictionary = TRUE, reader = reader)

    # Check for saving in results
    if(ans == 1){

      # Keep dictionary in results
      final_res <- keep_dictionary_in_results(final_res, full_dictionary, orig_dictionary)

      # Let user know
      message("\nDictionary saved to your output object under '$dictionary' (the new words alone are in '$added_words')")

    }else if(ans == 2){ # Save in working directory

      # Ask for dictionary name
      dictionary_name <- prompt_dictionary_name(reader)

      # Try saving dictionary
      saved <- try_save_dictionary(full_dictionary, dictionary_name, "wd")

      # Check for successful save
      if(saved){

        # Let user know
        message(paste("\nDictionary saved to your working directory: '", getwd(), "'", sep = ""))

      }else{ # Save failed

        # Keep dictionary in results
        final_res <- keep_dictionary_in_results(final_res, full_dictionary, orig_dictionary)

      }

    }else if(ans == 3){ # Choose directory

      # Ask for dictionary name
      dictionary_name <- prompt_dictionary_name(reader)

      # "choose" opens a directory picker through `tcltk`, which is missing or fails to load on
      # many systems (e.g., macOS without XQuartz): fall back to the working directory
      if(tcltk_available()){

        # Try saving to chosen directory
        saved <- try_save_dictionary(full_dictionary, dictionary_name, "choose")

      }else{ # No directory chooser

        # Try saving to working directory
        saved <- try_save_dictionary(full_dictionary, dictionary_name, "wd")

        # Check for successful save
        if(saved){

          # Let user know
          message(paste(
            "\nA directory chooser is not available on this system ('tcltk' could not be loaded).",
            "\nDictionary saved to your working directory instead: '", getwd(), "'", sep = ""
          ))

        }

      }

      # Check for failed save
      if(!saved){

        # Keep dictionary in results
        final_res <- keep_dictionary_in_results(final_res, full_dictionary, orig_dictionary)

      }

    }else{

      message("\nDictionary was not saved") # Do not save

    }

  }

  # Run ad hoc check (if necessary). `has_topic_dictionary` is whether a topic dictionary (not
  # just a spell-check dictionary like cocaspell) was requested
  available_dictionaries <- get_available_dictionaries()

  # Check for topic dictionaries
  has_topic_dictionary <- any(dictionary %in% available_dictionaries[available_dictionaries != "general"])

  # Check whether any manual decisions were made
  if(main_count != 1){

    # Check for topic dictionary
    if(has_topic_dictionary){

      # Let user know
      message("\nRunning ad hoc check for common misspellings and monikers...", appendLF = FALSE)

      # Get requested topic dictionaries
      target_dict <- dictionary[na.omit(match(available_dictionaries, dictionary))]

      # Load monikers
      target_moniker <- get_monikers(target_dict)

      # Check each response for monikers
      replace_to <- parallel_process(
        length(to),
        function(idx, to, target_moniker, spelling){

          # Loop over words
          unlist(lapply(to[[idx]], function(x){

            # Check for missing
            if(!is.na(x)){

              # Convert monikers
              unlist(moniker(x, target_moniker, spelling = spelling))

            }

          }))

        },
        to = to, target_moniker = target_moniker, spelling = spelling,
        cores = cores, progress = FALSE
      )

      # Name by response
      names(replace_to) <- names(to)

      # Store corrected responses
      to <- replace_to

      # Let user know when done
      message("done")

    }

  }

  # Check for free association
  if(type == "free"){

    # Separate automated indices (`as.vector()` removes the attributes of `na.omit()`)
    auto_ind <- as.vector(na.omit(
      unlist(
        lapply(auto_ind, function(idx){

          # Get original response
          from_response <- from[[idx]]

          # Get corrected response
          to_response <- to[[idx]]

          # Check whether the response was unchanged
          if(all(!is.na(match(from_response, to_response)))){

            # Drop unchanged responses
            return(NA)

          }else{ # Response changed

            # Keep changed responses
            return(idx)

          }

        })
      )
    ))

    # Check for monikers
    if(main_count != 1 && has_topic_dictionary){

      # Remove responses changed by monikers
      auto_ind <- auto_ind[!from[auto_ind] %in% target_moniker]

    }

    # Store original responses
    final_res$from <- from

    # With keepStrings = FALSE strings are separated: an unresolved multi-word string that was
    # skipped or added is split like the fluency branch does, instead of staying whole next to
    # split neighbors
    final_res$to <- if(keep_strings) to else split_unknown_strings(to, ind, full_dictionary)

    # Store manual responses
    final_res$manual <- ind

    # Store automated responses
    final_res$auto <- auto_ind

    # Store data
    final_res$data <- data

    # Store type
    final_res$type <- "free"

    # Flag as finished
    final_res$stop <- FALSE

  }else{ # Fluency

    # Store original responses
    final_res$from <- from

    # keepStrings = TRUE keeps strings as strings, except that a string whose
    # words are all dictionary words (after correction) is split unless the
    # string itself is a dictionary entry, see `split_valid_strings()`
    final_res$to <- if(keep_strings) split_valid_strings(to, full_dictionary) else split_unknown_strings(to, initial$manual, full_dictionary)

    # Store manual responses
    final_res$manual <- initial$manual

    # Store automated responses
    final_res$auto <- initial$auto

    # Store data
    final_res$data <- data

    # Store type
    final_res$type <- "fluency"

    # Flag as finished
    final_res$stop <- FALSE

  }

  # Set class
  class(final_res) <- "textcleaner"

  # Return results
  return(final_res)

}

#' @noRd
# Prompt dictionary name ----
# Asks for the name of a dictionary file to save and asks again until it is usable (a name
# with "/" would fail, and an empty name would save nothing, after the whole session)
# Updated 14.10.2026
prompt_dictionary_name <- function(
    reader = readline # function used to read answers
)
{

  # Set prompt
  prompt <- "Name for the dictionary: "

  # Ask until the name is usable
  repeat{

    # Read name
    name <- trimws(read_input(prompt, reader = reader))

    # Remove dictionary suffix
    name <- sub("\\.dictionary$", "", name)

    # Check for valid name
    if(grepl("^[A-Za-z][A-Za-z0-9._-]*$", name)){

      # Return name
      return(name)

    }

    # Explain the rule
    message("\nDictionary names must start with a letter and contain only letters, numbers, '.', '_' or '-'.")

    # Update prompt
    prompt <- "Name for the dictionary (letters, numbers, '.', '_' or '-'): "

  }

}

#' @noRd
# Try save dictionary ----
# Saves the dictionary, but never lets a failed save (read-only directory,
# full disk, ...) take the finished manual session down with it: on
# failure the problem is reported and FALSE is returned so the caller can
# keep the dictionary in the results instead.
# Updated 14.10.2026
try_save_dictionary <- function(
    full_dictionary, # dictionary to save
    dictionary_name, # name of the dictionary file
    save_location # "wd" or "choose"
)
{

  # Try saving dictionary
  tryCatch({

    # Words to save (the name must not contain "dictionary" or `append.dictionary()`
    # treats them as the existing dictionary with no new words and keeps an old file as is)
    words <- full_dictionary

    # Save dictionary
    SemNetDictionaries::append.dictionary(
      words, dictionary.name = dictionary_name,
      save.location = save_location, textcleaner = TRUE
    )

    # Return success
    TRUE

  }, error = function(e){ # Save failed

    # Let user know
    message(
      "\nThe dictionary could not be saved to a file (", conditionMessage(e), ").",
      "\nIt was saved to your output object under '$dictionary' instead."
    )

    # Return failure
    FALSE

  })

}

#' @noRd
# Split unknown strings ----
# With `keepStrings = FALSE` strings are separated (`?textcleaner`): after the wizard every
# manually resolved element that contains a space and is not a dictionary entry (the
# dictionary includes anything the user added) is split into its words; entries such as
# "polar bear" stay whole
# Updated 14.10.2026
split_unknown_strings <- function(
    to, # corrected responses
    indices, # indices of manually checked responses
    full_dictionary # dictionary used to check strings
)
{

  # Loop over manually checked responses
  for(i in indices){

    # Get corrected elements
    elements <- to[[i]]

    # Check for nothing to split
    if(length(elements) == 0 || !is.character(elements)){

      # Skip to next response
      next

    }

    # Find elements with spaces
    has_space <- !is.na(elements) & grepl(" ", elements, fixed = TRUE)

    # Find elements that are not dictionary entries
    unknown <- has_space & !dictionary_contains(elements, full_dictionary)

    # Check for any unknown strings
    if(any(unknown)){

      # Convert to list
      pieces <- as.list(elements)

      # Split unknown strings into words
      pieces[unknown] <- lapply(elements[unknown], function(string){

        # Split on spaces
        words <- strsplit(trimws(string), "[[:space:]]+")[[1]]

        # Drop blank words
        words[nzchar(words)]

      })

      # Store split elements
      to[[i]] <- unlist(pieces)

    }

  }

  # Return corrected responses
  return(to)

}

#' @noRd
# Keep dictionary in results ----
# Puts the session's dictionary in the results: `$dictionary` is the whole dictionary (the
# one started from plus the added words, spelling-converted), which can be passed back as
# `textcleaner(dictionary = result$dictionary)`, and `$added_words` is only the words added
# during the session
# Updated 14.10.2026
keep_dictionary_in_results <- function(
    final_res, # results of the spell-check
    full_dictionary, # dictionary with additions
    orig_dictionary # dictionary before additions
)
{

  # Store whole dictionary
  final_res$dictionary <- full_dictionary

  # Store added words
  final_res$added_words <- setdiff(full_dictionary, orig_dictionary)

  # Return results
  return(final_res)

}

#' @noRd
# Announce go back to previous ----
# GO BACK from the first decision of a response returns to the previous response and asks
# about it again from its first word: everything decided for it is undone (dictionary
# additions and recorded changes included) and it is restored to the automated spell-check's
# result. Inside a multi-word response GO BACK undoes only the last word
# Updated 14.10.2026
announce_go_back_to_previous <- function(
    previous_response # response that is reopened
)
{

  # Let user know
  message(
    "\nGoing back to the previous response, '", paste(previous_response, collapse = " "),
    "'. Any changes have been undone."
  )

}
