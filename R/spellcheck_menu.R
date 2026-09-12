#' @noRd
# Builds and emits the position-aware "Response was <verb>: '...'" message
# used by every context-branch (multi-word) word-option handler when
# `keep_strings` is TRUE -- shows the surrounding context with the target
# word replaced/bolded, correctly handling first/last/middle position in
# the string. Extracted from 4 byte-identical first/middle/last if-else
# blocks inside `spellcheck.menu()` (SKIP WORD, ADD WORD TO DICTIONARY,
# TYPE MY OWN WORD, BAD WORD, and the response-option branch).
# Real bug fixed (#53): the middle-position branch located the target's
# own index via `which(context[check] == unlist(strsplit(context, split =
# " ")))` -- a VALUE match, not a position lookup -- instead of just using
# `check` directly. Since every element of `context` is already a single
# token (space-splitting it is a no-op here; `unlist(strsplit(context, "
# "))` is always identical to `context` itself), this only ever needed to
# be `check`. Confirmed reachable and real via direct reproduction:
# whenever the target word's value recurs elsewhere in the same multi-word
# response (e.g. `context = c("the","cat","sat","cat")`, `check = 4`),
# `which()` returns EVERY matching position (here `c(2, 4)`), and the
# subsequent `1:(word_num - 1)` / `(word_num + 1):n_words` arithmetic
# silently recycles/truncates to just the first match's position with an
# "only the first element used" warning -- displaying the confirmation
# message built around the WRONG occurrence of the word. Inherited
# byte-identically from the untouched original (5 separate copies of this
# exact pattern in `utils-SemNetCleaner.R`). Cosmetic-only (this function
# only builds a user-facing message; `changes`/`context` themselves are
# never touched here), but a real, silently-wrong-output bug all the same.
# Updated 10.09.2026
describe_word_change <- function(context, check, verb, replacement)
{

  n_words <- length(context)
  styled <- style_text(replacement, defaults = "bold")

  if(check == 1){

    message(paste("\nResponse was ", verb, ": '",
                  styled, " ", paste(context[-check], collapse = " "), "'",
                  sep = "", collapse = " "))

  }else if(check == n_words){

    message(paste("\nResponse was ", verb, ": '",
                  paste(context[-check], collapse = " "), " ", styled, "'",
                  sep = "", collapse = " "))

  }else{

    message(paste("\nResponse was ", verb, ": '",
                  paste(context[1:(check - 1)], collapse = " "), " ", styled, " ",
                  paste(context[(check + 1):n_words], collapse = " "), "'",
                  sep = "", collapse = " "))

  }

}

#' @noRd
# For every one of `words` not already in `full_dictionary`, asks whether it
# should be added and does so on "Yes". Extracted from 3 byte-identical
# copies inside `spellcheck.menu()` (TYPE MY OWN WORD, TYPE MY OWN STRING,
# and the single-response TYPE MY OWN). `seq_along(non_dict)` used in place
# of the original's `1:length(non.dict)` for the established reverse-range
# discipline, though not a live bug here -- this loop is only ever reached
# when `non_dict` has at least one element.
# Robustness/speed fix, per direct request: the original (and this
# rebuild's first pass) called `words %in% full_dictionary` TWICE --
# once to decide whether to enter the loop at all, again to compute
# `non_dict` -- and `%in%` rebuilds its lookup from scratch every call, an
# especially real cost against `cocaspell` (~109k words, the default
# `free`-association dictionary). Replaced with one
# `dictionary_contains()` call (helpers.R's memoized/hashed lookup,
# already the established pattern at every other dictionary-membership
# check site) feeding both the entry check and `non_dict`.
# Updated 10.09.2026
offer_add_missing_to_dictionary <- function(words, full_dictionary, reader = readline)
{

  in_dictionary <- dictionary_contains(words, full_dictionary)

  if(!all(in_dictionary)){

    non_dict <- which(!in_dictionary)

    for(i in seq_along(non_dict)){

      message(paste("\n'", words[non_dict[i]], "'", sep = ""), " was not found in the dictionary.\n")

      dict_ans <- yes_no_menu(
        paste("Should ", paste("'", words[non_dict[i]], "'", sep = ""), " be added to the dictionary", sep = ""),
        reader = reader
      )

      if(dict_ans == 1){
        full_dictionary <- SemNetDictionaries::append.dictionary(
          words[non_dict[i]], full_dictionary,
          dictionary.name = "full", save.location = "envir", textcleaner = TRUE
        )
      }

    }

  }

  full_dictionary

}

#' @noRd
# The actual per-response manual spell-check decision menu -- the wizard's
# core interaction, called once per word (context-branch, for a multi-word
# response) or once per response (single-response branch) from
# `spellcheck_dictionary()`. Ported from `spellcheck.menu()`, rebuilt on
# `read_input()`/a `reader = readline` parameter (binding decision #3) and
# a `search_fn = searcher::search_site` parameter (same rationale -- a real
# browser launch is just as wrong to fire during a test as a real
# `readline()` block).
#
# Real bugs fixed:
# - Bug #7 (3 identical `collpase = ""` typos, GOOGLE WORD/STRING/IT): a
#   misspelled argument name silently absorbed by `paste()`'s `...` instead
#   of setting `collapse`. Fixed to `collapse = ""` at all 3 sites.
# - Bug #32: the "word already in dictionary" fast path built
#   `res$target <- ifelse(is.null(context), dict_check, context)` --
#   `ifelse()` is vectorized on its `test` argument (length 1 here), so it
#   always returns a length-1 result, silently truncating a multi-word
#   `context` down to just `context[1]` instead of the whole vector.
#   Confirmed via direct reproduction. Fixed via a plain `if`/`else`.
# - Bug #33: the BAD STRING branch (context mode) never set `end <- TRUE`,
#   unlike every other whole-string-resolving branch (KEEP ORIGINAL, KEEP
#   AUTO-CORRECT, TYPE MY OWN STRING). Confirmed via direct reproduction
#   against the untouched original: choosing BAD STRING sets every word in
#   `context` to `"NA"` but leaves `res$end` `FALSE`, so
#   `spellcheck_dictionary()`'s caller-side multi-word loop does not stop
#   early -- it continues to the next not-yet-checked word position, now
#   searching for it inside a `context` that has already been fully
#   overwritten to `"NA"`, finding nothing (`which(...)` returns
#   `integer(0)`) and calling this menu again with `check = integer(0)`.
# Updated 09.09.2026
spellcheck_menu <- function(
  check, context = NULL, possible, original,
  current_index, changes, full_dictionary, category,
  dictionary, keep_strings, go_back_count,
  reader = readline, search_fn = searcher::search_site
)
{

  ans <- 30
  go_back <- FALSE

  # Check if word is already in dictionary (due to dictionary updates)
  if(!is.null(context)){
    dict_check <- unique(context[check])
  }else{
    dict_check <- check
  }

  if(dictionary_contains(dict_check, full_dictionary)){

    if(!is.null(context)){
      message(paste("\nResponse '",
                    style_text(dict_check, defaults = "bold"), "' in '", style_text(paste(context, collapse = " "), defaults = "bold"),
                    "' was KEPT AS IS based on a previous manual spell-check decision",
                    sep = ""))
    }else{
      message(paste("\nResponse '",
                    style_text(dict_check, defaults = "bold"),
                    "' was KEPT AS IS based on a previous manual spell-check decision",
                    sep = ""))
    }

    res <- list()
    res$go_back <- go_back
    res$target <- if(is.null(context)) dict_check else context
    res$changes <- changes
    res$full_dictionary <- full_dictionary
    res$end <- if(is.null(context)) NULL else FALSE
    res$go_back_count <- go_back_count + 1

    if(!"general" %in% dictionary){
      Sys.sleep(1)
    }

    return(res)

  }

  # Set up based on context (multiple responses)
  if(!is.null(context)){

    end <- FALSE

    while(ans == 30){

      if(keep_strings){

        title <- paste(paste("\nOriginal string: ", "'", original, "'", sep = ""),
                       paste("Target word: ", paste("'", color_text(context[check], defaults = "highlight"), "'", sep = ""), sep = ""),
                       sep = "\n\n")

      }else{

        title <- paste(paste("\nOriginal string: ", "'", original, "'", sep = ""),
                       paste("Auto-corrected string: ", paste("'", context, "'", sep = "", collapse = " "), sep = ""),
                       paste("Target word: ", paste("'", color_text(context[check], defaults = "highlight"), "'", sep = ""), sep = ""),
                       sep = "\n\n")

      }

      word <- c("SKIP WORD", "ADD WORD TO DICTIONARY", "TYPE MY OWN WORD", "GOOGLE WORD", "BAD WORD")
      if(keep_strings){
        string <- c("KEEP ORIGINAL", "TYPE MY OWN STRING", "GOOGLE STRING", "BAD STRING")
      }else{
        string <- c("KEEP ORIGINAL", "KEEP AUTO-CORRECT", "TYPE MY OWN STRING", "GOOGLE STRING", "BAD STRING")
      }

      choices <- c(word, string, possible)
      default <- length(c(word, string))

      custom_menu(choices = choices, title = title, default = default)

      message("Press 'B' to GO BACK, 'H' for HELP, or 'X' to EXIT.\n")

      ans <- read_input("Selection (accepts lowercase): ", reader = reader)

      if(tolower(ans) == "x" || ans == ""){return("STOP")}

      original_ans <- tolower(ans)

      ans <- appropriate_answer(answer = ans, choices = choices, default = default, reader = reader)

      if(keep_strings){
        if(ans >= 7 && ans <= length(choices)){
          ans <- ans + 1
        }
      }

      if(ans == 1){ # SKIP WORD

        if(keep_strings){
          describe_word_change(context, check, "SKIPPED", context[check])
        }else{
          message(paste("\nResponse was SKIPPED:", paste("'", context[check], "'", sep = "")))
        }

      }else if(ans == 2){ # ADD WORD TO DICTIONARY

        full_dictionary <- SemNetDictionaries::append.dictionary(context[check],
                                                                  full_dictionary,
                                                                  dictionary.name = "full",
                                                                  save.location = "envir", textcleaner = TRUE)

        if(keep_strings){
          describe_word_change(context, check, "KEPT AS", context[check])
        }

      }else if(ans == 3){ # TYPE MY OWN WORD

        message("\nType '30' (no quotations) to go back to the other response options\n")

        tmo <- read_input("Use commas for multiple words (dog, fish, etc.): ", reader = reader)

        if(tmo != "30"){

          tmo_split <- trimws(unlist(strsplit(tmo, split = ",")))

          change_mat <- t(as.matrix(c(context[check], tmo_split)))
          colnames(change_mat) <- c("from", paste("to", 1:(ncol(change_mat) - 1), sep = "_"))

          changes <- change_format(change_list = changes,
                                   change_matrix = change_mat,
                                   current_index = current_index)

          full_dictionary <- offer_add_missing_to_dictionary(tmo_split, full_dictionary, reader = reader)

          if(keep_strings){
            describe_word_change(context, check, "CHANGED TO", paste(tmo_split, collapse = " "))
          }else{
            message(paste("\nResponse was CHANGED TO:", paste("'", tmo_split, "'", sep = "", collapse = " ")))
          }

          context_change <- as.list(context)
          context_change[[check]] <- tmo_split
          context <- unlist(context_change)

        }else{ans <- 30}

      }else if(ans == 4){ # GOOGLE WORD

        search_fn(paste(category, " '", context[check], "'", sep = "", collapse = ""),
                  site = "google", rlang = FALSE)

        ans <- 30

      }else if(ans == 5){ # BAD WORD

        change_mat <- t(as.matrix(c(context[check], NA)))
        colnames(change_mat) <- c("from", paste("to", 1:(ncol(change_mat) - 1), sep = "_"))

        changes <- change_format(change_list = changes,
                                 change_matrix = change_mat,
                                 current_index = current_index)

        if(keep_strings){
          describe_word_change(context, check, "CHANGED TO", NA)
        }else{
          message(paste("\nResponse was CHANGED TO:", paste(NA, sep = "", collapse = " ")))
        }

        context_change <- as.list(context)
        context_change[[check]] <- "NA"
        context <- unlist(context_change)

      }else if(ans == 6){ # KEEP ORIGINAL

        message(paste("\nString was REVERTED TO ORIGINAL:", paste("'", original, "'", sep = "")))

        context <- original
        end <- TRUE

        if(!dictionary_contains(original, full_dictionary)){

          message(paste("\n'", original, "'", sep = ""),
                  " was not found in the dictionary.\n")

          dict_ans <- yes_no_menu(
            paste("Should ",
                  paste("'", original, "'", sep = ""),
                  " be added to the dictionary", sep = ""),
            reader = reader
          )

          if(dict_ans == 1){
            full_dictionary <- SemNetDictionaries::append.dictionary(original,
                                                                     full_dictionary,
                                                                     dictionary.name = "full",
                                                                     save.location = "envir", textcleaner = TRUE)
          }
        }

      }else if(ans == 7){ # KEEP AUTO-CORRECT

        message(paste("\nString was KEPT AS AUTO-CORRECT:", paste("'", context, "'", sep = "", collapse = " ")))

        context <- context
        end <- TRUE

      }else if(ans == 8){ # TYPE MY OWN STRING

        message("\nType '30' (no quotations) to go back to the other response options\n")

        ams <- read_input("Use commas for multiple words (dog, fish, etc.): ", reader = reader)

        if(ams != "30"){

          ams_split <- trimws(unlist(strsplit(ams, split = ",")))

          change_mat <- t(as.matrix(c(paste(context, collapse = " "), ams_split)))
          colnames(change_mat) <- c("from", paste("to", 1:(ncol(change_mat) - 1), sep = "_"))

          changes <- change_format(change_list = changes,
                                   change_matrix = change_mat,
                                   current_index = current_index)

          full_dictionary <- offer_add_missing_to_dictionary(ams_split, full_dictionary, reader = reader)

          message(paste("\nString was CHANGED TO:", paste("'", ams_split, "'", sep = "", collapse = " ")))

          context <- ams_split
          end <- TRUE

        }else{ans <- 30}

      }else if(ans == 9){ # GOOGLE STRING

        search_fn(paste(category, " '", original, "'", sep = "", collapse = ""),
                  site = "google", rlang = FALSE)

        ans <- 30

      }else if(ans == 10){ # BAD STRING

        change_mat <- cbind(context, rep(NA, length(context)))
        colnames(change_mat) <- c("from", paste("to", 1:(ncol(change_mat) - 1), sep = "_"))

        changes <- change_format(change_list = changes,
                                 change_matrix = change_mat,
                                 current_index = current_index)

        message(paste("\nALL responses were CHANGED TO:", paste(NA, sep = "", collapse = " ")))

        context <- rep("NA", length(context))
        end <- TRUE

      }else if(ans == length(choices) + 1 & original_ans == "b"){ # GO BACK

        go_back <- TRUE

      }else if(ans == length(choices) + 2 & original_ans == "h"){ # HELP

        textcleaner_help(check, context, original, possible, reader = reader)

        ans <- 30

      }else{ # RESPONSE OPTION

        change_mat <- t(as.matrix(c(context[check], possible[ans - 10])))
        colnames(change_mat) <- c("from", paste("to", 1:(ncol(change_mat) - 1), sep = "_"))

        changes <- change_format(change_list = changes,
                                 change_matrix = change_mat,
                                 current_index = current_index)

        if(keep_strings){
          describe_word_change(context, check, "CHANGED TO", possible[ans - 10])
        }else{
          message(paste("\nResponse was CHANGED TO:", paste("'", possible[ans - 10], "'", sep = "")))
        }

        context[check] <- possible[ans - 10]
      }
    }

    res <- list()
    res$go_back <- go_back
    res$target <- context
    res$changes <- changes
    res$full_dictionary <- full_dictionary
    res$end <- end
    res$go_back_count <- go_back_count

    if(!"general" %in% dictionary){
      Sys.sleep(1)
    }

    return(res)

  }else{

    # Single response
    while(ans == 30){

      title <- paste(paste("\nTarget word: ", paste("'", color_text(check, defaults = "highlight"), "'", sep = ""), sep = ""))

      choices <- c("SKIP", "ADD TO DICTIONARY", "TYPE MY OWN", "GOOGLE IT", "BAD WORD", possible)

      custom_menu(choices = choices, title = title, default = 5)

      message("Press 'B' to GO BACK, 'H' for HELP, or 'X' to EXIT.\n")

      ans <- read_input("Selection (accepts lowercase): ", reader = reader)

      if(tolower(ans) == "x" || ans == ""){return("STOP")}

      ans <- appropriate_answer(answer = ans, choices = choices, default = 5, reader = reader)

      if(ans == 1){ # SKIP

        message(paste("\nResponse was SKIPPED:", paste("'", check, "'", sep = "")))

      }else if(ans == 2){ # ADD TO DICTIONARY

        full_dictionary <- SemNetDictionaries::append.dictionary(check,
                                                                  full_dictionary,
                                                                  dictionary.name = "full",
                                                                  save.location = "envir", textcleaner = TRUE)

      }else if(ans == 3){ # TYPE MY OWN

        message("\nType '30' (no quotations) to go back to the other response options\n")

        tmo <- read_input("Use commas for multiple words (dog, fish, etc.): ", reader = reader)

        if(tmo != "30"){

          tmo_split <- trimws(unlist(strsplit(tmo, split = ",")))

          change_mat <- t(as.matrix(c(check, tmo_split)))
          colnames(change_mat) <- c("from", paste("to", 1:(ncol(change_mat) - 1), sep = "_"))

          changes <- change_format(change_list = changes,
                                   change_matrix = change_mat,
                                   current_index = current_index)

          full_dictionary <- offer_add_missing_to_dictionary(tmo_split, full_dictionary, reader = reader)

          message(paste("\nResponse was CHANGED TO:", paste("'", tmo_split, "'", sep = "", collapse = " ")))

          check <- tmo_split

        }else{ans <- 30}

      }else if(ans == 4){ # GOOGLE IT

        search_fn(paste(category, " '", check, "'", sep = "", collapse = ""),
                  site = "google", rlang = FALSE)

        ans <- 30

      }else if(ans == 5){ # BAD RESPONSE

        change_mat <- t(as.matrix(c(check, NA)))
        colnames(change_mat) <- c("from", paste("to", 1:(ncol(change_mat) - 1), sep = "_"))

        changes <- change_format(change_list = changes,
                                 change_matrix = change_mat,
                                 current_index = current_index)

        message(paste("\nResponse was CHANGED TO:", paste(NA, sep = "", collapse = " ")))

        check <- "NA"

      }else if(ans == length(choices) + 1){ # GO BACK

        go_back <- TRUE

      }else if(ans == length(choices) + 2){ # HELP

        textcleaner_help(check, context, original, possible, reader = reader)

        ans <- 30

      }else{ # RESPONSE OPTION

        change_mat <- t(as.matrix(c(check, possible[ans - 5])))
        colnames(change_mat) <- c("from", paste("to", 1:(ncol(change_mat) - 1), sep = "_"))

        changes <- change_format(change_list = changes,
                                 change_matrix = change_mat,
                                 current_index = current_index)

        message(paste("\nResponse was CHANGED TO:", paste("'", possible[ans - 5], "'", sep = "")))

        check <- possible[ans - 5]
      }
    }

    res <- list()
    res$go_back <- go_back
    res$target <- check
    res$changes <- changes
    res$full_dictionary <- full_dictionary
    res$go_back_count <- go_back_count

    if(!"general" %in% dictionary){
      Sys.sleep(1)
    }

    return(res)
  }
}
