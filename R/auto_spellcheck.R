#' @noRd
# Resolve an automated spell-check stage ----
# Checks the candidates of one stage of the pipeline against the dictionary and returns
# one of three outcomes: nothing resolved (revert to `fallback`, the input of the stage),
# everything resolved (finish), or some resolved (continue with the rest)
#
# `$auto` is always a numeric index vector of every response that is not left for manual
# review
# Updated 14.10.2026
resolve_auto_spellcheck_stage <- function(
    transformed, # candidates after this stage
    fallback, # candidates before this stage
    full_dictionary, # dictionary used to check words
    orig, # corrected responses so far
    names_ind # names of resolved responses
)
{

  # Match candidates to the dictionary
  targets <- match(unlist(transformed), full_dictionary)

  # No candidates left counts as "everything resolved" (`all(is.na())` of an empty vector is
  # `TRUE`), for example when the stop-word stage removed the last unresolved response
  if(length(targets) > 0 && all(is.na(targets))){

    # Nothing resolved -- discard this stage's transformation, revert to
    # what fed into it, and continue to the next stage unchanged
    return(list(done = FALSE, orig = orig, names_ind = names_ind, next_candidates = fallback))

  }else if(all(!is.na(targets))){ # Everything resolved

    # Everything resolved -- update orig and return immediately
    resolved <- which(!is.na(targets))

    # Update corrected responses
    orig[names(transformed)[resolved]] <- transformed[resolved]

    # Initialize results
    result <- list()

    # Set no manual responses
    result$manual <- numeric(0)

    # `names(orig)` is the full set of responses (only its values change), so responses
    # resolved in earlier stages stay in `$auto`
    result$auto <- setdiff(as.numeric(names(orig)), result$manual)

    # Store corrected responses
    result$to <- orig

    # Return finished result
    return(list(done = TRUE, result = result))

  }else{ # Some resolved

    # Some resolved -- update orig/names_ind, continue with the remainder
    resolved <- which(!is.na(targets))

    # Update corrected responses
    orig[names(transformed)[resolved]] <- transformed[resolved]

    # Add to resolved responses
    names_ind <- sort(c(names_ind, names(transformed)[resolved]))

    # Return remaining candidates
    return(list(
      done = FALSE, orig = orig, names_ind = names_ind,
      next_candidates = orig[-as.numeric(names_ind)]
    ))

  }

}

#' @noRd
# Automated spell-check ----
# Automated spell-check pipeline. In order: (1) responses already in the dictionary,
# (2) [free only] stop words and one- and two-letter responses, (3) singular forms,
# (4) monikers, (5) monikers of singular forms, (6) word-by-word best guesses,
# (7) merging and splitting of multi-word responses, and (8) splitting responses into
# dictionary words [fluency, or free without `keep_strings`]
#
# Free association skips the multi-word stages when `keep_length` is 1 (nothing to parse)
# and the splitting stage when `keep_strings = TRUE`. `cores = 1` is the default (the
# number of cores is not detected automatically)
# Updated 14.10.2026
auto_spellcheck <- function(
    check, # unique responses to check
    full_dictionary, # dictionary used to check words
    dictionary, # names of dictionaries requested
    spelling, # "UK" or "US" spelling
    keep_strings, # whether strings are kept
    type = c("fluency", "free"), # type of data
    keep_length = NULL, # number of words kept in free association
    cores = 1 # number of cores
)
{

  # Match type
  type <- match.arg(type)

  # Change names of indices
  names(check) <- formatC(
    seq_along(check),
    width = nchar(as.character(length(check))),
    format = "d", flag = "0"
  )

  # Keep full indices
  full_names <- names(check)

  # Save original responses
  orig <- check

  # Initialize resolved responses
  names_ind <- character(0)

  #-------------------------------#
  ## Correctly spelled responses ##
  #-------------------------------#

  # Let user know progress
  message("\nIdentifying correctly spelled responses...", appendLF = FALSE)

  # Check which responses are spelled correctly
  stage <- resolve_auto_spellcheck_stage(check, check, full_dictionary, orig, names_ind)

  # Return if all are resolved
  if(stage$done){

    return(stage$result)

  }

  # Update responses and remaining candidates
  orig <- stage$orig
  names_ind <- stage$names_ind
  check <- stage$next_candidates

  # Let user know remaining responses
  message(paste("done.\n(", length(check), " of ", length(orig), " unique responses still need to be corrected)", sep = ""))

  #---------------------------------------#
  ## [free only] Short and bad responses ##
  #---------------------------------------#

  # Check for free association
  if(type == "free"){

    # Remove stop words
    stop_words <- SemNetDictionaries::load.dictionaries("stop_words")

    # Build pattern of stop words
    stop_format <- paste("\\b", stop_words, "\\b", sep = "", collapse = "|")

    # Remove stop words
    uniq_resp <- gsub(stop_format, "", unlist(check))

    # Remove leading and trailing spaces
    uniq_resp <- trimws(uniq_resp)

    # Remove bad words
    bad_uniq_resp <- bad_response(uniq_resp)

    # Find bad responses
    bad_index <- which(is.na(bad_uniq_resp))

    # Remove single letters
    rm_letters <- letters[!dictionary_contains(letters, full_dictionary)]

    # Find single letters
    one_index <- which(uniq_resp %in% rm_letters)

    # Remove two letters
    two_letters <- get_two_letter_combinations()

    # Find two-letter combinations that are not words
    rm_letters <- two_letters[!dictionary_contains(two_letters, full_dictionary)]

    # Find two letters
    two_index <- which(uniq_resp %in% rm_letters)

    # Concatenate bad indices
    na_index <- unique(sort(c(bad_index, one_index, two_index)))

    # Name by response
    names(na_index) <- names(uniq_resp)[na_index]

    # Update original responses
    orig[names(na_index)] <- NA

    # Update correctly spelled indices
    names_ind <- unique(sort(c(names_ind, names(na_index))))

    # Remove responses from check
    check <- check[setdiff(names(check), names(na_index))]

  }

  #------------------------#
  ## Pluralized responses ##
  #------------------------#

  # Let user know progress
  message("\nSingularizing responses...", appendLF = FALSE)

  # Singularize responses
  sing <- lapply(check, singularize, dictionary = FALSE)

  # Check which singularized responses are correct
  stage <- resolve_auto_spellcheck_stage(sing, check, full_dictionary, orig, names_ind)

  # Return if all are resolved
  if(stage$done){

    return(stage$result)

  }

  # Update responses and remaining candidates
  orig <- stage$orig
  names_ind <- stage$names_ind
  sing <- stage$next_candidates

  # Let user know remaining responses
  message(paste("done.\n(", length(sing), " unique responses still need to be corrected)", sep = ""))

  #--------------------------------------------#
  ## Correct common misspellings and monikers ##
  #--------------------------------------------#

  # Check for dictionaries without monikers
  if(all(dictionary == "general") || all(dictionary == "hunspell")){

    # Skip moniker conversion
    mons2 <- sing

  }else if(any(dictionary %in% get_available_dictionaries())){ # Dictionaries with monikers

    # Let user know progress
    message("\nAuto-correcting common misspellings and monikers...", appendLF = FALSE)

    # Load monikers
    monikers <- get_monikers(dictionary)

    if(length(monikers) != 0){ # Checks in case of only using general dictionary

      # Check for monikers
      mons <- unlist(lapply(sing, moniker, monikers, spelling = spelling), recursive = FALSE)

      # Check which converted responses are correct
      stage <- resolve_auto_spellcheck_stage(mons, sing, full_dictionary, orig, names_ind)

      # Return if all are resolved
      if(stage$done){

        return(stage$result)

      }

      # Update responses and remaining candidates
      orig <- stage$orig
      names_ind <- stage$names_ind
      mons <- stage$next_candidates

      # Check for pluralized monikers
      mons2 <- unlist(
        lapply(mons, function(x, monikers, spelling){

          # Convert monikers of the singular form
          moniker(singularize(x, dictionary = FALSE), monikers, spelling)

        }, monikers, spelling = spelling),
        recursive = FALSE
      )

      # Check which pluralized monikers are correct
      stage <- resolve_auto_spellcheck_stage(mons2, mons, full_dictionary, orig, names_ind)

      # Return if all are resolved
      if(stage$done){

        return(stage$result)

      }

      # Update responses and remaining candidates
      orig <- stage$orig
      names_ind <- stage$names_ind
      mons2 <- stage$next_candidates

    }else{ # No monikers

      # Skip moniker conversion
      mons2 <- sing

    }

    # Let user know progress
    message("done.")

  }else{ # No dictionaries with monikers

    # Skip moniker conversion
    mons2 <- sing

  }

  #------------------------------#
  ## Individualized spell-check ##
  #------------------------------#

  # Let user know progress
  message(paste("\nAttempting to auto-correct the remaining", length(mons2), "responses individually..."), appendLF = FALSE)

  # Let user know about parallel processing
  message("\nUsing parallel processing to speed up individual word check...")

  # Correct each response word by word
  ind_check <- parallel_process(
    length(mons2),
    function(i){

      # Correct response word by word
      ind_word_check(mons2[[i]], full_dictionary = full_dictionary, dictionary = dictionary, spelling = spelling)

    },
    cores = cores
  )

  # Name by response
  names(ind_check) <- names(mons2)

  # Flatten corrections
  ind_check <- unlist(ind_check, recursive = FALSE)

  # Check which corrections are correct
  stage <- resolve_auto_spellcheck_stage(ind_check, mons2, full_dictionary, orig, names_ind)

  # Return if all are resolved
  if(stage$done){

    return(stage$result)

  }

  # Update responses and remaining candidates
  orig <- stage$orig
  names_ind <- stage$names_ind
  ind_check <- stage$next_candidates

  # Let user know remaining responses
  message(paste("(", length(ind_check), " unique responses still need to be corrected)", sep = ""))

  #-------------------------------------------#
  ## Parse strings with multi-word responses ##
  #-------------------------------------------#

  # Fluency always parses multi-word responses; free skips entirely when
  # keep_length is exactly 1 (single-word-only data has nothing to parse)
  parse_multi_word <- (type == "fluency") || is.null(keep_length) || keep_length > 1

  # Check for multi-word parsing
  if(parse_multi_word){

    # Let user know progress
    message("\nParsing multi-word responses...", appendLF = FALSE)

    # Set multiple word minimum in response to be considered for split
    multi_min <- get_multi_min(full_dictionary)

    # Let user know about parallel processing
    message("\nUsing parallel processing to speed up multiple word check...")

    # Merge and correct multi-word responses
    multi_word <- parallel_process(
      length(ind_check),
      function(i){

        # Merge and correct multi-word response
        multiple_words(
          ind_check[[i]], multi_min = multi_min, full_dictionary = full_dictionary,
          dictionary = dictionary, spelling = spelling, keep_strings = keep_strings
        )

      },
      cores = cores
    )

    # Name by response
    names(multi_word) <- names(ind_check)

    # Check responses that changed
    changed <- vapply(multi_word, function(x){ any(x$correct) }, logical(1))

    # Update original responses
    if(!keep_strings){

      # Get changed responses
      responses <- lapply(multi_word[changed], function(x){ x$response })

      # Check for any changed responses
      if(length(responses) != 0){

        # Update original responses
        orig[names(responses)] <- responses

      }

    }else{ # Strings are kept

      # Update the original responses (also with `keep_strings = TRUE`, which free association
      # needs to rebuild its cues and IDs)
      responses <- lapply(multi_word[changed], function(x){ x$response })

      # Check for any responses
      if(length(responses) != 0){

        # Update original responses
        orig[names(responses)] <- responses

      }

    }

    # Indices of correctly spelled responses
    all_correct <- vapply(multi_word, function(x){ all(x$correct) }, logical(1))

    # Check for fully correct responses
    if(length(names(multi_word)[all_correct]) != 0){

      # Add to resolved responses
      names_ind <- sort(c(names_ind, names(multi_word)[all_correct]))

    }

    # Always rebuild what is left to review from `orig` by name: `multi_word` can still hold
    # the raw results of `multiple_words()`, and an empty `names_ind` must not become an
    # empty negative index
    multi_word <- orig[setdiff(names(orig), names_ind)]

    # Search through responses that can be individually split into
    # separate responses (fluency: always; free: only if !keep_strings)
    if(type == "fluency" || !isTRUE(keep_strings)){

      # Split responses into dictionary words
      multi_word <- lapply(multi_word, response_splitter, full_dictionary = full_dictionary)

      # Find fully split responses
      fully_split <- vapply(multi_word, function(x){ all(dictionary_contains(x, full_dictionary)) }, logical(1))

      # Check for any fully split responses
      if(length(multi_word[fully_split]) != 0){

        # Update original responses
        orig[names(multi_word)[fully_split]] <- multi_word[fully_split]

        # Add to resolved responses
        names_ind <- sort(c(names_ind, names(multi_word)[fully_split]))

        # Remove from remaining responses
        multi_word <- orig[-as.numeric(names_ind)]

      }

    }

  }else{ # No multi-word parsing

    # Keep remaining responses
    multi_word <- ind_check

  }

  # Let user know how many responses need to be spell-checked
  if(length(multi_word) != 0){

    # Let user know how many remain
    message(paste("\nAutomated spell-checking complete.\nAbout ", length(multi_word), " responses still need to be manually spell-checked", sep = ""))

  }else{ # All responses passed

    # Let user know all passed
    message("\nAll words passed automated spell-check. Ending spell-check...")

  }

  # Initialize result list
  res <- list()

  # Set manual responses
  res$manual <- as.numeric(names(multi_word))

  # Set automated responses
  res$auto <- setdiff(as.numeric(full_names), res$manual)

  # Set corrected responses
  res$to <- orig

  # Return results
  return(res)

}
