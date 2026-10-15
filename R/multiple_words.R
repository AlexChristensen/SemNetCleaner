#' @noRd
# Multiple words ----
# Tries to de-combine a multi-word response against the dictionary ("fish dog cat" to
# "fish" "dog" "cat") and to repair accidentally split dictionary terms by merging
# adjacent words ("black" "board" to "blackboard")
#
# Words are visited with a position pointer that only moves forward, because merging
# shrinks the word vector and positions computed beforehand would go stale. The monikers
# are loaded once and passed to every `moniker()` call
# Updated 14.10.2026
multiple_words <- function(
    vec, # response string
    multi_min, # minimum words to try merging
    full_dictionary, # dictionary used to check words
    dictionary, # names of dictionaries requested
    spelling, # "UK" or "US" spelling
    keep_strings = FALSE # whether strings are kept
)
{

  # Original word
  original_word <- vec

  # Split vector
  split_words <- unlist(strsplit(vec, " "))

  # Remove bad responses
  split_words <- na.omit(bad_response(split_words))

  # Try to combine multiple words
  combined <- paste(split_words, sep = "", collapse = "")

  # Initialize result list
  res <- list()

  # Check if combined exists
  if(dictionary_contains(combined, full_dictionary)){

    # Set response to the combined word
    res$response <- combined

    # Mark as correct
    res$correct <- TRUE

    # Return result
    return(res)

  }

  # Check for multiple words
  if(length(split_words) >= multi_min){

    # Loaded once and shared by every moniker() call below (memoized, since this function runs
    # once per response with the same dictionary)
    monikers <- get_monikers(dictionary)

    # The word before and the word after `words[i]`, each merged with it
    # (with or without a space, through the monikers), when exactly ONE of the
    # two spellings is a dictionary entry; NA otherwise
    merge_candidates <- function(words, i){

      # Initialize merge with previous word
      merge_before <- NA_character_

      # Initialize merge with next word
      merge_after <- NA_character_

      ## Check word before
      if(i != 1){

        # Get merged spellings (with and without space)
        spacing <- c(paste(words[i - 1], words[i]), paste(words[i - 1], words[i], sep = ""))

        ### Convert monikers
        spacing <- unique(unlist(lapply(spacing, moniker, misnom = monikers, spelling = spelling)))

        ### Check for only one solution
        spacing_found <- dictionary_contains(spacing, full_dictionary)

        # Check for only one match
        if(sum(spacing_found) == 1){

          # Store merged entry
          merge_before <- spacing[which(spacing_found)]

        }

      }

      ## Check word after
      if(i != length(words)){

        # Get merged spellings (with and without space)
        spacing <- c(paste(words[i], words[i + 1]), paste(words[i], words[i + 1], sep = ""))

        ### Convert monikers
        spacing <- unique(unlist(lapply(spacing, moniker, misnom = monikers, spelling = spelling)))

        ### Check for only one solution
        spacing_found <- dictionary_contains(spacing, full_dictionary)

        # Check for only one match
        if(sum(spacing_found) == 1){

          # Store merged entry
          merge_after <- spacing[which(spacing_found)]

        }

      }

      # Return merge candidates
      list(before = merge_before, after = merge_after)

    }

    # Position pointer, only ever advanced -- never re-derived from a
    # stale, pre-computed index set (bug fix; see the file header comment)
    i <- 1

    # Loop over words
    while(i <= length(split_words)){

      # Already a real word -- nothing to do, move on
      if(dictionary_contains(split_words[i], full_dictionary)){

        # Move to next word
        i <- i + 1

        # Skip to next word
        next

      }

      # Merge candidates for the word as typed come first: adjacent words that together are a
      # dictionary entry ("sea sponge") must be merged before any guess, which changes "sea" to "seal"
      merges <- merge_candidates(split_words, i)

      # Check for no merge candidates
      if(is.na(merges$before) && is.na(merges$after)){

        # Individual check
        individual_guess <- best_guess(split_words[i], full_dictionary = full_dictionary, dictionary = dictionary)

        # Update based on individual guess
        if(length(individual_guess) == 1){

          # Use individual guess
          split_words[i] <- individual_guess

        }

        # Recompute merge candidates
        merges <- merge_candidates(split_words, i)

      }

      # Get merge with previous word
      merge_before <- merges$before

      # Get merge with next word
      merge_after <- merges$after

      # 'merge_before' and 'merge_after' were selected from confirmed dictionary entries, so only
      # the NA check is needed
      has_merge_before <- !is.na(merge_before)

      # Check for merge with next word
      has_merge_after <- !is.na(merge_after)

      # Verify singular multi-word solution
      if(xor(has_merge_before, has_merge_after)){

        ## Change response, remove the merged-away neighbour. `merged` is the
        ## position of the merged entry afterwards
        if(has_merge_before){

          # Merge with previous word
          split_words[i] <- merge_before

          # Remove merged word
          split_words <- split_words[-(i - 1)]

          # Set position of merged entry
          merged <- i - 1

        }else{

          # Merge with next word
          split_words[i] <- merge_after

          # Remove merged word
          split_words <- split_words[-(i + 1)]

          # Set position of merged entry
          merged <- i

        }

        ## A merged entry can itself be part of a longer one ("cocker
        ## spaniel" + "english" = "english cocker spaniel"): keep merging
        ## while exactly one neighbour completes a dictionary entry
        repeat{

          # Get merge candidates of merged entry
          more <- merge_candidates(split_words, merged)

          # Check for only a previous merge
          if(!is.na(more$before) && is.na(more$after)){

            # Merge with previous word
            split_words[merged] <- more$before

            # Remove merged word
            split_words <- split_words[-(merged - 1)]

            # Update position
            merged <- merged - 1

          }else if(is.na(more$before) && !is.na(more$after)){ # Only a next merge

            # Merge with next word
            split_words[merged] <- more$after

            # Remove merged word
            split_words <- split_words[-(merged + 1)]

          }else{ # No further merges

            # Stop merging
            break

          }

        }

        ## The next unprocessed word follows the merged entry
        i <- merged + 1

      }else{ # No unambiguous merge

        # No unambiguous merge; move on regardless of the individual-guess outcome
        i <- i + 1

      }

    }

    # Check for common misspellings and monikers
    vec <- drop_repeated_entry_words(unlist(lapply(split_words, moniker, misnom = monikers, spelling = spelling)))

  }else if(length(split_words) > 1){ # Fewer words than minimum

    # Correct if only one best guess exists
    guess <- best_guess(vec, full_dictionary = full_dictionary, dictionary = dictionary)

    # A guess for the whole string is only taken when it keeps every word that is already a
    # dictionary word ("tardigrade goose" must not become "tardigrade")
    valid_words <- split_words[dictionary_contains(split_words, full_dictionary)]

    # Use guess when it keeps every valid word
    if(length(guess) == 1 && all(vapply(valid_words, grepl, TRUE, x = guess, fixed = TRUE))){

      vec <- guess # Use guess

    }

  }

  # Check for keeping strings
  if(isTRUE(keep_strings)){

    # Combine words
    combined_word <- paste(
      vec, sep = "", collapse = " "
    )

    # Check against original word
    if(
      original_word == combined_word &&
      !all(dictionary_contains(unique(vec), full_dictionary))
    ){

      # Keep original string
      res$response <- original_word

      # Mark as uncorrected
      res$correct <- FALSE

    }else{ # Use corrected string

      # Set corrected string
      res$response <- combined_word

      # Mark as correct
      res$correct <- TRUE

    }

  }else{ # Split strings

    # Set unique words
    res$response <- unique(vec)

    # Check each word against dictionary
    res$correct <- dictionary_contains(unique(vec), full_dictionary)

  }

  # Return result
  return(res)

}
