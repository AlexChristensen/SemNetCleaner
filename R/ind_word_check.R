#' @noRd
# Ind word check ----
# Auto-corrects a response string word by word with `best_guess()`, then converts
# monikers and the UK/US spelling of the re-joined response
# Updated 14.10.2026
ind_word_check <- function(
    string, # response string to correct
    full_dictionary, # dictionary used to check words
    dictionary, # names of dictionaries requested
    spelling # "UK" or "US" spelling
)
{

  # Make string only alphanumeric characters
  string <- gsub("[^[:alnum:][:space:]]", "", string)

  # Split string
  split_string <- unlist(strsplit(string, " "))

  # Remove bad responses
  split_string <- na.omit(bad_response(split_string))

  # Words that together are a dictionary entry ("sea sponge") are kept together, longest
  # entry (up to 4 words) first, because guessing each word alone changes "sea" to "seal"
  # Initialize units (single words or kept-together entries)
  units <- list()

  # Initialize word position
  position <- 1

  # Get number of words
  n_words <- length(split_string)

  # Loop over words
  while(position <= n_words){

    # Default to taking one word
    take <- 1

    # Get number of words remaining
    remaining <- n_words - position + 1

    # Check for at least two words remaining
    if(remaining >= 2){

      # Loop from longest to shortest candidate entry
      for(len in seq(min(remaining, 4), 2, by = -1)){

        # Check whether the words form a dictionary entry
        if(dictionary_contains(paste(split_string[position:(position + len - 1)], collapse = " "), full_dictionary)){

          # Take the whole entry
          take <- len

          # Stop at the longest entry
          break

        }

      }

    }

    # Store unit
    units[[length(units) + 1]] <- paste(split_string[position:(position + take - 1)], collapse = " ")

    # Move to next unit
    position <- position + take

  }

  # Flatten units
  units <- unlist(units)

  # Apply best guess across words (and the entries kept together)
  guesses <- lapply(units, function(unit){

    # Check for multi-word entry
    if(grepl(" ", unit, fixed = TRUE)){

      # Keep entry as is
      unit

    }else{

      # Guess single word
      best_guess(unit, full_dictionary = full_dictionary, dictionary = dictionary, tolerance = 1)

    }

  })

  # Check for multiple best guesses
  guess_lengths <- unlist(lapply(guesses, length))

  # Replace non-automated responses with the original word
  if(any(guess_lengths > 1)){

    # Replace with original words
    guesses[which(guess_lengths > 1)] <- units[which(guess_lengths > 1)]

  }

  # Convert guesses back into a stringed response
  response <- paste(drop_repeated_entry_words(unlist(guesses)), collapse = " ")

  # Re-check for monikers when a topic dictionary was requested
  if(any(dictionary %in% get_available_dictionaries())){

    # Convert monikers
    response <- moniker(word = response, misnom = get_monikers(dictionary), spelling = spelling)

  }

  # Return corrected response
  return(response)

}
