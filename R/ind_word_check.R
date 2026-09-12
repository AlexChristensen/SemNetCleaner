#' @noRd
# Auto-corrects a single (possibly multi-word) response string, word by
# word, via `best_guess()`, falling back to the moniker/UK-US conversion
# for the whole re-joined response. Ported from `ind.word.check()`. Minor
# cleanup: renamed the local `lengths` variable (it shadowed base R's own
# `lengths()` function, harmless here since `lengths()` is never called in
# this function, but confusing to read).
# Updated 09.09.2026
ind_word_check <- function(string, full_dictionary, dictionary, spelling)
{

  # Make string only alphanumeric characters
  string <- gsub("[^[:alnum:][:space:]]", "", string)

  # Split string
  split_string <- unlist(strsplit(string, " "))

  # Remove bad responses
  split_string <- na.omit(bad_response(split_string))

  # Apply best guess across words
  guesses <- lapply(split_string, best_guess, full_dictionary = full_dictionary, dictionary = dictionary, tolerance = 1)

  # Check for multiple best guesses
  guess_lengths <- unlist(lapply(guesses, length))

  # Replace non-automated responses with the original word
  if(any(guess_lengths > 1))
  {guesses[which(guess_lengths > 1)] <- split_string[which(guess_lengths > 1)]}

  # Convert guesses back into a stringed response
  response <- paste(unlist(guesses), collapse = " ")

  # Re-check for misnomers
  # Check if any dictionaries were imported from SemNetDictionaries.
  # get_available_dictionaries()/get_monikers() (helpers.R) memoize what
  # would otherwise be a real-file-I/O call and a ~1.6ms load, respectively,
  # both repeated on every single ind_word_check() call otherwise
  if(any(dictionary %in% get_available_dictionaries()))
  {response <- moniker(word = response, misnom = get_monikers(dictionary), spelling = spelling)}

  return(response)

}
