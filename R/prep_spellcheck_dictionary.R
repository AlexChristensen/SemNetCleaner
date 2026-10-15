#' @noRd
# Normalize responses ----
# Shared normalization for `prep_spellcheck_dictionary()`: removes or keeps punctuation
# and numbers, trims white space, lowercases, collapses new lines and tabs to spaces,
# removes characters without an ASCII equivalent, and reverts the text "NA" to a real `NA`.
# It works on a flat character vector; each step treats every element on its own and is
# vectorized over the whole vector
# Updated 14.10.2026
normalize_responses <- function(
    responses, # vector of responses
    allow_punctuations, # punctuation to keep
    allow_numbers # whether numbers are kept
)
{

  # An empty input has nothing to normalize (`bad_response()` would turn `character(0)`
  # into a single `NA`)
  if(length(responses) == 0){

    # Return empty input
    return(responses)

  }

  # Transliterate accented and typographic characters to ASCII first, so that typographic
  # punctuation (curly quotes, em dashes) is handled like ordinary punctuation
  responses <- stringi::stri_trans_general(responses, "Latin-ASCII")

  # Remove/allow punctuation. `allow_punctuations` may be "all", the characters to keep, or
  # empty (`NULL`, "", `character(0)`, `NA`) for none; allowed characters are escaped
  # before they go into a bracket expression
  allowed <- unique(unlist(strsplit(as.character(allow_punctuations), "")))

  # Remove missing characters
  allowed <- allowed[!is.na(allowed)]

  # Check for restricted punctuation
  if(!"all" %in% as.character(allow_punctuations)){

    # Separators written without spaces ("cow/pig", "fish,bird") become spaces first, so that
    # they separate words instead of gluing them together; anything the user allowed is left alone
    separators <- setdiff(c("/", ",", ";", "|", "&", "+", ":", "\\"), allowed)

    # Check for separators
    if(length(separators) > 0){

      # Escape special characters
      escaped_separators <- gsub("([][\\\\^-])", "\\\\\\1", separators)

      # Replace separators with spaces
      responses <- gsub(paste0("[", paste(escaped_separators, collapse = ""), "]"), " ", responses, perl = TRUE)

    }

    # Check for no allowed punctuation
    if(length(allowed) == 0){

      # Remove all punctuation
      responses <- gsub("[[:punct:]]", "", responses)

    }else{ # Keep allowed punctuation

      # Escape allowed characters
      escaped <- gsub("([][\\\\^-])", "\\\\\\1", allowed)

      # Build pattern
      characters <- paste("([", paste(escaped, collapse = ""), "])|[[:punct:]]", sep = "")

      # Remove other punctuation
      responses <- gsub(characters, "\\1", responses, perl = TRUE)

    }

  }

  # Remove/allow numbers. Only stand-alone numbers ("1.", "24", list numbering) are removed;
  # digits inside a word ("d0g", "3rd") are kept so the spell-check can correct or flag them
  if(!isTRUE(allow_numbers)){

    # Remove stand-alone numbers
    responses <- gsub("(?<![[:alnum:]])[[:digit:]]+(?![[:alnum:]])", "", responses, perl = TRUE)

  }

  # Remove white spaces
  responses <- gsub("^[[:space:]]+|[[:space:]]+$", "", bad_response(trimws(responses)))

  # Change all to lowercase (capitalization does not change the word and the dictionaries
  # are lowercase)
  responses <- tolower(responses)

  # Remove new lines, and collapse runs of whitespace ("polar  bear" is the
  # dictionary entry "polar bear", and separators turned into spaces above
  # can leave doubles)
  responses <- gsub("[[:space:]]+", " ", gsub("[\n\r\t]", " ", responses))

  # Remove leading and trailing spaces
  responses <- gsub("^ | $", "", responses)

  # Remove special characters
  ## Split words
  split_responses <- strsplit(responses, split = " ")

  ## Remove special characters with one gsub() over every word of every response, then
  ## regroup by response with an explicit `factor()` (a plain `split()` drops empty responses)
  response_lengths <- lengths(split_responses)

  ## Accented and typographic characters were already transliterated at the top of this
  ## function; what is left has no ASCII equivalent (e.g., CJK) and is removed
  flat_words <- gsub("[^\x20-\x7E]", "", unlist(split_responses))

  # Group words by response
  response_groups <- factor(rep(seq_along(response_lengths), response_lengths), levels = seq_along(response_lengths))

  # Regroup words
  split_responses <- unname(split(flat_words, response_groups))

  ## Re-combine words
  responses <- unlist(lapply(split_responses, paste, sep = "", collapse = " "))

  # A response of only characters without an ASCII equivalent (e.g., CJK) is empty by now;
  # it is missing data like any other blank
  responses[!is.na(responses) & !nzchar(trimws(responses))] <- NA

  # Revert NAs
  # (assignment, not `ifelse()`, which would return a logical vector for all-NA input)
  responses[!is.na(responses) & responses == "NA"] <- NA_character_

  # Return normalized responses
  return(responses)

}

#' @noRd
# Prepare responses for spell-check ----
# Prepares the responses for `spellcheck_dictionary()`: removes punctuation, numbers, and
# white space, lowercases, and converts to a data frame. A matrix (fluency) is flattened,
# normalized by `normalize_responses()`, and reshaped; for free association only the
# "Response" column is normalized
# Updated 14.10.2026
prep_spellcheck_dictionary <- function(
    data, # matrix or data frame of responses
    allowPunctuations, # punctuation to keep
    allowNumbers, # whether numbers are kept
    type # "fluency" or "free"
)
{

  # Check for fluency
  if(type == "fluency"){

    # Get dimensions
    original_dim <- dim(data)

    # Get dimension names
    original_dimnames <- dimnames(data)

    # Normalize all responses
    normalized <- normalize_responses(as.vector(as.matrix(data)), allowPunctuations, allowNumbers)

    # Restore matrix shape
    data <- matrix(normalized, nrow = original_dim[1], ncol = original_dim[2], dimnames = original_dimnames)

    # Convert to data frame
    data <- as.data.frame(data, stringsAsFactors = FALSE)

  }else if(type == "free"){ # Free association

    # Normalize responses
    data[, "Response"] <- normalize_responses(data[, "Response"], allowPunctuations, allowNumbers)

    # Convert to data frame
    data <- as.data.frame(data, stringsAsFactors = FALSE)

  }

  # Return data
  return(data)

}
