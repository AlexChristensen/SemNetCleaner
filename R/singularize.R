#' @noRd
# Additional irregular singular forms (no plural counterpart; the suffix rules would mishandle them)
additional_irregular_singulars <- c(
  valves = "valve", trees = "tree", scribbles = "scribble", peduncles = "peduncle",
  mallees = "mallee", panicles = "panicle", ridges = "ridge", petioles = "petiole",
  angles = "angle", bristles = "bristle", edges = "edge", fissures = "fissure",
  sutures = "suture", occurrences = "occurrence", bees = "bee", asses = "ass"
)

#' @noRd
# Cache of the irregular singular forms: the reversed `irregular_plurals` table plus the
# extra entries above, built on first use so that file load order does not matter
# Updated 14.10.2026
# Cache irregular singular forms
.irregular_singulars_cache <- new.env(parent = emptyenv())

# Get irregular singulars ----
# Get irregular singular forms
# Updated 14.10.2026
get_irregular_singulars <- function()
{

  # Check for cached value
  if(is.null(.irregular_singulars_cache$value)){

    # Reverse plural table and add extra forms
    .irregular_singulars_cache$value <- c(
      setNames(names(irregular_plurals), irregular_plurals),
      additional_irregular_singulars
    )

  }

  # Return irregular singular forms
  return(.irregular_singulars_cache$value)

}

#' Converts Words to their Singular Form
#'
#' @description A function to change words to their singular form.
#' The rules for converting words to their singular forms
#' are based on the \strong{\emph{inverse}} of the grammar rules.
#' This function handles most special cases and some irregular cases (see examples)
#' but caution is necessary. If no singular form is identified, then the original
#' word is returned.
#'
#' @param word Character.
#' A word
#'
#' @param dictionary Boolean.
#' Should dictionary be used to verify word exists?
#' Default to \code{TRUE}
#'
#' @return Returns the word in singular form, unless a singular form
#' could not be found (then the original word is returned)
#'
#' @examples
#' # Handles any prototypical cases
#' # "dog"
#' singularize("dogs")
#'
#' # "fox"
#' singularize("foxes")
#'
#' # "wolf"
#' singularize("wolves")
#'
#' # "octopus"
#' singularize("octopi")
#'
#' # "taxon"
#' singularize("taxa")
#'
#' # And most special cases:
#' # "wife"
#' singularize("wives")
#'
#' # "fez"
#' singularize("fezzes")
#'
#' # "roof"
#' singularize("roofs")
#'
#' # "photo"
#' singularize("photos")
#'
#' # And some irregular cases:
#' # "child"
#' singularize("children")
#'
#' # "tooth"
#' singularize("teeth")
#'
#' # "mouse"
#' singularize("mice")
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Singularize ----
# Converts a word to its singular: irregular words come from the reversed
# `irregular_plurals` table plus the extra entries, the rest from suffix rules; with
# `dictionary = TRUE` the result must be a recognized word, otherwise the original word
# is returned
# Updated 14.10.2026
singularize <- function(
    word, # word to singularize
    dictionary = TRUE # require the result to be a recognized word
)
{

  # Check for errors
  singularize_errors(word, dictionary)

  # Check for multiple words
  split_words <- unlist(strsplit(word, " "))

  # Check for multiple words
  if(length(split_words) > 1){

    # Use last word
    word <- split_words[length(split_words)]

    # Flag multiple words
    multiple <- TRUE

  }else{

    multiple <- FALSE # Single word

  }

  # Original word
  orig_word <- word

  # Irregular cases
  matched <- unname(get_irregular_singulars()[word])

  # Find irregular matches
  chn <- !is.na(matched)

  # Check for irregular match
  if(chn){

    # Trust the curated irregular table regardless of `dictionary` (its entries may not be in a dictionary)
    word <- matched

    # Check for multiple words
    if(multiple){

      # Replace last word
      split_words[length(split_words)] <- word

      # Rejoin words
      word <- paste(split_words, collapse = " ")

    }

    # Return irregular singular
    return(word)

  }

  ## Last letter
  last_letter <- substr(word, nchar(word), nchar(word))

  # chn is always FALSE past the early return above -- no longer wrapped
  # in `if(!chn)`, which was otherwise always TRUE here
  if(any(last_letter == c("s", "i", "a"))){

    # Check for 's' ending
    if(last_letter == "s"){

      # Remove 's'
      word <- substr(word, 1, nchar(word) - 1)

      # Flag change
      chn <- TRUE

      # Check whether result is a word
      if(!coca_dictionary_contains(word)){

        # Revert to original word
        word <- orig_word

        # Flag no change
        chn <- FALSE

      }

    }else if(last_letter == "i"){ # Check for 'i' ending

      # Remove 'i'
      word <- substr(word, 1, nchar(word) - 1)

      # Add 'us'
      word <- paste(word, "us", sep = "", collapse = "")

      # Flag change
      chn <- TRUE

      # Check whether result is a word
      if(!coca_dictionary_contains(word)){

        # Revert to original word
        word <- orig_word

        # Flag no change
        chn <- FALSE

      }

    }else if(last_letter == "a"){ # Check for 'a' ending

      # Remove 'a'
      word <- substr(word, 1, nchar(word) - 1)

      # Add 'on'
      word <- paste(word, "on", sep = "", collapse = "")

      # Flag change
      chn <- TRUE

      # Check whether result is a word
      if(!coca_dictionary_contains(word)){

        # Revert to original word
        word <- orig_word

        # Flag no change
        chn <- FALSE

      }

    }

  }

  ## Last two letters
  last_letters_2 <- substr(word, nchar(word) - 1, nchar(word))

  # Check for no change
  if(!chn){

    # Check for 'es' ending
    if(last_letters_2 == "es"){

      # Remove 'es'
      word <- substr(word, 1, nchar(word) - 2)

      # Flag change
      chn <- TRUE

      # Check whether result is a word
      if(!coca_dictionary_contains(word)){

        # Add 'is'
        word <- paste(word, "is", sep = "", collapse = "")

      }

      # Check whether result is a word
      if(!coca_dictionary_contains(word)){

        # Revert to original word
        word <- orig_word

        # Flag no change
        chn <- FALSE

      }

    }

  }

  ## Last three letters
  last_letters_3 <- substr(word, nchar(word) - 2, nchar(word))

  # Check for no change
  if(!chn){

    # Check for 'ves', 'ies', 'zes', or 'ses' ending
    if(any(last_letters_3 == c("ves", "ies", "zes", "ses"))){

      # Check for 'ves' ending
      if(last_letters_3 == "ves"){

        # Remove 'ves'
        word <- substr(word, 1, nchar(word) - 3)

        # Add 'f'
        word <- paste(word, "f", sep = "", collapse = "")

        # Check whether result is a word
        if(!coca_dictionary_contains(word)){

          # Add 'e'
          word <- paste(word, "e", sep = "", collapse = "")

        }

        # Flag change
        chn <- TRUE

        # Check whether result is a word
        if(!coca_dictionary_contains(word)){

          # Revert to original word
          word <- orig_word

          # Flag no change
          chn <- FALSE

        }

      }else if(last_letters_3 == "ies"){ # Check for 'ies' ending

        # Remove 'ies'
        word <- substr(word, 1, nchar(word) - 3)

        # Add 'y'
        word <- paste(word, "y", sep = "", collapse = "")

        # Check whether result is a word
        if(!coca_dictionary_contains(word)){

          # Check for 'ie' ending
          word <- substr(word, 1, nchar(word) - 1)

          # Add ie ending
          word <- paste(word, "ie", sep = "", collapse = "")

        }

        # Flag change
        chn <- TRUE

        # Check whether result is a word
        if(!coca_dictionary_contains(word)){

          # Revert to original word
          word <- orig_word

          # Flag no change
          chn <- FALSE

        }

      }else if(any(last_letters_3 == c("zes", "ses"))){ # Check for 'zes' or 'ses' ending

        # Remove 'zes' or 'ses'
        word <- substr(word, 1, nchar(word) - 3)

        # Flag change
        chn <- TRUE

        # Check whether result is a word
        if(!coca_dictionary_contains(word)){

          # Revert to original word
          word <- orig_word

          # Flag no change
          chn <- FALSE

        }

      }

    }

  }

  # Return word
  if(isTRUE(dictionary)){

    # Check for word in dictionary (COCA dictionary, then hunspell)
    if(!word_is_recognized(word)){

      # Check for multiple words
      if(isTRUE(multiple)){

        orig_word <- paste(split_words, collapse = " ") # Rejoin words

      }

      # Return original word
      return(orig_word)

    }else{ # Recognized word

      # Check for multiple words
      if(multiple){

        # Replace last word
        split_words[length(split_words)] <- word

        # Rejoin words
        word <- paste(split_words, collapse = " ")

      }

      # Return singular
      return(word)

    }

  }else{ # Recognition not required

    # Check for multiple words
    if(multiple){

      # Replace last word
      split_words[length(split_words)] <- word

      # Rejoin words
      word <- paste(split_words, collapse = " ")

    }

    # Return singular
    return(word)

  }

}

#' @noRd
# Singularize errors ----
# Up-front validation for `singularize()`
# Updated 14.10.2026
singularize_errors <- function(
    word, # word to singularize
    dictionary # require the result to be a recognized word
)
{

  # Check word type
  typeof_error(word, "character", "singularize")

  # Check word length
  length_error(word, 1, "singularize")

  # Check word for missing
  na_error(word, "singularize")

  # Check dictionary type
  typeof_error(dictionary, "logical", "singularize")

  # Check dictionary length
  length_error(dictionary, 1, "singularize")

}
