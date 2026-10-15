#' Converts Words to their Plural Form
#'
#' @description A function to change words to their plural form.
#' The rules for converting words to their plural forms
#' are based on the grammar rules.
#'
#' This function handles most special cases and some irregular cases (see examples)
#' but caution is necessary. If no plural form is identified, then the original
#' word is returned.
#'
#' @param word A word
#'
#' @return Returns the word in plural form, unless a plural form
#' could not be found (then the original word is returned)
#'
#' @examples
#' # Handles any prototypical cases
#' "dogs"
#' pluralize("dog")
#'
#' "foxes"
#' pluralize("fox")
#'
#' "wolves"
#' pluralize("wolf")
#'
#' "octopi"
#' pluralize("octopus")
#'
#' "taxa"
#' pluralize("taxon")
#'
#' # And most special cases:
#' "wives"
#' pluralize("wife")
#'
#' "roofs"
#' pluralize("roof")
#'
#' "photos"
#' pluralize("photo")
#'
#' # And some irregular cases:
#' "children"
#' pluralize("child")
#'
#' "teeth"
#' pluralize("tooth")
#'
#' "mice"
#' pluralize("mouse")
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Pluralize ----
# Converts a word to its plural: irregular words come from the shared
# `irregular_plurals` table, the rest from suffix rules; the result must be a
# recognized word, otherwise the original word is returned
# Updated 14.10.2026
pluralize <- function(
    word # word to pluralize
)
{

  # Check for errors
  pluralize_errors(word)

  # Original word
  orig_word <- word

  # Irregular cases
  matched <- unname(irregular_plurals[word])

  # Check for match
  chn <- !is.na(matched)

  # Check for irregular match
  if(chn){

    # Trust the curated irregular table; its entries may not be in a general-purpose dictionary
    return(matched)

  }

  # Identify last two letters
  last_letters <- substr(word, nchar(word) - 1, nchar(word))

  # chn is always FALSE past the early return above -- no longer wrapped
  # in `if(!chn)`, which was otherwise always TRUE here
  if(any(c("ss", "sh", "ch") == last_letters)){

    word <- paste(word, "es", sep = "", collapse = "") # Ends in ss, sh, or ch

  }else if(any(c("fe") == last_letters)){ # Ends in 'fe'

    # Remove 'fe'
    word <- substr(word, 1, nchar(word) - 2)

    # Add 'ves'
    word <- paste(word, "ves", sep = "", collapse = "")

  }else if(any(c("us") == last_letters)){ # Ends in 'us'

    # Remove 'us'
    word <- substr(word, 1, nchar(word) - 2)

    # Add 'i'
    word <- paste(word, "i", sep = "", collapse = "")

  }else if(any(c("is") == last_letters)){ # Ends in 'is'

    # Remove 'is'
    word <- substr(word, 1, nchar(word) - 2)

    # Add 'es'
    word <- paste(word, "es", sep = "", collapse = "")

  }else if(any(c("on") == last_letters)){ # Ends in 'on'

    # Remove 'on'
    word <- substr(word, 1, nchar(word) - 2)

    # Add 'a'
    word <- paste(word, "a", sep = "", collapse = "")

  }else{ # Other endings

    # Identify last letter
    last_letter <- substr(word, nchar(word), nchar(word))

    # Change to plural based on last letter
    if(any(c("s", "x", "z") == last_letter)){

      # Add 'es'
      word <- paste(word, "es", sep = "", collapse = "")

      # Check for 's' ending
      if("s" == last_letter){

        # Check whether result is a word
        if(!coca_dictionary_contains(word)){

          # Check for 'ses' ending
          word <- paste(orig_word, "ses", sep = "", collapse = "")

        }

      }else if("z" == last_letter){ # Ends in 'z'

        # Check whether result is a word
        if(!coca_dictionary_contains(word)){

          # Check for 'zes' ending
          word <- paste(orig_word, "zes", sep = "", collapse = "")

        }

      }

    }else if(any(c("f") == last_letter)){ # Ends in 'f'

      # Remove 'f'
      word <- substr(word, 1, nchar(word) - 1)

      # Add 'ves'
      word <- paste(word, "ves", sep = "", collapse = "")

      # Check whether result is a word
      if(!coca_dictionary_contains(word)){

        # Check for 's' ending
        word <- paste(orig_word, "s", sep = "", collapse = "")

      }

    }else if(any(c("y") == last_letter)){ # Ends in 'y'

      # Check for vowel before 'y'
      if(any(c("a", "e", "i", "o", "u") == substr(word, nchar(word) - 1, nchar(word) - 1))){

        word <- paste(word, "s", sep = "", collapse = "") # Add s

      }else{ # Consonant before 'y'

        # Remove 'y'
        word <- substr(word, 1, nchar(word) - 1)

        # Add 'ies'
        word <- paste(word, "ies", sep = "", collapse = "")

      }

    }else if(any(c("o") == last_letter)){ # Ends in 'o'

      # Add 'es'
      word <- paste(word, "es", sep = "", collapse = "")

      # Check whether result is a word
      if(!coca_dictionary_contains(word)){

        # Check for 's' ending
        word <- paste(orig_word, "s", sep = "", collapse = "")

      }

    }else{

      word <- paste(orig_word, "s", sep = "", collapse = "") # Ends in other letters

    }

  }

  # Final gate: a word recognized by the COCA dictionary or hunspell is returned; otherwise
  # the "+s" and "+es" forms are tried ("virus" to "viruses") before the original word is returned
  if(!word_is_recognized(word)){

    # Try adding 's'
    word_with_s <- paste(orig_word, "s", sep = "", collapse = "")

    # Check for recognized word
    if(word_is_recognized(word_with_s)){

      return(word_with_s) # Return form with s

    }

    # Try adding 'es'
    word_with_es <- paste(orig_word, "es", sep = "", collapse = "")

    # Check for recognized word
    if(word_is_recognized(word_with_es)){

      return(word_with_es) # Return form with es

    }

    # Return original word
    return(orig_word)

  }else{

    return(word) # Recognized word

  }

}

#' @noRd
# Pluralize errors ----
# Up-front validation for `pluralize()`
# Updated 14.10.2026
pluralize_errors <- function(
    word # word to pluralize
)
{

  # Check word type
  typeof_error(word, "character", "pluralize")

  # Check word length
  length_error(word, 1, "pluralize")

  # Check word for missing
  na_error(word, "pluralize")

}
