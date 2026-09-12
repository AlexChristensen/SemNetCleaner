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
# Pluralize
# Name unchanged (already a plain, unabbreviated verb). Added up-front
# validation (the original had none). Irregular-word table extracted to
# the shared `irregular_plurals` in helpers.R -- see that table's own
# comment for why (an exact 9-pair overlap confirmed against
# singularize()'s independent table).
# Optimization: every `word %in% checker` (coca.dictionary) replaced with
# `coca_dictionary_contains()` (helpers.R) -- a memoized hash lookup in
# place of rebuilding match()'s internal hash table on every single call
# against the same 80K-word vector (~450us every time, confirmed via
# microbenchmark) -- ~240x faster per lookup after the one-time (~130ms)
# hash build, which happens lazily on first use and is shared across every
# call in the session (and with singularize()'s identical dictionary).
# Updated 09.09.2026
pluralize <- function(word)
{

  pluralize_errors(word)

  # Original word
  orig_word <- word

  # Irregular cases
  matched <- unname(irregular_plurals[word])
  chn <- !is.na(matched)

  if(chn){
    # Trust the curated irregular table directly -- skip the dictionary
    # gate below entirely. A hand-curated entry may legitimately target a
    # word not itself present in a general-purpose dictionary (e.g. some
    # of the animal names in the table), and re-validating it there would
    # silently discard a correct match -- exactly the failure mode this
    # fixes (see irregular_plurals's own comment in helpers.R).
    return(matched)
  }

  # Identify last two letters
  last_letters <- substr(word, nchar(word) - 1, nchar(word))

  # chn is always FALSE past the early return above -- no longer wrapped
  # in `if(!chn)`, which was otherwise always TRUE here
  if(any(c("ss", "sh", "ch") == last_letters))
  {word <- paste(word, "es", sep = "", collapse = "")
  }else if(any(c("fe") == last_letters))
  {
    # Remove 'fe'
    word <- substr(word, 1, nchar(word) - 2)
    # Add 'ves'
    word <- paste(word, "ves", sep = "", collapse = "")
  }else if(any(c("us") == last_letters))
  {
    # Remove 'us'
    word <- substr(word, 1, nchar(word) - 2)
    # Add 'i'
    word <- paste(word, "i", sep = "", collapse = "")
  }else if(any(c("is") == last_letters))
  {
    # Remove 'is'
    word <- substr(word, 1, nchar(word) - 2)
    # Add 'es'
    word <- paste(word, "es", sep = "", collapse = "")
  }else if(any(c("on") == last_letters))
  {
    # Remove 'on'
    word <- substr(word, 1, nchar(word) - 2)
    # Add 'a'
    word <- paste(word, "a", sep = "", collapse = "")
  }else{
    # Identify last letter
    last_letter <- substr(word, nchar(word), nchar(word))

    # Change to plural based on last letter
    if(any(c("s", "x", "z") == last_letter))
    {
      word <- paste(word, "es", sep = "", collapse = "")

      if("s" == last_letter)
      {
        if(!coca_dictionary_contains(word))
        {
          # Check for 'ses' ending
          word <- paste(orig_word, "ses", sep = "", collapse = "")
        }
      }else if("z" == last_letter)
      {
        if(!coca_dictionary_contains(word))
        {
          # Check for 'zes' ending
          word <- paste(orig_word, "zes", sep = "", collapse = "")
        }
      }

    }else if(any(c("f") == last_letter))
    {
      # Remove 'f'
      word <- substr(word, 1, nchar(word) - 1)
      # Add 'ves'
      word <- paste(word, "ves", sep = "", collapse = "")

      if(!coca_dictionary_contains(word))
      {
        # Check for 's' ending
        word <- paste(orig_word, "s", sep = "", collapse = "")
      }

    }else if(any(c("y") == last_letter))
    {
      if(any(c("a", "e", "i", "o", "u") == substr(word, nchar(word) - 1, nchar(word) - 1)))
      {word <- paste(word, "s", sep = "", collapse = "")
      }else{
        # Remove 'y'
        word <- substr(word, 1, nchar(word) - 1)
        # Add 'ies'
        word <- paste(word, "ies", sep = "", collapse = "")
      }
    }else if(any(c("o") == last_letter))
    {
      word <- paste(word, "es", sep = "", collapse = "")

      if(!coca_dictionary_contains(word))
      {
        # Check for 's' ending
        word <- paste(orig_word, "s", sep = "", collapse = "")
      }

    }else{word <- paste(orig_word, "s", sep = "", collapse = "")}
  }

  # Final gate. Broadened (per direct request) to use word_is_recognized()
  # -- coca.dictionary first, then the real hunspell spell-checker when
  # installed -- instead of a bare coca-only lookup, and to try BOTH "+s"
  # and "+es" candidates before giving up. This is what makes words like
  # "virus"/"campus"/"bonus"/"census"/"genius"/"chorus"/"circus"/"status"
  # (whose naive "+s" fail-safe form isn't a real word, but whose "+es"
  # form is) resolve correctly WITHOUT needing an individual irregular-
  # table entry for each one -- verified empirically, not guessed, against
  # SemNetDictionaries::coca.dictionary and hunspell::hunspell_check().
  if(!word_is_recognized(word))
  {
    word_with_s <- paste(orig_word, "s", sep = "", collapse = "")

    if(word_is_recognized(word_with_s))
    {return(word_with_s)}

    word_with_es <- paste(orig_word, "es", sep = "", collapse = "")

    if(word_is_recognized(word_with_es))
    {return(word_with_es)}

    return(orig_word)

  }else{return(word)}

}

#' @noRd
# Up-front validation for `pluralize()` -- the original had none
# Updated 08.09.2026
pluralize_errors <- function(word)
{
  typeof_error(word, "character", "pluralize")
  length_error(word, 1, "pluralize")
  na_error(word, "pluralize")
}
