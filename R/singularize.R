#' @noRd
# Singularize-only irregular entries -- no pluralize-side counterpart
# exists for these (the regular suffix rules would mishandle them, or a
# dictionary-specific correction was needed).
additional_irregular_singulars <- c(
  valves = "valve", trees = "tree", scribbles = "scribble", peduncles = "peduncle",
  mallees = "mallee", panicles = "panicle", ridges = "ridge", petioles = "petiole",
  angles = "angle", bristles = "bristle", edges = "edge", fissures = "fissure",
  sutures = "suture", occurrences = "occurrence", bees = "bee", asses = "ass"
)

#' @noRd
# Private cache for the derived (reversed) irregular_plurals table combined
# with singularize()'s own extra entries. Computed lazily, at first CALL
# (not as bare top-level file code), specifically so it does NOT depend on
# R's alphabetical file-collation order sourcing helpers.R (where
# irregular_plurals lives) before this file -- that exact kind of implicit
# cross-file load-order dependency is what caused bug #1. Memoized after
# the first call since the two source tables are static.
# Updated 08.09.2026
.irregular_singulars_cache <- new.env(parent = emptyenv())

get_irregular_singulars <- function()
{
  if(is.null(.irregular_singulars_cache$value)){
    .irregular_singulars_cache$value <- c(
      setNames(names(irregular_plurals), irregular_plurals),
      additional_irregular_singulars
    )
  }
  .irregular_singulars_cache$value
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
# Real bug found during Phase 8's roxygen/NAMESPACE audit (same class as
# qwerty_distance.R's own instance): this doc block's `@export` tag used to
# sit atop the file, ahead of `additional_irregular_singulars <- c(...)` --
# roxygen attaches a doc block to the very next R statement, not the
# function this whole comment actually describes. `singularize()` was
# therefore silently never exported (masked in dev-mode testing, since
# `devtools::load_all()` exposes internal objects too), while the private
# irregular-word vector was wrongly exported instead. Moved to directly
# precede the function it documents. Name unchanged (already a plain,
# unabbreviated verb). Added up-front validation (the original had none).
# Irregular-word table: the first 9 of the original's own 25-entry table
# were an exact mirror of pluralize()'s independent table -- derived here
# from the shared `irregular_plurals` (helpers.R) instead of being
# hand-duplicated, with this function's own 16 singularize-only entries
# (words the regular suffix rules don't handle correctly on their own)
# kept alongside it. Optimization: every `word %in% checker`
# (coca.dictionary) replaced with `coca_dictionary_contains()` (helpers.R)
# -- a memoized hash lookup shared with pluralize() in place of rebuilding
# match()'s internal hash table on every single call against the same 80K-
# word vector. See that helper's own comment in helpers.R for the full
# benchmark.
# Updated 12.09.2026
singularize <- function(word, dictionary = TRUE)
{

  singularize_errors(word, dictionary)

  # Check for multiple words
  split_words <- unlist(strsplit(word, " "))

  if(length(split_words) > 1)
  {
    word <- split_words[length(split_words)]
    multiple <- TRUE
  }else{multiple <- FALSE}

  # Original word
  orig_word <- word

  # Irregular cases
  matched <- unname(get_irregular_singulars()[word])
  chn <- !is.na(matched)

  if(chn){
    # Trust the curated irregular table directly -- skip the dictionary
    # gate below entirely, regardless of the `dictionary` argument (which
    # only governs how much to trust the REGULAR-rule-derived candidate
    # below, not a hand-curated match). Fixes e.g. "peduncles"/"mallees":
    # their correct singular targets aren't in coca.dictionary, so the
    # previous architecture silently discarded an already-correct match
    # -- the same underlying failure mode as bug #1's shadowing and bug
    # #14's dropped disambiguation, just one architectural layer over.
    word <- matched

    if(multiple){
      split_words[length(split_words)] <- word
      word <- paste(split_words, collapse = " ")
    }

    return(word)
  }

  ## Last letter
  last_letter <- substr(word, nchar(word), nchar(word))

  # chn is always FALSE past the early return above -- no longer wrapped
  # in `if(!chn)`, which was otherwise always TRUE here
  if(any(last_letter == c("s", "i", "a")))
  {
    if(last_letter == "s")
    {
      # Remove 's'
      word <- substr(word, 1, nchar(word) - 1)

      chn <- TRUE

      if(!coca_dictionary_contains(word))
      {
        word <- orig_word
        chn <- FALSE
      }

    }else if(last_letter == "i")
    {
      # Remove 'i'
      word <- substr(word, 1, nchar(word) - 1)
      # Add 'us'
      word <- paste(word, "us", sep = "", collapse = "")

      chn <- TRUE

      if(!coca_dictionary_contains(word))
      {
        word <- orig_word
        chn <- FALSE
      }

    }else if(last_letter == "a")
    {
      # Remove 'a'
      word <- substr(word, 1, nchar(word) - 1)
      # Add 'on'
      word <- paste(word, "on", sep = "", collapse = "")

      chn <- TRUE

      if(!coca_dictionary_contains(word))
      {
        word <- orig_word
        chn <- FALSE
      }

    }
  }

  ## Last two letters
  last_letters_2 <- substr(word, nchar(word) - 1, nchar(word))

  if(!chn)
  {
    if(last_letters_2 == "es")
    {
      # Remove 'es'
      word <- substr(word, 1, nchar(word) - 2)

      chn <- TRUE

      if(!coca_dictionary_contains(word))
      {
        # Add 'is'
        word <- paste(word, "is", sep = "", collapse = "")
      }

      if(!coca_dictionary_contains(word))
      {
        word <- orig_word
        chn <- FALSE
      }
    }

  }

  ## Last three letters
  last_letters_3 <- substr(word, nchar(word) - 2, nchar(word))

  if(!chn)
  {
    if(any(last_letters_3 == c("ves", "ies", "zes", "ses")))
    {
      if(last_letters_3 == "ves")
      {
        # Remove 'ves'
        word <- substr(word, 1, nchar(word) - 3)
        # Add 'f'
        word <- paste(word, "f", sep = "", collapse = "")

        if(!coca_dictionary_contains(word))
        {
          # Add 'e'
          word <- paste(word, "e", sep = "", collapse = "")
        }

        chn <- TRUE

        if(!coca_dictionary_contains(word))
        {
          word <- orig_word
          chn <- FALSE
        }

      }else if(last_letters_3 == "ies")
      {
        # Remove 'ies'
        word <- substr(word, 1, nchar(word) - 3)
        # Add 'y'
        word <- paste(word, "y", sep = "", collapse = "")

        if(!coca_dictionary_contains(word))
        {
          # Check for 'ie' ending
          word <- substr(word, 1, nchar(word) - 1)
          word <- paste(word, "ie", sep = "", collapse = "")
        }

        chn <- TRUE

        if(!coca_dictionary_contains(word))
        {
          word <- orig_word
          chn <- FALSE
        }

      }else if(any(last_letters_3 == c("zes", "ses")))
      {
        # Remove 'zes' or 'ses'
        word <- substr(word, 1, nchar(word) - 3)
        chn <- TRUE

        if(!coca_dictionary_contains(word))
        {
          word <- orig_word
          chn <- FALSE
        }
      }
    }
  }

  # Return word
  if(isTRUE(dictionary)){

    # Check for word in dictionary. Broadened (per direct request) to use
    # word_is_recognized() -- coca.dictionary first, then the real
    # hunspell spell-checker when installed -- instead of a bare coca-only
    # lookup, same rationale/verification as pluralize()'s final gate.
    if(!word_is_recognized(word)){
      if(isTRUE(multiple))
      {orig_word <- paste(split_words, collapse = " ")}

      return(orig_word)
    }else{

      if(multiple)
      {
        split_words[length(split_words)] <- word
        word <- paste(split_words, collapse = " ")
      }

      return(word)
    }

  }else{

    if(multiple)
    {
      split_words[length(split_words)] <- word
      word <- paste(split_words, collapse = " ")
    }

    return(word)

  }
}

#' @noRd
# Up-front validation for `singularize()` -- the original had none
# Updated 08.09.2026
singularize_errors <- function(word, dictionary)
{
  typeof_error(word, "character", "singularize")
  length_error(word, 1, "singularize")
  na_error(word, "singularize")
  typeof_error(dictionary, "logical", "singularize")
  length_error(dictionary, 1, "singularize")
}
