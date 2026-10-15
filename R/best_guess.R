#' Makes Best Guess for Spelling Correction
#'
#' @description A wrapper function for the best guess of a spelling mistake
#' based on the letters, the ordering of those letters, and the potential
#' for letters to be interchanged. The
#' \href{https://en.wikipedia.org/wiki/Damerau-Levenshtein_distance}{Damerau-Levenshtein distance}
#' is used to guide inferences into what word the participant was trying to spell from a dictionary
#' (see \code{SemNetDictionaries})
#'
#' @param word Character.
#' A word to get best guess spelling options from dictionary
#'
#' @param full_dictionary Character vector.
#' The dictionary to search for best guesses in.
#' See \code{SemNetDictionaries}
#'
#' @param dictionary Character.
#' A dictionary from \code{SemNetDictionaries} for monikers (enhances guessing)
#'
#' @param tolerance Numeric.
#' The distance tolerance set for automatic spell-correction purposes.
#' This function uses the function \code{\link[stringdist]{stringdist}}
#' to compute the Damerau-Levenshtein distance, which is used to determine potential best guesses
#'
#' Unique words (i.e., \emph{n} = 1) that are within the (distance) tolerance are
#' automatically output as best guess responses. This default is based on Damerau's (1964)
#' proclamation that more than 80\% of all human misspellings can be expressed by a single error
#' (e.g., insertion, deletion, substitution, and transposition). If there is more than one word
#' that is within or below the distance tolerance, then these will be provided as potential
#' options.
#'
#' The recommended and default distance tolerance is \code{tolerance = 1},
#' which only spell corrects a word if there is only one word with a DL distance of 1.
#'
#' @return The best guess(es) of the word
#'
#' @examples
#' # Misspelled "bombay"
#' best_guess("bomba", full_dictionary = SemNetDictionaries::animals.dictionary)
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @references
#' Damerau, F. J. (1964).
#' A technique for computer detection and correction of spelling errors.
#' \emph{Communications of the ACM}, \emph{7}, 171-176.
#'
#' Philips, L. (1990).
#' Hanging on the metaphone.
#' \emph{Computer Language}, \emph{7}(12), 39-43.
#'
#' @export
#' @importFrom SemNetDictionaries dictionaries load.dictionaries load.monikers append.dictionary
# Best guess ----
# Suggests corrections for a misspelled word: the dictionary words within
# Damerau-Levenshtein distance of the word, narrowed by monikers (lists of common
# misspellings) and then by agreement between the QWERTY keyboard distance and a phonetic
# (Metaphone) match. When the guess is still ambiguous, up to 10 candidates are returned
# for manual review
# The `importFrom` tags above only satisfy `R CMD check` for `SemNetDictionaries`, which is
# in `Depends`; every call is namespace-qualified
# Updated 14.10.2026
best_guess <- function(
    word, # word to correct
    full_dictionary, # dictionary used to check words
    dictionary = NULL, # names of dictionaries requested
    tolerance = 1 # maximum edit distance for automatic correction
)
{

  # Check for errors
  best_guess_errors(word, full_dictionary, dictionary, tolerance)

  # Remove extra characters for spell-check
  word <- gsub("([-])|[[:punct:]]", "\\1", word)

  # Check for the word in the dictionary (memoized lookup, fast for large dictionaries)
  if(!dictionary_contains(word, full_dictionary)){

    # Grab Damerau-Levenshtein distances
    guess <- as.vector(stringdist::stringdist(word, full_dictionary, method = "dl",
                                              weight = c(d = 1, # deletion
                                                         i = 1, # insertion
                                                         s = 1, # substitution
                                                         t = 1 # transposition
                                                         )))

    # Compute minimum distance
    min_guess <- min(guess)

    # Grab guesses less than or equal to plus one of minimum
    target <- which(guess <= (min_guess + 1))

    # Target distances
    target_dist <- guess[target]

    # Check for plural variations
    plural_variants <- unlist(lapply(full_dictionary[target], pluralize))

    # Plural distances
    plural_dist <- stringdist::stringdist(word, plural_variants, method = "dl",
                                          weight = c(d = 1, # deletion
                                                     i = 1, # insertion
                                                     s = 1, # substitution
                                                     t = 1 # transposition
                                          ))

    # Check for plural variants closer than the word's guess
    if(any(plural_dist < target_dist)){

      # Update target distances
      target_dist[plural_dist < target_dist] <- plural_dist[plural_dist < target_dist]

    }

    # Determine if auto-correction is possible
    if(length(which(target_dist <= tolerance)) == 1){

      # Suggest option as best guess
      bestguess <- full_dictionary[target[which(target_dist <= tolerance)]]

    }else if(length(target_dist) != 1){ # Multiple candidates

      # Check monikers, phonetic and qwerty distance

      # Check for monikers
      if(!is.null(dictionary)){

        # Get the available dictionaries once (memoized, since `dictionaries()` reads a file)
        non_general_dictionaries <- get_available_dictionaries()

        # Remove general dictionary
        non_general_dictionaries <- non_general_dictionaries[non_general_dictionaries != "general"]

        # Check for moniker dictionaries
        if(any(dictionary %in% non_general_dictionaries)){

          # Monikers
          moniker_dictionaries <- dictionary[dictionary %in% non_general_dictionaries]

          # Load monikers
          monikers <- get_monikers(moniker_dictionaries)

          # Grab Damerau-Levenshtein distances
          moniker_guess <- as.vector(stringdist::stringdist(word, monikers, method = "dl",
                                                              weight = c(d = 1, # deletion
                                                                         i = 1, # insertion
                                                                         s = 1, # substitution
                                                                         t = 1 # transposition
                                                                         )))

          # Compute minimum distance
          min_moniker_guess <- min(moniker_guess)

          # Grab guesses less than or equal to plus one of minimum
          moniker_target <- which(moniker_guess <= (min_moniker_guess + 1))

          # Target distances
          moniker_target_dist <- moniker_guess[moniker_target]

          # Identify target monikers
          moniker_index <- moniker_target[which(moniker_target_dist <= tolerance)]

          # Get unique monikers (lowercase, since the moniker table capitalizes proper nouns)
          uniq_monikers <- unique(tolower(gsub("[[:digit:]]+", "", names(monikers[moniker_index]))))

        }

      }

      # Tolerance-eligible candidates -- the same subset both QWERTY and
      # phonetic disambiguation operate on
      tolerance_candidates <- full_dictionary[target[which(target_dist <= tolerance)]]

      # Check QWERTY distance
      dists <- unlist(lapply(tolerance_candidates, function(x){qwerty_distance(x, word)}))

      # Check phonetic (Metaphone) match, a second signal alongside QWERTY for the same
      # tolerance-eligible candidates
      phon_matches <- which(vapply(tolerance_candidates, function(x){

        # Check phonetic match
        phonetic_match(word, x)

      }, logical(1)))

      # Check if monikers exist and are unique; a unique moniker wins outright
      if(exists("uniq_monikers", envir = environment()) && length(uniq_monikers) == 1){

        # Use unique moniker
        bestguess <- uniq_monikers

      }else{ # No unique moniker

        # Check for unique QWERTY candidate
        qwerty_unique <- length(which(dists <= 1)) == 1

        # Check for unique phonetic candidate
        phonetic_unique <- length(phon_matches) == 1

        # QWERTY and phonetic matches are treated as peers: agreement reinforces the answer and
        # disagreement is ambiguous, so it falls through to manual review
        if(qwerty_unique && phonetic_unique){

          # Use candidate only when both agree
          if(which(dists <= 1) == phon_matches){

            # Use the agreed candidate
            bestguess <- tolerance_candidates[which(dists <= 1)]

          }

        }else if(qwerty_unique){ # Only QWERTY is unique

          bestguess <- tolerance_candidates[which(dists <= 1)] # Use QWERTY candidate

        }else if(phonetic_unique){ # Only phonetic is unique

          bestguess <- tolerance_candidates[phon_matches] # Use phonetic candidate

        }

      }

    }

    # Check for no automatic guess
    if(!exists("bestguess", envir = environment())){

      # Order by least to greatest DL value
      target_order <- target[order(target_dist, decreasing = FALSE)]

      # Order guesses by guess values (only first 10)
      if(length(target_order) > 10){

        bestguess <- full_dictionary[target_order][1:10] # More than ten guesses

      }else{

        bestguess <- full_dictionary[target_order] # Ten or fewer guesses

      }

    }

  }else{

    bestguess <- word # Word is in dictionary

  }

  # Return best guess(es)
  return(bestguess)

}

#' @noRd
# Best guess errors ----
# Up-front validation for `best_guess()`
# Updated 14.10.2026
best_guess_errors <- function(
    word, # word to correct
    full_dictionary, # dictionary used to check words
    dictionary, # names of dictionaries requested
    tolerance # maximum edit distance for automatic correction
)
{

  # Check word type
  typeof_error(word, "character", "best_guess")

  # Check word length
  length_error(word, 1, "best_guess")

  # Check word for missing
  na_error(word, "best_guess")

  # Check dictionary type
  typeof_error(full_dictionary, "character", "best_guess")

  # An empty dictionary leaves nothing to guess against (`min()` of nothing is `Inf`)
  if(length(full_dictionary) == 0){

    # Send error
    stop("In `best_guess()`: `full_dictionary` must not be empty.", call. = FALSE)

  }

  # Check for dictionary names
  if(!is.null(dictionary)){

    # Check names type
    typeof_error(dictionary, "character", "best_guess")

  }

  # Check tolerance type
  typeof_error(tolerance, "numeric", "best_guess")

  # Check tolerance length
  length_error(tolerance, 1, "best_guess")

  # Check tolerance range
  range_error(tolerance, c(0, Inf), "best_guess")

}
