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
# Best Guess
# Real bug found during Phase 8's dependency audit: `NAMESPACE`'s
# whole-package `import(SemNetDictionaries)` was dropped entirely (every
# real call site already fully qualifies with `::`, so no `import`/
# `importFrom` was ever load-bearing for those calls to work) -- but
# `R CMD check` still flags a `Depends`-listed package with NO
# `importFrom` at all as a NOTE ("these packages need to be imported from
# ... for when this namespace is loaded but not attached"). These 4 tags
# (one per real `SemNetDictionaries::` function actually called anywhere
# in this package, confirmed via a whole-codebase grep) exist purely to
# satisfy that check, not because any call site actually needs them --
# every real call still uses `::` explicitly, unchanged.
# Ported from `best.guess()`. Real bug fixed: the moniker-disambiguation
# branch's inner check was `if(length(uniq.monix == 1))` -- misplaced
# parentheses mean this computes `length(uniq.monix == 1)`, which (since
# `==` returns a same-length logical vector) always just equals
# `length(uniq.monix)`, and any nonzero number is truthy in `if()`. So the
# check was always TRUE whenever `uniq.monix` was non-empty, regardless of
# whether it actually had exactly one element -- confirmed by direct
# reproduction (`length(c("a","b","c") == 1)` is 3, not FALSE). This meant
# that whenever the outer OR-condition was satisfied via the *qwerty*-
# distance path (`length(which(dists <= 1)) == 1`) while `uniq.monix` had
# 2+ (ambiguous) entries, the buggy inner check still took the moniker
# branch and returned the whole ambiguous set instead of falling through
# to the correctly-disambiguated single qwerty-distance guess. Fixed to
# the clearly-intended `if(length(uniq.monix) == 1)`, matching the outer
# condition's own (correct) comparison.
# Added up-front validation (the original had none).
# Updated 08.09.2026
best_guess <- function(word, full_dictionary, dictionary = NULL, tolerance = 1)
{

  best_guess_errors(word, full_dictionary, dictionary, tolerance)

  # Remove extra characters for spell-check
  word <- gsub("([-])|[[:punct:]]", "\\1", word)

  # Optimization (microbenchmark, per direct request to look for a faster
  # implementation): raw `%in%` replaced with the already-built, already-
  # memoized `dictionary_contains()` (helpers.R) -- ~50x faster once
  # cached for a realistic large free-association dictionary
  # (`cocaspell.dictionary`, ~109k words: ~761us raw vs ~16us cached,
  # median) with no meaningful cost for a small one like
  # `animals.dictionary` (dictionary_contains()'s own size-aware dispatch
  # already falls back to raw `%in%` below its 3000-word threshold).
  # Verified identical() output.
  if(!dictionary_contains(word, full_dictionary))
  {
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

    if(any(plural_dist < target_dist)){
      # Update target distances
      target_dist[plural_dist < target_dist] <- plural_dist[plural_dist < target_dist]
    }

    # Determine if auto-correction is possible
    if(length(which(target_dist <= tolerance)) == 1){
      # Suggest option as best guess
      bestguess <- full_dictionary[target[which(target_dist <= tolerance)]]

    }else if(length(target_dist) != 1){
      # Check monikers, phonetic and qwerty distance

      # Check for monikers
      if(!is.null(dictionary)){

        # Hoisted: the original called SemNetDictionaries::dictionaries(TRUE)
        # 4 separate times in this block for the exact same result --
        # computed once here instead, via the memoized get_available_
        # dictionaries() (helpers.R; dictionaries() does real file I/O
        # every call, confirmed ~155us each, microbenchmark)
        non_general_dictionaries <- get_available_dictionaries()
        non_general_dictionaries <- non_general_dictionaries[non_general_dictionaries != "general"]

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

          # Get unique monikers
          uniq_monikers <- unique(gsub("[[:digit:]]+", "", names(monikers[moniker_index])))
        }
      }

      # Tolerance-eligible candidates -- the same subset both QWERTY and
      # phonetic disambiguation operate on
      tolerance_candidates <- full_dictionary[target[which(target_dist <= tolerance)]]

      # Check QWERTY distance
      dists <- unlist(lapply(tolerance_candidates, function(x){qwerty_distance(x, word)}))

      # Check phonetic (Metaphone) match -- a second, independent
      # disambiguation signal alongside QWERTY, confined to the same
      # tolerance-eligible candidate set (not the wider min_guess+1
      # window -- see this file's own header comment for why that's a
      # deliberately deferred, separate decision). `phonetic_match()`
      # (helpers.R) wraps `phonics::metaphone()`, a hard `Imports`
      # dependency.
      phon_matches <- which(vapply(tolerance_candidates, function(x){
        phonetic_match(word, x)
      }, logical(1)))

      # Check if monikers exist and are unique -- monikers still win
      # outright when unique, unchanged from the original (bug #14-fixed)
      # priority
      if(exists("uniq_monikers", envir = environment()) && length(uniq_monikers) == 1)
      {
        bestguess <- uniq_monikers

      }else{

        qwerty_unique <- length(which(dists <= 1)) == 1
        phonetic_unique <- length(phon_matches) == 1

        # QWERTY and phonetic are evaluated as peers, neither trusted
        # over the other: agreement (same candidate) reinforces the
        # answer; disagreement (different candidates) means the case is
        # genuinely ambiguous -- neither wins, falls through to manual
        # review -- rather than arbitrarily preferring one heuristic.
        # Confirmed reproducible via direct testing: e.g. a "gremble"
        # typo of "grumble" has QWERTY's adjacent-key logic pick
        # "tremble" while Metaphone correctly picks "grumble" -- two
        # independent heuristics with no principled reason one should
        # always outrank the other.
        if(qwerty_unique && phonetic_unique)
        {
          if(which(dists <= 1) == phon_matches)
          {bestguess <- tolerance_candidates[which(dists <= 1)]}

        }else if(qwerty_unique)
        {bestguess <- tolerance_candidates[which(dists <= 1)]
        }else if(phonetic_unique)
        {bestguess <- tolerance_candidates[phon_matches]}

      }
    }

    if(!exists("bestguess", envir = environment()))
    {
      # Order by least to greatest DL value
      target_order <- target[order(target_dist, decreasing = FALSE)]

      # Order guesses by guess values (only first 10)
      if(length(target_order) > 10)
      {bestguess <- full_dictionary[target_order][1:10]
      }else{bestguess <- full_dictionary[target_order]}
    }

  }else{bestguess <- word}

  return(bestguess)

}

#' @noRd
# Up-front validation for `best_guess()` -- the original had none
# Updated 08.09.2026
best_guess_errors <- function(word, full_dictionary, dictionary, tolerance)
{
  typeof_error(word, "character", "best_guess")
  length_error(word, 1, "best_guess")
  na_error(word, "best_guess")
  typeof_error(full_dictionary, "character", "best_guess")

  # Robustness fix (Phase 2 review, per direct request): an empty
  # `full_dictionary` isn't caught by the type check above, but
  # `min(stringdist(...))` over the resulting empty distance vector
  # silently returns `Inf` (a warning, not an error) rather than rejecting
  # a call that has no dictionary to guess against in the first place --
  # confirmed directly. Same "min()/max() over empty" footgun already
  # hardened in `range_error()`/`seq_min_max()` during the Phase 1 review.
  if(length(full_dictionary) == 0){
    stop("In `best_guess()`: `full_dictionary` must not be empty.", call. = FALSE)
  }

  if(!is.null(dictionary)){
    typeof_error(dictionary, "character", "best_guess")
  }
  typeof_error(tolerance, "numeric", "best_guess")
  length_error(tolerance, 1, "best_guess")
  range_error(tolerance, c(0, Inf), "best_guess")
}
