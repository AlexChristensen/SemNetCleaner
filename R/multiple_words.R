#' @noRd
# Tries to de-combine a multi-word response against the dictionary (e.g.
# "fish dog cat" -> "fish" "dog" "cat"), and to repair accidentally-split
# multi-word dictionary terms by merging adjacent words back together
# (e.g. "black" "board" -> "blackboard", if "blackboard" is a real
# dictionary entry but "black"/"board" individually aren't in context).
# Ported from `multiple.words()`.
#
# Real bug mechanism confirmed by direct reproduction against the
# untouched original (not just read): the original's `for(i in misspl)`
# iterates over word positions computed ONCE, before the loop starts
# (`misspl <- which(!spl %in% full.dict)`). But merging a pair of words
# inside the loop body shrinks `spl` (`spl <- spl[-(i-1)]` or
# `spl[-(i+1)]`) -- so every later, still-to-run iteration's stale `i` no
# longer points to the word it was originally computed for. Confirmed via
# `spl = "black board big game"`: once positions 1-2 merge, `spl` shrinks
# to length 2, and the stale `i = 3`/`i = 4` iterations silently index
# past the end (`spl[i]` returns `NA`) instead of ever re-examining the
# words that are actually still there. Extensive further testing (a dozen+
# constructed multi-merge scenarios against the real, unstubbed original)
# did NOT turn up a case where this actually corrupts the final returned
# response -- `best.guess()` handles an out-of-bounds `NA` gracefully
# (no crash, no guess), and the algorithm's redundant bidirectional
# before/after checking at each visited position tends to still catch a
# real merge opportunity from its OTHER side even when the "expected"
# stale index misses it. So this is fixed as a confirmed latent-fragility/
# wasted-work issue (relying on lucky arithmetic alignment between shrink
# rate and stale-index advancement, not sound logic), not a demonstrated
# data-corruption bug -- flagging that distinction rather than overstating
# it. Fixed by re-deriving the next word to check from the CURRENT
# (possibly-shrunk) `split_words` on every iteration, via an explicit
# position pointer that's only ever advanced (never reset) -- this
# preserves the original's "try each flagged word once, left to right"
# intent while making the bookkeeping correct by construction rather than
# by coincidence. See the loop body below for the precise pointer-
# advancement rule, derived by manually tracing exactly which position
# each of the two merge branches leaves the next unprocessed word at.
#
# Second real bug fixed: two of the three `moniker()` calls in this
# function (the "check word before"/"check word after" merge-candidate
# steps) passed the bare `dictionary` argument (a dictionary NAME, e.g.
# "animals") directly as `moniker()`'s `misnom` parameter, instead of the
# actual loaded moniker list `SemNetDictionaries::load.monikers(dictionary)`
# -- which the THIRD call site (after the loop) does correctly. Since
# `moniker()` does `match(word, unlist(misnom))`, passing a bare
# dictionary name as `misnom` means `unlist("animals")` is just
# `"animals"`, and no real word will ever match that string -- so the
# "convert candidate through known monikers before checking the
# dictionary" step was silently a no-op in the original. Fixed by loading
# the monikers once, up front, and passing that to all three call sites.
#
# Updated 09.09.2026
multiple_words <- function(vec, multi_min, full_dictionary, dictionary, spelling, keep_strings = FALSE)
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

    res$response <- combined
    res$correct <- TRUE

    return(res)

  }

  # Check for multiple words
  if(length(split_words) >= multi_min){

    # Loaded once, up front -- shared by every moniker() call below (bug fix).
    # Uses the memoized get_monikers() (helpers.R) since multiple_words()
    # itself gets called repeatedly, per response, with the same dictionary
    # argument, across a whole spellcheck session.
    monikers <- get_monikers(dictionary)

    # Position pointer, only ever advanced -- never re-derived from a
    # stale, pre-computed index set (bug fix; see the file header comment)
    i <- 1

    while(i <= length(split_words)){

      # Already a real word -- nothing to do, move on
      if(dictionary_contains(split_words[i], full_dictionary)){
        i <- i + 1
        next
      }

      # Individual check
      individual_guess <- best_guess(split_words[i], full_dictionary = full_dictionary, dictionary = dictionary)

      # Update based on individual guess
      if(length(individual_guess) == 1){
        split_words[i] <- individual_guess
      }

      # Multi-word merge candidates: word before, and word after
      merge_before <- NA_character_
      merge_after <- NA_character_

      ## Check word before
      if(i != 1){

        spacing <- c(paste(split_words[i - 1], split_words[i]), paste(split_words[i - 1], split_words[i], sep = ""))

        ### Convert monikers
        spacing <- unique(unlist(lapply(spacing, moniker, misnom = monikers, spelling = spelling)))

        ### Check for only one solution
        spacing_found <- dictionary_contains(spacing, full_dictionary)
        if(sum(spacing_found) == 1){
          merge_before <- spacing[which(spacing_found)]
        }

      }

      ## Check word after
      if(i != length(split_words)){

        spacing <- c(paste(split_words[i], split_words[i + 1]), paste(split_words[i], split_words[i + 1], sep = ""))

        ### Convert monikers
        spacing <- unique(unlist(lapply(spacing, moniker, misnom = monikers, spelling = spelling)))

        ### Check for only one solution
        spacing_found <- dictionary_contains(spacing, full_dictionary)
        if(sum(spacing_found) == 1){
          merge_after <- spacing[which(spacing_found)]
        }

      }

      # 'merge_before'/'merge_after', when set (non-NA), were already
      # selected via `spacing[which(spacing_found)]` above -- i.e.
      # already confirmed dictionary members by construction. Re-checking
      # `dictionary_contains()` on them here (as the original's
      # `multi[1] %in% full.dict` equivalent also redundantly did) is
      # dead weight -- simplified to just the NA check.
      has_merge_before <- !is.na(merge_before)
      has_merge_after <- !is.na(merge_after)

      # Verify singular multi-word solution
      if(has_merge_before && !has_merge_after){

        ## Change response, remove the merged-away previous word. The
        ## just-merged result now sits at position i - 1 (everything from
        ## i onward shifts down by one) -- so the pointer 'i' now
        ## correctly refers to the NEXT unprocessed word without
        ## adjustment
        split_words[i] <- merge_before
        split_words <- split_words[-(i - 1)]

      }else if(!has_merge_before && has_merge_after){

        ## Change response, remove the merged-away next word. The merged
        ## result stays at position i -- advance past it to continue
        split_words[i] <- merge_after
        split_words <- split_words[-(i + 1)]
        i <- i + 1

      }else{

        # No unambiguous merge; move on regardless of the individual-guess outcome
        i <- i + 1

      }

    }

    # Check for common misspellings and monikers
    vec <- unlist(lapply(split_words, moniker, misnom = monikers, spelling = spelling))

  }else if(length(split_words) > 1){

    # Correct if only one best guess exists
    guess <- best_guess(vec, full_dictionary = full_dictionary, dictionary = dictionary)

    if(length(guess) == 1)
    {vec <- guess}

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

      res$response <- original_word
      res$correct <- FALSE

    }else{

      res$response <- combined_word
      res$correct <- TRUE

    }

  }else{

    res$response <- unique(vec)
    res$correct <- dictionary_contains(unique(vec), full_dictionary)

  }

  return(res)

}
