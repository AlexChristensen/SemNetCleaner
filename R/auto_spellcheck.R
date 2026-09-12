#' @noRd
# Shared "check candidates against the dictionary, three-way branch" stage,
# used identically ~5 times within `auto_spellcheck()` below (once per
# transformation: as-is, singularized, moniker-converted, moniker+
# singularized, individually spell-checked). Every one of the original's
# `auto.spellcheck()`/`auto.spellcheck.free()`'s per-stage blocks was this
# exact ~35-line pattern, hand-copied with only the transformed candidate's
# variable name changing.
#
# Real bug fixed: the FIRST stage's own "everything resolved" branch was
# hand-coded slightly differently from every other stage's identical
# branch, returning `res$auto` as an empty 0x2 character matrix
# (`colnames = c("from", "to_1")`) instead of the numeric index vector
# every OTHER stage returns there -- confirmed via the actual downstream
# consumer (`corr.mat[spell.check$auto, ]`, in `textcleaner.fluency`/
# `.free` and `spellcheck.dictionary`/`.free`) that a numeric index vector
# is what's actually expected everywhere `$auto` is read. So a dataset
# that happens to have every response already correctly spelled on the
# very first check would have returned the wrong TYPE for `$auto`,
# breaking that downstream row-indexing. Extracting one shared helper,
# used for every stage including the first, fixes this as a structural
# consequence rather than a special-cased patch.
#
# Also drops `auto.spell`: confirmed write-only/dead in the original --
# assigned after every single stage (7+ times), never once read anywhere
# in either function.
#
# 'fallback' is what to revert to when NOTHING resolves this stage: not
# `transformed` itself, but what fed INTO this stage (its pre-
# transformation input) -- confirmed this is the original's actual
# behavior in every occurrence (e.g. singularizing: `sing <- check` on
# total failure, discarding the singularized guesses and reverting to the
# pre-singularize responses), not "leave transformed as-is".
# Updated 09.09.2026
resolve_auto_spellcheck_stage <- function(transformed, fallback, full_dictionary, orig, names_ind)
{

  targets <- match(unlist(transformed), full_dictionary)

  if(all(is.na(targets))){

    # Nothing resolved -- discard this stage's transformation, revert to
    # what fed into it, and continue to the next stage unchanged
    return(list(done = FALSE, orig = orig, names_ind = names_ind, next_candidates = fallback))

  }else if(all(!is.na(targets))){

    # Everything resolved -- update orig and return immediately
    resolved <- which(!is.na(targets))
    orig[names(transformed)[resolved]] <- transformed[resolved]

    result <- list()
    result$manual <- numeric(0)
    # Real bug fixed, found integrating this into `textcleaner()` (Phase
    # 7): `names(transformed)` is only THIS stage's remaining candidates
    # -- whenever an EARLIER stage already resolved some responses (moving
    # them out of the candidate stream and into `names_ind`/`orig`
    # instead, the ordinary case for any input mixing already-correct and
    # misspelled responses), this silently DROPPED every response
    # resolved before this stage from `result$auto`, even though `orig`
    # (returned as `result$to`, correct) already has the right value for
    # every one of them. Confirmed directly: `c("dogg", "cat", "bird")`
    # with the animals dictionary resolves "cat"/"bird" at the very first
    # stage and "dogg" only after individual-word-check -- the old code
    # returned `auto = 1` (just "dogg") instead of `auto = 1:3`, silently
    # excluding "cat"/"bird" from every downstream consumer of `$auto`
    # (`textcleaner()`'s `$spellcheck$automated`, and therefore the
    # `correct_changes()` review step). `names(orig)` is the full original
    # index set regardless of how many stages have run (only `orig`'s
    # VALUES change; its names/length never do) -- using it instead
    # matches the function's own end-of-pipeline `res$auto <-
    # setdiff(as.numeric(full_names), res$manual)` computation.
    result$auto <- setdiff(as.numeric(names(orig)), result$manual)
    result$to <- orig

    return(list(done = TRUE, result = result))

  }else{

    # Some resolved -- update orig/names_ind, continue with the remainder
    resolved <- which(!is.na(targets))
    orig[names(transformed)[resolved]] <- transformed[resolved]
    names_ind <- sort(c(names_ind, names(transformed)[resolved]))

    return(list(
      done = FALSE, orig = orig, names_ind = names_ind,
      next_candidates = orig[-as.numeric(names_ind)]
    ))

  }

}

#' @noRd
# Automated spell-check pipeline: tries, in order, (1) as-is dictionary
# membership, (2) [free only] stop-word/single-and-double-letter removal,
# (3) singularized form, (4) moniker conversion, (5) moniker + singularized,
# (6) individual per-word best-guess correction, (7) multi-word merge/split
# repair, (8) [fluency, or free without `keep_strings`] response splitting.
# Ported from `auto.spellcheck()`/`auto.spellcheck.free()`.
#
# Merged per binding decision #2 (confirmed ~85% line-identical: only 135
# of the fluency version's 469 lines differed from the free version's 556).
# The two real differences besides the extra free-only stage: (a) fluency
# always runs the multi-word/response-splitter stages unconditionally,
# while free skips BOTH entirely when `keep_length` is exactly 1 (single-
# word-only free-association data has nothing to parse) and further skips
# just the response-splitter when `keep_strings = TRUE`; (b) free's
# multi-word stage updates `orig` even when `keep_strings = TRUE` (needed
# for its own downstream cue/ID reconstruction), while fluency's original
# only ever updated `orig` when `keep_strings = FALSE`, leaving nothing to
# do in the `TRUE` case (matches fluency's simpler wide-matrix shape,
# which has no keepStrings-affected cue reconstruction step of its own).
#
# Real bugs fixed: bug #9 (all `parallel::makeCluster()`/`pbapply::pblapply()`
# sites, 2 of the package's 5, replaced with `parallel_process()` --
# structurally forecloses the leak-on-error and zero-core risks; also
# drops the free variant's `parallel::clusterExport()` of package-
# namespace function NAMES as strings, which only ever worked by
# coincidence of the original's exact installed-namespace layout and adds
# nothing `future`'s own automatic closure serialization doesn't already
# handle). Plus the `res$auto` type-consistency bug documented on
# `resolve_auto_spellcheck_stage()` above.
#
# No cores auto-detection by default (`cores = 1`), matching this
# rebuild's and `../SemNeT`'s own established precedent -- the original
# always called `parallel::detectCores() / 2` internally with no way to
# override it.
# Updated 09.09.2026
auto_spellcheck <- function(
  check, full_dictionary, dictionary, spelling, keep_strings,
  type = c("fluency", "free"), keep_length = NULL, cores = 1
)
{

  type <- match.arg(type)

  # Change names of indices
  names(check) <- formatC(
    seq_along(check),
    width = nchar(as.character(length(check))),
    format = "d", flag = "0"
  )

  # Keep full indices
  full_names <- names(check)

  # Save original responses
  orig <- check
  names_ind <- character(0)

  #-------------------------------#
  ## Correctly spelled responses ##
  #-------------------------------#

  message("\nIdentifying correctly spelled responses...", appendLF = FALSE)

  stage <- resolve_auto_spellcheck_stage(check, check, full_dictionary, orig, names_ind)
  if(stage$done){ return(stage$result) }
  orig <- stage$orig; names_ind <- stage$names_ind; check <- stage$next_candidates

  message(paste("done.\n(", length(check), " of ", length(orig), " unique responses still need to be corrected)", sep = ""))

  #---------------------------------------#
  ## [free only] Short and bad responses ##
  #---------------------------------------#

  if(type == "free"){

    # Remove stop words
    stop_words <- SemNetDictionaries::load.dictionaries("stop_words")
    stop_format <- paste("\\b", stop_words, "\\b", sep = "", collapse = "|")
    uniq_resp <- gsub(stop_format, "", unlist(check))
    uniq_resp <- trimws(uniq_resp)

    # Remove bad words
    bad_uniq_resp <- bad_response(uniq_resp)
    bad_index <- which(is.na(bad_uniq_resp))

    # Remove single letters
    rm_letters <- letters[!dictionary_contains(letters, full_dictionary)]
    one_index <- which(uniq_resp %in% rm_letters)

    # Remove two letters
    two_letters <- get_two_letter_combinations()
    rm_letters <- two_letters[!dictionary_contains(two_letters, full_dictionary)]
    two_index <- which(uniq_resp %in% rm_letters)

    # Concatenate bad indices
    na_index <- unique(sort(c(bad_index, one_index, two_index)))
    names(na_index) <- names(uniq_resp)[na_index]

    # Update original responses
    orig[names(na_index)] <- NA

    # Update correctly spelled indices
    names_ind <- unique(sort(c(names_ind, names(na_index))))

    # Remove responses from check
    check <- check[setdiff(names(check), names(na_index))]

  }

  #------------------------#
  ## Pluralized responses ##
  #------------------------#

  message("\nSingularizing responses...", appendLF = FALSE)

  sing <- lapply(check, singularize, dictionary = FALSE)

  stage <- resolve_auto_spellcheck_stage(sing, check, full_dictionary, orig, names_ind)
  if(stage$done){ return(stage$result) }
  orig <- stage$orig; names_ind <- stage$names_ind; sing <- stage$next_candidates

  message(paste("done.\n(", length(sing), " unique responses still need to be corrected)", sep = ""))

  #--------------------------------------------#
  ## Correct common misspellings and monikers ##
  #--------------------------------------------#

  if(all(dictionary == "general") || all(dictionary == "hunspell")){

    mons2 <- sing

  }else if(any(dictionary %in% get_available_dictionaries())){

    message("\nAuto-correcting common misspellings and monikers...", appendLF = FALSE)

    monikers <- get_monikers(dictionary)

    if(length(monikers) != 0){ # Checks in case of only using general dictionary

      # Check for monikers
      mons <- unlist(lapply(sing, moniker, monikers, spelling = spelling), recursive = FALSE)

      stage <- resolve_auto_spellcheck_stage(mons, sing, full_dictionary, orig, names_ind)
      if(stage$done){ return(stage$result) }
      orig <- stage$orig; names_ind <- stage$names_ind; mons <- stage$next_candidates

      # Check for pluralized monikers
      mons2 <- unlist(
        lapply(mons, function(x, monikers, spelling){
          moniker(singularize(x, dictionary = FALSE), monikers, spelling)
        }, monikers, spelling = spelling),
        recursive = FALSE
      )

      stage <- resolve_auto_spellcheck_stage(mons2, mons, full_dictionary, orig, names_ind)
      if(stage$done){ return(stage$result) }
      orig <- stage$orig; names_ind <- stage$names_ind; mons2 <- stage$next_candidates

    }else{
      mons2 <- sing
    }

    message("done.")

  }else{
    mons2 <- sing
  }

  #------------------------------#
  ## Individualized spell-check ##
  #------------------------------#

  message(paste("\nAttempting to auto-correct the remaining", length(mons2), "responses individually..."), appendLF = FALSE)
  message("\nUsing parallel processing to speed up individual word check...")

  ind_check <- parallel_process(
    length(mons2),
    function(i){
      ind_word_check(mons2[[i]], full_dictionary = full_dictionary, dictionary = dictionary, spelling = spelling)
    },
    cores = cores
  )
  names(ind_check) <- names(mons2)
  ind_check <- unlist(ind_check, recursive = FALSE)

  stage <- resolve_auto_spellcheck_stage(ind_check, mons2, full_dictionary, orig, names_ind)
  if(stage$done){ return(stage$result) }
  orig <- stage$orig; names_ind <- stage$names_ind; ind_check <- stage$next_candidates

  message(paste("(", length(ind_check), " unique responses still need to be corrected)", sep = ""))

  #-------------------------------------------#
  ## Parse strings with multi-word responses ##
  #-------------------------------------------#

  # Fluency always parses multi-word responses; free skips entirely when
  # keep_length is exactly 1 (single-word-only data has nothing to parse)
  parse_multi_word <- (type == "fluency") || is.null(keep_length) || keep_length > 1

  if(parse_multi_word){

    message("\nParsing multi-word responses...", appendLF = FALSE)

    # Set multiple word minimum in response to be considered for split
    multi_min <- get_multi_min(full_dictionary)

    message("\nUsing parallel processing to speed up multiple word check...")

    multi_word <- parallel_process(
      length(ind_check),
      function(i){
        multiple_words(
          ind_check[[i]], multi_min = multi_min, full_dictionary = full_dictionary,
          dictionary = dictionary, spelling = spelling, keep_strings = keep_strings
        )
      },
      cores = cores
    )
    names(multi_word) <- names(ind_check)

    # Check responses that changed
    changed <- vapply(multi_word, function(x){ any(x$correct) }, logical(1))

    # Update original responses
    if(!keep_strings){

      responses <- lapply(multi_word[changed], function(x){ x$response })
      if(length(responses) != 0){
        orig[names(responses)] <- responses
      }

    }else{

      # Free's original still updated `orig` here even when keep_strings
      # is TRUE (needed for its own downstream cue/ID reconstruction);
      # fluency's original never reached this branch in the first place
      # (nothing else in its own pipeline reads `orig` under keep_strings)
      # -- kept as one shared branch, harmless for fluency either way
      responses <- lapply(multi_word[changed], function(x){ x$response })
      if(length(responses) != 0){
        orig[names(responses)] <- responses
      }

    }

    # Indices of correctly spelled responses
    all_correct <- vapply(multi_word, function(x){ all(x$correct) }, logical(1))

    if(length(names(multi_word)[all_correct]) != 0){
      names_ind <- sort(c(names_ind, names(multi_word)[all_correct]))
      multi_word <- orig[-as.numeric(names_ind)]
    }

    # Search through responses that can be individually split into
    # separate responses (fluency: always; free: only if !keep_strings)
    if(type == "fluency" || !isTRUE(keep_strings)){

      multi_word <- lapply(multi_word, response_splitter, full_dictionary = full_dictionary)

      fully_split <- vapply(multi_word, function(x){ all(dictionary_contains(x, full_dictionary)) }, logical(1))

      if(length(multi_word[fully_split]) != 0){
        orig[names(multi_word)[fully_split]] <- multi_word[fully_split]
        names_ind <- sort(c(names_ind, names(multi_word)[fully_split]))
        multi_word <- orig[-as.numeric(names_ind)]
      }

    }

  }else{
    multi_word <- ind_check
  }

  # Let user know how many responses need to be spell-checked
  if(length(multi_word) != 0){
    message(paste("\nAutomated spell-checking complete.\nAbout ", length(multi_word), " responses still need to be manually spell-checked", sep = ""))
  }else{
    message("\nAll words passed automated spell-check. Ending spell-check...")
  }

  # Initialize result list
  res <- list()
  res$manual <- as.numeric(names(multi_word))
  res$auto <- setdiff(as.numeric(full_names), res$manual)
  res$to <- orig

  return(res)

}
