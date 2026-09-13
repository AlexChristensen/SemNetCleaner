#' @noRd
#' @importFrom utils setTxtProgressBar txtProgressBar
# Merges `spellcheck.dictionary()`/`spellcheck.dictionary.free()` into one
# `spellcheck_dictionary(type = c("fluency", "free"), ...)`, per binding
# decision #2. Orchestrates the manual spell-check wizard: runs
# `auto_spellcheck()` first, then drives `spellcheck_menu()` once per word
# (multi-word responses) or once per response for everything the automated
# pass couldn't resolve, with a full save/resume ("continue") and
# GO-BACK/undo state machine. Rebuilt on `read_input()`/a `reader =
# readline` parameter and an injectable `search_fn` (both threaded down
# into `spellcheck_menu()`), per binding decision #3.
#
# Confirmed genuinely different between the two originals (not just
# renamed scaffolding, so kept as internal `type` branches rather than
# force-unified): `free` always uses dictionary = "cocaspell"/category =
# "define" with no category-detection at all (`fluency` detects one of 8
# categories from the dictionary argument); `free` tracks a separate
# `auto_ind` through the whole function and does a substantial post-loop
# recomputation of it (dropping any "auto"-resolved response that turned
# out unchanged, and any whose source word is itself a moniker key) that
# `fluency` has no equivalent of at all; `free`'s multi-word branch has an
# extra empty-string-after-punctuation-stripping stage `fluency` doesn't;
# `free`'s post-menu target-update rule differs
# (`if(length(result$target) == 1)` special case) from `fluency`'s
# unconditional `target <- result$target`.
#
# The post-loop "ad hoc moniker check" (re-running `moniker()` over every
# response once manual spell-check is done) was `fluency`'s own plain
# nested `for` loop vs. `free`'s `parallel::makeCluster()`-based version
# (the 5th and last of bug #9's unguarded cluster sites) -- unified here
# into one shared block using `parallel_process()` (`cores = 1` behaves
# like `fluency`'s original serial loop; `cores > 1` gives `free`'s
# original parallelism, correctly this time). This also incidentally
# closes out bug #8's `for(i in 1:length(to))`-style reverse-range risk in
# `fluency`'s original version of this block, never audited before since
# it had no standalone existence prior to this merge.
#
# Bug #41 fixed (found during Phase 6's `correct_data.R` audit, which
# turned up the same `x[-which(cond)]`-on-empty pattern as bugs #39/#40 --
# checked every other occurrence of it across the whole codebase as a
# result, and found this one too): both GO-BACK-undo sites did
# `changes <- changes[-which(names(changes) == paste(ind[main_count -
# go_back_count]))]` -- on a LIST, this same pattern doesn't just fail to
# remove the target entry when `which()` is empty, it empties the WHOLE
# list. Confirmed reachable: GO BACK can revert to a response that was
# resolved via SKIP or KEEP AUTO-CORRECT, neither of which ever calls
# `change_format()` -- so `changes` has no entry for that response's
# index, and every OTHER already-recorded change (for completely
# unrelated responses) is silently discarded along with it. Fixed via a
# positive `!=` filter. A full end-to-end wizard reproduction proved
# impractical to hand-construct reliably (the go_back_count/main_count
# bookkeeping is intricate enough that isolating this one path from
# everything else it touches wasn't tractable) -- covered instead by a
# direct regression test of the exact mechanism (`test-spellcheck_dictionary.R`),
# same as bug #14's own (`best_guess.R`) precedent.
# Updated 09.09.2026
spellcheck_dictionary <- function(
  uniq_resp = NULL, dictionary = NULL, spelling = NULL,
  add_path = NULL, keep_strings = NULL, keep_length = NULL, keep_cue = FALSE,
  data = NULL, continue = NULL, type = c("fluency", "free"),
  reader = readline, search_fn = searcher::search_site, cores = 1
)
{

  type <- match.arg(type)

  # Continuation check
  if(is.null(continue)){

    if(type == "free"){

      if(is.null(dictionary)){dictionary <- "cocaspell"}
      category <- "define"
      full_dictionary <- SemNetDictionaries::load.dictionaries("cocaspell")

    }else{

      categories <- get_available_dictionaries()

      if(any(dictionary %in% categories)){

        target <- dictionary[na.omit(match(categories, dictionary))]

        if("general" %in% target){category <- "dictionary"
        }else if("fruits" %in% target && "vegetables" %in% target){category <- "food"
        }else if("fruits" %in% target && !"vegetables" %in% target){category <- "fruits"
        }else if(!"fruits" %in% target && "vegetables" %in% target){category <- "vegetables"
        }else if("jobs" %in% target){category <- "jobs"
        }else if("hot" %in% target || "good" %in% target){category <- "synonym"
        }else if("animals" %in% target){category <- "animals"
        }else{category <- "define"}

      }else{category <- "define"}

      full_dictionary <- SemNetDictionaries::load.dictionaries(dictionary, add.path = add_path)

    }

    message(paste("\nConverting dictionary to '", spelling, "' spelling...", sep = ""), appendLF = FALSE)
    full_dictionary <- brit_us_conversion_vector(full_dictionary, spelling = spelling, dictionary = TRUE)
    message("done")

    orig_dictionary <- full_dictionary

    from <- as.list(uniq_resp)
    if(type == "free"){
      names(from) <- formatC(
        seq_along(from), width = nchar(as.character(length(from))), format = "d", flag = "0"
      )
    }
    to <- from

    initial <- try(
      auto_spellcheck(
        check = from, full_dictionary = full_dictionary, dictionary = dictionary,
        spelling = spelling, keep_strings = keep_strings, type = type,
        keep_length = keep_length, cores = cores
      ),
      silent = TRUE
    )

    if(inherits(initial, "try-error")){
      error_report(initial, "auto_spellcheck", "textcleaner")
      res <- list()
      res$stop <- TRUE
    }

    ind <- initial$manual
    if(type == "free"){auto_ind <- initial$auto}
    to <- initial$to

    initial_to <- to

    if(keep_strings){
      multi_ind <- which(lapply(to[ind], function(x){
        length(unlist(strsplit(x, split = " ")))
      }) >= 2)
    }else{
      multi_ind <- which(lapply(to[ind], length) >= 2)
    }

    single_ind <- setdiff(ind, as.numeric(names(multi_ind)))
    ind <- c(as.numeric(names(multi_ind)), single_ind)

    changes <- list()
    main_count <- 1
    go_back_count <- 1
    go_back_reset <- FALSE

    if(type == "fluency"){
      # Add artificial pause for smoother feel
      Sys.sleep(2)
    }

  }else{

    type <- continue$type
    dictionary <- continue$dictionary
    full_dictionary <- continue$full_dictionary
    orig_dictionary <- continue$orig_dictionary
    spelling <- continue$spelling
    category <- continue$category
    from <- continue$from
    to <- continue$to
    initial <- continue$initial
    initial_to <- continue$initial_to
    ind <- continue$ind
    if(type == "free"){auto_ind <- continue$auto_ind}
    changes <- continue$changes
    main_count <- continue$main_count
    go_back_count <- continue$go_back_count
    go_back_reset <- continue$go_back_reset
    if(!is.null(continue$multi_count)){multi_count <- continue$multi_count}
    keep_strings <- continue$keep_strings
    data <- continue$data

  }

  # Check if manual spell-check is necessary
  if(length(ind) != 0){

    walk_through(FALSE, reader = reader)

    if(Sys.info()["sysname"] == "Windows" && requireNamespace("tcltk", quietly = TRUE)){
      pb <- tcltk::tkProgressBar(title = "R progress bar", label = "Spell-check progress",
                                 min = 0, max = length(ind), initial = 0, width = 300)
      invisible(tcltk::getTkProgressBar(pb))
    }else{pb <- txtProgressBar(min = 0, max = length(ind), style = 3)}

    line_break()

  }

  # `pending` (Phase 11, additive-only): the structured render context for
  # whichever decision the wizard was about to ask about when it stopped --
  # `context`/`check`/`possible`/`original`/`category`/`keep_strings`,
  # exactly matching the arguments the very next `spellcheck_menu()` call
  # would have been given. Passed in explicitly by each call site below
  # (rather than reconstructed here via `exists()`-sniffing the enclosing
  # scope) because `target`/`check_words`/`multi_count` are ordinary local
  # variables that persist, unset, across main-loop iterations -- an
  # `exists()`-based check inside `save_state()` itself could silently read
  # a STALE value left over from a previous response instead of erroring.
  # No existing caller reads `$pending` -- this cannot change any existing
  # tested behavior, only add a new field consumed by
  # `textcleaner_shiny()`'s wizard module (`mod_wizard.R`), which otherwise
  # has no way to render the next prompt/buttons without re-deriving this
  # bookkeeping a second time.
  save_state <- function(stop_target = NULL, pending = NULL){

    res <- list()
    res$type <- type
    res$dictionary <- dictionary
    res$full_dictionary <- full_dictionary
    res$orig_dictionary <- orig_dictionary
    res$spelling <- spelling
    res$category <- category
    res$from <- from
    res$to <- to
    res$initial <- initial
    res$initial_to <- initial_to
    res$ind <- ind
    if(type == "free"){res$auto_ind <- auto_ind}
    res$changes <- changes
    res$main_count <- main_count
    res$go_back_count <- go_back_count
    res$go_back_reset <- go_back_reset
    if(exists("multi_count")){
      res$multi_count <- multi_count
      # Two bugs found via live crashes, both from the SAME root cause:
      # `target`/`target_history` are the multi-word response's own
      # in-progress working copy and undo STACK -- mutated word by word
      # across the inner `multi_count` loop, but never folded into `to[[i]]`
      # until that whole loop finishes. Every OTHER resumable variable in
      # this function is restored from `continue` on the next call; these
      # weren't, so:
      # (1) a `textcleaner_shiny()` resume mid multi-word response silently
      #     reverted every earlier word in the SAME response back to its
      #     pristine auto-corrected form (`target <- to[[i]]` recomputes it
      #     from scratch below), and GO BACK crashed outright ("object
      #     'prev_target' not found") the moment a fresh function call
      #     referenced an undo-buffer it had never once assigned.
      # (2) even within a single continuous call (no resume at all -- a
      #     plain console `textcleaner()` session), a SECOND consecutive
      #     GO BACK crashed too ("input `word` has length 0" in
      #     `best_guess()`), because the ORIGINAL single-scalar
      #     `prev_target` only ever held the last real decision's
      #     pre-state -- seeing this same bug live is what turned this
      #     into a full history stack instead of a second scalar.
      # Saved/restored as a pair with `multi_count` itself (same
      # `exists()` guard) since neither is meaningful without it.
      res$wip_target <- target
      if(exists("target_history")){res$wip_target_history <- target_history}
    }
    res$data <- data
    res$keep_strings <- keep_strings
    res$stop <- TRUE
    if(!is.null(stop_target)){res$target <- stop_target}
    if(!is.null(pending)){res$pending <- pending}

    class(res) <- "textcleaner"
    res

  }

  # Loop through for manual spell-check
  while(main_count != (length(ind) + 1)){

    i <- ind[main_count]
    target <- to[[i]]

    if(keep_strings){
      target <- unlist(strsplit(target, split = " "))
      if(type == "free"){
        target <- na.omit(ifelse(target == "", NA, target))
      }
    }

    if(length(target) > 1){

      target_punct <- gsub("[^[:alnum:][:space:]]", "", target)

      if(type == "free" && any(target_punct == "")){

        target <- target[-which(target_punct == "")]
        target_punct <- na.omit(ifelse(target_punct == "", NA, target_punct))

        check_words <- target[which(!target_punct %in% full_dictionary)]

        if(length(check_words) == 0){
          target <- paste(target, collapse = " ")
          check_words <- target[!target %in% full_dictionary]
        }

      }else{

        check_words <- target[which(!target_punct %in% full_dictionary)]

        if(type == "free" && length(check_words) == 0){

          single_string <- paste(target_punct, collapse = " ")

          if(!single_string %in% unlist(to[1:i])){
            check_words <- target
          }

        }

      }

      if(is.null(continue$multi_count)){
        multi_count <- 1
        # One entry per word position, `target_history[[k]]` holding
        # `target` exactly as it stood right before word k's own decision
        # was applied -- a real undo STACK, not a single scalar. Bug found
        # via a live double-GO-BACK crash ("In `best_guess()`: input `word`
        # has length 0"): the original design used one `prev_target`
        # scalar, overwritten only on a REAL (non-go-back) decision. GO
        # BACK never refreshed it, so a SECOND consecutive GO BACK (no
        # real decision in between) reused the SAME stale value, decrementing
        # `multi_count` without actually reverting `target` to match. That
        # then broke `menu_check <- which(check_words[multi_count] ==
        # target)` at the next word position down: its original text had
        # already been overwritten by an earlier BAD WORD/TYPE MY OWN WORD
        # decision and never really undone, so it no longer appeared
        # anywhere in `target` -- an empty match, fed straight into
        # `best_guess()` as a length-0 `word`. A full history stack fixes
        # this for any number of consecutive GO BACKs, not just one.
        target_history <- list()
      }else{
        # Restore this same multi-word response's in-progress working copy
        # (`target`) and undo history (`target_history`) alongside
        # `multi_count` itself -- see the history's own initialization
        # comment above for the crash this, together with the stack itself,
        # closes. Guarded the same way `multi_count`'s own restore is
        # (`continue$multi_count` non-null), so this only ever fires once,
        # for the exact response it was saved mid-way through.
        if(!is.null(continue$wip_target)){target <- continue$wip_target}
        target_history <- if(!is.null(continue$wip_target_history)) continue$wip_target_history else list()
        continue$multi_count <- NULL
      }

      if(length(check_words) == 0){

        result <- list()
        result$go_back_count <- go_back_count + 1

        if(keep_strings){
          message(paste("\nResponse '",
                        style_text(paste(target, collapse = " "), defaults = "bold"),
                        "' was KEPT AS IS based on a previous manual spell-check decision",
                        sep = ""))
        }else{
          message(paste("\nResponse '",
                        style_text(target, defaults = "bold"),
                        "' was KEPT AS IS based on a previous manual spell-check decision",
                        sep = ""))
        }

        if(result$go_back_count == go_back_count){
          if(go_back_reset){
            go_back_count <- 1
          }else{
            go_back_reset <- TRUE
          }
        }else{
          go_back_count <- result$go_back_count
          go_back_reset <- FALSE
        }

        line_break()

      }

      # Loop through words that need to be checked
      while(multi_count != (length(check_words) + 1)){

        # Precomputed once, reused both for the real spellcheck_menu() call
        # below and (Phase 11) for save_state()'s own `pending` render
        # context on STOP/error -- avoids computing this twice from
        # independently-drifting expressions
        menu_check <- which(check_words[multi_count] == target)
        menu_possible <- best_guess(
          if(type == "free") unique(target[menu_check]) else target[menu_check],
          full_dictionary = full_dictionary, dictionary = dictionary
        )
        menu_original <- if(keep_strings) paste(target, collapse = " ") else from[[i]]

        pending <- list(
          context = target, check = menu_check, possible = menu_possible,
          original = menu_original, category = category, keep_strings = keep_strings
        )

        result <- try(
          spellcheck_menu(
            check = menu_check, context = target, possible = menu_possible,
            original = menu_original,
            current_index = i, changes = changes, full_dictionary = full_dictionary,
            category = category, dictionary = dictionary, keep_strings = keep_strings,
            go_back_count = go_back_count, reader = reader, search_fn = search_fn
          ),
          silent = TRUE
        )

        line_break()

        if("STOP" %in% result){

          message("\nUser stopped. Saving progress...\n")
          res <- save_state(pending = pending)
          close(pb)
          if(type == "free"){Sys.sleep(2)}
          return(res)

        }else if(inherits(result, "try-error")){

          error_report(result, "spellcheck_menu", "textcleaner")
          res <- save_state(target, pending = pending)
          close(pb)
          message("\nSaving progress...\n")
          Sys.sleep(2)
          return(res)

        }

        if(result$go_back_count == go_back_count){
          if(go_back_reset){
            go_back_count <- 1
          }else{
            go_back_reset <- TRUE
          }
        }else{
          go_back_count <- result$go_back_count
          go_back_reset <- FALSE
        }

        if(result$go_back){

          if(multi_count == 1){

            if(main_count != 1){

              to[[i]] <- initial_to[[i]]

              prev_resp <- initial_to[[ind[main_count - go_back_count]]]
              prev_resp_split <- unlist(strsplit(prev_resp, split = " "))

              if(any(!prev_resp_split %in% full_dictionary == prev_resp_split %in% orig_dictionary)){
                target_rms <- which(!prev_resp_split %in% full_dictionary == prev_resp_split %in% orig_dictionary)
                ind_rms <- match(prev_resp_split[target_rms], full_dictionary)
                full_dictionary <- full_dictionary[-ind_rms]
              }

              to[[ind[main_count - go_back_count]]] <- prev_resp
              changes <- changes[names(changes) != paste(ind[main_count - go_back_count])]

              main_count <- main_count - (go_back_count + 1)
              go_back_count <- 1

              break

            }else{
              message("\nThis is the first response. 'GO BACK' is not available.")
              line_break()
              Sys.sleep(0.5)
            }

          }else{

            # Read from the history STACK at position `multi_count - 1`,
            # not a single `prev_target` scalar -- see `target_history`'s
            # own initialization comment above for why: a scalar only ever
            # holds the LAST real decision's pre-state, which a second
            # consecutive GO BACK (no real decision in between) would reuse
            # unchanged instead of reverting any further.
            prior_target <- target_history[[multi_count - 1]]
            target <- prior_target

            if(any(prior_target %in% full_dictionary & !prior_target %in% orig_dictionary)){
              target_rms <- which(prior_target %in% full_dictionary & !prior_target %in% orig_dictionary)
              ind_rms <- match(prior_target[target_rms], full_dictionary)
              full_dictionary <- full_dictionary[-ind_rms]
            }

            # Bug #52 fixed, found during a Phase 4 robustness review:
            # `changes <- changes[[which(...)]][-(multi_count - 1), ]`
            # doesn't just update THIS response's own change matrix within
            # the list -- it REPLACES THE WHOLE `changes` VARIABLE with a
            # bare matrix (this response's history, minus one row),
            # silently destroying every other already-recorded change for
            # every OTHER response in the entire session. Confirmed
            # directly: a 2-entry `changes` list collapses to a 1-row
            # character vector the moment this line runs, regardless of
            # whether `which()` found a match. Confirmed inherited
            # identically from the untouched original (both the
            # `textcleaner.fluency`/`.free` branches, byte-identical).
            # Worse, when NO entry exists yet for this response (GO BACK
            # on word 2+ of a multi-word response, where word 1 was
            # resolved via SKIP/KEEP AUTO-CORRECT -- neither ever calls
            # `change_format()`, per bug #41's own established finding),
            # `which()` returns empty and `changes[[integer(0)]]` crashes
            # outright ("attempt to select less than one element") --
            # OUTSIDE the surrounding `try()`, so this took down the whole
            # wizard with no save-state recovery. Fixed by updating the
            # list ENTRY in place, only when one exists.
            change_idx <- which(names(changes) == paste(ind[main_count]))
            if(length(change_idx) != 0){
              changes[[change_idx]] <- changes[[change_idx]][-(multi_count - 1), , drop = FALSE]
            }
            multi_count <- multi_count - 1

          }

        }else{

          # Push this position's pre-decision `target` onto the undo stack
          # (see `target_history`'s own initialization comment above)
          # BEFORE applying the decision below -- `target_history[[k]]`
          # always means "target as it stood right before word k's own
          # decision", read back by GO BACK at `target_history[[multi_count
          # - 1]]` once `multi_count` has moved on to word k+1.
          target_history[[multi_count]] <- target
          if(type == "free" && length(result$target) == 1){
            target[multi_count] <- result$target
          }else{
            target <- result$target
          }
          changes <- result$changes
          full_dictionary <- result$full_dictionary

          if(all(target == "NA")){
            result$end <- TRUE
          }

          if(result$end){
            multi_count <- length(check_words) + 1
          }else{multi_count <- multi_count + 1}

        }

      }

      if(keep_strings){
        target <- paste(target, collapse = " ")
      }

      to[[i]] <- target

    }else{

      # Single response. Wrapped in a `repeat` (Bug found via direct code
      # review while investigating the multi-word GO BACK crashes above):
      # GO BACK denied on the very first manual response (`main_count ==
      # 1`) printed "This is the first response. 'GO BACK' is not
      # available." but then fell straight through to this loop's own
      # `main_count <- main_count + 1` below -- silently ADVANCING PAST
      # this response instead of re-asking it, leaving it stuck at
      # whatever the automated pass produced, with no real user decision
      # ever recorded for it. The multi-word branch above never had this
      # bug: its own per-word decision already lives inside a real
      # `while(multi_count != ...)` loop, so a denied GO BACK there
      # (leaving `multi_count` unchanged) naturally re-loops and re-asks.
      # This response has no such loop of its own -- added one here,
      # breaking out on every OTHER path (a real decision applied, or a
      # GO BACK that actually succeeded) exactly where execution used to
      # fall through anyway, so those paths are unchanged.
      repeat{

        menu_possible <- best_guess(target, full_dictionary = full_dictionary, dictionary)

        # Phase 11: same precompute-once-reuse-for-`pending` pattern as the
        # multi-word branch above -- `context = NULL` here since there is no
        # surrounding multi-word string for a single-response decision
        pending <- list(
          context = NULL, check = target, possible = menu_possible,
          original = from[[i]], category = category, keep_strings = keep_strings
        )

        result <- try(
          spellcheck_menu(
            check = target, context = NULL, possible = menu_possible,
            original = from[[i]], current_index = i, changes = changes,
            full_dictionary = full_dictionary, category = category, dictionary = dictionary,
            keep_strings = keep_strings, go_back_count = go_back_count,
            reader = reader, search_fn = search_fn
          ),
          silent = TRUE
        )

        line_break()

        if("STOP" %in% result){

          message("\nUser stopped. Saving progress...\n")
          res <- save_state(pending = pending)
          close(pb)
          Sys.sleep(2)
          return(res)

        }else if(inherits(result, "try-error")){

          error_report(result, "spellcheck_menu", "textcleaner")
          res <- save_state(target, pending = pending)
          close(pb)
          message("\nSaving progress...\n")
          Sys.sleep(2)
          return(res)

        }

        if(result$go_back_count == go_back_count){
          if(go_back_reset){
            go_back_count <- 1
          }else{
            go_back_reset <- TRUE
          }
        }else{
          go_back_count <- result$go_back_count
          go_back_reset <- FALSE
        }

        if(result$go_back){

          if(main_count != 1){

            to[[i]] <- initial_to[[i]]

            prev_resp <- initial_to[[ind[main_count - go_back_count]]]
            prev_resp_split <- unlist(strsplit(prev_resp, split = " "))

            if(any(!prev_resp_split %in% full_dictionary == prev_resp_split %in% orig_dictionary)){
              target_rms <- which(!prev_resp_split %in% full_dictionary == prev_resp_split %in% orig_dictionary)
              ind_rms <- match(prev_resp_split[target_rms], full_dictionary)
              full_dictionary <- full_dictionary[-ind_rms]
            }

            to[[ind[main_count - go_back_count]]] <- prev_resp
            changes <- changes[names(changes) != paste(ind[main_count - go_back_count])]

            main_count <- main_count - (go_back_count + 1)
            go_back_count <- 1

            break

          }else{
            message("This is the first response. 'GO BACK' is not available.")
            line_break()
            Sys.sleep(0.5)
          }

        }else{
          to[[i]] <- result$target
          changes <- result$changes
          full_dictionary <- result$full_dictionary
          break
        }

      }
    }

    if(Sys.info()["sysname"] == "Windows" && requireNamespace("tcltk", quietly = TRUE)){
      percent <- floor((main_count / length(ind)) * 100)
      info <- suppressWarnings(sprintf(paste(main_count, "of", length(ind), "responses done"), percent))
      tcltk::setTkProgressBar(pb, main_count, sprintf("Spell-check Progress (%s)", info), info)
    }else{
      cat("\n")
      setTxtProgressBar(pb, main_count)
      Sys.sleep(0.10)
      cat("\n")
    }

    main_count <- main_count + 1

  }

  if(main_count != 1){
    close(pb)
  }

  final_res <- list()

  # Update dictionary
  if(length(orig_dictionary) != length(full_dictionary)){

    choices <- c("In my results",
                 "In my working directory",
                 "I'd like to choose the directory",
                 "Don't save it")

    title <- "\nWhere would you like to save your additional dictionary entries?"

    custom_menu(choices = choices, title = title, default = 4, dictionary = TRUE)

    ans <- read_input("Selection: ", reader = reader)
    ans <- appropriate_answer(answer = ans, choices = choices, default = 4, dictionary = TRUE, reader = reader)

    if(ans == 1){

      final_res$dictionary <- full_dictionary
      message("\nDictionary saved to your output object under '$dictionary'")

    }else if(ans == 2){

      dictionary_name <- read_input("Name for the dictionary: ", reader = reader)
      dictionary_name <- gsub(".dictionary", "", dictionary_name)

      SemNetDictionaries::append.dictionary(full_dictionary, dictionary.name = dictionary_name,
                                            save.location = "wd", textcleaner = TRUE)

      message(paste("\nDictionary saved to your working directory: '", getwd(), "'", sep = ""))

    }else if(ans == 3){

      dictionary_name <- read_input("Name for the dictionary: ", reader = reader)
      dictionary_name <- gsub(".dictionary", "", dictionary_name)

      SemNetDictionaries::append.dictionary(full_dictionary, dictionary.name = dictionary_name,
                                            save.location = "choose", textcleaner = TRUE)

    }else{message("\nDictionary was not saved")}
  }

  # Run ad hoc check (if necessary). `has_topic_dictionary` hoists the
  # "is this a real topic dictionary, not just a raw spell-check
  # dictionary like cocaspell" check -- computed once here via the
  # memoized get_available_dictionaries() (in place of 4 raw
  # SemNetDictionaries::dictionaries(TRUE) calls, 2 of them repeated
  # twice on the same line -- that function does real file I/O every
  # call) and reused below for the free-only auto_ind filter, instead of
  # recomputing the identical check a second time.
  available_dictionaries <- get_available_dictionaries()
  has_topic_dictionary <- any(dictionary %in% available_dictionaries[available_dictionaries != "general"])

  if(main_count != 1){

    if(has_topic_dictionary){

      message("\nRunning ad hoc check for common misspellings and monikers...", appendLF = FALSE)

      target_dict <- dictionary[na.omit(match(available_dictionaries, dictionary))]
      target_moniker <- get_monikers(target_dict)

      replace_to <- parallel_process(
        length(to),
        function(idx, to, target_moniker, spelling){
          unlist(lapply(to[[idx]], function(x){
            if(!is.na(x)){
              unlist(moniker(x, target_moniker, spelling = spelling))
            }
          }))
        },
        to = to, target_moniker = target_moniker, spelling = spelling,
        cores = cores, progress = FALSE
      )
      names(replace_to) <- names(to)

      to <- replace_to

      message("done")

    }

  }

  if(type == "free"){

    # Separate automated indices. as.vector() strips na.omit()'s
    # na.action/class attributes -- cosmetic only (they never affect
    # length()/indexing), but they don't belong on a plain numeric index
    # vector that's part of this function's public return value
    auto_ind <- as.vector(na.omit(
      unlist(
        lapply(auto_ind, function(idx){
          from_response <- from[[idx]]
          to_response <- to[[idx]]
          if(all(!is.na(match(from_response, to_response)))){
            return(NA)
          }else{
            return(idx)
          }
        })
      )
    ))

    if(main_count != 1 && has_topic_dictionary){
      auto_ind <- auto_ind[!from[auto_ind] %in% target_moniker]
    }

    final_res$from <- from
    final_res$to <- to
    final_res$manual <- ind
    final_res$auto <- auto_ind
    final_res$data <- data
    final_res$type <- "free"
    final_res$stop <- FALSE

  }else{

    final_res$from <- from
    final_res$to <- to
    final_res$manual <- initial$manual
    final_res$auto <- initial$auto
    final_res$data <- data
    final_res$type <- "fluency"
    final_res$stop <- FALSE

  }

  class(final_res) <- "textcleaner"

  return(final_res)

}
