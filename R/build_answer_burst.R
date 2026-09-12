#' Build a Wizard Answer Burst
#'
#' @description Builds the exact queued-answer burst one
#' \code{\link{textcleaner_shiny}} wizard button click needs to feed a
#' single resume call (\code{textcleaner(continue = ..., reader = <queue
#' reader>, ...)}) all the way through the one decision it represents,
#' without starving or overrunning the console wizard's own prompts. See
#' \code{\link{advance_wizard}}.
#'
#' Exported (not just internal) because \code{inst/Shiny/}'s own module
#' files are sourced into the global environment by Shiny's own app-loading
#' mechanism, not into the package namespace -- an unexported function is
#' not reachable from there by its bare name.
#'
#' @param menu_number Character or numeric, length 1.
#' The real, currently-displayed menu position (or letter, for GO BACK/HELP/
#' a response-option suggestion) to send -- see \code{\link{wizard_menu_labels}}/
#' \code{\link{wizard_response_letters}}
#'
#' @param typed_text Character or \code{NULL}.
#' The free text typed for a TYPE MY OWN WORD/STRING decision. \code{NULL}
#' (the default) for every other decision
#'
#' @param add_to_dictionary Logical.
#' Whether to answer "yes" to any dictionary-add prompt this decision
#' triggers. Defaults to \code{FALSE}
#'
#' @param full_dictionary Character vector.
#' The current full dictionary, used to determine how many (if any)
#' dictionary-add prompts \code{typed_text}/\code{original_check} will
#' trigger. Required whenever either of those is supplied
#'
#' @param original_check Character or \code{NULL}.
#' The pending decision's own \code{original} text, supplied only when
#' \code{menu_number} corresponds to KEEP ORIGINAL -- see this function's
#' own header comment for the real hang this closes
#'
#' @return A character vector: the answers to queue, in order
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Builds the exact queued-answer burst one wizard button click needs to feed
# a single resume call (`textcleaner(continue = ..., reader = <queue
# reader>, ...)`) all the way through the ONE decision it represents,
# without either starving or overrunning the console wizard's own prompts.
#
# New (Phase 11), for `textcleaner_shiny()`'s wizard module (`mod_wizard.R`,
# `inst/Shiny/R/`). Package-level (not app-level) specifically because its
# correctness is safety-critical, not just a UI convenience -- confirmed via
# direct reading of `spellcheck_menu.R`/`helpers.R`:
#
# - `spellcheck_menu()`'s *outer* "Selection" read is the only one where an
#   exhausted queue (`reader()` returning `""`) means STOP -- it's caught by
#   `if(tolower(ans) == "x" || ans == ""){return("STOP")}` right where it's
#   read. That's the safe, intended way a resume call ends.
# - TYPE MY OWN WORD/STRING's own free-text `read_input()` has NO such
#   guard -- an exhausted queue there is read as the literal empty string
#   `""`, split into zero words, and used as-is (not treated as STOP).
# - `offer_add_missing_to_dictionary()`'s per-word yes/no prompt
#   (`yes_no_menu()`, `helpers.R`) is worse: `yes_no_menu()` re-prompts in an
#   UNGUARDED `while(is.na(yes_no(ans)))` loop. Once the queue is exhausted,
#   every further `reader()` call also returns `""`, which `yes_no()` can
#   never resolve -- an under-supplied burst here doesn't pause the wizard,
#   it hangs the whole synchronous resume call in an infinite loop.
#
# So a button's queued burst must supply EXACTLY the number of answers the
# decision it represents will actually consume: 1 for a plain menu choice;
# 3 for TYPE MY OWN WORD/STRING (menu number, the typed text, one "y"/"n"
# per word `offer_add_missing_to_dictionary()` will genuinely prompt about
# -- computed here via the same `dictionary_contains()` check that function
# itself uses, never a naive fixed-size guess).
#
# A THIRD, easy-to-miss site of this exact hazard (found via a real hang
# during Phase 11's own end-to-end wizard testing, not anticipated up
# front): KEEP ORIGINAL (context/multi-word branch only) has its own,
# independent `dictionary_contains(original, full_dictionary)` check --
# `original` there is the WHOLE, un-split multi-word response text (e.g.
# `"turtle catdog elephant"`), which is essentially never itself a literal
# dictionary entry, so choosing KEEP ORIGINAL on a multi-word response
# reliably triggers one more `yes_no_menu()` prompt asking whether to add
# that whole string as a dictionary entry -- confirmed via direct
# reproduction that an under-supplied burst here hangs identically to the
# TYPE MY OWN WORD/STRING case. `original_check` closes this: pass the
# pending decision's own `original` value whenever the button being sent
# is KEEP ORIGINAL (and only then -- every other meta-option never reaches
# this check).
#
# Updated 12.09.2026
build_answer_burst <- function(
  menu_number, typed_text = NULL, add_to_dictionary = FALSE, full_dictionary = NULL,
  original_check = NULL
)
{

  build_answer_burst_errors(menu_number, typed_text, add_to_dictionary, full_dictionary, original_check)

  answers <- as.character(menu_number)

  if(!is.null(typed_text)){

    answers <- c(answers, typed_text)

    words <- trimws(unlist(strsplit(typed_text, split = ",")))
    words <- words[nzchar(words)]

    if(length(words) > 0){

      # Matches offer_add_missing_to_dictionary()'s own `which(!in_dictionary)`
      # exactly -- one "y"/"n" answer per word not already in
      # `full_dictionary`, in the same order that function iterates them
      needs_prompt <- !dictionary_contains(words, full_dictionary)

      if(any(needs_prompt)){
        answers <- c(answers, rep(if(isTRUE(add_to_dictionary)) "y" else "n", sum(needs_prompt)))
      }

    }

  }

  if(!is.null(original_check) && !dictionary_contains(original_check, full_dictionary)){
    answers <- c(answers, if(isTRUE(add_to_dictionary)) "y" else "n")
  }

  answers

}

#' @noRd
# Up-front validation for `build_answer_burst()`
# Updated 12.09.2026
build_answer_burst_errors <- function(menu_number, typed_text, add_to_dictionary, full_dictionary, original_check)
{

  length_error(menu_number, 1, "build_answer_burst")

  if(!is.null(typed_text) || !is.null(original_check)){

    typeof_error(full_dictionary, "character", "build_answer_burst")

    if(length(full_dictionary) == 0){
      stop("In `build_answer_burst()`: `full_dictionary` must not be empty when `typed_text` or `original_check` is supplied.", call. = FALSE)
    }

  }

  if(!is.null(typed_text)){
    typeof_error(typed_text, "character", "build_answer_burst")
    length_error(typed_text, 1, "build_answer_burst")
  }

  if(!is.null(original_check)){
    typeof_error(original_check, "character", "build_answer_burst")
    length_error(original_check, 1, "build_answer_burst")
  }

  typeof_error(add_to_dictionary, "logical", "build_answer_burst")
  length_error(add_to_dictionary, 1, "build_answer_burst")

}
