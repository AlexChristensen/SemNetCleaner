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
#' @keywords internal
# Build answer burst ----
# Builds the answers that one button click of the Shiny wizard feeds to a single
# resume call, so that the wizard neither runs out of answers nor is given too many
#
# The count has to be exact: running out at a menu safely ends the call, but running
# out at a yes/no prompt (`yes_no_menu()`) loops forever. A plain menu choice needs
# one answer; TYPE MY OWN WORD/STRING needs the menu number, the typed text, and one
# "y"/"n" for each typed word that is not in `full_dictionary`; KEEP ORIGINAL needs one
# more "y"/"n" when the whole `original` string is not a dictionary entry
# (`original_check`)
# Updated 14.10.2026
build_answer_burst <- function(
    menu_number, # number chosen in the menu
    typed_text = NULL, # text typed after the menu choice
    add_to_dictionary = FALSE, # answer to the add-to-dictionary prompts
    full_dictionary = NULL, # dictionary used to check typed words
    original_check = NULL # original string checked against the dictionary
)
{

  # Check for errors
  build_answer_burst_errors(menu_number, typed_text, add_to_dictionary, full_dictionary, original_check)

  # Start with the menu number
  answers <- as.character(menu_number)

  # Check for typed text
  if(!is.null(typed_text)){

    # Add typed text
    answers <- c(answers, typed_text)

    # Split typed text into words
    words <- trimws(unlist(strsplit(typed_text, split = ",")))

    # Remove empty words
    words <- words[nzchar(words)]

    # Check for any words
    if(length(words) > 0){

      # Matches offer_add_missing_to_dictionary()'s own `which(!in_dictionary)`
      # exactly -- one "y"/"n" answer per word not already in
      # `full_dictionary`, in the same order that function iterates them
      needs_prompt <- !dictionary_contains(words, full_dictionary)

      # Check for words that need a prompt
      if(any(needs_prompt)){

        # Answer each prompt
        answers <- c(answers, rep(if(isTRUE(add_to_dictionary)) "y" else "n", sum(needs_prompt)))

      }

    }

  }

  # Check whether the original string needs a prompt
  if(!is.null(original_check) && !dictionary_contains(original_check, full_dictionary)){

    # Answer the prompt
    answers <- c(answers, if(isTRUE(add_to_dictionary)) "y" else "n")

  }

  # Return answers
  return(answers)

}

#' @noRd
# Build answer burst errors ----
# Up-front validation for `build_answer_burst()`
# Updated 14.10.2026
build_answer_burst_errors <- function(
    menu_number, # number chosen in the menu
    typed_text, # text typed after the menu choice
    add_to_dictionary, # answer to the add-to-dictionary prompts
    full_dictionary, # dictionary used to check typed words
    original_check # original string checked against the dictionary
)
{

  # Check menu number length
  length_error(menu_number, 1, "build_answer_burst")

  # Check for typed text or original string
  if(!is.null(typed_text) || !is.null(original_check)){

    # Check dictionary type
    typeof_error(full_dictionary, "character", "build_answer_burst")

    # Check for empty dictionary
    if(length(full_dictionary) == 0){

      # Send error
      stop("In `build_answer_burst()`: `full_dictionary` must not be empty when `typed_text` or `original_check` is supplied.", call. = FALSE)

    }

  }

  # Check for typed text
  if(!is.null(typed_text)){

    # Check typed text type
    typeof_error(typed_text, "character", "build_answer_burst")

    # Check typed text length
    length_error(typed_text, 1, "build_answer_burst")

  }

  # Check for original string
  if(!is.null(original_check)){

    # Check original string type
    typeof_error(original_check, "character", "build_answer_burst")

    # Check original string length
    length_error(original_check, 1, "build_answer_burst")

  }

  # Check answer type
  typeof_error(add_to_dictionary, "logical", "build_answer_burst")

  # Check answer length
  length_error(add_to_dictionary, 1, "build_answer_burst")

}
