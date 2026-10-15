#' Wizard Menu Labels
#'
#' @description The exact, position-ordered menu labels
#' \code{spellcheck_menu()} itself displays for a given \code{context}/
#' \code{keep_strings} combination -- used by
#' \code{\link{textcleaner_shiny}}'s wizard module to render one button per
#' meta-option, each labeled with its real text and sending its real
#' 1-indexed display position as the raw answer (see
#' \code{\link{build_answer_burst}}).
#'
#' Exported (not just internal) because \code{inst/Shiny/}'s own module
#' files are sourced into the global environment by Shiny's own app-loading
#' mechanism, not into the package namespace -- an unexported function is
#' not reachable from there by its bare name.
#'
#' @param context Character vector or \code{NULL}.
#' The pending decision's own \code{$pending$context} -- \code{NULL} for a
#' single-response decision, the multi-word vector otherwise
#'
#' @param keep_strings Logical.
#' The pending decision's own \code{$pending$keep_strings}
#'
#' @return A character vector of menu labels, in real display order
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @keywords internal
# Wizard menu labels ----
# The menu labels, in display order, that `spellcheck_menu()` shows for a
# `context`/`keep_strings` combination. The internal Shiny wizard labels one button per
# option and sends its display position as the answer, which reproduces the menu's own
# renumbering for `keepStrings = TRUE`
#
# Keep in sync with the `word`, `string`, and `choices` vectors in `spellcheck_menu()`
# Updated 14.10.2026
wizard_menu_labels <- function(
    context, # NULL for single word or words of string
    keep_strings # whether strings are kept
)
{

  # Check for single-word menu
  if(is.null(context)){

    # Return single-word labels
    return(c("SKIP", "ADD TO DICTIONARY", "TYPE MY OWN", "GOOGLE IT", "BAD WORD"))

  }

  # Set word labels
  word <- c("SKIP WORD", "ADD WORD TO DICTIONARY", "TYPE MY OWN WORD", "GOOGLE WORD", "BAD WORD")

  # Set string labels
  string <- if(keep_strings){

    c("KEEP ORIGINAL", "TYPE MY OWN STRING", "GOOGLE STRING", "BAD STRING") # Strings are kept (no auto-correct option)

  }else{

    c("KEEP ORIGINAL", "KEEP AUTO-CORRECT", "TYPE MY OWN STRING", "GOOGLE STRING", "BAD STRING") # Strings are split

  }

  # Return all labels
  return(c(word, string))

}

#' Wizard Response Letters
#'
#' @description The QWERTY letters \code{appropriate_answer()} assigns to
#' non-default choices (\code{possible}, i.e. \code{\link{best_guess}}'s own
#' candidate list) in display order -- used by \code{\link{textcleaner_shiny}}'s
#' wizard module to label its suggestion buttons and to build the right raw
#' answer for one (see \code{\link{build_answer_burst}}).
#'
#' Exported (not just internal) for the same reason as
#' \code{\link{wizard_menu_labels}} -- reachability from \code{inst/Shiny/}.
#'
#' @param n Numeric.
#' The number of candidates to label (\code{length(pending$possible)}).
#' Explicitly capped at 10 here (not just assumed) -- \code{best_guess()}
#' itself never returns more than 10 candidates in its give-up list, but
#' indexing a 10-element vector with \code{seq_len(n)} for \code{n > 10}
#' would otherwise silently return \code{n} elements, the extra ones
#' \code{NA}, rather than truncating
#'
#' @return A character vector of QWERTY letters, in display order
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @keywords internal
# Wizard response letters ----
# Updated 14.10.2026
wizard_response_letters <- function(
    n # number of suggestions
)
{

  # Return letters for suggestions (maximum of 10)
  return(c("Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P")[seq_len(min(n, 10))])

}
