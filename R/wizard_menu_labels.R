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
#' @export
# The exact, position-ordered menu labels `spellcheck_menu()` itself
# displays for a given `context`/`keep_strings` combination -- used by
# `textcleaner_shiny()`'s wizard module (`mod_wizard.R`) to render one
# button per meta-option, each labeled with its real text and sending its
# real 1-indexed DISPLAY position as the raw answer.
#
# This is deliberately the ONLY piece `mod_wizard.R` needs to get right for
# the meta-options: `spellcheck_menu()`'s own `if(keep_strings){if(ans >= 7
# && ans <= length(choices)){ans <- ans + 1}}` remap (needed because
# `keep_strings = TRUE` displays 4 string options where `keep_strings =
# FALSE` displays 5, but the if/else dispatch chain below it always checks
# against the 5-option numbering) operates on the RAW typed number, not on
# any "logical" choice identity -- so sending the exact position a real
# console user would type for the SAME visible label reproduces the correct
# remapped behavior automatically, with no need to replicate that remap
# here. Verified end to end in `test-wizard_menu_labels.R` via
# `advance_wizard()` against both `keep_strings` values, not just compared
# against a hand-copied literal vector.
#
# Copied literally from `spellcheck_menu.R`'s own `word`/`string`/`choices`
# construction (context branch) and its single-response `choices` vector
# (non-context branch) -- keep these two files in sync if either changes.
# Updated 12.09.2026
wizard_menu_labels <- function(context, keep_strings)
{

  if(is.null(context)){
    return(c("SKIP", "ADD TO DICTIONARY", "TYPE MY OWN", "GOOGLE IT", "BAD WORD"))
  }

  word <- c("SKIP WORD", "ADD WORD TO DICTIONARY", "TYPE MY OWN WORD", "GOOGLE WORD", "BAD WORD")

  string <- if(keep_strings){
    c("KEEP ORIGINAL", "TYPE MY OWN STRING", "GOOGLE STRING", "BAD STRING")
  }else{
    c("KEEP ORIGINAL", "KEEP AUTO-CORRECT", "TYPE MY OWN STRING", "GOOGLE STRING", "BAD STRING")
  }

  c(word, string)

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
#' @export
wizard_response_letters <- function(n)
{
  c("Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P")[seq_len(min(n, 10))]
}
