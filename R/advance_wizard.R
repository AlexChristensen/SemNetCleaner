#' Advance the \code{textcleaner_shiny()} Wizard by One Decision
#'
#' @description Wraps the mechanics common to every
#' \code{\link{textcleaner_shiny}} wizard button click: builds a one-shot
#' queued reader out of \code{answers} (typically from
#' \code{\link{build_answer_burst}}), resumes \code{textcleaner(continue =
#' ...)} with it, and returns whatever it stops at next -- either a further
#' \code{$stop = TRUE}/\code{$pending} decision, or the finished result.
#'
#' Exported (not just internal) because \code{inst/Shiny/}'s own module
#' files are sourced into the global environment by Shiny's own app-loading
#' mechanism, not into the package namespace -- an unexported function is
#' not reachable from there by its bare name.
#'
#' @param continue List.
#' The current \code{textcleaner()}/\code{spellcheck_dictionary()} return
#' value to resume from (its own \code{$stop = TRUE}/\code{$pending} state)
#'
#' @param answers Character vector.
#' The answers to queue for this one resume call, typically built by
#' \code{\link{build_answer_burst}}. Defaults to \code{character(0)} (a
#' resume with no real answer queued -- immediately stops again at the same
#' decision, e.g. after only a GOOGLE WORD/STRING/IT click)
#'
#' @param search_fn Function.
#' Passed through to \code{textcleaner()}'s own \code{search_fn}. Defaults
#' to \code{searcher::search_site}; \code{textcleaner_shiny()} itself passes
#' \code{\link{make_shiny_search_fn}}'s closure instead
#'
#' @param edit_fn Function.
#' Passed through to \code{textcleaner()}'s own \code{edit_fn}. Defaults to
#' a no-op (\code{function(x) x}), deferring the real automated-corrections
#' review to \code{textcleaner_shiny()}'s own later step
#'
#' @param cores Numeric.
#' Passed through to \code{textcleaner()}'s own \code{cores}. Defaults to 1
#'
#' @return Whatever \code{textcleaner()} returns for this resume call -- a
#' further \code{$stop = TRUE}/\code{$pending} decision, or the finished
#' \code{textcleaner}-classed result
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @keywords internal
# Advance wizard ----
# Advances the manual spell-check wizard by exactly one decision for the internal
# Shiny wizard: builds a one-shot reader from `answers`, resumes
# `textcleaner(continue = )` with it, and returns where it stops next (or the
# finished result)
#
# The leading "" in the queue answers the "Press ENTER to start manual
# spell-check..." prompt that every resume shows; without it every real answer
# would be read one position early
# Updated 14.10.2026
advance_wizard <- function(
    continue, # unfinished result of textcleaner
    answers = character(0), # answers to give the wizard
    search_fn = searcher::search_site, # function used to search the web
    edit_fn = function(x) x, # function used to edit changes
    cores = 1 # number of cores
)
{

  # Every click resumes the wizard; the app has its own help, so no browser
  # window is opened for each of them (see `help_window_enabled()`)
  old_options <- options(SemNetCleaner.help_window = FALSE)

  # Restore options on exit
  on.exit(options(old_options), add = TRUE)

  # Set up answer queue (leading blank clears the start prompt)
  queue <- c("", answers)

  # Initialize answer counter
  index <- 0L

  # Create reader that returns queued answers
  queue_reader <- function(prompt = ""){

    # Move to next answer
    index <<- index + 1L

    # Check for exhausted queue
    if(index > length(queue)){

      # Return blank when out of answers
      return("")

    }

    # Return next answer
    queue[[index]]

  }

  # Resume wizard with queued answers
  return(textcleaner(
    continue = continue, reader = queue_reader, search_fn = search_fn,
    edit_fn = edit_fn, cores = cores
  ))

}
