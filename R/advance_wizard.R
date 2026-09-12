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
#' @export
# Advances the manual spell-check wizard by exactly ONE user decision, for
# `textcleaner_shiny()`'s wizard module (Phase 11). Wraps the mechanics
# common to every button click: build a one-shot queued reader out of
# `answers` (typically from `build_answer_burst()`), resume
# `textcleaner(continue = ...)` with it, and return whatever it stops at
# next (either a further `$stop = TRUE`/`$pending` decision, or the finished
# result).
#
# The leading `""` prepended to every call's queue is not itself an answer
# to any real decision -- confirmed via direct reading of `walk_through.R`:
# whenever `length(ind) != 0` (there is at least one manual response for the
# whole session), `spellcheck_dictionary()` calls `walk_through(FALSE,
# reader = reader)` at the top of EVERY resume, not just the first, and
# `walk_through()`'s own final line (`read_input("Press ENTER to start
# manual spell-check...", reader = reader)`) unconditionally consumes one
# reader() call whose return value is discarded entirely -- any string
# satisfies it. Omitting this placeholder would misalign every real answer
# in `answers` by one position against the actual sequence of `read_input()`
# calls a resume triggers.
#
# Updated 12.09.2026
advance_wizard <- function(
  continue, answers = character(0), search_fn = searcher::search_site,
  edit_fn = function(x) x, cores = 1
)
{

  queue <- c("", answers)
  index <- 0L

  queue_reader <- function(prompt = ""){
    index <<- index + 1L
    if(index > length(queue)){
      return("")
    }
    queue[[index]]
  }

  textcleaner(
    continue = continue, reader = queue_reader, search_fn = search_fn,
    edit_fn = edit_fn, cores = cores
  )

}
