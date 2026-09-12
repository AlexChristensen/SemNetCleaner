# Every pre-2.0.0 exported name that changed lives in this one file, as a
# thin wrapper calling straight into its new-named replacement -- keeps the
# whole compatibility surface auditable in one place (see REFACTOR_PLAN.md's
# Phase 8 write-up). `textcleaner()` and `convert2snafu()` kept their names
# unchanged (binding decision #7) and need no wrapper here.
#
# Real bug found and fixed here during Phase 8: this file originally opened
# with a roxygen `#' @noRd` line of its own (a leftover habit from every
# other file's own header, most of which really do want to mark the very
# next statement `@noRd`) -- but with nothing except comments between it and
# `bad.response()` below, roxygen2 attached BOTH this stray block and
# `bad.response()`'s own `@rdname`/`@export` block to the same function,
# and the inherited `@noRd` silently suppressed `bad.response` from ever
# being merged into `bad_response.Rd`'s `\alias{}` list (confirmed directly:
# `bad.response` still ended up correctly `export()`-ed to NAMESPACE, since
# that comes from its own block's `@export` tag, but `?bad.response` had no
# documentation to find at all). The other 5 wrappers below were never
# affected, since nothing else in this file precedes them. Made a plain,
# non-roxygen comment so it can't attach to anything.
# Updated 12.09.2026

#' @rdname bad_response
#' @export
# Legacy name for `bad_response()`.
bad.response <- function(word, ...)
{
  return(bad_response(word, ...))
}

#' @param full.dictionary Legacy name for `full_dictionary`.
#' @rdname best_guess
#' @export
# Legacy name for `best_guess()`.
best.guess <- function(word, full.dictionary, dictionary = NULL, tolerance = 1)
{
  return(best_guess(word, full_dictionary = full.dictionary, dictionary = dictionary, tolerance = tolerance))
}

#' @param rmat Legacy name for `binary_matrix`.
#' @param to.data.frame Legacy name for `to_data_frame`.
#' @rdname binary_to_responses
#' @export
# Legacy name for `binary_to_responses()`.
bin2resp <- function(rmat, to.data.frame = FALSE)
{
  return(binary_to_responses(binary_matrix = rmat, to_data_frame = to.data.frame))
}

#' @param wordA Legacy name for `word_a`.
#' @param wordB Legacy name for `word_b`.
#' @rdname qwerty_distance
#' @export
# Legacy name for `qwerty_distance()`.
qwerty.dist <- function(wordA, wordB)
{
  return(qwerty_distance(word_a = wordA, word_b = wordB))
}

#' @rdname read_data
#' @export
# Legacy name for `read_data()`.
read.data <- function(file = file.choose(), header = TRUE, sep = ",", ...)
{
  return(read_data(file = file, header = header, sep = sep, ...))
}

#' @param resp Legacy name for `responses`.
#' @rdname responses_to_binary
#' @export
# Legacy name for `responses_to_binary()`.
resp2bin <- function(resp)
{
  return(responses_to_binary(responses = resp))
}
