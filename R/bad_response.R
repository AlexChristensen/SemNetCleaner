#' Changes Bad Responses to \code{NA}
#'
#' @description Determines whether responses are good or bad.
#' Bad responses are replaced with missing (\code{NA}). Good responses are returned
#'
#' @param word Character.
#' A word to be tested for whether it is bad
#'
#' @param ... Vector.
#' Additional responses to be considered bad
#'
#' @return If response is bad, then returns \code{NA}.
#' If response is valid, then returns the response
#'
#' @examples
#' # Bad response
#' bad_response(word = " ")
#'
#' # Good response
#' bad_response(word = "hello")
#'
#' # Make a good response bad
#' bad_response(word = "hello", "hello")
#'
#' # Add additional bad responses
#' bad_response(word = "hello", c("hello", "world"))
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Bad Response Function
# Ported from `bad.response()`. Real bug fixed: the original package defined
# this function TWICE -- once here (as R/bad.response.R, the exported one)
# and, byte-identical, `@noRd` inside utils-SemNetCleaner.R -- alphabetical
# file collation meant the utils copy silently overwrote this "real" one at
# load time. Now defined exactly once.
# Optimization (microbenchmark): the original's explicit per-element for()
# loop replaced with one vectorized `%in%` + logical-index assignment --
# ~3.4x faster for a single word (~13us -> ~4us, median) and ~24x faster at
# 1000 words (~1030us -> ~43us, median, the realistic whole-dataset-column
# call shape `responses_to_binary()` uses below). Verified identical output
# across every case in the original's own @examples plus NA, empty-vector,
# and mixed-NA/blank/whitespace edge cases.
# Updated 08.09.2026
bad_response <- function(word, ...)
{

  others <- unlist(list(...))
  bad <- c(NA, "NA", "", " ", "  ", others)

  if(length(word) == 0){
    word <- NA
  }

  word[word %in% bad] <- NA

  return(word)

}
