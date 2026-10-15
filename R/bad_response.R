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
# Bad response ----
# Replaces bad responses (`NA`, "NA", blanks, and anything passed in `...`) with `NA`
# Vectorized, so a whole column of responses is checked in one call
# Updated 14.10.2026
bad_response <- function(word, ...)
{

  # Get additional bad responses
  others <- unlist(list(...))

  # Set all bad responses
  bad <- c(NA, "NA", "", " ", "  ", others)

  # Check for empty input
  if(length(word) == 0){

    # Set to missing
    word <- NA

  }

  # Replace bad responses with missing
  word[word %in% bad] <- NA

  # Return cleaned responses
  return(word)

}
