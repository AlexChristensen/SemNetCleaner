# Every pre-2.0.0 exported name that changed lives in this file as a thin wrapper that
# calls its new-named replacement, so the compatibility surface can be audited in one
# place (`textcleaner()` and `convert2snafu()` kept their names and need no wrapper)
#
# This comment is deliberately not roxygen: a stray `#' @noRd` here would attach to
# `bad.response()` and remove it from the help page
# Updated 14.10.2026

#' @rdname bad_response
#' @export
# Legacy: bad_response ----
# Legacy name for `bad_response()`.
# Updated 14.10.2026
bad.response <- function(
    word, # word(s) to check
    ... # additional bad responses
)
{

  # Call bad_response
  return(bad_response(word, ...))

}

#' @param full.dictionary Legacy name for `full_dictionary`.
#' @rdname best_guess
#' @export
# Legacy: best_guess ----
# Legacy name for `best_guess()`.
# Updated 14.10.2026
best.guess <- function(
    word, # word to correct
    full.dictionary, # dictionary used to check words
    dictionary = NULL, # names of dictionaries requested
    tolerance = 1 # maximum edit distance for automatic correction
)
{

  # Call best_guess
  return(best_guess(word, full_dictionary = full.dictionary, dictionary = dictionary, tolerance = tolerance))

}

#' @param rmat Legacy name for `binary_matrix`.
#' @param to.data.frame Legacy name for `to_data_frame`.
#' @rdname binary_to_responses
#' @export
# Legacy: binary_to_responses ----
# Legacy name for `binary_to_responses()`.
# Updated 14.10.2026
bin2resp <- function(
    rmat, # binary matrix of responses
    to.data.frame = FALSE # return a data frame instead of a list
)
{

  # Call binary_to_responses
  return(binary_to_responses(binary_matrix = rmat, to_data_frame = to.data.frame))

}

#' @param wordA Legacy name for `word_a`.
#' @param wordB Legacy name for `word_b`.
#' @rdname qwerty_distance
#' @export
# Legacy: qwerty_distance ----
# Legacy name for `qwerty_distance()`.
# Updated 14.10.2026
qwerty.dist <- function(
    wordA, # first word
    wordB # second word
)
{

  # Call qwerty_distance
  return(qwerty_distance(word_a = wordA, word_b = wordB))

}

#' @rdname read_data
#' @export
# Legacy: read_data ----
# Legacy name for `read_data()`.
# Updated 14.10.2026
read.data <- function(
    file = file.choose(), # path to the file
    header = TRUE, # first row contains names
    sep = ",", # separator for delimited files
    ... # additional arguments passed to the reading function
)
{

  # Call read_data
  return(read_data(file = file, header = header, sep = sep, ...))

}

#' @param resp Legacy name for `responses`.
#' @rdname responses_to_binary
#' @export
# Legacy: responses_to_binary ----
# Legacy name for `responses_to_binary()`.
# Updated 14.10.2026
resp2bin <- function(
    resp # matrix or data frame of responses
)
{

  # Call responses_to_binary
  return(responses_to_binary(responses = resp))

}
