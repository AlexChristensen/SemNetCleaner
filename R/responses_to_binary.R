#' Converts a Response Matrix to a Binary Response Matrix
#'
#' @description Converts a response matrix (verbal fluency or other linguistic
#' data, one participant per row) into a binary (participant x vocabulary)
#' response matrix
#'
#' @param responses Matrix or data frame.
#' A response matrix of verbal fluency or linguistic data
#'
#' @return A binary matrix with one row per participant and one column per
#' unique response observed across the whole sample
#'
#' @examples
#' # Toy example
#' raw <- open.animals[c(1:10), -c(1:3)]
#'
#' if(interactive())
#' {
#'   # Clean and preprocess data
#'   clean <- textcleaner(open.animals[, -c(1:2)], partBY = "row", dictionary = "animals")
#'
#'   # Change response matrix to binary response matrix
#'   binary_matrix <- responses_to_binary(clean$responses$corrected)
#' }
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @importFrom stats na.omit
#'
#' @export
# Response matrix to binary matrix
# Ported from `resp2bin()`, renamed per the naming convention: reuses the
# exact name `../SemNeT`'s own `helpers.R` already established for this same
# underlying operation ("convert a raw word-response matrix into a binary
# participant x vocabulary matrix") -- confirmed the two implementations are
# fundamentally the same algorithm on direct comparison; kept this function's
# plain-matrix return shape (not SemNeT's order-tracking list/class wrapper),
# matching this package's own original documented contract.
# Real bug fixed: `for(i in 1:n)` reverse-ranges to `1:0` when `nrow(responses)
# == 0` (a degenerate but reachable input, e.g. after upstream filtering drops
# every participant), indexing row 1 of a 0-row matrix -- fixed via seq_len().
# Second real bug fixed, confirmed inherited (the untouched original
# resp2bin() crashes identically): a `responses` matrix with 0 total
# elements (0 rows, OR 0 columns with any number of rows) has
# `length(mat) == 0`, so bad_response()'s own `if(length(word) == 0) word <-
# NA` guard collapses the WHOLE MATRIX into a bare scalar NA -- destroying
# its dimensions, so the later `nrow(mat)` no longer returns a number
# ("non-numeric matrix extent"). Fixed with an early return before
# bad_response() is ever called, rather than changing bad_response()'s own
# (separately relied-upon) zero-length guard. NOTE: an earlier version of
# this fix only guarded `nrow(mat) == 0`, missing the 0-column/>0-row case
# -- caught during a follow-up review, re-checking `length(mat) == 0`
# (total elements) instead covers both degenerate shapes correctly.
# Updated 08.09.2026
responses_to_binary <- function(responses)
{

  responses_to_binary_errors(responses)

  # Data matrix
  mat <- as.matrix(responses)
  n_cases <- nrow(mat)

  # A completely empty input (0 rows, or 0 columns with 0+ rows) has no
  # vocabulary to observe -- short-circuit before bad_response() ever sees
  # it (see the bug note above), preserving the participant count either way
  if(length(mat) == 0){
    binary_matrix <- matrix(0, nrow = n_cases, ncol = 0)
    row.names(binary_matrix) <- row.names(responses)
    return(binary_matrix)
  }

  # Replace bad responses with NA so they never become a "word"
  mat <- bad_response(mat)

  # Full vocabulary observed across all participants, alphabetized
  vocabulary <- sort(na.omit(unique(as.vector(mat))))

  # Initialize binary matrix
  binary_matrix <- matrix(0, nrow = n_cases, ncol = length(vocabulary))
  colnames(binary_matrix) <- vocabulary
  row.names(binary_matrix) <- row.names(responses)

  # Mark each participant's given responses. Optimization (microbenchmark,
  # real 516x34 open.clean data): the original's per-row loop replaced with
  # one vectorized match() + matrix-index assignment -- ~60x faster (~137ms
  # -> ~2.2ms, median, n=100). Verified identical() output across real
  # bundled data plus empty-vector/NA/single-row/0-row/0-column edge cases.
  matched <- match(mat, colnames(binary_matrix))
  row_indices <- rep(seq_len(n_cases), times = ncol(mat))
  valid <- !is.na(matched)
  binary_matrix[cbind(row_indices[valid], matched[valid])] <- 1

  return(binary_matrix)

}

#' @noRd
# Up-front validation for `responses_to_binary()`
# Updated 08.09.2026
responses_to_binary_errors <- function(responses)
{
  object_error(responses, c("matrix", "data.frame", "vector"), "responses_to_binary")
}
