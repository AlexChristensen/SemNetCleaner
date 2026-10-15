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
#' # First 10 participants of the cleaned data
#' binary_matrix <- responses_to_binary(open.clean[1:10, ])
#'
#' # Participants by responses (0 = not given, 1 = given)
#' binary_matrix[, 1:10]
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @importFrom stats na.omit
#'
#' @export
# Responses to binary ----
# Converts a matrix of responses (participants in rows) to a binary matrix of
# participants by vocabulary; a matrix with no elements gives a matrix with no columns
# Updated 14.10.2026
responses_to_binary <- function(
    responses # matrix or data frame of responses
)
{

  # Check for errors
  responses_to_binary_errors(responses)

  # Data matrix
  mat <- as.matrix(responses)

  # Get number of participants
  n_cases <- nrow(mat)

  # A completely empty input (0 rows, or 0 columns with 0+ rows) has no
  # vocabulary to observe -- short-circuit before bad_response() ever sees
  # it (see the bug note above), preserving the participant count either way
  if(length(mat) == 0){

    # Create empty binary matrix
    binary_matrix <- matrix(0, nrow = n_cases, ncol = 0)

    # Keep participant names
    row.names(binary_matrix) <- row.names(responses)

    # Return empty matrix
    return(binary_matrix)

  }

  # Replace bad responses with NA so they never become a "word"
  mat <- bad_response(mat)

  # Full vocabulary observed across all participants, alphabetized
  vocabulary <- sort(na.omit(unique(as.vector(mat))))

  # Initialize binary matrix
  binary_matrix <- matrix(0, nrow = n_cases, ncol = length(vocabulary))

  # Name columns by word
  colnames(binary_matrix) <- vocabulary

  # Name rows by participant
  row.names(binary_matrix) <- row.names(responses)

  # Mark each participant's given responses with one vectorized match
  matched <- match(mat, colnames(binary_matrix))

  # Get participant of each response
  row_indices <- rep(seq_len(n_cases), times = ncol(mat))

  # Get non-missing responses
  valid <- !is.na(matched)

  # Mark given responses
  binary_matrix[cbind(row_indices[valid], matched[valid])] <- 1

  # Return binary matrix
  return(binary_matrix)

}

#' @noRd
# Responses to binary errors ----
# Up-front validation for `responses_to_binary()`
# Updated 14.10.2026
responses_to_binary_errors <- function(
    responses # matrix or data frame of responses
)
{

  # Check for valid input type
  object_error(responses, c("matrix", "data.frame", "vector"), "responses_to_binary")

}
