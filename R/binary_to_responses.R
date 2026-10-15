#' Binary Responses to Character Responses
#'
#' @description Converts a binary response matrix into characters for each participant
#'
#' @param binary_matrix Matrix.
#' A binarized response matrix of verbal fluency or linguistic data
#'
#' @param to_data_frame Boolean.
#' Should output be a data frame where participants are columns?
#' Defaults to \code{FALSE}.
#' Set to \code{TRUE} to convert output to data frame
#'
#' @return A list containing objects for each participant and their responses
#'
#' @examples
#' # Binary matrix of the first 10 participants of the cleaned data
#' binary_matrix <- responses_to_binary(open.clean[1:10, ])
#'
#' # Each participant's responses, as a list
#' binary_to_responses(binary_matrix)
#'
#' # As a participants by responses matrix
#' binary_to_responses(binary_matrix, to_data_frame = TRUE)
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Binary to responses ----
# Converts a binary (0/1) response matrix back to each participant's responses, as a
# list (or a participants-by-responses matrix when `to_data_frame = TRUE`)
# Input with no participants or no responses is returned empty instead of failing
# Updated 14.10.2026
binary_to_responses <- function(
    binary_matrix, # binary matrix of responses
    to_data_frame = FALSE # return a data frame instead of a list
)
{

  # Check for errors
  binary_to_responses_errors(binary_matrix, to_data_frame)

  # Get response names (`colnames()` is `NULL` for a matrix without columns)
  response_names <- colnames(binary_matrix)

  # Check for missing names
  if(is.null(response_names)){

    # Set to empty
    response_names <- character(0)

  }

  # Fill in each column's response name wherever it was actually given
  word_matrix <- matrix(
    response_names[col(binary_matrix)],
    nrow = nrow(binary_matrix), ncol = ncol(binary_matrix)
  )

  # Remove words that were not given
  word_matrix[binary_matrix != 1] <- NA

  # Number of participants
  n_participants <- nrow(binary_matrix)

  # Get each participant's given responses in column order, then split by participant
  given <- which(!is.na(word_matrix), arr.ind = TRUE)

  # Order by participant and column
  given <- given[order(given[, 1], given[, 2]), , drop = FALSE]

  # Split by participant
  participant_list <- split(word_matrix[given], given[, 1])

  # Add participants without responses back as empty vectors
  full_list <- setNames(vector("list", n_participants), as.character(seq_len(n_participants)))

  # Fill in participants with responses
  full_list[names(participant_list)] <- participant_list

  # Find participants without responses
  empty_slots <- lengths(full_list) == 0

  # Fill in empty participants
  full_list[empty_slots] <- replicate(sum(empty_slots), character(0), simplify = FALSE)

  # Name by participant
  names(full_list) <- row.names(binary_matrix)

  # Store full list
  participant_list <- full_list

  # Convert output to data frame
  if(to_data_frame){

    # Get the number of responses of each participant
    response_lengths <- as.numeric(lengths(participant_list))

    # Get number of participants
    n_lengths <- length(response_lengths)

    # Get largest number of responses
    max_responses <- if(n_lengths == 0){ 0 }else{ max(response_lengths) }

    # Initialize participant matrix
    participant_matrix <- matrix("", nrow = max_responses, ncol = n_lengths)

    # Loop over participants
    for(i in seq_len(n_lengths)){

      # Get amount of padding
      padding <- max_responses - response_lengths[i]

      # Fill participant responses
      participant_matrix[, i] <- c(unlist(participant_list[[i]]), rep("", padding))

    }

    # Convert to data frame
    participant_list <- as.data.frame(participant_matrix)

    # Name columns
    colnames(participant_list) <- row.names(binary_matrix)

    # Transpose to participants by responses
    participant_list <- t(participant_list)

  }

  # Return responses
  return(participant_list)

}

#' @noRd
# Binary to responses errors ----
# Up-front validation for `binary_to_responses()`
# Updated 14.10.2026
binary_to_responses_errors <- function(
    binary_matrix, # binary matrix of responses
    to_data_frame # return a data frame instead of a list
)
{

  # Check input type
  object_error(binary_matrix, c("matrix", "data.frame"), "binary_to_responses")

  # Check argument type
  typeof_error(to_data_frame, "logical", "binary_to_responses")

  # Check argument length
  length_error(to_data_frame, 1, "binary_to_responses")

}
