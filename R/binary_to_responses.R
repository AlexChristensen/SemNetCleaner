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
#' # Toy example
#' raw <- open.animals[c(1:10), -c(1:3)]
#'
#' if(interactive())
#' {
#'   # Clean and preprocess data
#'   clean <- textcleaner(open.animals[, -c(1:2)], partBY = "row", dictionary = "animals")
#'
#'   # Change binary response matrix to word response matrix
#'   char_matrix <- binary_to_responses(clean$responses$binary)
#' }
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Binary to Response
# Ported from `bin2resp()`, renamed per the naming convention (reuses
# `../SemNeT`'s own established name for the paired concept). Real bugs
# fixed: 4 separate `for(i in 1:n)`-style loops all reverse-range whenever
# their respective dimension (ncol/nrow of `binary_matrix`, or the derived
# participant count) is 0 -- a degenerate but reachable input (e.g. an
# empty vocabulary, or every participant filtered out upstream). All 4
# fixed via seq_len().
# Follow-up review found a second, previously-masked bug: with 0
# participants, `max(response_lengths)` on a length-0 vector returns
# `-Inf` (with a warning), which then crashes `matrix(nrow = -Inf, ...)`.
# In the ORIGINAL this was never reachable -- its own `1:p` reverse-range
# bug (now fixed above) crashed first, on the main collection loop, before
# ever reaching this later code. Guarded explicitly instead.
#
# Optimization pass (microbenchmark, real 516x368 open.clean-derived
# data), both verified identical() against the original loop-based logic
# on real data plus 0-participant/0-response/all-NA/single-row edge cases:
# - The column-fill loop replaced with one vectorized
#   `response_names[col(binary_matrix)]` construction -- ~4-5x faster
#   (~16.8ms -> ~3.6ms, median).
# - The participant-collection loop replaced with a single `which()` +
#   `split()` pass -- ~7x faster (~10.9ms -> ~1.5ms, median). Confirmed
#   the original never actually names each participant's individual
#   responses (word_matrix carries no column names in either version), so
#   no name-preservation logic was needed here.
# Benchmarked and explicitly rejected: replacing the final padding loop
# (inside `to_data_frame`) with a `sapply()`-based version measured no
# improvement (~1.8ms either way) and wasn't `identical()` besides
# (dimnames differences) -- not applied.
# The vectorized column-fill introduced its own new regression, caught
# immediately by the existing 0-column test: `colnames()` returns `NULL`
# (not `character(0)`) for a 0-column matrix, which the original's loop
# never noticed (seq_len(0) just skips it) but the vectorized one-shot
# `response_names[col(binary_matrix)]` construction evaluates eagerly --
# `matrix(NULL, ...)` errors outright. Guarded with an explicit
# `is.null()` coalesce.
# Updated 09.09.2026
binary_to_responses <- function(binary_matrix, to_data_frame = FALSE)
{

  binary_to_responses_errors(binary_matrix, to_data_frame)

  # Response (column) names. `colnames()` returns NULL (not `character(0)`)
  # for a 0-column matrix, which `matrix()` below would reject outright
  # ("'data' must be of a vector type, was 'NULL'") -- confirmed reachable
  # via a 0-column `binary_matrix` with row names but no column names set
  response_names <- colnames(binary_matrix)
  if(is.null(response_names)){
    response_names <- character(0)
  }

  # Fill in each column's response name wherever it was actually given
  word_matrix <- matrix(
    response_names[col(binary_matrix)],
    nrow = nrow(binary_matrix), ncol = ncol(binary_matrix)
  )
  word_matrix[binary_matrix != 1] <- NA

  # Number of participants
  n_participants <- nrow(binary_matrix)

  # Collect each participant's given responses: every non-missing cell's
  # (row, column) position, ordered so each participant's own responses
  # stay in their original left-to-right column order, then split by row
  given <- which(!is.na(word_matrix), arr.ind = TRUE)
  given <- given[order(given[, 1], given[, 2]), , drop = FALSE]

  participant_list <- split(word_matrix[given], given[, 1])

  # Participants with zero responses have no entry in 'given' at all --
  # fill those back in as empty vectors, matching the original's own
  # per-participant loop, which always creates a (possibly zero-length) entry
  full_list <- setNames(vector("list", n_participants), as.character(seq_len(n_participants)))
  full_list[names(participant_list)] <- participant_list
  empty_slots <- lengths(full_list) == 0
  full_list[empty_slots] <- replicate(sum(empty_slots), character(0), simplify = FALSE)
  names(full_list) <- row.names(binary_matrix)

  participant_list <- full_list

  # Convert output to data frame
  if(to_data_frame){

    # lengths() replaces the original's own per-element length() loop --
    # ~14x faster (~62us -> ~4.5us, median) though this specific loop was
    # never a meaningful share of the function's total runtime
    response_lengths <- as.numeric(lengths(participant_list))
    n_lengths <- length(response_lengths)

    max_responses <- if(n_lengths == 0){ 0 }else{ max(response_lengths) }

    participant_matrix <- matrix("", nrow = max_responses, ncol = n_lengths)

    for(i in seq_len(n_lengths)){
      padding <- max_responses - response_lengths[i]
      participant_matrix[, i] <- c(unlist(participant_list[[i]]), rep("", padding))
    }

    participant_list <- as.data.frame(participant_matrix)
    colnames(participant_list) <- row.names(binary_matrix)
    participant_list <- t(participant_list)

  }

  return(participant_list)

}

#' @noRd
# Up-front validation for `binary_to_responses()`
# Updated 08.09.2026
binary_to_responses_errors <- function(binary_matrix, to_data_frame)
{
  object_error(binary_matrix, c("matrix", "data.frame"), "binary_to_responses")
  typeof_error(to_data_frame, "logical", "binary_to_responses")
  length_error(to_data_frame, 1, "binary_to_responses")
}
