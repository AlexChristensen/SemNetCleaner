#' @noRd
# Correspondence matrix ----
# Builds the "from"/"to_*" correspondence matrix: one row per unique response with its
# original value ("from") and every value it was changed to ("to_1", "to_2", ...),
# padded with `NA` to the widest response; no responses give a zero-row matrix
# Updated 14.10.2026
correspondence_matrix <- function(from, to)
{

  # Get number of unique responses
  n <- length(from)

  # Get widest number of corrected values (zero when empty)
  to_width <- if(length(to) == 0) 0 else max(unlist(lapply(to, length)))

  # Initialize correspondence matrix
  corr_mat <- matrix(NA, nrow = n, ncol = (to_width + 1))

  # Name columns (from, to_1, to_2, ...)
  colnames(corr_mat) <- c("from", paste("to_", seq_len(to_width), sep = "", recycle0 = TRUE))

  # Loop over responses
  for(i in seq_len(n)){

    # Combine original and corrected values
    insert <- c(from[[i]], to[[i]])

    # Fill row (remaining columns stay NA)
    corr_mat[i, 1:length(insert)] <- insert

  }

  # Return correspondence matrix
  return(corr_mat)

}
