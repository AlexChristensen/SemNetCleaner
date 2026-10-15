#' @noRd
# Correct data ----
# Applies the final (automated and manual) corrections to the fluency response matrix
# and counts perseverations (repeated responses) and intrusions (bad responses, "NA")
# for each participant
#
# Removals use a logical filter (`x[!cond]`) because `x[-which(cond)]` returns an
# empty vector when nothing matches
# Updated 14.10.2026
correct_data <- function(
    data, # matrix of original responses
    corr_mat # correspondence matrix
)
{

  # Get number of participants
  n <- nrow(data)

  # Initialize behavioral matrix
  behav_mat <- matrix(0, nrow = n, ncol = 2)

  # Name columns
  colnames(behav_mat) <- c("Perseverations", "Intrusions")

  # Name rows
  row.names(behav_mat) <- row.names(data)

  # First pass: collect every participant's corrected responses because the width of
  # the output depends on the longest one (a response can be corrected into several words)
  correct_list <- vector("list", n)

  # Loop over participants
  for(i in seq_len(n)){

    # Match responses to correspondence matrix
    ind <- match(data[i, !is.na(data[i, ])], corr_mat[, "from"])

    # Get corrected responses
    corr <- corr_mat[ind, -1]

    # Check for vector (single response)
    if(!is.matrix(corr)){

      # Ensure matrix
      corr <- as.matrix(corr)

    }

    # Get corrected responses in order (without missing)
    correct_ord <- na.omit(as.vector(t(corr)))

    # Check for any responses
    if(length(correct_ord) > 0){

      # Count intrusions (bad words)
      behav_mat[i, "Intrusions"] <- sum(correct_ord == "NA")

      # Count perseverations (repeated words)
      behav_mat[i, "Perseverations"] <- length(correct_ord[correct_ord != "NA"]) - length(unique(correct_ord[correct_ord != "NA"]))

      # Store corrected responses
      correct_list[[i]] <- as.vector(correct_ord)

    }

  }

  # Initialize corrected matrix (widest participant)
  correct_mat <- matrix(NA, nrow = n, ncol = max(0, lengths(correct_list)))

  # Loop over participants
  for(i in seq_len(n)){

    # Check for any responses
    if(length(correct_list[[i]]) > 0){

      # Fill corrected responses
      correct_mat[i, seq_along(correct_list[[i]])] <- correct_list[[i]]

    }

  }

  # Remove columns that are all NA
  correct_mat <- correct_mat[, !apply(correct_mat, 2, function(x){ all(is.na(x)) }), drop = FALSE]

  # Name rows
  row.names(correct_mat) <- row.names(data)

  # Name columns
  colnames(correct_mat) <- paste("Response_", formatC(seq_len(ncol(correct_mat)),
                                                      digits = nchar(ncol(correct_mat)) - 1,
                                                      flag = "0"), sep = "", recycle0 = TRUE)

  # Initialize results
  res <- list()

  # Store behavioral results
  res$behavioral <- behav_mat

  # Store corrected responses
  res$corrected <- correct_mat

  # Return results
  return(res)

}
