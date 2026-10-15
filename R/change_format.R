#' @noRd
# Change format ----
# Adds one change (a one-row "from"/"to_*" matrix) to the running change log of a
# response, widening the log when the change has more "to" columns than the earlier
# ones (or padding the change with `NA` when it has fewer)
# Updated 14.10.2026
change_format <- function(
    change_list, # running list of changes per response
    change_matrix, # new change to add
    current_index # index of current response
)
{

  # Check for unnamed list
  if(is.null(names(change_list))){

    # Wrap in a list
    change_list <- list(change_list)

    # Name list by response index
    names(change_list) <- paste(current_index)

  }

  # Get number of columns in existing changes
  n_list <- ncol(change_list[[paste(current_index)]])

  # Get number of columns in new change
  n_matrix <- ncol(change_matrix)

  # Get number of rows in existing changes
  r_list <- nrow(change_list[[paste(current_index)]])

  # Check for existing changes
  if(!is.null(n_list)){

    # Check whether new change is wider
    if(n_list < n_matrix){

      # Initialize wider matrix
      new_matrix <- matrix(NA, nrow = r_list, ncol = n_matrix)

      # Match column names
      colnames(new_matrix) <- colnames(change_matrix)

      # Loop over existing changes
      for(i in seq_len(r_list)){

        # Get existing row
        row_target <- as.vector(change_list[[paste(current_index)]][i, ])

        # Fill row (remaining columns stay NA)
        new_matrix[i, seq_along(row_target)] <- row_target

      }

      # Append new change
      new_matrix <- rbind(new_matrix, change_matrix)

      # Store updated changes
      change_list[[paste(current_index)]] <- new_matrix

      # Reset row names
      row.names(change_list[[paste(current_index)]]) <- NULL

    }else if(n_list > n_matrix){ # New change is narrower

      # Get number of missing columns
      diff_cols <- n_list - n_matrix

      # Pad new change with missing values
      change_matrix <- c(as.vector(change_matrix), rep(NA, diff_cols))

      # Append padded change
      change_list[[paste(current_index)]] <- rbind(change_list[[paste(current_index)]], change_matrix)

      # Reset row names
      row.names(change_list[[paste(current_index)]]) <- NULL

    }else{ # Same number of columns

      # Append new change
      change_list[[paste(current_index)]] <- rbind(change_list[[paste(current_index)]], change_matrix)

      # Reset row names
      row.names(change_list[[paste(current_index)]]) <- NULL

    }

  }else{ # No existing changes

    # Store new change
    change_list[[paste(current_index)]] <- change_matrix

  }

  # Return updated changes
  return(change_list)

}
