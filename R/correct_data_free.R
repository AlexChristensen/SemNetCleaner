#' @noRd
# Correct data free ----
# Applies the final (automated and manual) corrections to the free association data
# (ID, Cue, Response rows) and counts perseverations and intrusions per participant
#
# A response corrected into several words is spliced in as several rows, which keeps
# the contiguous cue sequences aligned with `corr_mat` (see `seq_min_max()`). The rows
# above and below the splice are selected with explicit guards because `1:0`-style
# ranges are not empty
# Updated 14.10.2026
correct_data_free <- function(
    data, # data frame of ID, Cue, and Response
    corr_mat, # correspondence matrix
    ids # participant IDs
)
{

  # Initialize corrected data
  correct_mat <- data

  # Initialize behavioral matrix
  behav_mat <- matrix(0, nrow = length(ids), ncol = 2)

  # Name columns
  colnames(behav_mat) <- c("Perseverations", "Intrusions")

  # Name rows
  row.names(behav_mat) <- ids

  # Loop over participants
  for(i in seq_along(ids)){

    # Get rows of participant
    ind_p <- which(correct_mat[, "ID"] == ids[i])

    # Get participant's data
    target_p <- correct_mat[ind_p, ]

    # Get unique cues
    cues <- unique(target_p[, "Cue"])

    # Loop over cues
    for(j in seq_along(cues)){

      # Get rows of cue
      ind_c <- which(target_p[, "Cue"] == cues[j])

      # Nothing left for this cue (all of its responses were reduced to
      # nothing by an earlier step)
      if(length(ind_c) == 0){

        next

      }

      # Get cue's data
      target_c <- target_p[ind_c, ]

      # Check for a single row
      if(!is.data.frame(target_c) & !is.matrix(target_c)){

        # Ensure matrix
        target_c <- t(as.matrix(target_c))

      }

      # Identify sequences (more than one time point or condition)
      target_seq <- seq_min_max(as.numeric(row.names(target_c)))

      # Loop over sequences
      for(s in seq_along(target_seq$min)){

        # Check for a single sequence
        if(length(target_seq$min) == 1){

          # Set start of sequence
          start <- 1

          # Set end of sequence
          end <- nrow(target_c)

        }else{ # Multiple sequences

          # Set start of sequence
          start <- target_seq$starts[s]

          # Set end of sequence
          end <- target_seq$breaks[s]

        }

        # Match responses to correspondence matrix
        ind <- match(
          target_c[start:end, "Response"],
          corr_mat[, "from"]
        )

        # Get corrected responses
        corr <- corr_mat[ind, -1]

        # Check for vector
        if(!is.matrix(corr)){

          # Check for a single response
          if(length(ind) == 1){

            # Make one row
            corr <- matrix(corr, nrow = 1)

          }else{ # Single corrected column

            # Make one column
            corr <- matrix(corr, ncol = 1)

          }

        }

        # Find empty columns
        na_cols <- apply(corr, 2, function(x){ all(is.na(x)) })

        # Check for empty columns
        if(any(na_cols)){

          # Remove empty columns
          corr <- corr[, !na_cols]

          # Check for vector
          if(!is.matrix(corr)){

            # Ensure matrix
            corr <- matrix(corr, nrow = length(corr))

          }

        }

        # Check for one corrected response each
        if(length(ind) == nrow(corr) & ncol(corr) == 1){

          # Get corrected responses
          correct_ord <- as.vector(corr)

          # Check for any responses
          if(length(correct_ord) > 0){

            # Count intrusions
            behav_mat[i, "Intrusions"] <- behav_mat[i, "Intrusions"] + sum(is.na(correct_ord))

            # Count perseverations
            behav_mat[i, "Perseverations"] <- behav_mat[i, "Perseverations"] + sum(duplicated(correct_ord))

            # Check for repeats
            if(any(duplicated(correct_ord))){

              # Set repeats to missing
              correct_ord[duplicated(correct_ord)] <- NA

            }

            # Get rows of sequence
            target_resps <- ind_p[ind_c][start:end]

            # Replace responses
            correct_mat[target_resps, "Response"] <- correct_ord

          }else{ # No responses

            # Add no intrusions
            behav_mat[i, "Intrusions"] <- behav_mat[i, "Intrusions"] + 0

            # Add no perseverations
            behav_mat[i, "Perseverations"] <- behav_mat[i, "Perseverations"] + 0

          }

        }else{ # Responses split into several

          # Get corrected responses in order
          correct_ord <- unname(unlist(apply(corr, 1, function(x){ na.omit(x) })))

          # Count intrusions
          behav_mat[i, "Intrusions"] <- behav_mat[i, "Intrusions"] + sum(is.na(correct_ord))

          # Count perseverations
          behav_mat[i, "Perseverations"] <- behav_mat[i, "Perseverations"] + sum(duplicated(correct_ord))

          # Reorganize the corrections into rows to add; a response reduced to nothing (e.g.,
          # "i am") leaves `correct_ord` as `NULL` and is dropped (zero rows)
          add_responses <- as.character(correct_ord)

          # Check for repeats
          if(any(duplicated(add_responses))){

            # Set repeats to missing
            add_responses[duplicated(add_responses)] <- NA

          }

          # Get rows of sequence
          target_resps <- ind_p[ind_c][start:end]

          # Get first row of sequence
          minPosition <- min(target_resps)

          # Get rows above sequence
          data_top <- correct_mat[seq_len(minPosition - 1), , drop = FALSE]

          # Get first row below sequence
          bottom_start <- max(target_resps) + 1

          # Check whether sequence is last
          if(bottom_start > nrow(correct_mat)){

            # Use no rows below
            data_bottom <- correct_mat[0, , drop = FALSE]

          }else{ # Rows below exist

            # Get rows below sequence
            data_bottom <- correct_mat[bottom_start:nrow(correct_mat), , drop = FALSE]

          }

          # Create rows of new responses
          data_middle <- data.frame(
            ID = rep(ids[i], length(add_responses)),
            Cue = rep(cues[j], length(add_responses)),
            Response = add_responses
          )

          # Recombine rows
          correct_mat <- rbind(
            data_top,
            data_middle,
            data_bottom
          )

          # Rename row names -- necessary for seq_min_max()
          row.names(correct_mat) <- seq_len(nrow(correct_mat))

          # Update rows of participant
          ind_p <- which(correct_mat[, "ID"] == ids[i])

          # Update participant's data
          target_p <- correct_mat[ind_p, ]

          # Update rows of cue
          ind_c <- which(target_p[, "Cue"] == cues[j])

          # The responses were all removed: nothing left to sequence
          if(length(ind_c) == 0){

            break

          }

          # Update cue's data
          target_c <- target_p[ind_c, ]

          # Check for a single row
          if(!is.data.frame(target_c) & !is.matrix(target_c)){

            # Ensure matrix
            target_c <- t(as.matrix(target_c))

          }

          # Update sequences
          target_seq <- seq_min_max(as.numeric(row.names(target_c)))

        }

      }

    }

  }

  # Check for "NA" responses
  if(any(correct_mat[, "Response"] == "NA", na.rm = TRUE)){

    # Set "NA" responses to missing
    correct_mat[, "Response"] <- ifelse(
      correct_mat[, "Response"] == "NA",
      NA,
      correct_mat[, "Response"]
    )

  }

  # Remove rows that are all NA
  correct_mat <- na.omit(correct_mat)

  # Initialize results
  res <- list()

  # Store behavioral results
  res$behavioral <- behav_mat

  # Store corrected responses
  res$corrected <- correct_mat

  # Return results
  return(res)

}
