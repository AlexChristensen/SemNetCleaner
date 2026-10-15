#' @noRd
# Correct changes ----
# Lets the user review every automated spell-check change in a spreadsheet editor
# (`edit_fn`), then folds the edits back into `res$spellcheck$correspondence` and
# `res$spellcheck$verified`
#
# `edit_fn` (default `automated_edit`, the Shiny review app; `edit` is the lightweight
# alternative) and `reader` can be replaced so the review can be scripted. A cell
# filled in where there was none counts as a change
# Updated 14.10.2026
correct_changes <- function(
    textcleaner_obj, # unfinished results of textcleaner
    type = c("fluency", "free"), # type of data
    reader = readline, # function used to read answers
    edit_fn = automated_edit, # function used to edit changes
    window = help_window_enabled(), # show the instructions in a separate window
    open_fn = utils::browseURL # function used to open the window
)
{

  # Match type
  type <- match.arg(type)

  # Copy results
  res <- textcleaner_obj

  # Prepare automated corrections for review
  automated <- prepare_automated_for_review(res)

  # Get the last column with automated responses
  last_column <- 3 + (ncol(textcleaner_obj$spellcheck$automated) - 2)

  # Show the instructions in a separate window (if possible)
  opened <- isTRUE(window) && open_review_window(last_column, open_fn)

  # Check for the window
  if(opened){

    # Let user know
    cat(color_text("\nThe instructions for the review are open in a separate window.", defaults = "message"))
    cat(color_text("\nA spreadsheet will open next, allowing you to manually correct", defaults = "message"))
    cat(color_text("\nthe changes made during the automated spell-checking process.\n\n", defaults = "message"))

    # Wait for user
    read_input("Press ENTER to continue with spell-check.", reader = reader)

  }else{ # Instructions in the console

    # Explain the review
    cat(color_text("\nYou will now have a chance to correct the changes that", defaults = "message"))
    cat(color_text("\nwere made during the automated spell-checking process.", defaults = "message"))
    cat(color_text("\nA spreadsheet will open allowing you to manually correct", defaults = "message"))
    cat(color_text("\nthese changes.\n\n", defaults = "message"))

    # Wait for user
    read_input("Press ENTER to continue...", reader = reader)

    # Explain the columns
    cat(color_text("\nThe first column of the spreadsheet corresponds to the", defaults = "message"))
    cat(color_text("\nrow number provided in the output object `$spellcheck$correspondence`", defaults = "message"))
    cat(color_text("\n(see ?textcleaner for more information about this output).", defaults = "message"))

    cat(color_text("\n\nThe second column is the original response the participant provided", defaults = "message"))
    cat(color_text(paste("\nand columns 3 through", last_column,
                        "are the automated spell-check responses."), defaults = "message"))

    cat(color_text('\nThese columns will have names formatted with "to_#".\n\n', defaults = "message"))

    # Wait for user
    read_input("Press ENTER to continue...", reader = reader)

    # Explain how to edit
    cat(color_text(paste("\nYou should change columns 3 through", last_column,
                        "by manually typing responses."), defaults = "message"))

    cat(color_text('\nFor inappropriate responses, "NA" should be typed. When finished,', defaults = "message"))
    cat(color_text('\nyou can exit this process by clicking "Done" (or the "X" if using a', defaults = "message"))
    cat(color_text('\nlightweight spreadsheet editor via `edit_fn = edit`).\n\n', defaults = "message"))

    # Wait for user
    read_input("Press ENTER to continue with spell-check.", reader = reader)

  }

  # Let user edit corrections
  changes <- edit_fn(automated)

  # Convert originals to matrix
  automated <- as.matrix(automated)

  # Convert edits to matrix
  changes <- as.matrix(changes)

  # Set blank edits (empty or only spaces) to missing
  changes <- ifelse(!is.na(changes) & !nzchar(trimws(changes)), NA, changes)

  # The edited table may have extra "to_" columns (base `edit()` allows typing into a
  # new column), so the narrower table is padded with `NA`. The number of rows must
  # match because rows are matched to the correspondence matrix by position
  if(nrow(changes) != nrow(automated)){

    # Send error
    stop(
      "The edited table has ", nrow(changes), " rows but ", nrow(automated),
      " were expected. Rows cannot be added or removed during review.",
      call. = FALSE
    )

  }

  # Check for extra columns in edited table
  if(ncol(changes) > ncol(automated)){

    # Get new columns
    new_cols <- seq(ncol(automated) + 1, ncol(changes))

    # Pad original table
    automated <- cbind(automated, matrix(NA_character_, nrow = nrow(automated), ncol = length(new_cols)))

    # Name new columns
    colnames(automated)[new_cols] <- if(is.null(colnames(changes))){

      # Use default names
      paste0("to_", new_cols - 1)

    }else{

      # Use names of the edited table
      colnames(changes)[new_cols]

    }

  }else if(ncol(changes) < ncol(automated)){ # Edited table is narrower

    # Get missing columns
    new_cols <- seq(ncol(changes) + 1, ncol(automated))

    # Pad edited table
    changes <- cbind(changes, matrix(NA_character_, nrow = nrow(changes), ncol = length(new_cols)))

    # Name missing columns
    colnames(changes)[new_cols] <- colnames(automated)[new_cols]

  }

  # Let user know
  message("\nPreparing your data...")

  # Convert edits to matrix
  changes_mat <- as.matrix(changes)

  # Find cells that differ (including missing)
  differences <- (is.na(automated) != is.na(changes_mat)) |
    (!is.na(automated) & !is.na(changes_mat) & automated != changes_mat)

  # Check for vector
  if(!is.matrix(differences)){

    # Ensure matrix
    differences <- t(as.matrix(differences))

  }

  # Find changed responses
  target_changes <- which(apply(differences, 1, function(x){ any(x) }))

  # Check for no changes
  if(length(target_changes) == 0){

    # Let user know nothing changed
    message("\nNo responses changed during review.")

    # Return original results
    return(textcleaner_obj)

  }else{ # Changes were made

    # Initialize tracked changes
    track_changes <- list()

    # Loop over changed responses
    for(i in seq_along(target_changes)){

      # Combine previous and corrected responses
      chn_mat <- rbind(automated[target_changes[i], ], changes[target_changes[i], ])

      # Name columns
      colnames(chn_mat)[-1] <- rep("to", ncol(chn_mat) - 1)

      # Name rows
      row.names(chn_mat) <- c("Previous", "Corrected")

      # Find empty columns
      all_na_cols <- apply(chn_mat, 2, function(x){ all(is.na(x)) })

      # Check for empty columns
      if(any(all_na_cols)){

        # Remove empty columns
        chn_mat <- chn_mat[, -which(all_na_cols)]

      }

      # Store changes by response
      track_changes[[automated[target_changes[i], 1]]] <- chn_mat

    }

    # Store tracked changes
    res$spellcheck$verified <- track_changes

    # Get correspondence matrix
    correspondence <- res$spellcheck$correspondence

    # Check whether correspondence is wider
    if(ncol(correspondence) > ncol(changes)){

      # Get number of missing columns
      diff <- ncol(correspondence) - ncol(changes)

      # Pad edited table
      for(i in seq_len(diff)){

        changes <- as.matrix(cbind(changes, rep(NA, nrow(changes))))

      }

    }else if(ncol(correspondence) < ncol(changes)){ # Correspondence is narrower

      # Get number of missing columns
      diff <- ncol(changes) - ncol(correspondence)

      # Pad correspondence
      for(i in seq_len(diff)){

        correspondence <- as.matrix(cbind(correspondence, rep(NA, nrow(correspondence))))

      }

    }

    # Update corrected responses
    correspondence[row.names(res$spellcheck$automated), ] <- changes

    # Store correspondence matrix
    res$spellcheck$correspondence <- correspondence

    # Find empty columns
    na_cols <- apply(correspondence, 2, function(x){ all(is.na(x)) })

    # Remove empty columns
    correspondence <- correspondence[, !na_cols]

    # Get original responses
    from <- as.list(correspondence[, "from"])

    # Check for missing corrections
    if(any(is.na(correspondence[, grep("to", colnames(correspondence))]))){

      # Remove missing corrections
      to <- apply(as.matrix(correspondence[, grep("to", colnames(correspondence))]), 1, function(x){ unname(na.omit(x)) })

    }else{ # No missing corrections

      # Use single column
      to <- as.matrix(correspondence[, grep("to", colnames(correspondence))])

    }

    # Rebuild correspondence matrix
    corr_mat <- try(
      correspondence_matrix(from, to),
      silent = TRUE
    )

    # Check for error
    if(inherits(corr_mat, "try-error")){

      # Report error
      return(error_report(corr_mat, "correspondence_matrix", "correct_changes"))

    }

    # Store edited responses
    res$spellcheck$automated <- changes

    # Return results
    return(res)

  }

}

#' Prepare the Automated Corrections Matrix for Review
#'
#' @description Reconstructs the automated spell-check corrections matrix
#' \code{correct_changes()} itself hands to its \code{edit_fn} -- restoring
#' matrix-ness and the correspondence-matrix row name for a single-row
#' selection (see this file's own header comment for the real bug this
#' fixes), and padding with enough \code{"to_#"} columns for the widest
#' multi-word \code{"from"} entry. Exposed as its own function
#' so \code{\link{textcleaner_shiny}}'s corrections-review module can render
#' the exact same shape \code{correct_changes()} itself will later expect
#' back from \code{edit_fn} -- computing this independently (e.g. from the
#' raw, un-padded \code{res$spellcheck$automated}) produces a
#' narrower matrix than \code{correct_changes()}'s own internal copy,
#' which errors with "non-conformable arrays" the moment they're compared.
#'
#' Exported (not just internal) because \code{inst/Shiny/}'s own module
#' files are sourced into the global environment by Shiny's own app-loading
#' mechanism, not into the package namespace -- an unexported function is
#' not reachable from there by its bare name.
#'
#' @param res List.
#' The in-progress \code{textcleaner()} result, with
#' \code{$spellcheck$automated}/\code{$correspondence} already populated
#'
#' @return The padded automated-corrections matrix, ready to hand to
#' \code{automated_edit()}/\code{\link{automated_edit_server}} (or any
#' other \code{edit_fn})
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @keywords internal
# Prepare automated for review ----
# Updated 14.10.2026
prepare_automated_for_review <- function(
    res # unfinished results of textcleaner
)
{

  # Get automated corrections
  automated <- res$spellcheck$automated

  # Check for single response
  if(is.vector(automated)){

    # See correct_changes.R's own header comment for the full bug writeup
    # this recovers from -- a collapsed single-row selection losing its
    # correspondence-matrix row name entirely.
    automated <- t(as.matrix(automated))

    # Restore row name of the response
    row.names(automated) <- row.names(res$spellcheck$correspondence)[
      match(automated[, "from"], res$spellcheck$correspondence[, "from"])
    ]

  }

  # Get original responses
  from_vector <- automated[, "from"]

  # Split into words
  from_split <- strsplit(from_vector, split = " ")

  # Get largest number of words
  max_length <- max(unlist(lapply(from_split, length)), na.rm = TRUE)

  # Check for any words
  if(max_length > 0){

    # Add columns for corrections
    automated <- cbind(automated, matrix(NA, nrow = nrow(automated), ncol = max_length))

    # Name columns
    colnames(automated)[-1] <- paste("to", seq_len(ncol(automated) - 1), sep = "_")

    # Ensure matrix
    automated <- as.matrix(automated)

  }

  # Return automated corrections
  return(automated)

}
