#' Finalize a \code{textcleaner()} Result
#'
#' @description Reviews automated spell-check corrections and builds the
#' final cleaned/binary/behavioral output -- the exact logic
#' \code{\link{textcleaner}} itself runs once, internally, via a pure code
#' motion. Exposed so \code{\link{textcleaner_shiny}}'s
#' corrections-review module can re-run this same, already-tested logic a
#' second time once the user's real edits (from the embedded
#' \code{\link{automated_edit_server}}) are known, instead of duplicating
#' it. Byte-identical behavior to \code{textcleaner()}'s own original inline
#' block, covered by the full \code{test-textcleaner.R} suite unchanged.
#'
#' Exported (not just internal) because \code{inst/Shiny/}'s own module
#' files are sourced into the global environment by Shiny's own app-loading
#' mechanism, not into the package namespace -- an unexported function is
#' not reachable from there by its bare name.
#'
#' @param res List.
#' The in-progress \code{textcleaner()} result, with
#' \code{$spellcheck$correspondence}/\code{$automated}/\code{$manual}
#' already populated
#'
#' @param data Matrix or data frame.
#' The already-spell-checked response data (\code{res$responses$original}
#' for \code{type = "fluency"}, \code{res$data$original} for \code{type =
#' "free"})
#'
#' @param type Character.
#' \code{"fluency"} or \code{"free"}, matching \code{textcleaner()}'s own
#' \code{type}
#'
#' @param ids Character vector.
#' Participant IDs (\code{row.names(data)} for \code{type = "fluency"},
#' \code{unique(data[, "ID"])} for \code{type = "free"})
#'
#' @param reader Function.
#' Passed through to \code{correct_changes()}. Defaults to \code{readline}
#'
#' @param edit_fn Function.
#' The automated-corrections review function. Defaults to
#' \code{automated_edit()}
#'
#' @param spell_check List.
#' The error-path fallback return value, matching \code{textcleaner()}'s own
#' original (pre-extraction) behavior exactly. Defaults to \code{res}
#'
#' @param keep_repeats Boolean.
#' Should repeated responses (perseverations) be kept in the cleaned
#' responses? Only applies to \code{type = "fluency"}. Defaults to
#' \code{FALSE}, which removes repeats from \code{$responses$clean};
#' they are counted in \code{$behavioral$Perseverations} either way
#'
#' @return A finished, \code{textcleaner}-classed result
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @keywords internal
# Finalize textcleaner ----
# Updated 14.10.2026
finalize_textcleaner <- function(
    res, # results of the spell-check
    data, # data after missing values are converted
    type, # "fluency" or "free"
    ids, # participant IDs
    reader = readline, # function used to read answers
    edit_fn = automated_edit, # function used to edit automated changes
    spell_check = res, # results returned on failure
    keep_repeats = FALSE # keep repeated responses
)
{

  # Get correspondence matrix
  corr_mat <- res$spellcheck$correspondence

  # Correct auto-corrections
  if(length(res$spellcheck$automated) != 0){

    # Verify automated corrections
    res_check <- try(correct_changes(res, type = type, reader = reader, edit_fn = edit_fn), silent = TRUE)

    # Check for error
    if(inherits(res_check, "try-error")){

      # Report error
      error_report(res_check, "correct_changes", "textcleaner")

      # Return unfinished results
      return(res)

    }else{ # Store verified results

      # Store verified results
      res <- res_check

    }

    # Update correspondence matrix
    corr_mat <- res$spellcheck$correspondence

  }else{ # No automated corrections

    # Let user know
    message("\nNo auto-corrections were made. Skipping automated spell-check verification.")

  }

  # Check for fluency
  if(type == "fluency"){

    # Correct data
    corrected <- try(correct_data(data, corr_mat), silent = TRUE)

    # Check for error
    if(inherits(corrected, "try-error")){

      # Report error
      error_report(corrected, "correct_data", "textcleaner")

      # Return spell-check results
      return(spell_check)

    }

    # Get behavioral data
    behavioral <- corrected$behavioral

    # Convert "NA" responses to missing
    corrected$corrected[which(corrected$corrected == "NA")] <- NA

    # Repeats are dropped from the cleaned responses unless `keep_repeats` is `TRUE`
    # (e.g., for SNAFU, which counts them); perseverations and the binary matrix are unaffected
    cleaned_list <- lapply(seq_len(nrow(corrected$corrected)), function(i){

      # Remove missing responses
      responses <- na.omit(corrected$corrected[i, ])

      # Remove repeats unless kept
      if(isTRUE(keep_repeats)){

        responses

      }else{

        unique(responses)

      }

    })

    # Name by participant
    names(cleaned_list) <- row.names(corrected$corrected)

    # Get largest number of responses
    max_resp <- max(unlist(lapply(cleaned_list, length)))

    # `do.call(rbind, ...)` always returns a matrix (also when every participant has one response)
    cleaned_matrix <- do.call(rbind, lapply(cleaned_list, function(x, max_resp){

      # Pad with missing values
      c(as.character(x), rep(NA_character_, max_resp - length(x)))

    }, max_resp = max_resp))

    # Name response columns
    colnames(cleaned_matrix) <- paste("Response_", formatC(
      seq_len(ncol(cleaned_matrix)),
      digits = nchar(ncol(cleaned_matrix)) - 1,
      flag = "0"
    ), sep = "")

    # Store cleaned responses
    res$responses$clean <- cleaned_matrix

    # Compute binary responses
    res$responses$binary <- try(responses_to_binary(corrected$corrected), silent = TRUE)

    # Check for error
    if(inherits(res$responses$binary, "try-error")){

      # Report error
      error_report(corrected, "responses_to_binary", "textcleaner")

      # Return spell-check results
      return(spell_check)

    }

    # Add number of appropriate responses
    behavioral <- cbind(behavioral, rowSums(res$responses$binary))

    # Name column
    colnames(behavioral)[3] <- "Appropriate"

    # Convert to data frame and store
    res$behavioral <- as.data.frame(behavioral)

    # Set class
    class(res) <- c("textcleaner", "fluency")

    # Let user know spell-check is complete
    pause(1)

    # Let user know
    message("\nPreprocessing complete.\n")

    # Pause to read
    pause(2)

    # Check for dictionary
    has_dictionary <- "dictionary" %in% names(res)

    # Set dictionary message
    dictionary_output <- if(has_dictionary){

      # Set dictionary message text
      paste(text_symbol("bullet"), "Dictionary output: `OBJECT_NAME$dictionary`", sep = " ")

    }

    # Set moniker message
    moniker_output <- paste(text_symbol("bullet"), "Moniker output: `OBJECT_NAME$moniker`", sep = " ")

    # Store manual changes
    res$moniker <- res$spellcheck$manual

    # Print submission message
    cat(
      color_text(
        paste(
          paste(
            "Consider submitting your",
            ifelse(has_dictionary, " dictionary and", ""),
            " spelling corrections (i.e., monikers) to:\n\n",
            sep = ""
          ),
          "https://github.com/AlexChristensen/SemNetDictionaries/issues/new/choose\n\n",
          ifelse(has_dictionary, paste(dictionary_output, "\n\n"), ""),
          moniker_output, "\n\n"
        ),
        defaults = "message"
      )
    )

    # Pause to read message
    pause(2)

  }else{ # Free association

    # Correct data
    corrected <- try(correct_data_free(data, corr_mat, ids), silent = TRUE)

    # Check for error
    if(inherits(corrected, "try-error")){

      # Report error
      error_report(corrected, "correct_data_free", "textcleaner")

      # Return spell-check results
      return(spell_check)

    }

    # Get behavioral data
    behavioral <- corrected$behavioral

    # Remove missing responses
    cleaned_list <- na.omit(corrected$corrected)

    # Store cleaned data
    res$data$clean <- cleaned_list

    # Get unique responses
    unique_responses <- unique(cleaned_list$Response)

    # Get unique cues
    unique_cues <- unique(cleaned_list$Cue)

    # Initialize cue-by-response matrix
    cleaned_matrix <- matrix(0, nrow = length(unique_responses), ncol = length(unique_cues))

    # Name rows
    row.names(cleaned_matrix) <- unique_responses

    # Name columns
    colnames(cleaned_matrix) <- unique_cues

    # Loop over cues
    for(i in seq_along(unique_cues)){

      # Count responses for cue
      frequency <- table(cleaned_list$Response[cleaned_list$Cue == unique_cues[i]])

      # Fill frequencies
      cleaned_matrix[names(frequency), i] <- frequency

    }

    # Store cleaned matrix
    res$responses$clean <- cleaned_matrix

    # Initialize total responses
    total_vector <- vector(length = length(ids))

    # Name by participant
    names(total_vector) <- ids

    # Loop over participants
    for(i in seq_along(ids)){

      # Get participant's responses
      target_p <- cleaned_list[cleaned_list$ID == ids[i], ]

      # Count responses
      total_vector[i] <- nrow(target_p)

    }

    # Add number of appropriate responses
    behavioral <- cbind(behavioral, total_vector)

    # Name column
    colnames(behavioral)[3] <- "Appropriate"

    # Convert to data frame and store
    res$behavioral <- as.data.frame(behavioral)

    # Set class
    class(res) <- c("textcleaner", "free")

    # Let user know preprocessing is complete
    message("\nPreprocessing complete.\n")

  }

  # Return results
  return(res)

}
