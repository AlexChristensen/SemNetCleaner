#' Finalize a \code{textcleaner()} Result
#'
#' @description Reviews automated spell-check corrections and builds the
#' final cleaned/binary/behavioral output -- the exact logic
#' \code{\link{textcleaner}} itself runs once, internally, via a pure code
#' motion (Phase 11). Exposed so \code{\link{textcleaner_shiny}}'s
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
#' @return A finished, \code{textcleaner}-classed result
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
finalize_textcleaner <- function(res, data, type, ids, reader = readline, edit_fn = automated_edit, spell_check = res)
{

  corr_mat <- res$spellcheck$correspondence

  # Correct auto-corrections
  if(length(res$spellcheck$automated) != 0){

    res_check <- try(correct_changes(res, type = type, reader = reader, edit_fn = edit_fn), silent = TRUE)

    if(inherits(res_check, "try-error")){
      error_report(res, "correct_changes", "textcleaner")
      return(res)
    }else{
      res <- res_check
    }

    corr_mat <- res$spellcheck$correspondence

  }else{
    message("\nNo auto-corrections were made. Skipping automated spell-check verification.")
  }

  if(type == "fluency"){

    corrected <- try(correct_data(data, corr_mat), silent = TRUE)

    if(inherits(corrected, "try-error")){
      error_report(corrected, "correct_data", "textcleaner")
      return(spell_check)
    }

    behavioral <- corrected$behavioral

    corrected$corrected[which(corrected$corrected == "NA")] <- NA

    cleaned_list <- lapply(seq_len(nrow(corrected$corrected)), function(i){
      unique(na.omit(corrected$corrected[i, ]))
    })
    names(cleaned_list) <- row.names(corrected$corrected)

    max_resp <- max(unlist(lapply(cleaned_list, length)))

    cleaned_matrix <- t(sapply(
      lapply(cleaned_list, function(x, max_resp){
        c(x, rep(NA, max_resp - length(x)))
      }, max_resp = max_resp),
      rbind
    ))

    colnames(cleaned_matrix) <- paste("Response_", formatC(
      seq_len(ncol(cleaned_matrix)),
      digits = nchar(ncol(cleaned_matrix)) - 1,
      flag = "0"
    ), sep = "")

    res$responses$clean <- cleaned_matrix

    res$responses$binary <- try(responses_to_binary(corrected$corrected), silent = TRUE)

    if(inherits(res$responses$binary, "try-error")){
      error_report(corrected, "responses_to_binary", "textcleaner")
      return(spell_check)
    }

    behavioral <- cbind(behavioral, rowSums(res$responses$binary))
    colnames(behavioral)[3] <- "Appropriate"
    res$behavioral <- as.data.frame(behavioral)

    class(res) <- c("textcleaner", "fluency")

    # Let user know spell-check is complete
    Sys.sleep(1)
    message("\nPreprocessing complete.\n")
    Sys.sleep(2)

    has_dictionary <- "dictionary" %in% names(res)

    dictionary_output <- if(has_dictionary){
      paste(text_symbol("bullet"), "Dictionary output: `OBJECT_NAME$dictionary`", sep = " ")
    }

    moniker_output <- paste(text_symbol("bullet"), "Moniker output: `OBJECT_NAME$moniker`", sep = " ")

    res$moniker <- res$spellcheck$manual

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

    Sys.sleep(2)

  }else{

    corrected <- try(correct_data_free(data, corr_mat, ids), silent = TRUE)

    if(inherits(corrected, "try-error")){
      error_report(corrected, "correct_data_free", "textcleaner")
      return(spell_check)
    }

    behavioral <- corrected$behavioral

    cleaned_list <- na.omit(corrected$corrected)

    res$data$clean <- cleaned_list

    unique_responses <- unique(cleaned_list$Response)
    unique_cues <- unique(cleaned_list$Cue)

    cleaned_matrix <- matrix(0, nrow = length(unique_responses), ncol = length(unique_cues))
    row.names(cleaned_matrix) <- unique_responses
    colnames(cleaned_matrix) <- unique_cues

    for(i in seq_along(unique_cues)){

      frequency <- table(cleaned_list$Response[cleaned_list$Cue == unique_cues[i]])

      cleaned_matrix[names(frequency), i] <- frequency

    }

    res$responses$clean <- cleaned_matrix

    total_vector <- vector(length = length(ids))
    names(total_vector) <- ids

    for(i in seq_along(ids)){
      target_p <- cleaned_list[cleaned_list$ID == ids[i], ]
      total_vector[i] <- nrow(target_p)
    }

    behavioral <- cbind(behavioral, total_vector)
    colnames(behavioral)[3] <- "Appropriate"
    res$behavioral <- as.data.frame(behavioral)

    class(res) <- c("textcleaner", "free")

    message("\nPreprocessing complete.\n")

  }

  return(res)

}
