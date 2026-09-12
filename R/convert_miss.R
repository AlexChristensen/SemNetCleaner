#' @noRd
# Converts missing-data sentinel values (e.g. "999", "N/A") into empty
# strings, for either a whole fluency response matrix or just a free-
# association "Response" column. Ported from `convert.miss()`. Kept as one
# type-dispatching function (not split) -- the fluency/free branches operate
# on genuinely different shapes (whole matrix vs one named column), not
# duplicated scaffolding.
# Updated 09.09.2026
convert_miss <- function(data, miss, type)
{

  # Convert to matrix
  data <- as.matrix(data)

  if(type == "fluency"){

    # Change missing responses to NA
    data <- ifelse(data == paste(miss), NA, data)

    # Change bad responses to NA. Bug #51 fixed, found during a Phase 3
    # robustness review: `bad_response()`'s own `if(length(word) == 0)
    # word <- NA` guard (deliberately relied upon elsewhere, e.g. a
    # single scalar word) turns a 0-row column into a scalar NA instead
    # of `character(0)` -- since every column of a 0-row `data` behaves
    # identically, `apply()` then simplifies its result into a 1-row
    # matrix, silently manufacturing a phantom participant instead of
    # preserving 0 rows. Confirmed inherited identically from the
    # untouched original, and the exact same mechanism as the one already
    # fixed in `responses_to_binary()` (Phase 2) -- same fix pattern used
    # there: guard the caller against a 0-row/0-element matrix before
    # `bad_response()` ever sees it, rather than changing `bad_response()`
    # itself (which other callers rely on for genuine scalar input).
    if(nrow(data) > 0){
      data <- apply(data, 2, bad_response)
    }

    # Change NAs in data to ""
    data <- ifelse(is.na(data), "", data)

  }else if(type == "free"){

    # Target response
    responses <- data[, "Response"]

    # Change missing responses to NA
    responses <- ifelse(responses == paste(miss), NA, responses)

    # Change bad responses to NA
    responses <- bad_response(responses)

    # Change NAs in data to ""
    data[, "Response"] <- ifelse(is.na(responses), "", responses)

  }

  return(data)

}
