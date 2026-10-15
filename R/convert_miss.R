#' @noRd
# Convert miss ----
# Converts missing-data codes (e.g., 99, "N/A") to blanks, either for a whole fluency
# response matrix or for the "Response" column of free association data
# Updated 14.10.2026
convert_miss <- function(data, miss, type)
{

  # Convert to matrix
  # Convert to matrix
  data <- as.matrix(data)

  # Check for fluency
  if(type == "fluency"){

    # Change missing responses to NA
    data <- ifelse(data == paste(miss), NA, data)

    # Change bad responses to NA (skipped for a matrix with no rows, which
    # `bad_response()` would collapse to a single `NA`)
    if(nrow(data) > 0){

      # Rebuild matrix with original dimensions (`apply()` drops them for one participant)
      data <- matrix(
        apply(data, 2, bad_response), nrow = nrow(data), ncol = ncol(data),
        dimnames = dimnames(data)
      )

    }

    # Change NAs in data to ""
    data <- ifelse(is.na(data), "", data)

  }else if(type == "free"){ # Free association

    # Target response
    responses <- data[, "Response"]

    # Change missing responses to NA
    responses <- ifelse(responses == paste(miss), NA, responses)

    # Change bad responses to NA
    responses <- bad_response(responses)

    # Change NAs in data to ""
    data[, "Response"] <- ifelse(is.na(responses), "", responses)

  }

  # Return converted data
  return(data)

}
