#' @noRd
# Obtain id ----
# Determines the participant IDs: a named ID column (`ID`, `subject`, `participant`, `part`,
# `sub`, also in plural), the row names of fluency data, or an all-unique column that looks
# like IDs; otherwise fluency data are numbered by row (free association requires an ID
# column). Several candidate ID columns are an error rather than a guess
# Updated 14.10.2026
obtain_id <- function(
    data, # matrix or data frame of responses
    type # "fluency" or "free"
)
{

  # Check for named column
  named_id <- c("subject", "id", "participant", "part", "sub")

  # Plural and capitalized forms used as headers ("Participants", "IDs", "Subjects") are accepted
  named_id_pattern <- paste0("^(", paste(named_id, collapse = "|"), ")s?$")

  # Find matching column names
  named_id_matches <- grep(named_id_pattern, tolower(trimws(colnames(data))))

  # Check for any matches
  if(length(named_id_matches) > 0){

    # Set ID column
    id_col <- named_id_matches

    # Check for multiple matches
    if(length(id_col) > 1){

      # Send error
      stop(
        paste0(
          "Multiple possible ID columns found: ",
          paste0("'", colnames(data)[id_col], "'", collapse = ", "),
          ". Please ensure only one column name matches one of: ",
          paste0("'", named_id, "'", collapse = ", "), "."
        ),
        call. = FALSE
      )

    }

  }else if(type == "fluency" && has_explicit_row_names(data)){ # IDs are the row names

    # No ID column, but explicit row names: the layout `?textcleaner` documents for fluency
    # data, which is also what `partBY = "col"` produces (the column names become row names)
    # Flag row names as IDs
    ids_from_row_names <- TRUE

  }else{ # Search for unique values

    # Check for a variable with unique values (`vapply()` over a data frame, because `apply()`
    # can simplify to a matrix and then iterates over cells)
    if(nrow(data) <= 1){

      # With 0 or 1 rows every column has one unique value per row, so the heuristic cannot
      # pick an ID column
      # Skip heuristic
      unique_cols <- integer(0)

    }else{

      # Count unique values per column
      unique_counts <- vapply(as.data.frame(data), function(x){ length(unique(x)) }, integer(1))

      # Only a column that looks like IDs (numeric, or every value contains a digit: 1, 2, 3 or
      # P01, P02) can be inferred; a column of response words must not be taken as the IDs
      looks_like_ids <- vapply(as.data.frame(data, stringsAsFactors = FALSE), function(x){

        # Get non-missing values
        values <- as.character(x[!is.na(x)])

        # Check for numeric or digit-containing values
        length(values) > 0 && (is.numeric(x) || all(grepl("[0-9]", values)))

      }, logical(1))

      # Find columns with one unique value per row
      unique_cols <- which(unique_counts == nrow(data) & looks_like_ids)

    }

    # Only one unique column variable
    if(length(unique_cols) == 1){

      # Use the unique column
      id_col <- unique_cols

    }else if(length(unique_cols) > 1){ # Multiple candidates

      # Several all-unique candidate columns are ambiguous, so this is an error rather than a guess
      stop(
        paste0(
          "Multiple possible ID columns found (each has one unique value per row): ",
          paste0("'", colnames(data)[unique_cols], "'", collapse = ", "),
          ". Please name the intended ID column as one of: ",
          paste0("'", named_id, "'", collapse = ", "), "."
        ),
        call. = FALSE
      )

    }

  }

  # Check for fluency
  if(type == "fluency"){

    # Check for ID column
    if(exists("id_col", envir = environment())){

      # Grab unique identifier
      ids <- data[, id_col]

      # Check IDs
      check_ids(ids, colnames(data)[id_col], unique_required = TRUE)

      # Let user know what IDs refer to
      message(paste("\nIDs refer to variable:", " '", colnames(data)[id_col], "'", sep = ""))

      # Remove unique identifier from data (`drop = FALSE` keeps a matrix when one response
      # column is left)
      data <- data[, -id_col, drop = FALSE]

    }else if(exists("ids_from_row_names", envir = environment())){ # IDs are the row names

      # Get row names
      ids <- rownames(data)

      # Check IDs
      check_ids(ids, "row names", unique_required = TRUE)

      # Let user know IDs are the row names
      message("\nIDs refer to the row names")

    }else{ # No ID column

      # Make rows the IDs
      ids <- seq_len(nrow(data))

      # Let user know what IDs refer to
      message("\nIDs refer to row number")

    }

  }else if(type == "free"){ # Free association

    # Check for ID column
    if(exists("id_col", envir = environment())){

      # Grab unique identifier. Long format repeats each ID once per
      # response, so only missing/blank IDs are an error here
      check_ids(data[, id_col], colnames(data)[id_col], unique_required = FALSE)

      # Get unique IDs
      ids <- unique(data[, id_col])

      # Let user know what IDs refer to
      message(paste("\nIDs refer to variable:", " '", colnames(data)[id_col], "'", sep = ""))

    }else{ # No ID column

      # No ID column could be identified: free association data requires one
      stop("No IDs were identified in data. Please name ID column as \"ID\"", call. = FALSE)

    }

  }

  # Initialize result list
  res <- list()

  # Store data
  res$data <- data

  # Store IDs
  res$ids <- ids

  # Return results
  return(res)

}

#' @noRd
# Check ids ----
# Participant IDs become the row names of every result, so each participant needs a
# distinct, non-missing one. `unique_required = FALSE` is for long-format (free
# association) data, where an ID repeats once per response
# Updated 14.10.2026
check_ids <- function(
    ids, # participant IDs
    id_name, # name of the ID column
    unique_required = TRUE # whether IDs must be unique
)
{

  # Convert IDs to character
  ids <- as.character(ids)

  # Find missing or blank IDs
  missing_rows <- which(is.na(ids) | !nzchar(trimws(ids)))

  # Check for missing IDs
  if(length(missing_rows) > 0){

    # Send error
    stop(
      "The ID column '", id_name, "' has missing or blank values in row",
      if(length(missing_rows) > 1) "s " else " ",
      paste(utils::head(missing_rows, 10), collapse = ", "),
      if(length(missing_rows) > 10) ", ..." else "",
      ". Every participant needs an ID.",
      call. = FALSE
    )

  }

  # Check for duplicated IDs
  if(unique_required && anyDuplicated(ids) > 0){

    # Find duplicated IDs
    duplicated_ids <- unique(ids[duplicated(ids)])

    # Send error
    stop(
      "The ID column '", id_name, "' has duplicated values: ",
      paste0("'", utils::head(duplicated_ids, 10), "'", collapse = ", "),
      if(length(duplicated_ids) > 10) ", ..." else "",
      ". Each row must be a different participant, because the results are labeled by ID. ",
      "Make the IDs unique (e.g., `paste(ID, session)`) or remove the repeated rows.",
      call. = FALSE
    )

  }

  # Return silently
  invisible(TRUE)

}

#' @noRd
# Has explicit row names ----
# Whether `data` carries row names someone actually supplied. A matrix
# without row names has none (`NULL`); a data frame ALWAYS reports row names
# ("1", "2", ...), so its automatic ones are told apart with
# `.row_names_info()` (negative when automatic).
# Updated 14.10.2026
has_explicit_row_names <- function(
    data # matrix or data frame
)
{

  # Check for data frame
  if(is.data.frame(data)){

    # Check for non-automatic row names
    .row_names_info(data) > 0

  }else{ # Matrix

    # Check for row names
    !is.null(rownames(data))

  }

}
