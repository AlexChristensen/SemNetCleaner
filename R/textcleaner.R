# As unpadded matrix ----
# `as.matrix()` on a data frame pads numbers to a common width ("  4", " 10"), so the IDs
# would no longer match the input; `as.character()` leaves them as written
# Updated 14.10.2026
as_unpadded_matrix <- function(
    data # matrix or data frame
)
{

  # Check for data frame
  if(is.data.frame(data)){

    # Convert columns to character (without padding numbers)
    data[] <- lapply(data, function(column){

      # Check for character column
      if(is.character(column)){

        # Return column
        return(column)

      }

      # Convert to character
      return(as.character(column))

    })

  }

  # Convert to matrix
  return(as.matrix(data))

}

# Errors for textcleaner arguments ----
# Up-front checks on the arguments of `textcleaner()` that would otherwise fail (or be
# silently ignored) deep in the pipeline
# Updated 14.10.2026
textcleaner_errors <- function(
    miss, # value coded as missing
    keepStrings, # keep multi-word responses as strings
    keepLength, # maximum number of words in a response
    keepCue, # keep cue words in responses
    keepRepeats, # keep repeated responses
    allowNumbers, # allow numbers in responses
    continue, # unfinished result to continue
    cores, # number of cores
    reader, # function used to read answers
    search_fn, # function used to search the web
    edit_fn # function used to edit automated changes
)
{

  # Check missing value
  length_error(miss, 1, "textcleaner")
  na_error(miss, "textcleaner")

  # Check logical arguments
  logical_arguments <- list(
    keepStrings = keepStrings, keepCue = keepCue,
    keepRepeats = keepRepeats, allowNumbers = allowNumbers
  )

  # Loop over logical arguments
  for(name in names(logical_arguments)){

    # Check for TRUE or FALSE
    if(!isTRUE(logical_arguments[[name]]) && !isFALSE(logical_arguments[[name]])){

      # Send error
      stop(paste0("In `textcleaner()`: `", name, "` must be TRUE or FALSE."), call. = FALSE)

    }

  }

  # Check for maximum number of words
  if(!is.null(keepLength)){

    # Check number of words
    typeof_error(keepLength, "numeric", "textcleaner")
    length_error(keepLength, 1, "textcleaner")
    range_error(keepLength, c(1, Inf), "textcleaner")

  }

  # Check number of cores
  typeof_error(cores, "numeric", "textcleaner")
  length_error(cores, 1, "textcleaner")
  range_error(cores, c(1, Inf), "textcleaner")

  # Check functions
  for(argument in list(reader = reader, search_fn = search_fn, edit_fn = edit_fn)){

    # Check for function
    if(!is.function(argument)){

      # Send error
      stop("In `textcleaner()`: `reader`, `search_fn`, and `edit_fn` must be functions.", call. = FALSE)

    }

  }

  # Check for a result to continue
  if(!is.null(continue) && !is.list(continue)){

    # Send error
    stop(
      "In `textcleaner()`: `continue` must be the unfinished result that `textcleaner()` returned.",
      call. = FALSE
    )

  }

}

# Rows changed by the automated spell-check ----
# The automated spell-check resolves every response that is not left for manual review,
# including responses that were already correct. Only the rows whose corrected value
# differs from the original (a different "to_1", more than one "to_" value, or a "to_1"
# that is missing) need to be reviewed
# Updated 14.10.2026
changed_by_automation <- function(
    correspondence, # correspondence matrix ("from", "to_1", "to_2", ...)
    rows # rows resolved by the automated spell-check
)
{

  # Get the rows resolved automatically
  automated <- correspondence[rows, , drop = FALSE]

  # Get the corrected columns
  to_columns <- grep("^to_", colnames(automated))

  # Check for no corrected columns
  if(length(to_columns) == 0 || nrow(automated) == 0){

    # Return the rows as they are
    return(rows)

  }

  # Get the original responses
  original <- automated[, "from"]

  # Get the first corrected response
  first_to <- automated[, to_columns[1]]

  # Check for further corrected responses
  if(length(to_columns) > 1){

    # Find rows with further corrected responses
    has_more <- rowSums(!is.na(automated[, to_columns[-1], drop = FALSE])) > 0

  }else{ # Only one corrected column

    # No further corrected responses
    has_more <- rep(FALSE, nrow(automated))

  }

  # Find unchanged rows (the same response and nothing else)
  unchanged <- !is.na(first_to) & first_to == original & !has_more

  # Return the rows that changed
  return(rows[!unchanged])

}

# Check textcleaner data ----
# Up-front checks on `data`, so that the common unusable inputs stop with a
# plain-language message instead of an internal error several steps in (or,
# before this was added, a console error report followed by a silent NULL).
# Updated 14.10.2026
check_textcleaner_data <- function(
    data # matrix or data frame of responses
)
{

  # Check for usable data type
  if(is.null(data) || !(is.data.frame(data) || is.matrix(data))){

    # Send error
    stop(
      "`data` must be a data frame or matrix of responses (see ?textcleaner), not ",
      if(is.null(data)) "NULL" else paste0("a ", class(data)[1]), ".",
      call. = FALSE
    )

  }

  # Check for rows
  if(nrow(data) == 0){

    # Send error
    stop("`data` has no rows, so there are no participants to clean.", call. = FALSE)

  }

  # Check for columns
  if(ncol(data) == 0){

    # Send error
    stop("`data` has no columns, so there are no responses to clean.", call. = FALSE)

  }

  # Return silently
  invisible(TRUE)

}

# Check responses remain ----
# After cleaning, at least one response must remain; otherwise the
# spell-check has nothing to work on (and used to fail inside
# `parallel_process()` with "input `iterations` has a minimum value of 0").
# Updated 14.10.2026
check_responses_remain <- function(
    responses, # cleaned responses
    miss # value(s) coded as missing
)
{

  # Flatten responses
  responses <- as.vector(as.matrix(responses))

  # Check for any responses
  if(!any(!is.na(responses) & nzchar(responses))){

    # Send error
    stop(
      "No responses were found to spell-check: every response is missing ",
      "(`miss = ", paste(miss, collapse = ", "), "`), blank, or empty after cleaning.",
      call. = FALSE
    )

  }

  # Return silently
  invisible(TRUE)

}

# Detect task type ----
# Guesses the task type from the column names: long-format free association data has
# "Cue" and "Response" columns, anything else is wide-format fluency data
# Updated 14.10.2026
detect_task_type <- function(
    data # matrix or data frame of responses
)
{

  # Get lowercase column names
  column_names <- tolower(trimws(colnames(data)))

  # Check for cue and response columns
  if(all(c("cue", "response") %in% column_names)){

    # Return free association
    "free"

  }else{ # No cue and response columns

    # Return fluency
    "fluency"

  }

}

# Check free columns ----
# Free association data must be long format with "Cue" and "Response" columns; the names
# are matched regardless of case (and renamed to the exact spelling) and a message names
# what is expected
# Updated 14.10.2026
check_free_columns <- function(
    data # long-format free association data
)
{

  # Get column names
  column_names <- colnames(data)

  # Get lowercase column names
  lowered <- tolower(trimws(column_names))

  # Loop over required columns
  for(required in c("Cue", "Response")){

    # Find required column
    target <- which(lowered == tolower(required))

    # Check for missing column
    if(length(target) == 0){

      # Send error
      stop(
        "Free association data (type = \"free\") must be in long format with ",
        "columns named \"ID\", \"Cue\", and \"Response\" (see ?textcleaner), but no \"",
        required, "\" column was found. Columns found: ",
        paste0("\"", column_names, "\"", collapse = ", "), ". ",
        "If each participant's responses are in separate columns, use type = \"fluency\".",
        call. = FALSE
      )

    }

    # Use exact spelling of the name
    column_names[target[1]] <- required

  }

  # Set column names
  colnames(data) <- column_names

  # Return data
  return(data)

}

#' Text Cleaner
#'
#' @description An automated cleaning function for spell-checking, de-pluralizing,
#' removing duplicates, and binarizing text data
#'
#' @details Responses are cleaned before they are spell-checked: they are
#' lowercased, accented and typographic characters are transliterated to
#' ASCII (\code{"caf\u00e9"} becomes \code{"cafe"}), and characters with no
#' ASCII equivalent (e.g., Chinese or Cyrillic text) are removed, so a response
#' made only of such characters is treated as missing. The dictionaries
#' supplied by \pkg{SemNetDictionaries} are English.
#'
#' @param data Matrix or data frame.
#'
#' For \code{type = "fluency"}, data are expected to
#' follow wide formatting (IDs are the row names
#' and are \strong{not} a column in the matrix
#' or data frame; alternatively, a column named \code{"ID"}, \code{"subject"},
#' \code{"participant"}, \code{"part"} or \code{"sub"} is used as the IDs and
#' removed from the responses; failing that, a single column whose values are
#' all different and contain digits (e.g., \code{1, 2, 3} or \code{P01, P02}) is
#' taken to be the IDs; with none of these, rows are numbered):
#'
#' \tabular{cccc}{
#'
#' \code{row.names} \tab Response 1 \tab Response 2 \tab Response n \cr
#' ID_1 \tab 1 \tab 2 \tab n \cr
#' ID_2 \tab 1 \tab 2 \tab n \cr
#' ID_n \tab 1 \tab 2 \tab n
#' }
#'
#' For \code{type = "free"}, data are expected to
#' follow long formatting:
#'
#' \tabular{ccc}{
#'
#' ID \tab Cue \tab Response \cr
#' 1 \tab 1 \tab 1 \cr
#' 1 \tab 1 \tab 2 \cr
#' 1 \tab 1 \tab n \cr
#' 1 \tab 2 \tab 1 \cr
#' 1 \tab 2 \tab 2 \cr
#' 1 \tab 2 \tab n \cr
#' 1 \tab n \tab 1 \cr
#' 1 \tab n \tab 2 \cr
#' 1 \tab n \tab n \cr
#' 2 \tab 1 \tab 1 \cr
#' 2 \tab 1 \tab 2 \cr
#' 2 \tab 1 \tab n \cr
#' 2 \tab 2 \tab 1 \cr
#' 2 \tab 2 \tab 2 \cr
#' 2 \tab 2 \tab n \cr
#' 2 \tab n \tab 1 \cr
#' 2 \tab n \tab 2 \cr
#' 2 \tab n \tab n \cr
#' n \tab 1 \tab 1 \cr
#' n \tab 1 \tab 2 \cr
#' n \tab 1 \tab n \cr
#' n \tab 2 \tab 1 \cr
#' n \tab 2 \tab 2 \cr
#' n \tab 2 \tab n \cr
#' n \tab n \tab 1 \cr
#' n \tab n \tab 2 \cr
#' n \tab n \tab n
#'
#' }
#'
#' @param type Character vector.
#' Type of task to be preprocessed.
#' \itemize{
#'
#' \item \code{"fluency"} --- Verbal fluency data (e.g., categories, phonological,
#' synonyms)
#'
#' \item \code{"free"} --- Free association data (e.g., cue terms or words)
#'
#' }
#'
#' @param miss Numeric or character.
#' Value for missing data.
#' Defaults to \code{99}
#'
#' @param partBY Character.
#' Are participants by row or column?
#' Set to \code{"row"} for by row.
#' Set to \code{"col"} for by column (the data are transposed first, so the
#' column names become the participants' IDs, exactly as the row names are
#' when participants are by row)
#'
#' @param dictionary Character vector.
#' The name(s) of the dictionary (or dictionaries) to spell-check against,
#' e.g., \code{"animals"}, or a custom dictionary saved as
#' \code{<name>.dictionary.rds} in the working directory or in \code{add.path}.
#' It can also be a vector of the words themselves, such as a corpus or
#' \code{result$dictionary} from an earlier run; as in
#' \code{SemNetDictionaries::load.dictionaries()}, a vector of MORE than 10
#' elements is taken to be the words, and 10 or fewer are taken to be names.
#' For \code{type = "free"}, the spell-check is always against the
#' \code{cocaspell} word list; \code{dictionary} then only supplies its list of
#' common misspellings
#' Dictionary to be used for more efficient text cleaning.
#' Defaults to \code{NULL}, which will use \code{\link[SemNetDictionaries]{general.dictionary}}
#'
#' Use \code{dictionaries()} or \code{find.dictionaries()} for more options
#' (See \code{SemNetDictionaries} for more details)
#'
#' @param spelling Character vector.
#' English spelling to be used.
#' \itemize{
#'
#' \item \code{"UK"} --- For British spelling (e.g., colour, grey,
#' programme, theatre)
#'
#' \item \code{"US"} --- For American spelling (e.g., color, gray, program,
#' theater)
#'
#' }
#'
#' @param add.path Character.
#' Path to additional dictionaries to be found.
#' DOES NOT search recursively (through all folders in path)
#' to avoid time intensive search.
#' Set to \code{"choose"} to open an interactive directory explorer
#'
#' @param keepStrings Boolean.
#' Should strings be retained or separated?
#' Defaults to \code{FALSE}.
#' Set to \code{TRUE} to retain strings as strings. For fluency data, a string
#' whose words are all in the dictionary (after correction) is still split into
#' them, unless the string itself is a dictionary entry ("sea slug" stays
#' "sea slug"; "marmoset otter" becomes "marmoset" and "otter")
#'
#' @param allowPunctuations Character vector.
#' Allows punctuation characters to be included in responses.
#' Defaults to \code{"-"}.
#' Set to \code{"all"} to keep all punctuation characters
#'
#' @param allowNumbers Boolean.
#' Defaults to \code{FALSE}, which removes stand-alone numbers (e.g., list
#' numbering such as \code{"1."}). Words that mix letters and digits (e.g.,
#' \code{"d0g"}) are kept as written so that the spell-check can correct or
#' flag them.
#' Set to \code{TRUE} to keep all numbers in text
#'
#' @param keepLength Numeric.
#' Maximum number of words allowed in a response.
#' Defaults to \code{NULL}.
#' Set a number to keep responses with words less than
#' or equal to the number (e.g., \code{3} will keep responses
#' with three or less words)
#'
#' @param keepCue Boolean.
#' Should cue words be retained in the responses?
#' Defaults to \code{FALSE}.
#' Set to \code{TRUE} to allow cue words to be retained
#'
#' @param keepRepeats Boolean.
#' Should repeated responses (perseverations) within a participant be
#' retained in \code{$responses$clean}? Only applies to
#' \code{type = "fluency"}. Defaults to \code{FALSE}, which keeps only the
#' first occurrence of each response. Set to \code{TRUE} to keep every
#' occurrence (e.g., so that a later analysis, such as SNAFU, can count
#' them). The binary response matrix is unaffected, and repeats are counted
#' in \code{$behavioral$Perseverations} either way
#'
#' @param continue List.
#' A result previously unfinished that still needs to be completed.
#' Allows you to continue to manually spell-check your data
#' after you've exited early (\code{X}) or it stopped on an error: those are
#' the cases in which \code{textcleaner()} returns an unfinished result
#' instead of the final output. A finished result (one that already holds
#' \code{$responses}) cannot be passed back, because there is nothing left to
#' continue. The result of a manual spell-check stage that has already finished
#' (as the Shiny app hands over) is accepted too, to skip straight to
#' finalizing the output.
#' Defaults to \code{NULL}
#'
#' @param reader Function.
#' The input reader used for every interactive prompt in the manual
#' spell-check wizard. Defaults to \code{readline}. Exposed so the whole
#' wizard can be driven programmatically (e.g. in automated tests) with a
#' pre-scripted queue of answers instead of real console input
#'
#' @param search_fn Function.
#' The function used to open a browser search for the "GOOGLE WORD"/
#' "GOOGLE STRING" manual spell-check options. Defaults to
#' \code{searcher::search_site}
#'
#' @param cores Numeric.
#' Number of computer cores to use in parallelized computations (the
#' "ad hoc" moniker re-check after manual spell-check). Defaults to \code{1}
#' (sequential). With more than one core the workers load the installed
#' package, so \pkg{SemNetCleaner} must be installed (not loaded with
#' \code{devtools::load_all()})
#'
#' @param edit_fn Function.
#' The spreadsheet-editor function used to review automated spell-check
#' changes. Defaults to \code{automated_edit}, a \code{shiny}/\code{DT}-based
#' review app (requires those packages to be installed). Pass \code{edit_fn
#' = edit} to use base R's lightweight \code{edit()} instead, with no extra
#' dependencies. Exposed so this step can be driven programmatically instead
#' of opening a real interactive editor
#'
#' @return A list of class \code{"textcleaner"}. For \code{type = "fluency"}:
#'
#' \item{responses}{A list of three matrices:
#'
#' \itemize{
#'
#' \item \code{original} --- The responses as given, after basic preparation:
#' missing values (\code{miss}) and blank cells converted to \code{NA};
#' punctuation and stand-alone numbers removed (see \code{allowPunctuations}
#' and \code{allowNumbers}); accented letters converted to plain ones
#' (e.g., an accented "e" to a plain "e"); surrounding and repeated white space removed;
#' and letters lowercased
#'
#' \item \code{clean} --- The spell-checked, de-pluralized responses with
#' repeats removed (unless \code{keepRepeats = TRUE}); one row per
#' participant, labeled by ID. This can be used as a final dataset for
#' analyses (e.g., fluency of responses)
#'
#' \item \code{binary} --- A matrix where each row is a participant and
#' each column a unique response: \code{1} if the participant gave the
#' response and \code{0} if not
#'
#' }
#'
#' }
#'
#' \item{behavioral}{A data frame, one row per participant, with the number of
#' \code{Perseverations} (repeated responses), \code{Intrusions} (responses
#' marked bad), and \code{Appropriate} (responses kept)}
#'
#' \item{spellcheck}{A list of three matrices: \code{correspondence} --- every
#' unique response (\code{from}) and what it became (\code{to_1},
#' \code{to_2}, ...); \code{automated} --- the rows changed by the
#' automated spell-check; \code{manual} --- the rows that went through the
#' manual wizard}
#'
#' \item{moniker}{The \code{manual} rows of \code{spellcheck}: the corrections
#' made by hand. Consider submitting them to \code{SemNetDictionaries}}
#'
#' \item{dictionary, added_words}{Only present if words were added to the
#' dictionary and "In my results" was chosen when asked where to save them
#' (or a file could not be written). \code{dictionary} is the whole
#' dictionary, the one used plus the added words, so it can be passed back
#' as \code{dictionary = result$dictionary}. \code{added_words} is only
#' the words added during the session}
#'
#' For \code{type = "free"}, \code{data} holds the \code{original} and
#' \code{clean} long-format data, \code{responses$clean} is a response-by-cue
#' frequency matrix, and \code{behavioral}, \code{spellcheck} and the
#' dictionary objects are as above. If the manual spell-check is exited
#' early, the object instead holds the saved state and can be passed back
#' with \code{continue = }
#'
#' @examples
#' if(interactive())
#' {
#'   # Full example with the manual spell-check
#'   clean <- textcleaner(open.animals[, -c(1, 2)], partBY = "row", dictionary = "animals")
#' }
#'
#' @references
#' Christensen, A. P., & Kenett, Y. N. (2021).
#' Semantic network analysis (SemNA): A tutorial on preprocessing, estimating, and analyzing semantic networks.
#' \emph{Psychological Methods}. \doi{10.1037/met0000463}
#'
#' Hornik, K., & Murdoch, D. (2010).
#' Watch Your Spelling!.
#' \emph{The R Journal}, \emph{3}, 22-28.
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @importFrom stats na.omit
#' @importFrom utils edit
#'
#' @export
# Text cleaner ----
# One function for both tasks (`type = "fluency"` and `"free"`). The shared steps are their
# own functions (obtain ID, convert missing, normalize, spell-check, correspondence matrix,
# corrections); what differs by task is the `partBY` transposition, the whole-matrix versus
# long-format `keepLength`/`keepCue` filtering, `correct_data()` versus `correct_data_free()`,
# and the final reshape (a binary vocabulary matrix for fluency, a response by cue
# frequency matrix for free)
#
# `continue` is an unfinished result: it is resumed when its `$stop` flag is `TRUE` and
# used as the finished spell-check otherwise
#
# Free association keeps its "still in development" warning (there is no real-data
# reference fixture for the full pipeline, only synthetic reproductions)
# Updated 14.10.2026
textcleaner <- function(
    data = NULL, # matrix or data frame of responses
    type = c("fluency", "free"), # type of semantic task
    miss = 99, # value(s) coded as missing
    partBY = c("row", "col"), # participants in rows or columns
    dictionary = NULL, # dictionary names or a vector of words
    spelling = c("UK", "US"), # spelling used in the dictionary
    add.path = NULL, # directory with custom dictionaries
    keepStrings = FALSE, # keep multi-word responses as strings
    allowPunctuations, # punctuation allowed in responses
    allowNumbers = FALSE, # allow numbers in responses
    keepLength = NULL, # maximum number of words in a response
    keepCue = FALSE, # keep cue words in responses
    keepRepeats = FALSE, # keep repeated responses
    continue = NULL, # unfinished result to continue
    reader = readline, # function used to read answers
    search_fn = searcher::search_site, # function used to search the web
    cores = 1, # number of cores
    edit_fn = automated_edit # function used to edit automated changes
)
{

  # Check for errors
  textcleaner_errors(
    miss, keepStrings, keepLength, keepCue, keepRepeats, allowNumbers,
    continue, cores, reader, search_fn, edit_fn
  )

  # Check for new run
  if(is.null(continue)){

    # Check data
    check_textcleaner_data(data)

    # Resolve path to custom dictionaries
    add.path <- resolve_add_path(add.path)

  }

  # A finished `textcleaner()` result has nothing left to continue
  # Check for finished result
  if(!is.null(continue) && !is.null(continue$responses) && is.null(continue$from)){

    # Send error
    stop(
      "`continue` is a finished textcleaner() result (it already holds `$responses`), so there is nothing left to continue. ",
      "`continue` takes the UNFINISHED result that textcleaner() returns when you exit the manual spell-check early ('X') ",
      "or it stops on an error.",
      call. = FALSE
    )

  }

  # Check for an unfinished result
  if(!is.null(continue) && is.null(continue$type)){

    # Send error
    stop(
      "`continue` must be the unfinished result that `textcleaner()` returns when you exit the manual spell-check early ('X') or it stops on an error.",
      call. = FALSE
    )

  }

  # Resolve type
  # Check for continuing
  if(!is.null(continue)){

    # Use type of unfinished result
    type <- continue$type

  }else if(missing(type)){ # Type not specified

    # Guess type from data structure
    type <- detect_task_type(data)

    # Let user know
    message(
      paste0(
        'Assuming semantic task type based on data structure: "', type, '"'
      )
    )

  }else{ # Type specified

    # Match type
    type <- match.arg(type)

  }

  # Free-association-only defaults
  # Check for free association
  if(type == "free"){

    # Check for missing keepStrings
    if(missing(keepStrings)){

      # Keep strings by default
      keepStrings <- TRUE

      # Let user know
      message("'keepStrings' is set to TRUE for free association tasks by default. Set argument to change this behavior.")

    }

    # Check for missing dictionary
    if(missing(dictionary)){

      # Use free association dictionary by default
      dictionary <- "cocaspell"

    }

    # Check for repeats setting
    if(isTRUE(keepRepeats)){

      # Let user know
      message("'keepRepeats' only applies to fluency tasks and is ignored for free association tasks.")

    }

  }

  # Cleaning applied to the words the user types in the manual wizard (see
  # `normalize_typed_words()`), built here so that a resumed run gets it too;
  # `allowPunctuations` only receives its default further down for a fresh run, hence the fallback
  normalize_punctuation <- if(missing(allowPunctuations)) "-" else allowPunctuations

  # Create function to clean typed words
  normalize_fn <- function(words){

    # Clean words like other responses
    normalize_responses(words, normalize_punctuation, allowNumbers)

  }

  # Shared defaults (only used for a fresh run)
  # Check for new run
  if(is.null(continue)){

    # Check for missing spelling
    if(missing(spelling)){

      # Use US spelling by default
      spelling <- "US"

      # Let user know
      message("\nThe 'spelling' argument was not set. Using default: 'US' English spelling")

      # Pause to read
      pause(0.5)

    }else{ # Spelling specified

      # Match spelling
      spelling <- match.arg(spelling)

    }

    # Check for missing punctuation
    if(missing(allowPunctuations)){

      # Allow hyphens by default
      allowPunctuations <- "-"

    }

  }

  # Check for continuing
  if(!is.null(continue)){

    # Check for unfinished result
    if(isTRUE(continue$stop)){

      # Still mid-wizard -- hand straight back to spellcheck_dictionary()
      spell_check <- spellcheck_dictionary(
        continue = continue, reader = reader, search_fn = search_fn, cores = cores,
        normalize_fn = normalize_fn
      )

    }else{ # Completed result

      # Already a completed spell-check result (see bug fix note above)
      spell_check <- continue

    }

  }else if(type == "fluency"){ # Fluency

    # Real bug fixed: `match.arg()` on `partBY`'s own default (see above)
    partBY <- match.arg(partBY)

    # Convert to matrix (without padding numbers)
    data <- as_unpadded_matrix(data)

    # Check for participants by column
    if(partBY == "col"){

      # Transpose data
      data <- t(data)

      # Let user know
      message("\nParticipants were made to go across the rows")

    }

    # obtain_id() raises its own plain-language errors (ambiguous or missing
    # ID columns); they are user-input problems, not bugs, so they are
    # passed through as they are rather than wrapped in a bug report
    id_res <- obtain_id(data, type = "fluency")

    # Remove ID column
    data <- id_res$data

    # Store IDs
    ids <- id_res$ids

    # One response column whose cells each hold a whole list of words ("dog cat bird fish ...")
    # is the most common wrong format; the pipeline expects one response per column
    if(ncol(data) == 1 && !isTRUE(keepStrings)){

      # Count words in each cell
      words_per_cell <- lengths(strsplit(trimws(as.character(data[, 1])), "[[:space:]]+"))

      # Remove missing cells
      words_per_cell <- words_per_cell[!is.na(data[, 1])]

      # Check for several words in most cells
      if(length(words_per_cell) > 0 && mean(words_per_cell >= 3) > 0.5){

        # Send error
        stop(
          "Only one response column was found and most of its cells contain several words. ",
          "Fluency data must have ONE response per column (one row per participant), e.g.:\n\n",
          "  ID   Response_1  Response_2  Response_3\n",
          "  1    dog         cat         bird\n\n",
          "Split each participant's list into separate columns before calling textcleaner(), e.g. ",
          "with strsplit(). If each cell really is a single multi-word response, set keepStrings = TRUE.",
          call. = FALSE
        )

      }

    }

    # Set IDs as row names
    row.names(data) <- ids

    # Convert missing values
    data <- try(convert_miss(data, miss, type = "fluency"), silent = TRUE)

    # Check for error
    if(inherits(data, "try-error")){

      # Report error
      error_report(data, "convert_miss", "textcleaner", fatal = TRUE)

    }

    # Normalize responses
    data <- try(
      prep_spellcheck_dictionary(data, allowPunctuations, allowNumbers, type = "fluency"),
      silent = TRUE
    )

    # Check for error
    if(inherits(data, "try-error")){

      # Report error
      error_report(data, "prep_spellcheck_dictionary", "textcleaner", fatal = TRUE)

    }

    # Check for remaining responses
    check_responses_remain(data, miss)

    # Check for maximum words
    if(!is.null(keepLength)){

      # Convert to matrix
      data <- as.matrix(data)

      # Count the words of each cell and remove responses with more than `keepLength` words
      response_lengths <- apply(data, 1:2, function(x){ length(unlist(strsplit(x, split = " "))) })

      # Find responses that are too long
      remove_responses <- which(response_lengths > keepLength, arr.ind = TRUE)

      # Check for any too-long responses
      if(nrow(remove_responses) != 0){

        # Set to missing
        data[remove_responses] <- NA

      }

    }

    # Convert to data frame
    data <- as.data.frame(data)

    # Get unique responses
    uniq_resp <- na.omit(unique(unlist(data)))

    # Check for missing dictionary
    if(is.null(dictionary)){

      # Use default dictionary
      dictionary <- "cocaspell"

    }

    # Run spell-check
    spell_check <- try(
      spellcheck_dictionary(
        uniq_resp = uniq_resp, dictionary = dictionary, spelling = spelling,
        add_path = add.path, keep_strings = keepStrings, data = data,
        type = "fluency", reader = reader, search_fn = search_fn, cores = cores,
        normalize_fn = normalize_fn
      ),
      silent = TRUE
    )

    # Check for error
    if(inherits(spell_check, "try-error")){

      # Report error
      error_report(spell_check, "spellcheck_dictionary", "textcleaner", fatal = TRUE)

    }

  }else{ # Free association

    # Warn about development status
    warning(
      "Free association preprocessing is still in development. Use with caution and independently verify that the results are correct.\n\nPlease send bugs/issues to <https://github.com/AlexChristensen/SemNetCleaner/issues"
    )

    # Convert to matrix (without padding numbers)
    data <- as_unpadded_matrix(data)

    # Check for cue and response columns
    data <- check_free_columns(data)

    # Get IDs
    id_res <- obtain_id(data, type = "free")

    # Store data
    data <- id_res$data

    # Store IDs
    ids <- id_res$ids

    # Convert missing values
    data <- try(convert_miss(data, miss, type = "free"), silent = TRUE)

    # Check for error
    if(inherits(data, "try-error")){

      # Report error
      error_report(data, "convert_miss", "textcleaner", fatal = TRUE)

    }

    # Normalize responses
    data <- try(
      prep_spellcheck_dictionary(data, allowPunctuations, allowNumbers, type = "free"),
      silent = TRUE
    )

    # Check for error
    if(inherits(data, "try-error")){

      # Report error
      error_report(data, "prep_spellcheck_dictionary", "textcleaner", fatal = TRUE)

    }

    # Check for remaining responses
    check_responses_remain(data[, "Response"], miss)

    # Check for maximum words
    if(!is.null(keepLength)){

      # Count words in each response
      response_lengths <- lengths(strsplit(data[, "Response"], split = " "))

      # Keep short responses
      data <- data[response_lengths <= keepLength, ]

    }

    # Check for removing cues
    if(!isTRUE(keepCue)){

      # Get cues
      cues <- unique(data[, "Cue"])

      # Remove responses that are cues
      data <- data[is.na(match(data[, "Response"], cues)), ]

    }

    # Get unique responses
    uniq_resp <- na.omit(unique(unlist(data[, "Response"])))

    # Run spell-check
    spell_check <- try(
      spellcheck_dictionary(
        uniq_resp = uniq_resp, dictionary = dictionary, spelling = spelling,
        add_path = add.path, keep_strings = keepStrings, keep_length = keepLength,
        keep_cue = keepCue, data = data, type = "free",
        reader = reader, search_fn = search_fn, cores = cores, normalize_fn = normalize_fn
      ),
      silent = TRUE
    )

    # Check for error
    if(inherits(spell_check, "try-error")){

      # Report error
      error_report(spell_check, "spellcheck_dictionary", "textcleaner", fatal = TRUE)

    }

  }

  # Check if spell-check was stopped (either error or user stop)
  if(isTRUE(spell_check$stop)){

    # Return unfinished result
    return(spell_check)

  }

  # Initialize results to return
  res <- list()

  # Check for added dictionary
  if("dictionary" %in% names(spell_check)){

    # Store dictionary
    res$dictionary <- spell_check$dictionary

    # Store added words
    res$added_words <- spell_check$added_words

  }

  # Get data
  data <- spell_check$data

  # Get IDs
  ids <- if(type == "fluency") row.names(spell_check$data) else unique(spell_check$data[, "ID"])

  # Check for fluency
  if(type == "fluency"){

    # Store original responses
    res$responses$original <- data

  }else{ # Free association

    # Store original data
    res$data$original <- data

  }

  # Get original responses
  original <- spell_check$from

  # Get corrected responses
  checked <- spell_check$to

  # Create correspondence matrix
  corr_mat <- try(correspondence_matrix(original, checked), silent = TRUE)

  # Check for error
  if(inherits(corr_mat, "try-error")){

    # Report error
    error_report(corr_mat, "correspondence_matrix", "textcleaner")

    # Return unfinished result
    return(spell_check)

  }

  # (BAD WORD drops just the bad word in both tasks, so no response is turned into "NA" here)

  # Name rows
  row.names(corr_mat) <- formatC(seq_len(nrow(corr_mat)), digits = 2, flag = "0")

  # Store correspondence matrix
  res$spellcheck$correspondence <- corr_mat

  # Keep only the responses the automated spell-check changed (`drop = FALSE` keeps the row
  # names of the correspondence matrix when one row is selected)
  res$spellcheck$automated <- corr_mat[changed_by_automation(corr_mat, spell_check$auto), , drop = FALSE]

  # Store manual responses
  res$spellcheck$manual <- corr_mat[spell_check$manual, , drop = FALSE]

  # Correct the auto-corrections and build the cleaned, binary, and behavioral output (shared
  # with the internal Shiny wizard); `spell_check` is returned if that fails
  return(finalize_textcleaner(
    res, data, type, ids, reader = reader, edit_fn = edit_fn,
    spell_check = spell_check, keep_repeats = keepRepeats
  ))

}
#----
