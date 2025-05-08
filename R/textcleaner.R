#' Text Cleaner
#'
#' @description An automated cleaning function for spell-checking, de-pluralizing,
#' removing duplicates, and binarizing text data
#'
#' @param data Matrix or data frame.
#'
#' For \code{task = "fluency"}, data are expected to
#' follow wide formatting (IDs are the row names
#' and are \strong{not} a column in the matrix
#' or data frame):
#'
#' \tabular{cccc}{
#'
#' \code{row.names} \tab Response 1 \tab Response 2 \tab Response n \cr
#' ID_1 \tab 1 \tab 2 \tab n \cr
#' ID_2 \tab 1 \tab 2 \tab n \cr
#' ID_n \tab 1 \tab 2 \tab n
#' }
#'
#' For \code{task = "free"}, data are expected to
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
#' Set to \code{"col"} for by column
#'
#' @param dictionary Character vector.
#' Can be a vector of a corpus or any text for comparison.
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
#' Set to \code{TRUE} to retain strings as strings
#'
#' @param allowPunctuations Character vector.
#' Allows punctuation characters to be included in responses.
#' Defaults to \code{"-"}.
#' Set to \code{"all"} to keep all punctuation characters
#'
#' @param allowNumbers Boolean.
#' Defaults to \code{FALSE}.
#' Set to \code{TRUE} to keep numbers in text
#'
#' @param lowercase Boolean.
#' Should words be converted to lowercase?
#' Defaults to \code{TRUE}.
#' Set to \code{FALSE} to keep words as they are
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
#' @param continue List.
#' A result previously unfinished that still needs to be completed.
#' Allows you to continue to manually spell-check their data
#' after you've closed or errored out.
#' Defaults to \code{NULL}
#'
#' @return This function returns a list containing the following objects:
#'
#' \item{binary}{A matrix of responses where each row represents a participant
#' and each column represents a unique response. A response that a participant has provided is a '\code{1}'
#' and a response that a participant has not provided is a '\code{0}'}
#'
#' \item{responses}{A list containing two objects:
#'
#' \itemize{
#'
#' \item \code{clean} --- A response matrix that has been spell-checked
#' and de-pluralized with duplicates removed.
#' This can be used as a final dataset for analyses (e.g., fluency of responses)
#'
#' \item \code{original} --- The original response matrix that has had
#' white spaces before and after words response. Also converts all
#' upper-case letters to lower case
#'
#' }
#'
#' }
#'
#' \item{spellcheck}{A list containing three objects:
#'
#' \itemize{
#'
#' \item \code{full} --- All responses regardless of spell-checking changes
#'
#' \item \code{auto} --- Only the incorrect responses that were changed
#' during spell-check
#'
#' }
#'
#' }
#'
#' \item{removed}{A list containing two objects:
#'
#' \itemize{
#'
#' \item \code{rows} --- Identifies removed participants by their row
#' (or column) location in the original data file
#'
#' \item \code{ids} --- Identifies removed participants by their ID
#' (see argument \code{data})
#'
#' }
#'
#' }
#'
#' \item{partChanges}{A list where each participant is a list index with each
#' response that was been changed. Participants are identified by their ID (see argument \code{data}).
#' This can be used to replicate the cleaning process and to keep track of changes more generally.
#' Participants with \code{NA} did not have any changes from their original data
#' and participants with missing data are removed (see \code{removed$ids})}
#'
#' @examples
#' # Toy example
#' raw <- open.animals[c(1:10),-c(1:3)]
#'
#' if(interactive())
#' {
#'     #Full test
#'     clean <- textcleaner(open.animals[,-c(1,2)], partBY = "row", dictionary = "animals")
#' }
#'
#' @references
#' Christensen, A. P., & Kenett, Y. N. (in press).
#' Semantic network analysis (SemNA): A tutorial on preprocessing, estimating, and analyzing semantic networks.
#' \emph{Psychological Methods}.
#'
#' Hornik, K., & Murdoch, D. (2010).
#' Watch Your Spelling!.
#' \emph{The R Journal}, \emph{3}, 22-28.
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @importFrom stats na.omit
#'
#' @export
# Text Cleaner----
# Updated 21.06.2022
# Keep strings update: 06.08.2020
# Major update: 19.04.2020
# Added type of task: 21.10.2021
textcleaner <- function(
  data = NULL, type = c("fluency", "free"),
  miss = 99, partBY = c("row","col"),
  dictionary = NULL, spelling = c("UK", "US"),
  add.path = NULL, keepStrings = FALSE,
  allowPunctuations,
  allowNumbers = FALSE, lowercase = TRUE,
  keepLength = NULL, keepCue = FALSE,
  continue = NULL
)
{
  # Check if input is continue
  if(!is.null(continue)){
    type <- continue$type
  }

  # Check for type
  if(missing(type)){

    type <- ifelse(ncol(data) > 3, "fluency", "free")

    message(
      paste(
        'Assuming semantic task type based on data structure: "',
        type, '"', sep = ""
      )
    )

  }else{
    type <- match.arg(type)
  }

  # Keep strings
  if(type == "free"){

    if(missing(keepStrings)){
      keepStrings <- TRUE
      message("'keepStrings' is set to TRUE for free association tasks by default. Set argument to change this behavior.")
    }

    if(missing(dictionary)){
      dictionary <- "cocaspell"
    }

  }

  # Warning for keepStrings
  # if(isTRUE(keepStrings)){
  #   message("Keeping strings intact is a new feature. There may be bugs or unexpected behavior.")
  #   message("\nPlease send issues to:")
  #   message("\nhttps://github.com/AlexChristensen/SemNetCleaner/issues")
  # }

  # Check for missing arguments
  if(is.null(continue)){

    ## Spelling
    if(missing(spelling)){
      spelling <- "US"
      message("\nThe 'spelling' argument was not set. Using default: 'US' English spelling")
      Sys.sleep(0.5)
    }else{
      spelling <- match.arg(spelling)
    }

    ## Allow punctuations
    if(missing(allowPunctuations)){
      allowPunctuations <- "-"
    }

  }

  # Check for type
  if(type == "fluency"){

    ## Divert to fluency textcleaner
    res <- textcleaner.fluency(
      data = data, miss = miss, partBY = partBY,
      dictionary = dictionary, spelling = spelling,
      add.path = add.path, keepStrings = keepStrings,
      allowPunctuations = allowPunctuations,
      allowNumbers = allowNumbers, lowercase = lowercase,
      keepLength = keepLength,
      continue = continue
    )


  }else if(type == "free"){

    ## Divert to free textcleaner
    res <- textcleaner.free(
      data = data, miss = miss,
      spelling = spelling,
      add.path = add.path, keepStrings = keepStrings,
      allowPunctuations = allowPunctuations, dictionary = dictionary,
      allowNumbers = allowNumbers, lowercase = lowercase,
      keepLength = keepLength, keepCue = keepCue,
      continue = continue
    )

  }

  return(res)
}
#----