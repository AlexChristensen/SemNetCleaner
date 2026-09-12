#' Text Cleaner
#'
#' @description An automated cleaning function for spell-checking, de-pluralizing,
#' removing duplicates, and binarizing text data
#'
#' @param data Matrix or data frame.
#'
#' For \code{type = "fluency"}, data are expected to
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
#' after you've closed or errored out. Also accepts a previously
#' \strong{completed} result (e.g., a saved return value of this same
#' function, or of calling the manual spell-check stage directly) to
#' skip straight to finalizing the output without repeating the spell-check.
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
#' (sequential)
#'
#' @param edit_fn Function.
#' The spreadsheet-editor function used to review automated spell-check
#' changes. Defaults to \code{automated_edit}, a \code{shiny}/\code{DT}-based
#' review app (requires those packages to be installed). Pass \code{edit_fn
#' = edit} to use base R's lightweight \code{edit()} instead, with no extra
#' dependencies. Exposed so this step can be driven programmatically instead
#' of opening a real interactive editor
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
#' @importFrom utils edit
#'
#' @export
# Text Cleaner----
# Merges `textcleaner.fluency()`/`textcleaner.free()` into one function with
# internal `type` branches, per binding decision #2 -- the two originals'
# shared scaffolding (obtain ID, convert missing, normalize/prep, dispatch
# to the merged `spellcheck_dictionary()`, build a correspondence matrix,
# fold in manual corrections via `correct_changes()`) was already factored
# out into its own type-dispatching functions across Phases 2-6; what's left
# here is genuinely different per type: `partBY` transposition and the
# whole-matrix vs. long-format `keepLength`/`keepCue` filtering mechanics,
# `correct_data()` vs. `correct_data_free()` (binding decision #2's carve-
# out), and the final reshape (a binary vocabulary matrix for fluency, a
# response x cue frequency matrix for free).
#
# Real bugs found and fixed while building this (the first time either
# original function has been ported):
#
# - **`partBY`'s own default was never resolved with `match.arg()`.**
#   `if(partBY == "col")` on the unresolved 2-element default (`c("row",
#   "col")`) compares a length-2 vector against a scalar -- confirmed
#   directly that this crashes outright ("the condition has length > 1")
#   on any R >= 4.3, for the DEFAULT call path of the package's own
#   flagship function (`textcleaner(data, type = "fluency")` with no
#   explicit `partBY =`). Every one of this function's own doc examples
#   happens to always pass `partBY` explicitly, which is almost certainly
#   why this was never caught. Fixed via `match.arg(partBY)`.
# - **The fluency `keepLength` filter had two independent, compounding
#   bugs -- fixing only the first exposes the second.** (1) `response_lengths
#   <- apply(data, 1:2, length)` measures each cell's `length()` -- always
#   `1` for a single string, regardless of how many words it contains --
#   so this was a structural no-op for `keepLength >= 2` and would
#   instead flag EVERY cell for `keepLength <= 1`, always via the SAME
#   (masked) comparison as bug (2). Fixed via the free-association
#   branch's already-correct per-cell word count (`lengths(strsplit(x,
#   " "))`). (2) Once word counts are measured correctly, the REMOVAL
#   threshold itself is off by one: `which(response_lengths >=
#   keepLength)` removes any response with `>= keepLength` words --
#   confirmed directly that `keepLength = 1` then removes EVERY response
#   (every response trivially has >= 1 words), not just multi-word ones.
#   The function's own documented contract ("keep responses with words
#   less than or equal to the number") and the free-association branch's
#   already-correct `data[response_lengths <= keepLength, ]` both use
#   `<=` for KEEPING, equivalent to `>` for removing -- fixed to match
#   (`response_lengths > keepLength`). Bug (1) alone masked bug (2)
#   completely (a constant `1` is never `>= keepLength` for any
#   `keepLength >= 2`, the only values that would have exposed the
#   threshold error), which is almost certainly why this was never
#   caught -- confirmed unreachable through this exact code path in the
#   untouched original for any `keepLength` a user would plausibly set.
# - **The `continue` resume-vs-completed dispatch used the same fragile
#   `length(continue)`-counting hack as bug #10** (there, inside
#   `spellcheck.dictionary()`/`.free()` itself; here, one level up, in
#   `textcleaner.fluency()`/`.free()`, deciding whether a non-NULL
#   `continue` is a still-in-progress wizard state to hand to
#   `spellcheck_dictionary(continue = ...)`, or an already-COMPLETED
#   spell-check result to use directly) -- `length(continue) != 7` for
#   fluency, `> 8` for free, silently misrouting a completed result that
#   happened to also save its dictionary (`+1` field) into the "still
#   resuming" branch, handing `spellcheck_dictionary()` a `continue` with
#   none of the fields (`full_dictionary`, `ind`, `changes`, `main_count`,
#   ...) it actually expects. Both shapes already carry an explicit `$stop`
#   flag (`TRUE` mid-wizard, `FALSE` once finished, set by
#   `spellcheck_dictionary()` itself) -- using that instead of a field
#   count is unambiguous regardless of which optional fields happen to be
#   present, closing out bug #10's pattern at this second call site.
# - **Bug #54: the free-only correspondence-matrix "mark embedded bad
#   words" step used a bare substring match.** `grep("NA", x)` on each
#   "to_" cell was meant to catch a multi-word response like `"polar NA"`
#   (one word of a compound manually marked bad, via the "BAD WORD"
#   option) and treat the WHOLE compound as unusable -- but a substring
#   match also fires on any ordinary word that merely CONTAINS "NA", e.g.
#   `"CANADA"` or `"NASA"`, silently corrupting a perfectly valid response
#   into the bad-response sentinel. Confirmed directly (only reachable
#   with `lowercase = FALSE`, since a lowercased word can never contain an
#   uppercase "NA" substring). Fixed with a word-boundary-aware match
#   (`"(^|[[:space:]])NA([[:space:]]|$)"`) that only fires when "NA"
#   appears as its own token, and simplified from a branching
#   `apply()`/`lapply()` pair into one vectorized call (`ifelse()`/
#   `grepl()` are already vectorized over a whole matrix or vector alike).
#
# `free`'s "still in development" warning kept (not resolved) for this
# merged version -- unlike fluency, which has a real end-to-end reference
# fixture (`open.animals` raw data against `open.preprocess`'s known-good
# output), there's no equivalent bundled raw free-association dataset to
# `identical()`-verify the full pipeline against, so confidence in the
# merged free-association path rests on synthetic reproductions rather
# than a real-data regression check. Drops `textcleaner.free()`'s own
# ~40 lines of commented-out dead code (an unfinished cue-frequency-by-
# condition breakdown) and its unused `original` variable (computed but
# never read, the same write-only pattern as `auto.spell`/Phase 4).
# Updated 12.09.2026
textcleaner <- function(
  data = NULL, type = c("fluency", "free"),
  miss = 99, partBY = c("row", "col"),
  dictionary = NULL, spelling = c("UK", "US"),
  add.path = NULL, keepStrings = FALSE,
  allowPunctuations,
  allowNumbers = FALSE, lowercase = TRUE,
  keepLength = NULL, keepCue = FALSE,
  continue = NULL,
  reader = readline, search_fn = searcher::search_site, cores = 1,
  edit_fn = automated_edit
)
{

  # Resolve type
  if(!is.null(continue)){

    type <- continue$type

  }else if(missing(type)){

    type <- ifelse(ncol(data) > 3, "fluency", "free")

    message(
      paste0(
        'Assuming semantic task type based on data structure: "', type, '"'
      )
    )

  }else{
    type <- match.arg(type)
  }

  # Free-association-only defaults
  if(type == "free"){

    if(missing(keepStrings)){
      keepStrings <- TRUE
      message("'keepStrings' is set to TRUE for free association tasks by default. Set argument to change this behavior.")
    }

    if(missing(dictionary)){
      dictionary <- "cocaspell"
    }

  }

  # Shared defaults (only meaningful for a fresh run; harmlessly recomputed,
  # and unused, when resuming or given an already-completed `continue`,
  # matching the untouched original's own layout)
  if(is.null(continue)){

    if(missing(spelling)){
      spelling <- "US"
      message("\nThe 'spelling' argument was not set. Using default: 'US' English spelling")
      Sys.sleep(0.5)
    }else{
      spelling <- match.arg(spelling)
    }

    if(missing(allowPunctuations)){
      allowPunctuations <- "-"
    }

  }

  if(!is.null(continue)){

    if(isTRUE(continue$stop)){

      # Still mid-wizard -- hand straight back to spellcheck_dictionary()
      spell_check <- spellcheck_dictionary(
        continue = continue, reader = reader, search_fn = search_fn, cores = cores
      )

    }else{

      # Already a completed spell-check result (see bug fix note above)
      spell_check <- continue

    }

  }else if(type == "fluency"){

    # Real bug fixed: `match.arg()` on `partBY`'s own default (see above)
    partBY <- match.arg(partBY)

    data <- as.matrix(data)

    if(partBY == "col"){
      data <- t(data)
      message("\nParticipants were made to go across the rows")
    }

    id_res <- try(obtain_id(data, type = "fluency"), silent = TRUE)
    if(inherits(id_res, "try-error")){
      return(error_report(id_res, "obtain_id", "textcleaner"))
    }

    data <- id_res$data
    ids <- id_res$ids
    row.names(data) <- ids

    data <- try(convert_miss(data, miss, type = "fluency"), silent = TRUE)
    if(inherits(data, "try-error")){
      return(error_report(data, "convert_miss", "textcleaner"))
    }

    data <- try(
      prep_spellcheck_dictionary(data, allowPunctuations, allowNumbers, lowercase, type = "fluency"),
      silent = TRUE
    )
    if(inherits(data, "try-error")){
      return(error_report(data, "prep_spellcheck_dictionary", "textcleaner"))
    }

    if(!is.null(keepLength)){

      data <- as.matrix(data)

      # Two real, compounding bugs fixed here -- see the fix note above:
      # per-cell WORD count via `strsplit()` (not `length()`, which is
      # always 1 for a single string cell), and `> keepLength` (not `>=`)
      # for the removal threshold, matching "keep <= keepLength words"
      response_lengths <- apply(data, 1:2, function(x){ length(unlist(strsplit(x, split = " "))) })

      remove_responses <- which(response_lengths > keepLength, arr.ind = TRUE)

      if(nrow(remove_responses) != 0){
        data[remove_responses] <- NA
      }

    }

    data <- as.data.frame(data)

    uniq_resp <- na.omit(unique(unlist(data)))

    if(is.null(dictionary)){
      dictionary <- "cocaspell"
    }

    spell_check <- try(
      spellcheck_dictionary(
        uniq_resp = uniq_resp, dictionary = dictionary, spelling = spelling,
        add_path = add.path, keep_strings = keepStrings, data = data,
        type = "fluency", reader = reader, search_fn = search_fn, cores = cores
      ),
      silent = TRUE
    )

    if(inherits(spell_check, "try-error")){
      return(error_report(spell_check, "spellcheck_dictionary", "textcleaner"))
    }

  }else{

    warning(
      "Free association preprocessing is still in development. Use with caution and independently verify that the results are correct.\n\nPlease send bugs/issues to <https://github.com/AlexChristensen/SemNetCleaner/issues"
    )

    data <- as.matrix(data)

    id_res <- try(obtain_id(data, type = "free"), silent = TRUE)
    if(inherits(id_res, "try-error")){
      return(error_report(id_res, "obtain_id", "textcleaner"))
    }

    data <- id_res$data
    ids <- id_res$ids

    data <- try(convert_miss(data, miss, type = "free"), silent = TRUE)
    if(inherits(data, "try-error")){
      return(error_report(data, "convert_miss", "textcleaner"))
    }

    data <- try(
      prep_spellcheck_dictionary(data, allowPunctuations, allowNumbers, lowercase, type = "free"),
      silent = TRUE
    )
    if(inherits(data, "try-error")){
      return(error_report(data, "prep_spellcheck_dictionary", "textcleaner"))
    }

    if(!is.null(keepLength)){
      response_lengths <- lengths(strsplit(data[, "Response"], split = " "))
      data <- data[response_lengths <= keepLength, ]
    }

    if(!isTRUE(keepCue)){
      cues <- unique(data[, "Cue"])
      data <- data[is.na(match(data[, "Response"], cues)), ]
    }

    uniq_resp <- na.omit(unique(unlist(data[, "Response"])))

    spell_check <- try(
      spellcheck_dictionary(
        uniq_resp = uniq_resp, dictionary = dictionary, spelling = spelling,
        add_path = add.path, keep_strings = keepStrings, keep_length = keepLength,
        keep_cue = keepCue, data = data, type = "free",
        reader = reader, search_fn = search_fn, cores = cores
      ),
      silent = TRUE
    )

    if(inherits(spell_check, "try-error")){
      return(error_report(spell_check, "spellcheck_dictionary", "textcleaner"))
    }

  }

  # Check if spell-check was stopped (either error or user stop)
  if(isTRUE(spell_check$stop)){
    return(spell_check)
  }

  # Initialize results to return
  res <- list()

  if("dictionary" %in% names(spell_check)){
    res$dictionary <- spell_check$dictionary
  }

  data <- spell_check$data
  ids <- if(type == "fluency") row.names(spell_check$data) else unique(spell_check$data[, "ID"])

  if(type == "fluency"){
    res$responses$original <- data
  }else{
    res$data$original <- data
  }

  original <- spell_check$from
  checked <- spell_check$to

  corr_mat <- try(correspondence_matrix(original, checked), silent = TRUE)

  if(inherits(corr_mat, "try-error")){
    error_report(corr_mat, "correspondence_matrix", "textcleaner")
    return(spell_check)
  }

  if(type == "free" && ncol(corr_mat) > 1){

    # Bug #54 fix (see above): word-boundary-aware match instead of a bare
    # substring match, applied as one vectorized call over every "to_"
    # column at once (works identically whether that selection is a
    # genuine matrix or, with exactly one "to_" column, a plain vector)
    corr_mat[, -1] <- ifelse(
      grepl("(^|[[:space:]])NA([[:space:]]|$)", corr_mat[, -1]),
      "NA", corr_mat[, -1]
    )

  }

  row.names(corr_mat) <- formatC(seq_len(nrow(corr_mat)), digits = 2, flag = "0")
  res$spellcheck$correspondence <- corr_mat

  # `drop = FALSE`: a single-row selection here previously collapsed into a
  # plain named vector, losing its correspondence-matrix ROW NAME (only
  # column names survive a vector's own `names()`) -- see the severe bug
  # this caused downstream, fixed in `correct_changes()`. Keeping this a
  # genuine matrix here, at the source, is the real fix; the recovery
  # inside `correct_changes()` is defense in depth for any other caller.
  res$spellcheck$automated <- corr_mat[spell_check$auto, , drop = FALSE]
  res$spellcheck$manual <- corr_mat[spell_check$manual, , drop = FALSE]

  # Correct auto-corrections, build the final cleaned/binary/behavioral
  # output -- extracted (Phase 11) into `finalize_textcleaner()` so
  # `textcleaner_shiny()` can re-run this exact logic a second time once its
  # own corrections-review step knows the user's real edits. `spell_check`
  # passed through as the error-path fallback return value, matching this
  # function's own original (pre-extraction) behavior exactly.
  return(finalize_textcleaner(res, data, type, ids, reader = reader, edit_fn = edit_fn, spell_check = spell_check))

}
#----
