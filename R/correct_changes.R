#' @noRd
# Lets the user manually review/correct every automated spell-check
# change (a spreadsheet-editor pass over `res$spellcheck$automated`), then
# folds the edited values back into `res$spellcheck$correspondence` and
# `res$spellcheck$verified`. Ported from `correct.changes()`. Rebuilt on
# `read_input()`/a `reader = readline` parameter (binding decision #3) for
# its narrative "Press ENTER to continue..." pauses, and an injectable
# `edit_fn` parameter for the actual spreadsheet-editor call itself -- a
# real editor call blocks on a GUI exactly like a real `readline()` blocks
# on the console, so it needs the same treatment to be testable.
#
# Drops a confirmed ~55-line commented-out dead block (the original's own
# "Start/End 'add column' configuration (not functional)" fluency-data-
# correction path, entirely commented out). Also drops the `original`
# local variable entirely: it was computed (`res$responses$original` or
# `res$data$original`, depending on `type`) but never once read anywhere
# in the function -- confirmed write-only, the same "compute a value only
# the now-dead block would have used" pattern as `auto.spell` (Phase 4).
#
# Bug #36 fixed: `differences <- automated != as.matrix(changes)` -- when
# one side is `NA` and the other has a real value (e.g. the user filled in
# a previously-empty "to_" alternative that only exists because an
# earlier response needed more of them), `!=` against an `NA` produces
# `NA`, not `TRUE`. `any()`/`which()` only ever select `TRUE`, silently
# treating an `NA`-involving row as "no difference detected" even though
# it genuinely changed. Confirmed directly: a user-added "to_2" value on
# a response that previously had none is silently dropped -- the function
# takes the "No responses changed" early return and hands back the
# original, unmodified object, discarding the edit entirely. Fixed by
# treating a mismatched NA-ness between the two sides as a real
# difference in its own right, on top of the original's actual value
# comparison.
#
# Phase 9 addendum (picking up binding decision #4, the Shiny app rebuild):
# the original forked on OS here -- Linux called `edit_fn(automated)`
# directly; every other OS unconditionally wrote `automated` out to
# `tempdir()/automated.csv` (a confirmed DEAD write on the Linux branch,
# never read back there) via a hardcoded Windows-style `sep = "\\"` path
# join (bug #35/#11), then launched the deferred Shiny app to read that
# same file back. Now that `automated_edit()` (Phase 9) takes `automated`
# as a real argument instead of reading a temp file, both branches
# collapse into the exact same one-line call, `edit_fn(automated)` --
# `edit_fn`'s default is now `automated_edit` (the rebuilt Shiny review
# app) rather than base `edit()`, unifying the interactive default across
# every OS instead of forking on it. Real, documented behavior
# consequence: Linux users, who previously got a `shiny`/`DT`-free
# fallback via base `edit()`, now need those (still `Suggests`-only,
# `requireNamespace()`-guarded with a clear install message) packages
# installed for the default interactive path -- pass `edit_fn = edit`
# explicitly to keep the old, lightweight, dependency-free behavior.
# `write.csv()`/the whole `tempdir()`/CSV round-trip is gone entirely, so
# is the `#' @importFrom utils write.csv` tag this file used to carry.
#
# The blank-cell -> `NA` normalization (`ifelse(changes == "", NA,
# changes)`) used to run ONLY on the non-Linux/Shiny branch -- now that
# there's one path for every `edit_fn`, it runs unconditionally, which
# closes a real gap: this exact line had zero test coverage before Phase
# 9, since it only ever ran through the un-mockable original Shiny app
# (see `test-correct_changes.R`'s new regression test for it).
# Updated 12.09.2026
correct_changes <- function(
  textcleaner_obj, type = c("fluency", "free"),
  reader = readline, edit_fn = automated_edit
)
{

  type <- match.arg(type)

  res <- textcleaner_obj

  automated <- prepare_automated_for_review(res)

  cat(color_text("\nYou will now have a chance to correct the changes that", defaults = "message"))
  cat(color_text("\nwere made during the automated spell-checking process.", defaults = "message"))
  cat(color_text("\nA spreadsheet will open allowing you to manually correct", defaults = "message"))
  cat(color_text("\nthese changes.\n\n", defaults = "message"))

  read_input("Press ENTER to continue...", reader = reader)

  cat(color_text("\nThe first column of the spreadsheet corresponds to the", defaults = "message"))
  cat(color_text("\nrow number provided in the output object `$spellcheck$correspondence`", defaults = "message"))
  cat(color_text("\n(see ?textcleaner for more information about this output).", defaults = "message"))

  cat(color_text("\n\nThe second column is the original response the participant provided", defaults = "message"))
  cat(color_text(paste("\nand columns 3 through", 3 + (ncol(textcleaner_obj$spellcheck$automated) - 2),
                      "are the automated spell-check responses."), defaults = "message"))
  cat(color_text('\nThese columns will have names formatted with "to_#".\n\n', defaults = "message"))

  read_input("Press ENTER to continue...", reader = reader)

  cat(color_text(paste("\nYou should change columns 3 through", 3 + (ncol(textcleaner_obj$spellcheck$automated) - 2),
                      "by manually typing responses."), defaults = "message"))
  cat(color_text('\nFor inappropriate responses, "NA" should be typed. When finished,', defaults = "message"))
  cat(color_text('\nyou can exit this process by clicking "Done" (or the "X" if using a', defaults = "message"))
  cat(color_text('\nlightweight spreadsheet editor via `edit_fn = edit`).\n\n', defaults = "message"))

  read_input("Press ENTER to continue with spell-check.", reader = reader)

  changes <- edit_fn(automated)
  automated <- as.matrix(automated)
  changes <- as.matrix(changes)
  changes <- ifelse(changes == "", NA, changes)

  message("\nPreparing your data...")

  changes_mat <- as.matrix(changes)
  differences <- (is.na(automated) != is.na(changes_mat)) |
    (!is.na(automated) & !is.na(changes_mat) & automated != changes_mat)

  if(!is.matrix(differences)){
    differences <- t(as.matrix(differences))
  }

  target_changes <- which(apply(differences, 1, function(x){ any(x) }))

  if(length(target_changes) == 0){

    message("\nNo responses changed.")

    return(textcleaner_obj)

  }else{

    track_changes <- list()

    for(i in seq_along(target_changes)){

      chn_mat <- rbind(automated[target_changes[i], ], changes[target_changes[i], ])
      colnames(chn_mat)[-1] <- rep("to", ncol(chn_mat) - 1)
      row.names(chn_mat) <- c("Previous", "Corrected")

      all_na_cols <- apply(chn_mat, 2, function(x){ all(is.na(x)) })
      if(any(all_na_cols)){
        chn_mat <- chn_mat[, -which(all_na_cols)]
      }

      track_changes[[automated[target_changes[i], 1]]] <- chn_mat

    }

    res$spellcheck$verified <- track_changes

    correspondence <- res$spellcheck$correspondence

    if(ncol(correspondence) > ncol(changes)){

      diff <- ncol(correspondence) - ncol(changes)
      for(i in seq_len(diff)){ changes <- as.matrix(cbind(changes, rep(NA, nrow(changes)))) }

    }else if(ncol(correspondence) < ncol(changes)){

      diff <- ncol(changes) - ncol(correspondence)
      for(i in seq_len(diff)){ correspondence <- as.matrix(cbind(correspondence, rep(NA, nrow(correspondence)))) }

    }

    correspondence[row.names(res$spellcheck$automated), ] <- changes
    res$spellcheck$correspondence <- correspondence

    na_cols <- apply(correspondence, 2, function(x){ all(is.na(x)) })
    correspondence <- correspondence[, !na_cols]

    from <- as.list(correspondence[, "from"])

    if(any(is.na(correspondence[, grep("to", colnames(correspondence))]))){
      to <- apply(as.matrix(correspondence[, grep("to", colnames(correspondence))]), 1, function(x){ unname(na.omit(x)) })
    }else{
      to <- as.matrix(correspondence[, grep("to", colnames(correspondence))])
    }

    corr_mat <- try(
      correspondence_matrix(from, to),
      silent = TRUE
    )

    if(inherits(corr_mat, "try-error")){
      return(error_report(corr_mat, "correspondence_matrix", "correct_changes"))
    }

    res$spellcheck$automated <- changes

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
#' multi-word \code{"from"} entry. Exposed as its own function (Phase 11)
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
#' @export
prepare_automated_for_review <- function(res)
{

  automated <- res$spellcheck$automated

  if(is.vector(automated)){

    # See correct_changes.R's own header comment for the full bug writeup
    # this recovers from -- a collapsed single-row selection losing its
    # correspondence-matrix row name entirely.
    automated <- t(as.matrix(automated))
    row.names(automated) <- row.names(res$spellcheck$correspondence)[
      match(automated[, "from"], res$spellcheck$correspondence[, "from"])
    ]

  }

  from_vector <- automated[, "from"]
  from_split <- strsplit(from_vector, split = " ")
  max_length <- max(unlist(lapply(from_split, length)), na.rm = TRUE)

  if(max_length > 0){
    automated <- cbind(automated, matrix(NA, nrow = nrow(automated), ncol = max_length))
    colnames(automated)[-1] <- paste("to", seq_len(ncol(automated) - 1), sep = "_")
    automated <- as.matrix(automated)
  }

  automated

}
