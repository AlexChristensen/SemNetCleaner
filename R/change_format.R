#' @noRd
# Appends one manual-spell-check change (a 1-row "from"/"to_*" matrix) onto
# the running per-response change log, growing the log's column count to
# fit if this change has more "to" alternatives than any change recorded
# for this response so far (or padding this change with NA columns if it
# has fewer). Ported from `change.format()`. `seq_len(r_list)` used in
# place of the original's `1:r.list` for the established reverse-range
# discipline, though not a live bug here -- `r_list` only reaches this
# loop after at least one row already exists (every call either creates
# the first row directly or appends to an existing one, so the row count
# only ever grows from 1).
# Real bug fixed: when growing the log's column count, the original wrote
# `new.matrix[i, length(row.target)] <- row.target` -- assigning a whole
# multi-element row vector into a SINGLE cell (the one at column
# `length(row.target)`), rather than spreading it across the first
# `length(row.target)` columns. Confirmed via direct reproduction: crashes
# with "number of items to replace is not a multiple of replacement
# length" as soon as a later change for the same response index has MORE
# "to" alternatives than an earlier one did (e.g. "TYPE MY OWN WORD" with
# one replacement for an earlier word in a multi-word response, then three
# comma-separated replacements for a later word in that same response).
# Fixed via `new_matrix[i, seq_along(row_target)] <- row_target`.
# Second, compounding bug fixed in the same branch, only exposed once the
# crash above was fixed: `new.matrix <- matrix(NA, nrow = r.list + 1, ...)`
# reserves one MORE row than there are existing rows to copy in -- the
# loop only ever fills `1:r.list`, leaving row `r.list + 1` permanently
# NA, and the incoming `change.matrix` is then appended as a further row
# via `rbind()` regardless, producing a spurious all-NA row in the change
# log before every column-widening change. Fixed via `nrow = r_list`
# (the incoming row's own space comes from the `rbind()` below, not from
# this allocation).
# Updated 09.09.2026
change_format <- function(change_list, change_matrix, current_index)
{

  if(is.null(names(change_list))){
    change_list <- list(change_list)
    names(change_list) <- paste(current_index)
  }

  n_list <- ncol(change_list[[paste(current_index)]])
  n_matrix <- ncol(change_matrix)
  r_list <- nrow(change_list[[paste(current_index)]])

  if(!is.null(n_list)){

    if(n_list < n_matrix){

      new_matrix <- matrix(NA, nrow = r_list, ncol = n_matrix)
      colnames(new_matrix) <- colnames(change_matrix)

      for(i in seq_len(r_list)){
        row_target <- as.vector(change_list[[paste(current_index)]][i, ])
        new_matrix[i, seq_along(row_target)] <- row_target
      }

      new_matrix <- rbind(new_matrix, change_matrix)
      change_list[[paste(current_index)]] <- new_matrix
      row.names(change_list[[paste(current_index)]]) <- NULL

    }else if(n_list > n_matrix){

      diff_cols <- n_list - n_matrix
      change_matrix <- c(as.vector(change_matrix), rep(NA, diff_cols))
      change_list[[paste(current_index)]] <- rbind(change_list[[paste(current_index)]], change_matrix)
      row.names(change_list[[paste(current_index)]]) <- NULL

    }else{
      change_list[[paste(current_index)]] <- rbind(change_list[[paste(current_index)]], change_matrix)
      row.names(change_list[[paste(current_index)]]) <- NULL
    }

  }else{
    change_list[[paste(current_index)]] <- change_matrix
  }

  return(change_list)

}
