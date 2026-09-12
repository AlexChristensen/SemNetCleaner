#' @noRd
# Applies the final (automated + manual) spell-check corrections back onto
# the original free-association long-format data (ID/Cue/Response rows),
# counting perseverations and intrusions per participant. Ported from
# `correct.data.free()`. Kept separate from `correct_data()` per binding
# decision #2's carve-out -- confirmed genuinely different algorithms: this
# variant splices rows in and out of the data frame to keep contiguous
# cue/ID sequences aligned with `corr_mat` (via `seq_min_max()`, `helpers.R`),
# which `correct_data()` has no equivalent of at all.
# Bug #8 audit: all 3 `for(i in 1:length(...))`-style loops reverse-range
# to `1:0` on an empty `ids`/`cues`/`target_seq$min` -- fixed via
# `seq_along()`, same class as bugs #15-17.
#
# Bugs #42/#43 fixed, found on a second, closer re-read after Phase 6's
# review: the row-splicing branch's `data_top <- correct_mat[1:(minPosition
# - 1), ]` and `data_bottom <- correct_mat[(max(target_resps) + 1):
# nrow(correct_mat), ]` both reverse-range the same way as bugs #15-17,
# just on a computed offset instead of a bare `nrow()`/`length()`. Bug
# #42: when the response being split is the very FIRST row in the whole
# dataset (`minPosition == 1`), `1:(1-1)` is `1:0` -- not empty, `c(1,0)`
# -- so `data_top` incorrectly includes row 1 (the original, un-split
# response) instead of being empty. Bug #43, symmetric: when the response
# being split is the very LAST row (`max(target_resps) == nrow(correct_mat)`),
# `(nrow+1):nrow` is a *descending*, non-empty 2-element range (e.g.
# `4:3`) -- `seq()` has the identical behavior to `:` here, confirmed
# directly, so it's not a viable fix on its own -- producing a phantom
# out-of-bounds NA row plus a duplicate of the last real row in
# `data_bottom`. Confirmed reachable and reproduced end to end via
# `correct_data_free()` itself (not just the isolated indexing
# expression) with a single-participant, single-cue, single-response
# dataset that needs splitting -- `minPosition == 1 == nrow(data)`
# simultaneously exercises both: the untouched original returns 4 rows
# (with "catdog" duplicated) instead of the correct 2 (just "cat", "dog").
# Fixed via `seq_len(minPosition - 1)` for bug #42 (empty at 0, matching
# the established fix pattern) and an explicit `if(bottom_start >
# nrow(correct_mat))` guard for bug #43 (no single-expression form of `:`
# or `seq()` produces an empty result the way `seq_len()` does for a
# from-1 range).
#
# Bugs #46/#47 fixed, found on a third re-read, per direct request ("double
# check your work for bugs"):
#  - Bug #46: `corr <- corr_mat[ind, -1]` collapses to a plain vector in
#    TWO different, indistinguishable-by-`length()`-alone situations --
#    a single matched response (`length(ind) == 1`, any number of `to_`
#    columns) or a single `to_` column overall (`ncol(corr_mat) == 2`, any
#    number of matched responses) -- and only the first actually wants a
#    1-row reshape (`matrix(corr, ncol = length(corr))`, i.e. what the
#    code did unconditionally). For the second, that produces a 1-row,
#    multi-column matrix instead of the intended multi-row, 1-column one
#    -- confirmed directly reachable with any dataset where no response in
#    the whole correspondence table ever needed a multi-word split
#    (`corr_mat` then has exactly one `to_1` column), which mis-shapes
#    `nrow(corr)` down to 1 and forces the `length(ind) == nrow(corr) &
#    ncol(corr) == 1` branch condition below to take the wrong (row-
#    splicing) path for perfectly ordinary, non-splitting responses.
#    Confirmed the untouched original visibly corrupts data as a result
#    (duplicated rows) whenever that misrouting also lands on the bug
#    #42/#43 boundary condition; even when it doesn't, it still silently
#    feeds bug #47 below. Fixed by disambiguating the two collapse cases
#    explicitly instead of assuming `length(ind)` is always the row count.
#  - Bug #47: even on a genuine split (`ncol(corr) > 1` for real reasons),
#    `correct_ord <- as.vector(t(corr[, 1]))` in the row-splicing branch
#    only reads the FIRST `to_` column across all matched responses --
#    silently ignoring every other column's values entirely, including
#    the very split words the splicing logic exists to add. Perseveration
#    counting then misses any duplicate that only shows up once a
#    response's later `to_` columns are considered (e.g. one response
#    splits into "cat"+"dog" while another, unrelated response in the
#    same cue-sequence is itself just "dog" -- a real duplicate the
#    row-splicing logic below `correct_ord` already detects and silently
#    drops from the output via `add_responses`/`duplicated()`, but which
#    `behav_mat` never counted). Confirmed directly, isolated from bugs
#    #42/#43 (a boundary-free 6-row, 3-participant construction) and
#    against the untouched original identically. Fixed by deriving
#    `correct_ord` the same way `add_responses` already is a few lines
#    down -- a full row-wise, NA-omitted flatten of `corr` -- instead of
#    just its first column.
# Updated 10.09.2026
correct_data_free <- function(data, corr_mat, ids)
{

  correct_mat <- data

  behav_mat <- matrix(0, nrow = length(ids), ncol = 2)
  colnames(behav_mat) <- c("Perseverations", "Intrusions")
  row.names(behav_mat) <- ids

  for(i in seq_along(ids)){

    ind_p <- which(correct_mat[, "ID"] == ids[i])
    target_p <- correct_mat[ind_p, ]

    cues <- unique(target_p[, "Cue"])

    for(j in seq_along(cues)){

      ind_c <- which(target_p[, "Cue"] == cues[j])
      target_c <- target_p[ind_c, ]

      if(!is.data.frame(target_c) & !is.matrix(target_c)){
        target_c <- t(as.matrix(target_c))
      }

      # Identify sequences (more than one time point or condition)
      target_seq <- seq_min_max(as.numeric(row.names(target_c)))

      for(s in seq_along(target_seq$min)){

        if(length(target_seq$min) == 1){
          start <- 1
          end <- nrow(target_c)
        }else{
          start <- target_seq$starts[s]
          end <- target_seq$breaks[s]
        }

        ind <- match(
          target_c[start:end, "Response"],
          corr_mat[, "from"]
        )

        corr <- corr_mat[ind, -1]

        if(!is.matrix(corr)){
          if(length(ind) == 1){
            corr <- matrix(corr, nrow = 1)
          }else{
            corr <- matrix(corr, ncol = 1)
          }
        }

        na_cols <- apply(corr, 2, function(x){ all(is.na(x)) })

        if(any(na_cols)){

          corr <- corr[, !na_cols]

          if(!is.matrix(corr)){
            corr <- matrix(corr, nrow = length(corr))
          }

        }

        if(length(ind) == nrow(corr) & ncol(corr) == 1){

          correct_ord <- as.vector(corr)

          if(length(correct_ord) > 0){

            behav_mat[i, "Intrusions"] <- behav_mat[i, "Intrusions"] + sum(is.na(correct_ord))
            behav_mat[i, "Perseverations"] <- behav_mat[i, "Perseverations"] + sum(duplicated(correct_ord))

            if(any(duplicated(correct_ord))){
              correct_ord[duplicated(correct_ord)] <- NA
            }

            target_resps <- ind_p[ind_c][start:end]

            correct_mat[target_resps, "Response"] <- correct_ord

          }else{

            behav_mat[i, "Intrusions"] <- behav_mat[i, "Intrusions"] + 0
            behav_mat[i, "Perseverations"] <- behav_mat[i, "Perseverations"] + 0

          }

        }else{

          correct_ord <- unname(unlist(apply(corr, 1, function(x){ na.omit(x) })))

          behav_mat[i, "Intrusions"] <- behav_mat[i, "Intrusions"] + sum(is.na(correct_ord))
          behav_mat[i, "Perseverations"] <- behav_mat[i, "Perseverations"] + sum(duplicated(correct_ord))

          # Reorganize for rows to add -- same values as `correct_ord` above
          # (identical `apply()` over the same, unmodified `corr`); reuse
          # instead of recomputing, since R's copy-on-modify semantics make
          # this safe even though `add_responses` is mutated just below.
          add_responses <- correct_ord

          if(any(duplicated(add_responses))){
            add_responses[duplicated(add_responses)] <- NA
          }

          target_resps <- ind_p[ind_c][start:end]

          minPosition <- min(target_resps)
          maxPosition <- minPosition + length(add_responses) - 1
          position <- seq(minPosition, maxPosition, 1)

          data_top <- correct_mat[seq_len(minPosition - 1), , drop = FALSE]

          bottom_start <- max(target_resps) + 1
          if(bottom_start > nrow(correct_mat)){
            data_bottom <- correct_mat[0, , drop = FALSE]
          }else{
            data_bottom <- correct_mat[bottom_start:nrow(correct_mat), , drop = FALSE]
          }

          data_middle <- data.frame(
            ID = rep(ids[i], length(add_responses)),
            Cue = rep(cues[j], length(add_responses)),
            Response = add_responses
          )

          correct_mat <- rbind(
            data_top,
            data_middle,
            data_bottom
          )

          # Rename row names -- necessary for seq_min_max()
          row.names(correct_mat) <- 1:nrow(correct_mat)

          ind_p <- which(correct_mat[, "ID"] == ids[i])
          target_p <- correct_mat[ind_p, ]

          ind_c <- which(target_p[, "Cue"] == cues[j])
          target_c <- target_p[ind_c, ]

          if(!is.data.frame(target_c) & !is.matrix(target_c)){
            target_c <- t(as.matrix(target_c))
          }

          target_seq <- seq_min_max(as.numeric(row.names(target_c)))

        }

      }

    }

  }

  # Check for "NA" responses
  if(any(correct_mat[, "Response"] == "NA", na.rm = TRUE)){
    correct_mat[, "Response"] <- ifelse(
      correct_mat[, "Response"] == "NA",
      NA,
      correct_mat[, "Response"]
    )
  }

  # Remove rows that are all NA
  correct_mat <- na.omit(correct_mat)

  res <- list()
  res$behavioral <- behav_mat
  res$corrected <- correct_mat

  return(res)

}
