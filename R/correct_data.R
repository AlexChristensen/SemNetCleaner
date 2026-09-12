#' @noRd
# Applies the final (automated + manual) spell-check corrections back onto
# the original fluency response matrix, and counts perseverations
# (duplicate responses within a participant) and intrusions (responses
# marked bad, `"NA"`) along the way. Ported from `correct.data()`. Kept
# separate from `correct_data_free()` per binding decision #2's carve-out
# -- confirmed genuinely different algorithms (the free variant does
# cue/ID sequence row-splicing this one doesn't need at all, since fluency
# data has no cue/sequence structure to preserve).
# Bug #8 audit: `for(i in 1:n)` on `n <- nrow(data)` reverse-ranges to
# `1:0` on a 0-participant input -- fixed via `seq_len()`, same class as
# bugs #15-17.
# Bug #37 fixed: `apply(data, 2, function(x){ !is.na(x) })` --
# when `data` has exactly 1 row, `!is.na(x)` returns a length-1 logical
# per column, and `apply()` simplifies a per-column-scalar result down to
# a plain vector instead of a matrix. `rowSums()` then crashes ("'x' must
# be an array of at least two dimensions"). Confirmed the untouched
# original crashes identically on a 1-participant dataset. `!is.na(data)`
# is already vectorized over the whole matrix, giving the identical
# logical matrix directly -- no `apply()` (or its row-count-dependent
# simplification) needed at all.
# Bug #38 fixed, compounding bug #37 above (only ever exposed once it's
# fixed): `correct_mat[, -which(...)]` (removing all-NA columns) drops
# matrix-ness entirely when exactly 1 column survives -- R's default
# `drop = TRUE` collapses a single-column result to a plain vector.
# `row.names(correct_mat) <- row.names(data)` on the next line then
# crashes ("attempt to set 'rownames' on an object with no dimensions").
# Confirmed reachable with the same 1-participant shape that exposed bug
# #37. Fixed via `drop = FALSE`.
#
# `x[-which(cond)]` on a plain vector (not a matrix) returns `x`
# *emptied out*, not `x` unchanged, whenever `which(cond)` is itself
# empty -- R can't tell a "negative empty" index (exclude nothing) apart
# from a "positive empty" one (select nothing); both are just
# `integer(0)`, and indexing treats it as the latter. Confirmed directly:
# `c("dog","dog","cat")[-which(c("dog","dog","cat") == "NA")]` gives
# `character(0)`, not the original 3-element vector, whenever there's no
# literal `"NA"` entry to exclude. Two real, confirmed instances of this
# exact pattern here, both fixed the same way (a positive
# `!=`/`!all(is.na(...))` filter, which behaves correctly whether or not
# anything actually matches):
#  - Bug #39, severe and NOT an edge case -- reachable on completely
#    ordinary multi-participant data: the Perseverations count
#    (`correct_ord[-which(correct_ord == "NA")]`) was silently always 0
#    whenever a participant had no `"NA"`-string response -- the
#    ordinary case for anyone with no bad responses. Confirmed directly
#    against real `open.preprocess` data: the untouched original reports
#    a total of just 3 perseverations across all 516 participants; the
#    correct total, computed the same way but without this bug, is 116 --
#    a ~38x undercount, affecting 73 of 516 participants (14%).
#  - Bug #40: "Remove columns that are all NA"
#    (`correct_mat[, -which(apply(...))]`) collapses to a *zero-column*
#    matrix whenever there are no all-NA columns to remove (i.e. every
#    response slot got used, e.g. one response mapping to as many "to_"
#    alternatives as there are allocated columns). Confirmed directly
#    against the untouched original, which then crashes one line later
#    at `colnames(correct_mat) <- ...` (`1:ncol(correct_mat)`
#    reverse-ranging to `1:0` on the now-zero-width matrix -- fixed here
#    too, via `seq_len()`, same class as bugs #15-17).
#
# Bug #45 fixed, found on a follow-up review, compounding bug #37 (only
# ever exposed once it's fixed) on the OPPOSITE edge -- every response
# for a participant is entirely NA/unresolvable, so `ncol(correct_mat)`
# is 0 after the all-NA-column removal above: `paste("Response_",
# formatC(seq_len(0), ...), sep = "")` isn't `character(0)` the way it
# looks like it should be -- `paste()`'s default `recycle0 = FALSE`
# recycles the zero-length `formatC()` result to `""` instead, producing
# one bogus `"Response_"` column name for a matrix that actually has zero
# columns, and `colnames(correct_mat) <- ...` then crashes ("length of
# 'dimnames' [2] not equal to array extent"). Same underlying `paste()`
# gotcha as bug #44 in `correspondence_matrix()`; fixed the same way, via
# `recycle0 = TRUE`.
# Updated 09.09.2026
correct_data <- function(data, corr_mat)
{

  n <- nrow(data)

  # Maximum number of responses that are not NA in original data
  data_max <- max(rowSums(!is.na(data)))

  correct_mat <- matrix(NA, nrow = n, ncol = 2 * data_max)

  behav_mat <- matrix(0, nrow = n, ncol = 2)
  colnames(behav_mat) <- c("Perseverations", "Intrusions")
  row.names(behav_mat) <- row.names(data)

  for(i in seq_len(n)){

    ind <- match(data[i, !is.na(data[i, ])], corr_mat[, "from"])

    corr <- corr_mat[ind, -1]

    if(!is.matrix(corr)){
      corr <- as.matrix(corr)
    }

    correct_ord <- na.omit(as.vector(t(corr)))

    if(length(correct_ord) > 0){

      behav_mat[i, "Intrusions"] <- sum(correct_ord == "NA")
      behav_mat[i, "Perseverations"] <- length(correct_ord[correct_ord != "NA"]) - length(unique(correct_ord[correct_ord != "NA"]))

      correct_mat[i, 1:length(correct_ord)] <- correct_ord

    }else{

      behav_mat[i, "Intrusions"] <- 0
      behav_mat[i, "Perseverations"] <- 0

    }

  }

  # Remove columns that are all NA
  correct_mat <- correct_mat[, !apply(correct_mat, 2, function(x){ all(is.na(x)) }), drop = FALSE]
  row.names(correct_mat) <- row.names(data)
  colnames(correct_mat) <- paste("Response_", formatC(seq_len(ncol(correct_mat)),
                                                      digits = nchar(ncol(correct_mat)) - 1,
                                                      flag = "0"), sep = "", recycle0 = TRUE)

  res <- list()
  res$behavioral <- behav_mat
  res$corrected <- correct_mat

  return(res)

}
