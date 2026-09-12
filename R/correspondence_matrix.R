#' @noRd
# Builds the "from"/"to_*" correspondence matrix -- one row per unique
# response, its original ("from") value plus every value it was ever
# changed to during automated + manual spell-check ("to_1".."to_k",
# NA-padded to the widest response). Ported from `correspondence.matrix()`.
# Bug #8 audit: `for(i in 1:n)` on `n <- length(from)` reverse-ranges to
# `1:0` on a 0-response input -- fixed via `seq_len()`, matching the same
# fix already applied to the identical pattern elsewhere in this rebuild
# (bugs #15-17).
# Bug #44 fixed, found on a follow-up review: with 0 responses (`from`/`to`
# both empty), `max(unlist(lapply(to, length)))` is `max()` over nothing
# -- `-Inf`, with a warning -- and `matrix(NA, ncol = -Inf + 1)` then
# crashes ("invalid 'ncol' value"). Confirmed the untouched original
# crashes identically. Two more, compounding issues exposed once that's
# fixed, both only visible with `to_width == 0`: `1:to_width` (the
# original's own form) reverse-ranges to `1:0`; `seq_len(to_width)`
# avoids that, but `paste("to_", seq_len(0), sep = "")` still isn't
# `character(0)` the way it looks like it should be -- `paste()`'s
# default `recycle0 = FALSE` recycles a zero-length argument to `""`
# instead, producing one bogus `"to_"` column name rather than none.
# Confirmed directly. Fixed via `to_width == 0`/empty-`to` guard,
# `seq_len(to_width)` in place of `1:to_width` (same class as bugs
# #15-17), and `recycle0 = TRUE` so a genuinely empty `to_width` produces
# genuinely no "to_" column names.
# Updated 09.09.2026
correspondence_matrix <- function(from, to)
{

  n <- length(from)

  to_width <- if(length(to) == 0) 0 else max(unlist(lapply(to, length)))

  corr_mat <- matrix(NA, nrow = n, ncol = (to_width + 1))
  colnames(corr_mat) <- c("from", paste("to_", seq_len(to_width), sep = "", recycle0 = TRUE))

  for(i in seq_len(n)){
    insert <- c(from[[i]], to[[i]])
    corr_mat[i, 1:length(insert)] <- insert
  }

  return(corr_mat)

}
