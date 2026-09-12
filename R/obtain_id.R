#' @noRd
# Determines participant IDs for `textcleaner()`'s fluency/free pipelines --
# either a named ID column (checked against a fixed set of common names) or,
# for fluency data with no named ID column, the row number. Ported from
# `obtain.id()`. Kept as ONE type-dispatching function (not split into
# fluency/free variants) -- confirmed the two branches are genuinely
# different logic (fluency falls back to row-number IDs when none are
# found; free has no such fallback, since free-association data requires a
# real participant ID to group cue/response rows), not ~80% duplicated
# scaffolding like the auto_spellcheck()/spellcheck_dictionary() pairs.
# Real bug fixed: `ids <- 1:nrow(data)` reverse-ranges to `1:0` when
# `nrow(data) == 0` (a 0-row input, e.g. an entirely-empty data set) --
# fixed via `seq_len()`.
# Second real bug fixed, confirmed reachable: for `type == "free"` with no
# identifiable ID column (no named column, no uniquely-valued column), the
# original's `else` branch only ever printed an informative message ("No
# IDs were identified...") and never assigned `ids` at all -- the function
# then crashed with the confusing internal error "object 'ids' not found"
# immediately after telling the user what to do about it. The message was
# clearly meant to be the actual explanation for a real error; now raised
# as one via `stop()` instead of silently falling through to a crash.
# Third real bug fixed, per direct request: if `data` has MORE THAN ONE
# column matching `named_id` (e.g. both "Subject" and "ID"), the original
# silently let `id_col` become a multi-element vector -- `res$ids` then
# became a 2-column data frame instead of a single ID vector, and BOTH
# matched columns got stripped from `res$data`. Confirmed byte-identical
# to the untouched original, not a regression, but a genuine product
# ambiguity worth a real, informative error instead of silently guessing
# or generating malformed output.
# Fourth real bug fixed, found while building Phase 7 (`textcleaner()`,
# this function's only real caller): bug #50's own fix, `vapply(data,
# function(x){length(unique(x))}, integer(1))`, silently assumed `data` is
# a DATA FRAME -- `vapply()`/`lapply()`/`sapply()` on a plain MATRIX
# iterate per ELEMENT, not per column, since a matrix has no per-column
# list structure to begin with. `textcleaner()` (both the untouched
# original's `textcleaner.fluency()`/`.free()` and this rebuild's merged
# version) always calls `obtain_id()` with `data <- as.matrix(data)` done
# immediately beforehand -- its actual, only real calling context -- so
# this was unreachable from `obtain_id()`'s own Phase 3 tests (which
# exercised it with a data frame directly) but broke immediately on
# integration. Confirmed directly: on a 3-row, 2-column matrix with no
# named ID column, every one of the 6 individual elements trivially has
# 1 unique value, so `unique_counts` becomes a 6-element all-1s vector
# instead of a 2-element per-column vector -- `which(unique_counts ==
# nrow(data))` then only ever matches when `nrow(data) == 1`, silently
# defeating the whole "single all-unique column is the ID" heuristic for
# any real multi-row matrix input, even when a genuine candidate column
# exists. For `type == "free"` (no row-number fallback) this manifests as
# the "No IDs were identified" error firing on data that DOES have a
# legitimate ID column; for `type == "fluency"` it silently masks the
# problem by falling back to row-number IDs instead. Fixed by coercing to
# a data frame (`as.data.frame(data)`) before the per-column `vapply()`,
# which is a no-op when `data` already is one.
# Updated 12.09.2026
obtain_id <- function(data, type)
{

  # Check for named column
  named_id <- c("subject", "id", "participant", "part", "sub")

  if(any(named_id %in% tolower(colnames(data)))){

    id_col <- na.omit(match(named_id, tolower(colnames(data))))

    if(length(id_col) > 1){
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

  }else{

    # Check for variable with unique values. Bug #50 fixed, found during
    # a Phase 3 robustness review while testing the ambiguity fix just
    # below: `apply(data, 2, unique)` returns a LIST only when the
    # per-column `unique()` result lengths are ragged -- whenever they
    # happen to be EQUAL across every column (confirmed directly: any
    # dataset where every column's unique-value count ties, not just the
    # would-be ID candidates), `apply()` silently simplifies its result
    # into a MATRIX instead, and `lapply()` on a matrix iterates per CELL,
    # not per column -- every element then has length 1, so `unique_cols`
    # comes back empty regardless of how many genuine candidates exist.
    # Confirmed inherited identically from the untouched original
    # (`obtain.id()`'s own `uniq.cols <- which(lapply(apply(data, 2,
    # unique), length) == nrow(data))`, byte-identical mechanism), and
    # confirmed this "single genuine unique-value candidate, no named
    # column" scenario -- the actual intended use of this heuristic -- had
    # no existing test coverage at all; the only two prior tests covered
    # "named column found" and "zero candidates," neither of which
    # exercises this code path's real job. `vapply()` over the data frame
    # directly (iterating per column by construction, with a fixed
    # scalar return type) has no equivalent simplification pitfall.
    # A 0-row data set has no meaningful "one unique value per row"
    # concept -- every column trivially has 0 unique values, which would
    # otherwise trivially equal `nrow(data) == 0` for EVERY column at
    # once, misreporting total ambiguity instead of "no candidate."
    if(nrow(data) == 0){
      unique_cols <- integer(0)
    }else{
      unique_counts <- vapply(as.data.frame(data), function(x){ length(unique(x)) }, integer(1))
      unique_cols <- which(unique_counts == nrow(data))
    }

    # Only one unique column variable
    if(length(unique_cols) == 1){

      id_col <- unique_cols

    }else if(length(unique_cols) > 1){

      # Robustness fix (Phase 3 review, per direct request): consistent
      # with the named-column ambiguity check above (already fixed, "per
      # direct request", in an earlier phase) -- multiple all-unique
      # candidate columns previously fell through silently, indistinguishable
      # from "none found." For `type == "free"` that produced a genuinely
      # misleading "No IDs were identified" error even though candidates
      # WERE found, just ambiguous. Now raised as its own clear error.
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

  if(type == "fluency"){

    if(exists("id_col", envir = environment()))
    {
      # Grab unique identifier
      ids <- data[, id_col]

      # Let user know what IDs refer to
      message(paste("\nIDs refer to variable:", " '", colnames(data)[id_col], "'", sep = ""))

      # Remove unique identifier from data
      data <- data[, -id_col]
    }else{
      # Make rows the IDs
      ids <- seq_len(nrow(data))

      # Let user know what IDs refer to
      message("\nIDs refer to row number")
    }

  }else if(type == "free"){

    if(exists("id_col", envir = environment()))
    {
      # Grab unique identifier
      ids <- unique(data[, id_col])

      # Let user know what IDs refer to
      message(paste("\nIDs refer to variable:", " '", colnames(data)[id_col], "'", sep = ""))

    }else{
      # No ID column could be identified -- this used to fall through to a
      # confusing "object 'ids' not found" crash. A real, informative error
      stop("No IDs were identified in data. Please name ID column as \"ID\"", call. = FALSE)
    }

  }

  # Initialize result list
  res <- list()
  res$data <- data
  res$ids <- ids

  return(res)

}
