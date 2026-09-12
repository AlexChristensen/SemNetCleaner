#' @noRd
# Shared normalization core for `prep_spellcheck_dictionary()` below --
# strips/allows punctuation and digits, trims whitespace, lowercases,
# collapses newlines/tabs to spaces, strips non-ASCII characters per word,
# and reverts the literal string "NA" back to a real NA. Operates on a flat
# character vector -- this is exactly the original free-association
# branch's own logic, confirmed (see prep_spellcheck_dictionary()'s own
# comment) mathematically equivalent to the original fluency branch's
# per-column `apply()` version too, since every step here is applied
# independently per-element with no cross-element dependency.
#
# Optimization pass (per direct request, found during a Phase 3 review):
# profiled this function on real 516x34 open.clean data (17,544 elements)
# and found two `lapply()`-per-element loops accounted for ~97% of a
# ~286ms total (~167ms + ~82ms) -- both fully vectorizable:
# - `unlist(lapply(trimws(responses), remove_leading_space))` called
#   `remove_leading_space()` once per element. Its own body is already
#   `bad_response()` (vectorized since Phase 1) + a `gsub()` guarded by
#   `if(!is.na(word))` -- confirmed `gsub()` already propagates `NA`
#   correctly on its own (`gsub(pattern, "", NA)` returns `NA`, verified
#   directly), so the guard, and the whole per-element loop, was
#   unnecessary. Inlined as one vectorized `bad_response()` + `gsub()`
#   pair -- ~46x faster (~167ms -> ~3.6ms, median, microbenchmark n=20),
#   verified `identical()` output.
# - `lapply(split_responses, function(y) gsub("[^\x20-\x7E]", "", y))`
#   ran `gsub()` once per RESPONSE (still internally vectorized over that
#   response's own words, but the outer loop was per-response, not per-
#   dataset). Since `gsub()` here has no cross-word dependency, flattening
#   every response's words into one vector, running `gsub()` once, then
#   regrouping by original response via `split()` is behavior-identical
#   and ~14x faster (~77ms -> ~5.6ms, median), also verified `identical()`.
# Combined: the whole function dropped from ~286ms to ~15ms on the same
# real data (~19x faster end to end).
# Updated 09.09.2026
normalize_responses <- function(responses, allow_punctuations, allow_numbers, lowercase)
{

  # A genuinely empty input (e.g. a 0-row/0-column matrix flattened to
  # character(0)) has nothing to normalize -- short-circuit before
  # `bad_response()` ever sees it. Real bug caught during verification:
  # the original's per-element `lapply(trimws(responses),
  # remove_leading_space)` silently no-ops on `character(0)` (lapply over
  # zero elements never calls its FUN), but calling the vectorized
  # `bad_response()` directly on the whole vector below exposes its own
  # `if(length(word) == 0) word <- NA` guard -- designed for "a single
  # blank response", not "zero responses at all" -- which would otherwise
  # incorrectly expand `character(0)` into a length-1 `NA`, the same bug
  # class as `responses_to_binary()`'s bugs #15/#16.
  if(length(responses) == 0){
    return(responses)
  }

  # Remove/allow punctuation
  if(all(allow_punctuations != "all")){
    characters <- paste("([", paste(allow_punctuations, collapse = ""), "])|[[:punct:]]", sep = "", collapse = "")
    responses <- gsub(characters, "\\1", responses)
  }

  # Remove/allow numbers
  if(!isTRUE(allow_numbers)){
    responses <- gsub("[[:digit:]]+", "", responses)
  }

  # Remove white spaces
  responses <- gsub("^[[:space:]]+|[[:space:]]+$", "", bad_response(trimws(responses)))

  # Change all to lowercase
  if(isTRUE(lowercase)){
    responses <- tolower(responses)
  }

  # Remove new lines
  responses <- gsub("[\n\r\t]", " ", responses)

  # Remove special characters
  ## Split words
  split_responses <- strsplit(responses, split = " ")
  ## Remove special characters -- flattened to one gsub() call over every
  ## word in every response at once, then regrouped by response. Real bug
  ## caught before this shipped: `split()` silently DROPS a group with no
  ## members (an empty response, e.g. "" -> `character(0)` from
  ## `strsplit()`) unless its factor explicitly declares every expected
  ## level -- confirmed directly (`split(x, rep(...))` on 3 responses
  ## where the middle one is empty returned only 2 groups, silently
  ## misaligning every response after it). Fixed via an explicit `factor()`
  ## with `levels = seq_along(...)`, guaranteeing exactly one group per
  ## original response, empty or not.
  response_lengths <- lengths(split_responses)
  flat_words <- gsub("[^\x20-\x7E]", "", unlist(split_responses))
  response_groups <- factor(rep(seq_along(response_lengths), response_lengths), levels = seq_along(response_lengths))
  split_responses <- unname(split(flat_words, response_groups))
  ## Re-combine words
  responses <- unlist(lapply(split_responses, paste, sep = "", collapse = " "))

  # Revert NAs
  responses <- ifelse(responses == "NA", NA, responses)

  return(responses)

}

#' @noRd
# Prepares data for `spellcheck_dictionary()` by removing punctuation,
# digits, and white space; lowercasing; and converting to a data frame.
# Ported from `prep.spellcheck.dictionary()`.
#
# Merged (per binding decision #2): the original's fluency branch ran the
# EXACT same normalization sequence as the free branch, just applied
# per-column across a whole matrix via nested `apply()`/`lapply()`/
# `simplify2array()` gymnastics instead of to one flat vector. Verified
# empirically, not assumed: a synthetic matrix mixing punctuation, digits,
# newlines/tabs, mixed case, NA, quotes, apostrophes, and multi-word
# responses produced `identical()` output between the original's own
# apply()-based fluency branch and this flatten -> normalize_responses() ->
# reshape version, across multiple `allowPunctuations`/`allowNumbers`/
# `lowercase` combinations. Both branches now share `normalize_responses()`
# above; only the matrix-vs-column reshaping around it differs.
# Updated 09.09.2026
prep_spellcheck_dictionary <- function(data, allowPunctuations, allowNumbers, lowercase, type)
{

  if(type == "fluency"){

    original_dim <- dim(data)
    original_dimnames <- dimnames(data)

    normalized <- normalize_responses(as.vector(as.matrix(data)), allowPunctuations, allowNumbers, lowercase)

    data <- matrix(normalized, nrow = original_dim[1], ncol = original_dim[2], dimnames = original_dimnames)

    # Convert to data frame
    data <- as.data.frame(data, stringsAsFactors = FALSE)

  }else if(type == "free"){

    data[, "Response"] <- normalize_responses(data[, "Response"], allowPunctuations, allowNumbers, lowercase)

    # Convert to data frame
    data <- as.data.frame(data, stringsAsFactors = FALSE)

  }

  return(data)

}
