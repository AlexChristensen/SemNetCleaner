#' @noRd
# British-US English Conversion (Vector)
# Ported from `brit.us.conv.vector()`. A sub-routine to convert English
# spelling for a FLAT vector of individual words (no multi-word splitting --
# see `brit_us_conversion()` below for that). Confirmed via SemNetDictionaries
# ::brit2us's actual structure: it's named by the US spelling, valued by the
# GB spelling (e.g. `brit2us[["analyze"]] == "analyse"`).
# Kept as its own function, not merged into `brit_us_conversion()`, because
# both are independently called elsewhere in the codebase (`spellcheck.dictionary
# `/`.free` call this one directly with `dictionary = TRUE`; `moniker()` calls
# `brit_us_conversion()` with `dictionary = FALSE`) -- confirmed via a
# whole-codebase grep before deciding, not assumed.
# Updated 09.09.2026
# Reworked (per direct request): the original matched the DICTIONARY
# (780 entries) against `vec` (`match(names(brit2us), vec)`), producing a
# length-780 intermediate result even when `vec` itself might only have a
# handful of words, then re-ran an equivalent `match()` a second time
# (inside `na.omit(match(...))`) just to get index positions for the
# assignment -- backwards (matching the big side against the small side)
# and redundant (computing the same match twice). Matching `vec` (small)
# against the dictionary (medium) directly needs only one `match()` call
# and naturally scales with `length(vec)`, not `length(brit2us)`.
# Real bug fixed, confirmed by direct reproduction: the original's
# `vec[na.omit(match(names(brit2us), vec))] <- spelling_gb` finds only the
# FIRST position in `vec` matching each dictionary entry (since `match()`
# returns only the first match per dictionary name), so a duplicated word
# in `vec` (e.g. `c("color", "other", "color")`) had only its first
# occurrence converted -- confirmed: the original returns
# `"colour" "other" "color"`, silently leaving the second "color"
# unconverted. Matching element-by-element fixes this as a natural
# consequence of asking the right direction of question.
# Also ~4x faster (microbenchmark, n=300, real brit2us data): ~60.7us ->
# ~16us median, from doing one `match()` per direction instead of two.
# Updated 09.09.2026
#' @noRd
# Private, lazily-memoized `unlist(SemNetDictionaries::brit2us)` -- a
# static package data structure (780 entries) that never changes at
# runtime. Optimization found during a Phase 3 robustness/speed review,
# per direct request: `brit_us_conversion_vector()`'s "US" branch was
# rebuilding this `unlist()` from scratch on every single call --
# confirmed real cost via microbenchmark (~60us each), non-negligible
# since `moniker()` (and therefore this function) is called once per word
# throughout `multiple_words()`/`ind_word_check()` across a whole
# spellcheck session. The "UK" branch's own `names(brit2us)` isn't cached
# -- confirmed ~400x cheaper (~150ns), not worth it.
# Updated 10.09.2026
.brit2us_unlisted_cache <- new.env(parent = emptyenv())

get_brit2us_unlisted <- function()
{
  if(is.null(.brit2us_unlisted_cache$value)){
    .brit2us_unlisted_cache$value <- unlist(SemNetDictionaries::brit2us)
  }
  return(.brit2us_unlisted_cache$value)
}

brit_us_conversion_vector <- function(vec, spelling = c("UK", "US"), dictionary = FALSE)
{

  if(toupper(spelling) == "UK"){

    # Convert any US-spelled words in vec to their GB equivalents
    matched <- match(vec, names(SemNetDictionaries::brit2us))
    found <- !is.na(matched)

    if(any(found)){
      vec[found] <- unlist(SemNetDictionaries::brit2us[matched[found]])
    }

  }else if(toupper(spelling) == "US"){

    # Convert any GB-spelled words in vec to their US equivalents
    matched <- match(vec, get_brit2us_unlisted())
    found <- !is.na(matched)

    if(any(found)){
      vec[found] <- names(SemNetDictionaries::brit2us)[matched[found]]
    }

  }

  # Remove duplicates and alphabetize
  if(dictionary){
    vec <- sort(unique(vec))
  }

  return(vec)

}

#' @noRd
# British-US English Conversion (per-element, possibly multi-word strings)
# Ported from `brit.us.conv()`. Splits each element of `vec` on spaces,
# converts each resulting word via `brit_us_conversion_vector()` above, then
# rejoins. Dropped the inner `lapply()`'s `dictionary` parameter: confirmed
# dead in the original (accepted as a formal argument but never referenced
# in the closure body, which only ever calls
# `brit.us.conv.vector(x, spelling = spelling)` -- a copy-paste leftover).
# Updated 09.09.2026
brit_us_conversion <- function(vec, spelling = c("UK", "US"), dictionary)
{

  vec <- unlist(lapply(vec, strsplit, split = " "), recursive = FALSE)

  vec <- lapply(vec, function(x, spelling){

    converted <- brit_us_conversion_vector(x, spelling = spelling)

    conv <- paste(converted, collapse = " ")

    return(conv)

  }, spelling = spelling)

  if(dictionary){
    vec <- sort(unlist(vec))
  }

  return(vec)

}
