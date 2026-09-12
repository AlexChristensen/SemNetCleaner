#' @noRd
# QWERTY Distance
# Ported from `qwerty.dist()`. Full-word-expansion rename ("dist" -> "distance")
# per the naming convention -- an ordinary abbreviation, not an established
# domain acronym like SemNeT's TMFG/ASPL. Added up-front validation (the
# original had none). Real bug fixed: `for(i in 1:nchar(word_a))` reverse-
# ranges to `1:0` when comparing two empty strings (`nchar(word_a) ==
# nchar(word_b) == 0` passes the length-match guard), indexing a keyboard
# row by an empty-string lookup instead of correctly returning distance 0
# for two identical empty strings -- fixed via seq_len(), which loops zero
# times, leaving `sum(numeric(0)) == 0` as the (correct) result. Dropped
# the original's `count` variable: proven always identical to the loop
# index `i` (both start matched and increment in lockstep every iteration),
# so indexing directly with `i` is a behavior-identical simplification.
#
# Optimization pass (microbenchmark, n=500), both verified identical()
# against the original across real dictionary word pairs, punctuation/
# space characters, empty strings, an unrecognized-character error case,
# and a 26-character stress word -- ~3x faster overall (~797us -> ~252us,
# median):
# - The keyboard coordinate matrix was rebuilt from scratch on every
#   single call (this function's only real work, aside from the actual
#   distance computation) -- now built once and memoized in a private
#   cache, the same pattern as helpers.R's system_check().
# - The per-character `for()` loop, whose body called `apply(rbind(...), 2,
#   diff)` (general-purpose, high-overhead machinery) just to compute
#   `abs(B[i,] - A[i,])`, replaced with one `strsplit()` + matrix lookup +
#   a single vectorized `abs(A - B)` over every character at once.
# Updated 09.09.2026
.qwerty_keyboard_cache <- new.env(parent = emptyenv())

get_qwerty_keyboard <- function()
{

  if(is.null(.qwerty_keyboard_cache$value)){

    # Keyboard structure
    # Taken from <https://stackoverflow.com/questions/43946912/calculating-levenshtein-distance-permitting-qwerty-errors-in-r>
    m <- structure(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0, 1, 1, 2, 2, 3,
                     4, 5, 6, 4, 5, 6, 7, 8, 3, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                     2, 1, 2, 1, 2, 1, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2),
                   .Dim = c(27L, 2L),
                   .Dimnames = list(c("q", "w", "e", "r", "t", "y", "u", "i", "o", "p",
                                      "a", "z", "s", "x", "d", "c", "f", "b", "m", "j", "g",
                                      "h", "j", "k", "l", "v", "n"), c("x", "y")))

    # Compile qwerty locations
    keyb <- sweep(m, 2, c(1, -1), "*")
    # `-which(duplicated(...))` is only ever safe when a match is
    # guaranteed -- true here since `m` is a hardcoded constant with a
    # known "j" duplicate, but this is the exact `x[-which(cond)]`-on-
    # empty footgun eliminated everywhere else in this rebuild (bugs
    # #37-#41). Hardened proactively to the same positive-filter idiom, so
    # a future edit to `m` can't silently reintroduce it here.
    keyb <- keyb[!duplicated(row.names(keyb)), ]
    keyb["m", "x"] <- 6

    # Add keys
    keyb <- rbind(
      keyb,
      c(4, -3), # space
      c(9, 1), # hyphen
      c(9, 1), # underscore
      c(10, -1), # apostrophe
      c(10, -1), # quote
      c(7, -2), # comma
      c(8, -2), # period
      c(9, -2), # slash
      c(9, -2) # question mark
    )
    row.names(keyb)[27:nrow(keyb)] <- c(
      " ", "-", "_",
      "'", '"',
      ",", ".",
      "/", "?"
    )

    .qwerty_keyboard_cache$value <- keyb

  }

  return(.qwerty_keyboard_cache$value)

}

#' QWERTY Distance for Same Length Words
#'
#' @description Computes QWERTY distance for words that have
#' the same number of characters. Distance is computed based on
#' the number of keys a character is away from another character
#' on a QWERTY keyboard
#'
#' @param word_a Character.
#' Word to be compared
#'
#' @param word_b Character.
#' Word to be compared
#'
#' @return Numeric value for distance between \code{word_a} and \code{word_b}
#'
#' @examples
#' # Identical values for Damerau-Levenshtein
#' stringdist::stringdist("big", "pig", method = "dl")
#'
#' stringdist::stringdist("big", "bug", method = "dl")
#'
#' # Different distances for QWERTY
#' qwerty_distance("big", "pig")
#'
#' qwerty_distance("big", "bug") # Probably meant to type "bug"
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Real bug found during Phase 8's roxygen/NAMESPACE audit: this doc block
# (with its `@export` tag) used to sit atop the file, ahead of the private
# `.qwerty_keyboard_cache`/`get_qwerty_keyboard()` memoization helpers added
# during Phase 2's optimization pass -- roxygen attaches a doc block to the
# very next R statement, which was `.qwerty_keyboard_cache <- new.env(...)`,
# not this function. `qwerty_distance()` itself was silently never exported
# (masked in every phase's own dev-mode testing, since `devtools::load_all()`
# exposes internal objects too), while the private cache environment was
# wrongly exported instead. Moved to directly precede the function it
# actually documents.
# Updated 12.09.2026
qwerty_distance <- function(word_a, word_b)
{

  qwerty_distance_errors(word_a, word_b)

  # Remove diacritic characters
  word_a <- stringi::stri_trans_general(word_a, "Latin-ASCII")
  word_b <- stringi::stri_trans_general(word_b, "Latin-ASCII")

  if(nchar(word_a) == nchar(word_b)){

    keyb <- get_qwerty_keyboard()

    # Look up every character's keyboard coordinates at once
    A <- keyb[strsplit(word_a, "")[[1]], , drop = FALSE]
    B <- keyb[strsplit(word_b, "")[[1]], , drop = FALSE]

    # Sum of per-character, per-axis absolute coordinate differences
    distance <- sum(abs(A - B))

  }else{
    distance <- NA
  }

  return(distance)

}

#' @noRd
# Up-front validation for `qwerty_distance()` -- the original had none
# Updated 08.09.2026
qwerty_distance_errors <- function(word_a, word_b)
{
  typeof_error(word_a, "character", "qwerty_distance")
  typeof_error(word_b, "character", "qwerty_distance")
  length_error(word_a, 1, "qwerty_distance")
  length_error(word_b, 1, "qwerty_distance")
  na_error(word_a, "qwerty_distance")
  na_error(word_b, "qwerty_distance")
}
