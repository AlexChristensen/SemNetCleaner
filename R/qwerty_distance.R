#' @noRd
# Keyboard distance between two words of the same length: the sum of the horizontal and
# vertical key distances of each pair of characters (`NA` for words of different length
# or characters without a key position)
# The keyboard coordinates are built once and cached
# Updated 14.10.2026
# Cache keyboard coordinates
.qwerty_keyboard_cache <- new.env(parent = emptyenv())

# Get QWERTY keyboard ----
# Get keyboard coordinates
# Updated 14.10.2026
get_qwerty_keyboard <- function()
{

  # Check for cached keyboard
  if(is.null(.qwerty_keyboard_cache$value)){

    # Keyboard structure
    # Taken from <https://stackoverflow.com/questions/43946912/calculating-levenshtein-distance-permitting-qwerty-errors-in-r>
    m <- structure(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0, 1, 1, 2, 2, 3,
                     4, 5, 6, 4, 5, 6, 7, 8, 3, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                     2, 1, 2, 1, 2, 1, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2),
                   dim = c(27L, 2L),
                   dimnames = list(c("q", "w", "e", "r", "t", "y", "u", "i", "o", "p",
                                      "a", "z", "s", "x", "d", "c", "f", "b", "m", "j", "g",
                                      "h", "j", "k", "l", "v", "n"), c("x", "y")))

    # Compile qwerty locations
    keyb <- sweep(m, 2, c(1, -1), "*")

    # Remove duplicated keys ("j" is in the table twice)
    keyb <- keyb[!duplicated(row.names(keyb)), ]

    # Set position of m
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
      c(9, -2), # question mark
      # Number row, one row above "qwertyuiop" (x matches the key below)
      cbind(0:9, 1)
    )

    # Name added keys
    row.names(keyb)[27:nrow(keyb)] <- c(
      " ", "-", "_",
      "'", '"',
      ",", ".",
      "/", "?",
      c(1:9, 0)
    )

    # Store keyboard in cache
    .qwerty_keyboard_cache$value <- keyb

  }

  # Return keyboard
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
# QWERTY keyboard distance ----
# Keyboard distance between two words of the same length
# Updated 14.10.2026
qwerty_distance <- function(
    word_a, # first word
    word_b # second word
)
{

  # Check for errors
  qwerty_distance_errors(word_a, word_b)

  # Remove diacritic characters and lowercase (the lookup table only has lowercase keys)
  word_a <- tolower(stringi::stri_trans_general(word_a, "Latin-ASCII"))

  # Lowercase and transliterate second word
  word_b <- tolower(stringi::stri_trans_general(word_b, "Latin-ASCII"))

  # Check for equal lengths
  if(nchar(word_a) == nchar(word_b)){

    # Get keyboard coordinates
    keyb <- get_qwerty_keyboard()

    # Split first word into characters
    characters_a <- strsplit(word_a, "")[[1]]

    # Split second word into characters
    characters_b <- strsplit(word_b, "")[[1]]

    # A character with no known key position (symbols, other scripts) has
    # no keyboard distance, exactly like words of different lengths
    if(!all(c(characters_a, characters_b) %in% row.names(keyb))){

      # Return missing distance
      return(NA)

    }

    # Look up every character's keyboard coordinates at once
    A <- keyb[characters_a, , drop = FALSE]

    # Get coordinates of second word
    B <- keyb[characters_b, , drop = FALSE]

    # Sum of per-character, per-axis absolute coordinate differences
    distance <- sum(abs(A - B))

  }else{ # Different lengths

    # Set distance to missing
    distance <- NA

  }

  # Return distance
  return(distance)

}

#' @noRd
# Qwerty distance errors ----
# Up-front validation for `qwerty_distance()`
# Updated 14.10.2026
qwerty_distance_errors <- function(
    word_a, # first word
    word_b # second word
)
{

  # Check first word type
  typeof_error(word_a, "character", "qwerty_distance")

  # Check second word type
  typeof_error(word_b, "character", "qwerty_distance")

  # Check first word length
  length_error(word_a, 1, "qwerty_distance")

  # Check second word length
  length_error(word_b, 1, "qwerty_distance")

  # Check first word for missing
  na_error(word_a, "qwerty_distance")

  # Check second word for missing
  na_error(word_b, "qwerty_distance")

}
