#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Generic, cross-cutting helpers used by more than one exported function ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' @noRd
# Get object type ----
# Identify the practical "shape" of an object for validation messages (the same validation
# helpers as {SemNeT}, so both packages' `<name>_errors()` functions read alike)
# Updated 14.10.2026
get_object_type <- function(
    object # object to describe
)
{

  # Check for matrix
  if(is.matrix(object)){

    # Return matrix
    return("matrix")

  }

  # Check for data frame
  if(is.data.frame(object)){

    # Return data frame
    return("data.frame")

  }

  # Check for array
  if(is.array(object)){

    # Return array
    return("array")

  }

  # Check for factor
  if(is.factor(object)){

    # Return factor
    return("factor")

  }

  # Check for list
  if(is.list(object)){

    # Return list
    return("list")

  }

  # Check for vector
  if(is.vector(object)){

    # Return vector
    return("vector")

  }

  # Return type of object
  return(typeof(object))

}

#' @noRd
# Error for object type ----
# Stop with a clear message when an object is not one of the expected "shapes"
# Updated 14.10.2026
object_error <- function(
    input, # object to check
    expected_type, # expected types
    function_name # name of calling function
)
{

  # Get object type
  input_type <- get_object_type(input)

  # Check for unexpected type
  if(!input_type %in% expected_type){

    # Send error
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(input)),
        "` is a '", input_type, "' object.\n",
        "Expected: ", paste0("'", expected_type, "'", collapse = " or "), "."
      ),
      call. = FALSE
    )

  }

}

#' @noRd
# Error for `typeof` ----
# Stop with a clear message when an object's `typeof()` does not match expectations
# Updated 14.10.2026
typeof_error <- function(
    input, # object to check
    expected_type, # expected types
    function_name # name of calling function
)
{

  # Get type of input
  input_type <- typeof(input)

  # Collapse R's "integer"/"double" distinction down to "numeric" for both sides
  input_type <- swiftelse(input_type %in% c("integer", "double"), "numeric", input_type)

  # Collapse expected integer and double types to numeric
  expected_type <- swiftelse(expected_type %in% c("integer", "double"), "numeric", expected_type)

  # Collapse R's internal "closure" type down to the more familiar "function"
  input_type <- swiftelse(input_type == "closure", "function", input_type)

  # Collapse expected closure type to function
  expected_type <- swiftelse(expected_type == "closure", "function", expected_type)

  # Check for unexpected type
  if(!input_type %in% expected_type){

    # Send error
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(input)),
        "` is of type '", input_type, "'.\n",
        "Expected: ", paste0("'", expected_type, "'", collapse = " or "), "."
      ),
      call. = FALSE
    )

  }

}

#' @noRd
# Error for `length` ----
# Stop with a clear message when an object's length is not one of the expected lengths
# Updated 14.10.2026
length_error <- function(
    input, # object to check
    expected_lengths, # expected lengths
    function_name # name of calling function
)
{

  # Check for unexpected length
  if(!length(input) %in% expected_lengths){

    # Send error
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(input)),
        "` has length ", length(input), ".\n",
        "Expected length: ", paste0(expected_lengths, collapse = " or "), "."
      ),
      call. = FALSE
    )

  }

}

#' @noRd
# Error for `range` ----
# Stop with a clear message when an object's values fall outside an expected range (an
# input with no non-missing values is rejected explicitly, because `min()` and `max()` of
# nothing return `Inf` and `-Inf` and would pass both checks)
# Updated 14.10.2026
range_error <- function(
    input, # object to check
    expected_range, # expected minimum and maximum
    function_name # name of calling function
)
{

  # Check for no non-missing values
  if(length(input) == 0 || all(is.na(input))){

    # Send error
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(input)),
        "` has no non-missing values to check against the expected range."
      ),
      call. = FALSE
    )

  }

  # Get expected minimum
  expected_minimum <- min(expected_range, na.rm = TRUE)

  # Get expected maximum
  expected_maximum <- max(expected_range, na.rm = TRUE)

  # Get observed minimum
  observed_minimum <- min(input, na.rm = TRUE)

  # Get observed maximum
  observed_maximum <- max(input, na.rm = TRUE)

  # Check for values below the minimum
  if(observed_minimum < expected_minimum){

    # Send error
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(input)),
        "` has a minimum value of ", observed_minimum, ".\n",
        "Expected values between ", expected_minimum, " and ", expected_maximum, "."
      ),
      call. = FALSE
    )

  }

  # Check for values above the maximum
  if(observed_maximum > expected_maximum){

    # Send error
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(input)),
        "` has a maximum value of ", observed_maximum, ".\n",
        "Expected values between ", expected_minimum, " and ", expected_maximum, "."
      ),
      call. = FALSE
    )

  }

}

#' @noRd
# Error for missing values ----
# Stop with a clear message when an object contains `NA` (`typeof_error()` accepts
# `NA_character_`, whose type is still "character", and word-level functions would then
# fail deep inside or return `character(0)`)
# Updated 14.10.2026
na_error <- function(
    input, # object to check
    function_name # name of calling function
)
{

  # Check for missing values
  if(anyNA(input)){

    # Send error
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(input)),
        "` contains a missing (`NA`) value, which is not a valid word."
      ),
      call. = FALSE
    )

  }

}

#' @noRd
# Swift ifelse ----
# Fast, type-stable alternative to `ifelse()` -- see `swiftelse_scalar()` below
# for the length-1 fast path. Falls back to `ifelse()` itself whenever
# 'condition' contains `NA`, since direct index assignment can't propagate
# `NA` the way `ifelse()` does
# Updated 14.10.2026
swiftelse <- function(
    condition, # logical condition
    true, # value(s) when TRUE
    false # value(s) when FALSE
)
{

  # Get length of condition
  condition_length <- length(condition)

  # Check for single condition
  if(condition_length == 1){

    # Use scalar version
    return(swiftelse_scalar(condition, true, false))

  }

  # Check for missing values
  if(anyNA(condition)){

    # Use standard ifelse
    return(ifelse(condition, true, false))

  }

  # The fast path assumes `true` and `false` have length 1 or the length of `condition`;
  # otherwise fall back to `ifelse()` for its recycling
  if(!(length(true) %in% c(1, condition_length)) || !(length(false) %in% c(1, condition_length))){

    # Use standard ifelse
    return(ifelse(condition, true, false))

  }

  # Initialize result
  result <- vector(typeof(false), condition_length)

  # Check for single true value
  if(length(true) == 1){

    # Fill true values
    result[condition] <- true

  }else{ # Vector of true values

    # Fill true values
    result[condition] <- true[condition]

  }

  # Get opposite condition
  not_condition <- !condition

  # Check for single false value
  if(length(false) == 1){

    # Fill false values
    result[not_condition] <- false

  }else{ # Vector of false values

    # Fill false values
    result[not_condition] <- false[not_condition]

  }

  # Return result
  return(result)

}

#' @noRd
# Swift ifelse for a single condition ----
# The scalar branch used by `swiftelse()`
# Updated 14.10.2026
swiftelse_scalar <- function(
    condition, # single logical condition
    true, # value when TRUE
    false # value when FALSE
)
{

  # Check for true condition
  if(isTRUE(condition)){

    # Return true value
    return(true)

  }

  # Return false value
  return(false)

}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### INJECTABLE CONSOLE I/O ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' @noRd
# Read input ----
# Every interactive function reads user input through this function instead of calling
# `readline()` directly, so the whole console wizard can be driven by a scripted `reader`
# in tests (see `scripted_reader()` in tests/testthat/helper-scripted-reader.R)
# Updated 14.10.2026
read_input <- function(
    prompt = "", # text shown to the user
    reader = readline # function used to read answers
)
{

  # Read input
  return(reader(prompt))

}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### CONSOLE MENU HELPERS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' @noRd
# Appropriate answer ----
# Checks that an answer given to a console menu is appropriate (a menu number or a letter of
# the options shown) and asks again until it is; with `exit = TRUE` (the manual spell-check
# menus), "X" or a blank answer at the retry prompt returns 0
# Updated 14.10.2026
appropriate_answer <- function(
    answer, # answer given by the user
    choices, # menu options
    default, # number of default options
    dictionary = FALSE, # menu has no suggestions
    exit = FALSE, # allow exit at the retry prompt
    reader = readline # function used to read answers
)
{

  # Surrounding whitespace is ignored (a pasted or autocompleted " 1" or
  # "q " used to be rejected as an inappropriate selection)
  answer <- trimws(answer)

  # Initialize returned answer
  ret <- NA

  # Get default options
  defaults <- paste(seq_len(default))

  # Check for menu with suggestions
  if(!dictionary){

    # Get suggestions
    non_default_choices <- choices[-seq_len(default)]

    # Get letters for suggestions plus back and help
    qwert <- c(
      c("Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P")[seq_len(length(non_default_choices))],
      "B", "H"
    )

    # Combine all appropriate answers
    app_ans <- c(defaults, qwert)

    # Name by position
    names(app_ans) <- seq_along(app_ans)

  }else{ # Menu without suggestions

    # Use default options only
    app_ans <- defaults

    # Name by position
    names(app_ans) <- seq_len(length(defaults))

  }

  # Loop until the answer is appropriate
  while(is.na(ret)){

    # Check for appropriate answer
    if(toupper(answer) %in% app_ans){

      # Store answer
      ret <- toupper(answer)

    }else if(exit && (tolower(answer) == "x" || answer == "")){ # Exit requested

      # the menu advertises "X to EXIT" at every prompt, including this one
      # (callers check the FIRST answer themselves); signalled with 0
      return(0)

    }else{ # Inappropriate answer

      # Let user know
      message("Inappropriate selection.")

      # Read new answer
      answer <- trimws(read_input("Please enter a new selection: ", reader = reader))

    }

  }

  # Convert answer to its position
  ret <- as.numeric(names(which(ret == app_ans)))

  # Return position
  return(ret)

}

#' @noRd
# Custom menu ----
# Custom menu based on base R's `menu()`, laid out in a grid. It only formats and prints;
# callers pair it with `appropriate_answer()` and `read_input()` to capture the selection
# Updated 14.10.2026
custom_menu <- function(
    choices, # menu options
    title = NULL, # text above the menu
    default, # number of default options
    dictionary = FALSE, # menu has no suggestions
    help = FALSE # return suggestions instead of printing
)
{

  # There is no `interactive()` check because this function never reads input (the
  # `readline()` call happens in `appropriate_answer()`), which also lets it run
  # non-interactively in tests
  nc <- length(choices)

  # Check for title
  if(length(title) && nzchar(title[1L])){

    # Print title
    cat(title[1L], "\n")

  }

  # Check for menu with suggestions
  if(!dictionary){

    # Get suggestions
    non_default_choices <- choices[-seq_len(default)]

    # Get letters for suggestions
    qwert <- c("Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P")[seq_len(length(non_default_choices))]

    # Label options
    op <- paste0(format(c(seq_len(default), qwert)), ": ", choices)

  }else{ # Menu without suggestions

    # Label options
    op <- paste0(format(seq_len(default)), ": ", choices)

  }

  # Check for suggestions
  if(nc > default){

    # Format options
    fop <- format(op)

    # Get width of an option
    nw <- nchar(fop[1L], "w") + 2L

    # At least one column: a console too narrow for two columns (or long choice text) would
    # otherwise skip building `word`, `string`, and `def`, or fail in `rep.int()`
    ncol <- max(1L, getOption("width") %/% nw)

    # Check for word and string options
    if(default == 10 || default == 9){

      # Get word options
      word <- fop[1:5]

      # Lay out word options
      word <- paste0(word, c(rep.int("  ", 4), "\n"), collapse = "")

      # Get width of word options
      nw <- nchar(word)

      # Check for line ending
      if(substr(word, nw - 1, nw) != "\n"){

        # Add line ending
        word <- paste(substr(word, 1, nw - 2), "\n")

      }

      # Get string options
      string <- fop[6:default]

      # Get number of string options
      opts <- ifelse(default == 9, 3, 4)

      # Lay out string options
      string <- paste0(string, c(rep.int("  ", opts), "\n"), collapse = "")

      # Get width of string options
      ns <- nchar(string)

      # Check for line ending
      if(substr(string, ns - 1, ns) != "\n"){

        # Add line ending
        string <- paste(substr(string, 1, ns - 2), "\n")

      }

    }else{ # Single-response options

      # Get default options
      def <- fop[seq_len(default)]

      # (`min(default, ncol)`, not `min(nc, ncol)`: with a console of 132+
      # columns and any suggestion, a pattern longer than `def` recycled
      # `def[1]` into a second copy of "1: SKIP" at the end of the row)
      def <- paste0(def, c(rep.int("  ", min(default, ncol) - 1L), "\n"), collapse = "")

      # Get width of options
      n <- nchar(def)

      # Check for line ending
      if(substr(def, n - 1, n) != "\n"){

        # Add line ending
        def <- paste(substr(def, 1, n - 2), "\n")

      }

    }

    # 'fop' always has length > default here (guaranteed by the enclosing
    # `if(nc > default)`), so `(default + 1):length(fop)` can never reverse
    # Get suggestions
    resp <- fop[(default + 1):length(fop)]

    # Start suggestions on a new line
    resp[1] <- paste0("\n", resp[1], collapse = "")

    # Get number of suggestions
    resp_len <- length(resp)

    # Check whether suggestions exceed one row
    if(resp_len > ncol){

      # Lay out suggestions
      resp <- paste0(resp, c(rep.int("  ", min(nc, ncol) - 1L), "\n"), collapse = "")

      # Check for incomplete last row
      if((resp_len / ncol) %% 1 != 0){

        # End last row
        resp <- paste0(paste0(resp, collapse = ""), "\n")

      }

    }else{ # Suggestions fit one row

      # Lay out suggestions
      resp <- paste0(paste0(resp, collapse = ""), "\n")

    }

    # Check for word and string options
    if(default == 10 || default == 9){

      # Combine all options
      op <- paste0(
        style_text("Word options\n", defaults = "underline"), word,
        style_text("\nString options\n", defaults = "underline"), string,
        style_text("\nResponse options", defaults = "underline"), resp,
        collapse = ""
      )

    }else if(default == 5){ # Single-response options

      # Combine all options
      op <- paste0(
        style_text("Word options\n", defaults = "underline"), def,
        style_text("\nResponse options", defaults = "underline"), resp,
        collapse = ""
      )

    }else{ # Unknown menu

      # Send error
      stop("Error in custom_menu")

    }

  }else{ # No suggestions

    # Add blank line
    op <- c(op, "")

  }

  # Check whether to print menu
  if(!help){

    # Print menu
    cat("", op, sep = "\n")

  }else if(nc > default){ # Help requested

    # `resp` only exists when the `nc > default` branch above ran, so a menu without
    # suggestions returns blank for help
    paste0("", resp, sep = "")

  }else{ # No suggestions

    # Return blank
    ""

  }

}

#' @noRd
# Yes no menu ----
# Simple yes/no console prompt that asks again until the answer is recognized (yes/y or no/n)
# Updated 14.10.2026
yes_no_menu <- function(
    title = NULL, # question shown to the user
    reader = readline # function used to read answers
)
{

  # Convert answer to 1 (yes) or 2 (no)
  yes_no <- function(ans)
  {

    # Check for numeric answer
    if(is.numeric(ans)){

      # Return missing
      return(NA)

    }else if(is.character(ans)){ # Character answer

      # Clean answer
      ans <- tolower(trimws(ans))

      # Check for unrecognized answer
      if(ans != "y" && ans != "yes" && ans != "n" && ans != "no"){

        # Return missing
        return(NA)

      }else{ # Recognized answer

        # Return yes (1) or no (2)
        return(switch(ans, "y" = 1, "yes" = 1, "n" = 2, "no" = 2))

      }

    }else{ # Other answer

      # Return missing
      return(NA)

    }

  }

  # Add choices to the prompt
  title <- paste(title, "[Y/n]? ")

  # Read answer
  ans <- read_input(title, reader = reader)

  # Repeat until the answer is recognized
  while(is.na(yes_no(ans))){

    # Read new answer
    ans <- read_input("Inappropriate response. Try again [Y/n]. ", reader = reader)

  }

  # Return yes (1) or no (2)
  return(yes_no(ans))

}

#' @noRd
# Check for an installed package ----
# Updated 14.10.2026
package_installed <- function(
    package # name of the package
)
{

  # Check whether the package can be loaded
  return(requireNamespace(package, quietly = TRUE))

}

# Check for {tcltk} ----
# Whether `tcltk` can be loaded and used here (`requireNamespace()` returns `FALSE` when
# the package is absent or fails to load, e.g., no X11 on macOS); its own function so that
# tests can replace it
# Updated 14.10.2026
tcltk_available <- function()
{

  # Check for loadable tcltk
  return(requireNamespace("tcltk", quietly = TRUE) && isTRUE(unname(capabilities("tcltk"))))

}

#' @noRd
# Segment entries ----
# Splits `words` into the longest dictionary entries (up to `max_len` words)
# from left to right: c("sea", "slug", "dog") is c("sea slug", "dog") when
# "sea slug" is an entry. NULL when some word is not covered by an entry, so
# a string with an unresolved word is left alone by the callers.
# Updated 14.10.2026
segment_entries <- function(
    words, # words of a string
    full_dictionary, # dictionary used to check entries
    max_len = 4 # maximum words in an entry
)
{

  # Get number of words
  n_words <- length(words)

  # Initialize segments
  segments <- character()

  # Initialize word position
  position <- 1

  # Loop over words
  while(position <= n_words){

    # Initialize number of words taken
    take <- 0

    # Loop from longest to shortest candidate
    for(len in seq(min(max_len, n_words - position + 1), 1, by = -1)){

      # Check whether candidate is an entry
      if(dictionary_contains(paste(words[position:(position + len - 1)], collapse = " "), full_dictionary)){

        # Take the entry
        take <- len

        # Stop at the longest entry
        break

      }

    }

    # Check for uncovered word
    if(take == 0){

      # Return nothing
      return(NULL)

    }

    # Add entry
    segments <- c(segments, paste(words[position:(position + take - 1)], collapse = " "))

    # Move to next word
    position <- position + take

  }

  # Return segments
  return(segments)

}

#' @noRd
# Split valid strings ----
# `keepStrings = TRUE` (fluency): a multi-word string whose words are ALL
# dictionary words (after any correction) is split into them, unless the
# whole string is itself a dictionary entry ("sea slug" stays "sea slug";
# "marmoset otter" becomes "marmoset", "otter"; "sea slug dog" becomes "sea
# slug", "dog"). A string with a word that is not in the dictionary stays
# whole. `to` is the list of corrected responses.
# Updated 14.10.2026
split_valid_strings <- function(
    to, # list of corrected responses
    full_dictionary # dictionary used to check words
)
{

  # Loop over responses
  return(lapply(to, function(elements){

    # Check for nothing to split
    if(length(elements) == 0 || !is.character(elements)){

      # Return elements as is
      return(elements)

    }

    # Loop over elements
    unlist(lapply(elements, function(string){

      # Check for strings to keep whole
      if(is.na(string) || !grepl(" ", string, fixed = TRUE) || dictionary_contains(string, full_dictionary)){

        # Return string
        return(string)

      }

      # Split string into words
      words <- strsplit(trimws(string), "[[:space:]]+")[[1]]

      # Remove blank words
      words <- words[nzchar(words)]

      # Split into the longest dictionary entries
      segments <- segment_entries(words, full_dictionary)

      # Check for a word that is not covered
      if(is.null(segments)){

        # Return string
        return(string)

      }

      # Return segments
      return(segments)

    }))

  }))

}

#' @noRd
# Phrase covered ----
# Which words of a response sit inside a run of 2 to `max_len` neighbouring
# words that is a dictionary entry ("sting" and "ray" in "sting ray"). The
# manual word-by-word stage does not ask about them: "sting" on its own is
# not a dictionary word, but the entry it is part of is.
# Updated 14.10.2026
phrase_covered <- function(
    words, # words of a response
    full_dictionary, # dictionary used to check phrases
    max_len = 4 # maximum words in an entry
)
{

  # Get number of words
  n_words <- length(words)

  # Initialize covered words
  covered <- rep(FALSE, n_words)

  # Check for fewer than two words
  if(n_words < 2){

    # Return no covered words
    return(covered)

  }

  # Loop from longest to shortest phrase
  for(len in seq(min(n_words, max_len), 2, by = -1)){

    # Loop over starting positions
    for(start in seq_len(n_words - len + 1)){

      # Check for an uncovered dictionary entry
      if(!all(covered[start:(start + len - 1)]) &&
         dictionary_contains(paste(words[start:(start + len - 1)], collapse = " "), full_dictionary)){

        # Mark words as covered
        covered[start:(start + len - 1)] <- TRUE

      }

    }

  }

  # Return covered words
  return(covered)

}

#' @noRd
# Drop repeated entry words ----
# After the words of a response are corrected one by one, a word whose best guess is a
# multi-word dictionary entry ("polar" to "polar bear") can sit next to a word the entry
# already contains ("polar bear bear"). Words that repeat the rest, or only the end, of an
# expanded entry are dropped, and so is an identical entry twice in a row
# Updated 14.10.2026
drop_repeated_entry_words <- function(
    words # words of a corrected response
)
{

  # Convert to character
  words <- as.character(words)

  # Initialize word position
  i <- 1

  # Loop over words
  while(i <= length(words)){

    # Split word into its parts
    parts <- if(is.na(words[i])) character(0) else strsplit(words[i], " ", fixed = TRUE)[[1]]

    # Get number of parts
    k <- length(parts)

    # The words after an expanded entry that repeat the END of that entry:
    # all of its remaining words ("polar bear" + "bear") or only the last few,
    # when an earlier part of the entry came from merging with the word before
    # ("english cocker spaniel" + "spaniel" -- "cocker" had been expanded to
    # "cocker spaniel" before it was merged with "english"). Longest first.
    if(k > 1){

      # the same multi-word entry twice in a row ("tyrannosaurus" and "rex"
      # were each guessed as "tyrannosaurus rex")
      while(i < length(words) && !is.na(words[i + 1]) && words[i + 1] == words[i]){

        # Drop the repeat
        words <- words[-(i + 1)]

      }

      # Loop from longest to shortest repeat
      for(len in rev(seq_len(k - 1))){

        # Check for enough following words
        if(i + len <= length(words)){

          # Get following words
          following <- words[(i + 1):(i + len)]

          # Check whether they repeat the end of the entry
          if(!anyNA(following) && all(following == parts[(k - len + 1):k])){

            # Drop the repeat
            words <- words[-((i + 1):(i + len))]

            # Stop at the longest repeat
            break

          }

        }

      }

    }

    # Move to next word
    i <- i + 1

  }

  # Return words
  return(words)

}

#' @noRd
# Is interactive ----
# `add.path = "choose"` opens a directory chooser (`tcltk`) once, up front; a real path is
# returned unchanged and anything that cannot work stops with a message saying what to do
# instead. Its own function so that tests can replace `interactive()`
# Updated 14.10.2026
is_interactive <- function()
{

  # Check for interactive session
  return(interactive())

}

#' @noRd
# Resolve add path ----
# Updated 14.10.2026
resolve_add_path <- function(
    add_path # path to dictionaries or "choose"
)
{

  # Check for a path
  if(!identical(add_path, "choose")){

    # Return path
    return(add_path)

  }

  # Check for interactive session
  if(!is_interactive()){

    # Send error
    stop(
      "`add.path = \"choose\"` opens a directory chooser and needs an interactive R session. ",
      "Give the folder's path in `add.path` instead.",
      call. = FALSE
    )

  }

  # Check for tcltk
  if(!tcltk_available()){

    # Send error
    stop(
      "A directory chooser is not available on this system ('tcltk' could not be loaded). ",
      "Give the folder's path in `add.path` instead.",
      call. = FALSE
    )

  }

  # Open directory chooser
  chosen <- tcltk::tk_choose.dir(caption = "Choose the folder with your dictionaries")

  # Check for chosen folder
  if(length(chosen) != 1 || is.na(chosen) || !nzchar(chosen)){

    # Send error
    stop("No folder was chosen. Give the folder's path in `add.path`, or leave it out.", call. = FALSE)

  }

  # Return chosen folder
  return(chosen)

}

#' @noRd
# Pause ----
# The deliberate pauses of the console wizard (letting a message register before the next
# screen) go through here so that they can be turned off with
# `options(SemNetCleaner.pause = FALSE)`, for scripted or non-interactive use
# Updated 14.10.2026
pause <- function(
    seconds # number of seconds to wait
)
{

  # Check for pauses turned on
  if(isTRUE(getOption("SemNetCleaner.pause", TRUE))){

    # Wait
    Sys.sleep(seconds)

  }

  # Return silently
  invisible(NULL)

}

#' @noRd
# Prints diagnostic information for a GitHub issue when a wizard stage fails
# Updated 14.10.2026
#' @importFrom utils packageVersion
# Error report ----
# Updated 14.10.2026
error_report <- function(
    result, # error that occurred
    SUB_FUN, # function where the error occurred
    FUN, # function that called it
    fatal = FALSE # stop with an error after the report
)
{

  # Print where the error occurred
  message(paste("\nAn error has occurred in the '", SUB_FUN, "' function of '", FUN, "':\n", sep = ""))

  # Print error
  cat(paste(result))

  # Print where to report the error
  message("\nPlease open a new issue on GitHub (bug report): https://github.com/AlexChristensen/SemNetCleaner/issues/new/choose")

  # Get operating system
  OS <- as.character(Sys.info()["sysname"])

  # Get operating system version
  OSversion <- paste(as.character(Sys.info()[c("release", "version")]), collapse = " ")

  # Get R version
  Rversion <- paste(R.version$major, R.version$minor, sep = ".")

  # Get SemNetCleaner version
  SNCversion <- paste(unlist(packageVersion("SemNetCleaner")), collapse = ".")

  # Get SemNetDictionaries version
  SNDversion <- paste(unlist(packageVersion("SemNetDictionaries")), collapse = ".")

  # Print requested information
  message(paste("\nBe sure to provide the following information:\n"))

  # Print reproduction information
  message(style_text("To Reproduce:", defaults = "bold"))

  # Print function
  message(paste(" ", text_symbol("bullet"), " Function error occurred in: ", SUB_FUN, " function of ", FUN, sep = ""))

  # Print version information
  message(style_text("\nR, SemNetCleaner, and SemNetDictionaries versions:", defaults = "bold"))

  # Print R version
  message(paste(" ", text_symbol("bullet"), " R version: ", Rversion, sep = ""))

  # Print SemNetCleaner version
  message(paste(" ", text_symbol("bullet"), " SemNetCleaner version: ", SNCversion, sep = ""))

  # Print SemNetDictionaries version
  message(paste(" ", text_symbol("bullet"), " SemNetDictionaries version: ", SNDversion, sep = ""))

  # Print operating system information
  message(style_text("\nOperating System:", defaults = "bold"))

  # Print operating system
  message(paste(" ", text_symbol("bullet"), " OS: ", OS, sep = ""))

  # Print operating system version
  message(paste(" ", text_symbol("bullet"), " Version: ", OSversion, sep = ""))

  # A fatal report ends with a real R error carrying the first line of the cause, so that
  # scripts can tell it from success
  if(isTRUE(fatal)){

    # Get cause of error
    cause <- if(inherits(result, "try-error") && !is.null(attr(result, "condition"))){

      # Use message of the error
      conditionMessage(attr(result, "condition"))

    }else{ # Not a try-error

      # Paste result
      paste(result, collapse = " ")

    }

    # Keep first line of the cause
    cause <- trimws(sub("^Error[^:]*:", "", strsplit(cause, "\n")[[1]][1]))

    # Stop with the cause
    stop("`", FUN, "()` stopped in '", SUB_FUN, "': ", cause, call. = FALSE)

  }

}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### TEXT PRESENTATION LAYER  ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' @noRd
# Color text ----
# Colors text (8-bit ANSI color codes)
# Updated 14.10.2026
color_text <- function(
    text, # text to color
    number = NULL, # 8-bit color number
    defaults = NULL # preset color
)
{

  # Check system capabilities
  sys_check <- system_check()

  # Check for text color support
  if(sys_check$TEXT){

    # Check for valid color number
    if(is.null(number) || number < 0 || number > 231){

      # Use default color (white)
      number <- 15

    }

    # Check for preset color
    if(!is.null(defaults)){

      # Check for highlight
      if(defaults == "highlight"){

        # Check for RStudio
        if(sys_check$RSTUDIO && requireNamespace("rstudioapi", quietly = TRUE)){

          # Check for dark theme
          if(rstudioapi::getThemeInfo()$dark){

            # Use yellow
            number <- 226

          }else{ # Light theme

            # Use orange
            number <- 208

          }

        }else{ # Not RStudio

          # Use orange
          number <- 208

        }

      }else{ # Other presets

        # Look up color number
        number <- switch(defaults,
          message = 204,
          red = 9,
          orange = 208,
          yellow = 11,
          "light green" = 10,
          green = 34,
          cyan = 14,
          blue = 12,
          magenta = 13,
          pink = 211
        )

      }

    }

    # Return colored text
    return(paste("\033[38;5;", number, "m", text, "\033[0m", sep = ""))

  }else{ # No color support

    # Return plain text
    return(text)

  }

}

#' @noRd
# Style text ----
# Stylizes text (bold, italics, underline, strikethrough)
# Updated 14.10.2026
style_text <- function(
    text, # text to style
    defaults = c("bold", "italics", "highlight", "underline", "strikethrough") # style to apply
)
{

  # Check system capabilities
  sys_check <- system_check()

  # Check for text styling support
  if(sys_check$TEXT){

    # Check for missing style
    if(missing(defaults)){

      # Use no style
      number <- 0

    }else{ # Style specified

      # Look up style number
      number <- switch(defaults,
        bold = 1,
        italics = 3,
        underline = 4,
        highlight = 7,
        strikethrough = 9
      )

    }

    # Return styled text
    return(paste("\033[", number, ";m", text, "\033[0m", sep = ""))

  }else{ # No styling support

    # Return plain text
    return(text)

  }

}

#' @noRd
# Text symbol ----
# Unicode text symbols (Greek letters, checkmarks, bullets, ...). Ported
# from `textsymbol()`.
# Updated 14.10.2026
text_symbol <- function(
    symbol = c("alpha", "beta", "chi", "delta", "eta", "gamma", "lambda", "omega", "phi", "pi", "rho", "sigma", "tau", "theta", "square root", "infinity", "check mark", "x", "bullet") # symbol to return
)
{

  # \uXXXX escapes instead of literal UTF-8 glyphs (R CMD check wants ASCII-only code)
  sym <- switch(symbol,
    alpha = "\u03B1",
    beta = "\u03B2",
    chi = "\u03C7",
    delta = "\u03B4",
    eta = "\u03B7",
    gamma = "\u03B3",
    lambda = "\u03BB,",
    omega = "\u03C9",
    phi = "\u03C6",
    pi = "\u03C0",
    rho = "\u03C1",
    sigma = "\u03C3",
    tau = "\u03C4",
    theta = "\u03B8",
    "square root" = "\u221A",
    infinity = "\u221E",
    "check mark" = "\u2713",
    x = "\u2717",
    bullet = "\u2022"
  )

  # Return symbol
  return(sym)

}

#' @noRd
# Private cache for system_check() -- see below for why it's memoized
# Cache system capabilities
.system_check_cache <- new.env(parent = emptyenv())

#' @noRd
# System check ----
# Checks whether ANSI text styling is safe for the current OS and RStudio combination;
# memoized because neither changes during a session
# Updated 14.10.2026
system_check <- function(
    ... # not used
)
{

  # Check for cached value
  if(is.null(.system_check_cache$value)){

    # Get operating system
    OS <- unname(tolower(Sys.info()["sysname"]))

    # Check for RStudio
    RSTUDIO <- ifelse(Sys.getenv("RSTUDIO") == "1", TRUE, FALSE)

    # Assume text styling works
    TEXT <- TRUE

    # Check for outside RStudio
    if(!RSTUDIO){

      # Check for non-Linux system
      if(OS != "linux"){

        # Turn off text styling
        TEXT <- FALSE

      }

    }

    # Initialize results
    res <- list()

    # Store operating system
    res$OS <- OS

    # Store RStudio flag
    res$RSTUDIO <- RSTUDIO

    # Store text styling flag
    res$TEXT <- TEXT

    # Store in cache
    .system_check_cache$value <- res

  }

  # Return cached value
  return(.system_check_cache$value)

}

#' @noRd
# Line break ----
# Prints a horizontal divider line
# Updated 14.10.2026
line_break <- function(
    breaks = "\
" # text printed before the line
)
{

  # Print divider line
  cat(breaks, color_text(paste(rep("-", getOption("width")), collapse = ""), defaults = "message"), "\n")

}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### RESPONSE / WORD PRIMITIVES  ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' @noRd
# Remove leading space ----
# Removes leading and trailing spaces that `trimws()` does not catch
# Updated 14.10.2026
remove_leading_space <- function(
    word # word to clean
)
{

  # Set bad responses to missing
  word <- bad_response(word)

  # Check for missing
  if(!is.na(word)){

    # Remove leading and trailing spaces
    word <- gsub("^[[:space:]]+|[[:space:]]+$", "", word)

  }

  # Return word
  return(word)

}

#' @noRd
# Irregular singular and plural pairs shared by `pluralize()` and `singularize()`
# (singularize.R reverses this table and adds its own extra entries)
#
# Invariant plurals (the singular and plural are the same: deer, sheep, fish, ...) are
# identity mappings, because the "+s" form of several of them ("deers", "elks", "trouts")
# passes a dictionary or spell-check without being the idiomatic plural. The table also
# holds words that the suffix rules get wrong (bus to buses, walrus to walruses, octopus to
# octopi)
# Updated 14.10.2026
irregular_plurals <- c(
  # Irregular plurals
  child = "children", goose = "geese", man = "men", woman = "women",
  tooth = "teeth", foot = "feet", mouse = "mice", person = "people",
  louse = "lice", ox = "oxen",

  # Invariant plurals
  deer = "deer", moose = "moose", sheep = "sheep", swine = "swine",
  salmon = "salmon", trout = "trout", elk = "elk", bison = "bison",
  fish = "fish", shrimp = "shrimp", buffalo = "buffalo", species = "species",

  # Words the regular rules mishandle
  bus = "buses", walrus = "walruses", matrix = "matrices", vertex = "vertices",
  iris = "irises", hippopotamus = "hippopotamuses", platypus = "platypuses",
  octopus = "octopi",

  # "genii", the result of the suffix rule, is a recognized word but an archaic one; the
  # modern plural is "geniuses"
  genius = "geniuses"
)

#' @noRd
# Cache of dictionary hashes for the dictionary membership checks, keyed by `identical()`
# against a small (4 entry) most-recently-used list; `identical()` is cheap compared with
# the cost of building a hash
# Updated 14.10.2026
# Cache of dictionary hashes
.dictionary_hash_cache <- new.env(parent = emptyenv())
# Initialize cached entries
.dictionary_hash_cache$entries <- list()

# Get dictionary hash ----
# Updated 14.10.2026
get_dictionary_hash <- function(
    dictionary # vector of dictionary words
)
{

  # Get cached entries
  entries <- .dictionary_hash_cache$entries

  # Loop over cached dictionaries
  for(entry in entries){

    # Check for identical dictionary
    if(identical(entry$dictionary, dictionary)){

      # Return cached hash
      return(entry$hash)

    }

  }

  # NA and empty entries are dropped before hashing (`assign()` would turn an `NA` into a
  # variable named "NA", and zero-length names are rejected); the cache key stays the
  # unfiltered dictionary. `list2env()` builds the hash in one vectorized call, which matters
  # because every word added in the wizard creates a new dictionary vector
  non_na_dictionary <- dictionary[!is.na(dictionary) & nzchar(dictionary)]

  # Build hash environment
  hash_env <- list2env(
    structure(rep(list(TRUE), length(non_na_dictionary)), names = non_na_dictionary),
    envir = new.env(hash = TRUE, parent = emptyenv(), size = length(non_na_dictionary))
  )

  # Keep only the 4 most-recently-used dictionaries
  entries <- c(list(list(dictionary = dictionary, hash = hash_env)), entries)

  # Store most recent entries
  .dictionary_hash_cache$entries <- entries[seq_len(min(4, length(entries)))]

  # Return hash
  return(hash_env)

}

#' @noRd
#' @importFrom stats median sd setNames
# Memoized multi-word response threshold (`multi_min`) for the multi-word stage of
# `auto_spellcheck()`; measuring the word count of every dictionary entry is slow for large
# dictionaries. Same cache pattern as `get_dictionary_hash()`
# Updated 14.10.2026
.multi_min_cache <- new.env(parent = emptyenv())
# Initialize cached entries
.multi_min_cache$entries <- list()

# Get multi min ----
# Updated 14.10.2026
get_multi_min <- function(
    full_dictionary # dictionary used to check words
)
{

  # Get cached entries
  entries <- .multi_min_cache$entries

  # Loop over cached dictionaries
  for(entry in entries){

    # Check for identical dictionary
    if(identical(entry$dictionary, full_dictionary)){

      # Return cached threshold
      return(entry$multi_min)

    }

  }

  # `lengths(strsplit())` is the same word count as the former per-entry
  # `lapply()` (~30x faster: 1.8 s -> 0.06 s on the 370k-word "general"
  # dictionary)
  dict_lens <- lengths(strsplit(full_dictionary, " ", fixed = TRUE))

  # Compute threshold (median plus two standard deviations)
  multi_min <- ceiling(median(dict_lens) + 2 * sd(dict_lens))

  # Require at least two words
  if(multi_min == 1){

    multi_min <- 2

  }

  # Cache threshold
  entries <- c(list(list(dictionary = full_dictionary, multi_min = multi_min)), entries)

  # Store most recent entries
  .multi_min_cache$entries <- entries[seq_len(min(4, length(entries)))]

  # Return threshold
  return(multi_min)

}

#' @noRd
# Check dictionary membership ----
# Vectorized membership check, `dictionary_contains(words, dictionary)` in place of
# `words %in% dictionary`. Dictionaries below 3,000 words use `%in%`, which is faster
# there; larger ones use the memoized hash
# Updated 14.10.2026
dictionary_contains <- function(
    words, # words to check
    dictionary # vector of dictionary words
)
{

  # Check for small dictionary
  if(length(dictionary) < 3000){

    # Use standard matching
    return(words %in% dictionary)

  }

  # Get hash of dictionary
  hash_env <- get_dictionary_hash(dictionary)

  # Check each word in the hash
  return(vapply(words, function(word){

    # `exists()` errors on anything that is not a single string, whereas `%in%` reports no
    # match; non-character, missing, and empty words are not in the dictionary
    if(!is.character(word) || is.na(word) || word == ""){

      # Return not found
      return(FALSE)

    }

    # Check whether the word is in the hash
    exists(word, envir = hash_env, inherits = FALSE)

  }, logical(1), USE.NAMES = FALSE))

}

#' @noRd
# Check COCA dictionary membership ----
# Thin, coca-specific convenience wrapper around `dictionary_contains()` --
# kept so `pluralize()`/`singularize()`'s existing call sites don't need to
# repeat `SemNetDictionaries::coca.dictionary` at every one.
# Updated 14.10.2026
coca_dictionary_contains <- function(
    word # words to check
)
{

  # Check words against COCA dictionary
  return(dictionary_contains(word, SemNetDictionaries::coca.dictionary))

}

#' @noRd
# Memoized verdict of `word_is_recognized()` (COCA dictionary and hunspell combined) per
# word. `best_guess()` checks many rare words, which fall through to the slow `hunspell`
# check, and the verdict never changes during a session
# Updated 14.10.2026
.word_recognition_cache <- new.env(hash = TRUE, parent = emptyenv())

# Word is recognized ----
# Updated 14.10.2026
word_is_recognized <- function(
    word # word to check
)
{

  # Check for missing or blank word
  if(is.na(word) || word == ""){

    # Return not recognized
    return(FALSE)

  }

  # Get cached verdict
  cached <- .word_recognition_cache[[word]]

  # Check for cached verdict
  if(!is.null(cached)){

    # Return cached verdict
    return(cached)

  }

  # `hunspell` is a hard `Imports` dependency, so no `requireNamespace()` guard is needed
  result <- coca_dictionary_contains(word)

  # Check for failed COCA lookup
  if(!result){

    # Check with spell-checker
    result <- isTRUE(hunspell::hunspell_check(word))

  }

  # Cache verdict
  .word_recognition_cache[[word]] <- result

  # Return verdict
  return(result)

}

#' @noRd
# Memoized per-word Metaphone code, used by `best_guess()` as a second signal next to the
# QWERTY distance. `tryCatch()` keeps an unexpected `metaphone()` failure from crashing
# `best_guess()` (there is then no phonetic signal)
.phonetic_code_cache <- new.env(hash = TRUE, parent = emptyenv())

# Get phonetic code ----
# Updated 14.10.2026
get_phonetic_code <- function(
    word # word to encode
)
{

  # Check for missing or blank word
  if(is.na(word) || word == ""){

    # Return missing code
    return(NA_character_)

  }

  # Get cached code
  cached <- .phonetic_code_cache[[word]]

  # Check for cached code
  if(!is.null(cached)){

    # Return cached code
    return(cached)

  }

  # Compute phonetic code
  code <- tryCatch(phonics::metaphone(word), error = function(e){ NA_character_ })

  # Cache code
  .phonetic_code_cache[[word]] <- code

  # Return code
  return(code)

}

#' @noRd
# Phonetic match ----
# Whether two words share the same Metaphone code; `FALSE` (never `NA`) after a failed
# lookup, so it composes inside `vapply(..., logical(1))`
# Updated 14.10.2026
phonetic_match <- function(
    word_a, # first word
    word_b # second word
)
{

  # Get phonetic code of first word
  code_a <- get_phonetic_code(word_a)

  # Get phonetic code of second word
  code_b <- get_phonetic_code(word_b)

  # Check for missing code
  if(is.na(code_a) || is.na(code_b)){

    # Return no match
    return(FALSE)

  }

  # Check for identical codes
  return(identical(code_a, code_b))

}

#' @noRd
# Memoized `SemNetDictionaries::load.monikers(dictionary)`, called once per response with
# the same `dictionary` by `multiple_words()`, `ind_word_check()`, and `best_guess()`;
# cached with a small most-recently-used list like `get_dictionary_hash()`
# Updated 14.10.2026
.moniker_cache <- new.env(parent = emptyenv())
# Initialize cached entries
.moniker_cache$entries <- list()

# Get monikers ----
# Updated 14.10.2026
get_monikers <- function(
    dictionary # names of dictionaries
)
{

  # Get cached entries
  entries <- .moniker_cache$entries

  # Loop over cached dictionaries
  for(entry in entries){

    # Check for identical dictionary
    if(identical(entry$dictionary, dictionary)){

      # Return cached monikers
      return(entry$monikers)

    }

  }

  # Load monikers
  monikers <- SemNetDictionaries::load.monikers(dictionary)

  # Cache monikers
  entries <- c(list(list(dictionary = dictionary, monikers = monikers)), entries)

  # Store most recent entries
  .moniker_cache$entries <- entries[seq_len(min(4, length(entries)))]

  # Return monikers
  return(monikers)

}

#' @noRd
# Memoized `SemNetDictionaries::dictionaries(TRUE)` (it reads a file on every call); the
# available dictionaries cannot change during a session, so one cached value is enough
# Updated 14.10.2026
.available_dictionaries_cache <- new.env(parent = emptyenv())

# Get available dictionaries ----
# Updated 14.10.2026
get_available_dictionaries <- function()
{

  # Check for cached value
  if(is.null(.available_dictionaries_cache$value)){

    # Load available dictionaries
    .available_dictionaries_cache$value <- SemNetDictionaries::dictionaries(TRUE)

  }

  # Return available dictionaries
  return(.available_dictionaries_cache$value)

}

#' @noRd
# Lazily memoized "aa" to "zz" two-letter combinations, used by the free-only
# short-response stage of `auto_spellcheck()`
# Updated 14.10.2026
.two_letter_combinations_cache <- new.env(parent = emptyenv())

# Get two letter combinations ----
# Updated 14.10.2026
get_two_letter_combinations <- function()
{

  # Check for cached value
  if(is.null(.two_letter_combinations_cache$value)){

    # Build all two-letter combinations
    .two_letter_combinations_cache$value <- apply(expand.grid(letters, letters), 1, paste, collapse = "", sep = "")

  }

  # Return combinations
  return(.two_letter_combinations_cache$value)

}

#' @noRd
# Moniker ----
# Looks up a word in a moniker (common misspelling) list and converts between UK and US spelling
# Updated 14.10.2026
moniker <- function(
    word, # word to look up
    misnom, # list of common misspellings
    spelling # "UK" or "US" spelling
)
{

  # Flatten moniker list
  mis <- unlist(misnom)

  # Check for a moniker
  if(!is.na(match(word, mis))){

    # Get canonical form
    matched <- names(mis[match(word, mis)])

    # Lowercase: the moniker table capitalizes proper nouns ("I", "French"), but responses and
    # dictionaries are lowercase
    misnomed <- tolower(gsub("[[:digit:]]+", "", matched))

  }else{ # No moniker

    # Keep word
    misnomed <- word

  }

  # Convert spelling
  misnomed <- brit_us_conversion(vec = misnomed, spelling = spelling, dictionary = FALSE)

  # Return word
  return(misnomed)

}

#' @noRd
# Seq min max ----
# Obtains the minimum and maximum of each contiguous run in a numeric sequence (used for
# cue and ID row-splicing in `correct_data_free()`)
# Updated 14.10.2026
seq_min_max <- function(
    sequence # numeric sequence
)
{

  # An empty `sequence` is rejected (`min()` and `max()` of nothing return `Inf` and `-Inf`)
  if(length(sequence) == 0){

    # Send error
    stop("`seq_min_max()`: `sequence` must have at least one element.", call. = FALSE)

  }

  # Get differences between neighbors
  value_differences <- diff(sequence)

  # Check for one contiguous run
  if(all(value_differences == 1)){

    # Get minimum
    minimum <- min(sequence, na.rm = TRUE)

    # Get maximum
    maximum <- max(sequence, na.rm = TRUE)

  }else{ # Several runs

    # Find breaks between runs
    breaks <- which(value_differences != 1)

    # Add end of last run
    breaks <- c(breaks, length(sequence))

    # Find start of each run
    starts <- (breaks + 1) - c(breaks[1], diff(breaks))

    # Split into runs
    split_sequences <- lapply(seq_along(breaks), function(i){

      # Return run
      return(sequence[starts[i]:breaks[i]])

    })

    # Get minimum of each run
    minimum <- unlist(lapply(split_sequences, min, na.rm = TRUE))

    # Get maximum of each run
    maximum <- unlist(lapply(split_sequences, max, na.rm = TRUE))

  }

  # Initialize results
  res <- list()

  # Store minimums
  res$min <- minimum

  # Store maximums
  res$max <- maximum

  # Check for several runs
  if(exists("breaks", envir = environment())){

    # Store breaks
    res$breaks <- breaks

    # Store starts
    res$starts <- starts

    # Store lengths of runs
    res$lengths <- unlist(lapply(split_sequences, length))

  }

  # Return results
  return(res)

}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PARALLEL PROCESSING    ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' @noRd
# Parallel process ----
# Run `FUN(i, ...)` over `seq_len(iterations)`, in parallel when `cores > 1`; the previous
# `future` plan is always restored, even if `FUN` errors, and zero cores are never requested
# Updated 14.10.2026
#
# Two additions for the internal Shiny app, which runs this inside a `shiny::ExtendedTask`:
#
# - Nested-worker guard: the request is capped to `future::availableCores()`, because a
#   `multisession` worker cannot start another pool; this only degrades the nested case to
#   sequential and leaves console use unaffected
# - Progress-file channel: `progressr` cannot relay progress to a caller that does not wait
#   on the future, so the app can set `options(SemNetCleaner.progress_file = <path>)` and
#   every completed iteration is counted in that file (a count, because workers finish out
#   of order). Without the option nothing changes
parallel_process <- function(
    iterations, # number of iterations
    FUN, # function applied to each iteration
    ..., # arguments passed to the function
    cores = 1, # number of cores
    progress = TRUE # show a progress bar
)
{

  # Check iterations length
  length_error(iterations, 1, "parallel_process")

  # Check iterations type
  typeof_error(iterations, "numeric", "parallel_process")

  # Check iterations range
  range_error(iterations, c(1, Inf), "parallel_process")

  # Check cores length
  length_error(cores, 1, "parallel_process")

  # Check cores type
  typeof_error(cores, "numeric", "parallel_process")

  # Check cores range
  range_error(cores, c(1, Inf), "parallel_process")

  # Check function type
  typeof_error(FUN, "function", "parallel_process")

  # Check for a random seed in the global environment
  if(exists(".Random.seed", envir = globalenv(), inherits = FALSE)){

    # Get the user's random seed (`future.seed = TRUE` draws from it)
    user_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)

    # Restore the user's random seed on exit
    on.exit(assign(".Random.seed", user_seed, envir = globalenv()), add = TRUE)

  }else{ # No random seed

    # Remove the random seed created in this call on exit
    on.exit(
      if(exists(".Random.seed", envir = globalenv(), inherits = FALSE)){
        rm(".Random.seed", envir = globalenv())
      }, add = TRUE
    )

  }

  # Check for parallel processing
  if(cores > 1){

    # Limit cores to those available
    cores <- min(cores, future::availableCores())

  }

  # Check for parallel processing
  if(cores > 1){

    # Set up parallel workers
    old_plan <- future::plan(future::multisession, workers = cores)

  }else{ # Sequential

    # Set up sequential processing
    old_plan <- future::plan(future::sequential)

  }

  # Restore previous plan on exit
  on.exit(future::plan(old_plan), add = TRUE)

  # Get progress file (used by the Shiny app)
  progress_file <- getOption("SemNetCleaner.progress_file", default = NULL)

  # Set progress marker file
  progress_marker_file <- paste0(progress_file, ".progress")

  # Check for progress file
  if(!is.null(progress_file)){

    # Try to write progress files
    try({
      # Write total iterations
      writeLines(as.character(iterations), progress_file)
      # Reset completed iterations
      writeLines(character(0), progress_marker_file)
    }, silent = TRUE)

  }

  # Wrap function to report progress
  wrapped_FUN <- function(i, ...){

    # Run function
    result <- FUN(i, ...)

    # Check for progress file
    if(!is.null(progress_file)){

      # Record completed iteration
      try(cat("1", file = progress_marker_file, append = TRUE), silent = TRUE)

    }

    # Return result
    return(result)

  }

  # Check for no progress bar
  if(!progress){

    # Run without progress bar
    return(future.apply::future_lapply(seq_len(iterations), wrapped_FUN, ..., future.seed = TRUE))

  }

  # Run with progress bar
  return(
    progressr::with_progress({

      # Create progress reporter
      report_step <- progressr::progressor(steps = iterations)

      # Run in parallel
      future.apply::future_lapply(
        seq_len(iterations),
        function(i, ...){

          # Run function
          result <- wrapped_FUN(i, ...)

          # Report progress
          report_step()

          # Return result
          return(result)

        },
        ...,
        future.seed = TRUE
      )

    })
  )

}
