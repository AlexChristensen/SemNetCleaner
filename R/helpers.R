#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Generic, cross-cutting helpers used by more than one exported function ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' @noRd
# Identify the practical "shape" of an object for validation messages
# Ported from `../SemNeT`'s own `helpers.R` -- same validation primitives,
# reused here so both sibling packages' `<name>_errors()` subroutines read
# and error identically
# Updated 08.09.2026
get_object_type <- function(object)
{

  if(is.matrix(object)){
    return("matrix")
  }

  if(is.data.frame(object)){
    return("data.frame")
  }

  if(is.array(object)){
    return("array")
  }

  if(is.factor(object)){
    return("factor")
  }

  if(is.list(object)){
    return("list")
  }

  if(is.vector(object)){
    return("vector")
  }

  return(typeof(object))

}

#' @noRd
# Stop with a clear message when an object is not one of the expected "shapes"
# Updated 08.09.2026
object_error <- function(input, expected_type, function_name)
{

  input_type <- get_object_type(input)

  if(!input_type %in% expected_type){
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
# Stop with a clear message when an object's `typeof()` does not match expectations
# Updated 08.09.2026
typeof_error <- function(input, expected_type, function_name)
{

  input_type <- typeof(input)

  # Collapse R's "integer"/"double" distinction down to "numeric" for both sides
  input_type <- swiftelse(input_type %in% c("integer", "double"), "numeric", input_type)
  expected_type <- swiftelse(expected_type %in% c("integer", "double"), "numeric", expected_type)

  # Collapse R's internal "closure" type down to the more familiar "function"
  input_type <- swiftelse(input_type == "closure", "function", input_type)
  expected_type <- swiftelse(expected_type == "closure", "function", expected_type)

  if(!input_type %in% expected_type){
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
# Stop with a clear message when an object's length is not one of the expected lengths
# Updated 08.09.2026
length_error <- function(input, expected_lengths, function_name)
{

  if(!length(input) %in% expected_lengths){
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
# Stop with a clear message when an object's values fall outside an expected range
# Robustness fix: `min()`/`max()` on an all-`NA` or zero-length `input`
# return `Inf`/`-Inf` (with a warning, not an error) once `na.rm = TRUE`
# strips every element away. Both range checks below then spuriously pass
# (`Inf < expected_minimum` and `-Inf > expected_maximum` are both FALSE),
# silently letting genuinely invalid input (e.g. `NA_real_`, `numeric(0)`)
# through validation with nothing but a stray warning -- confirmed directly.
# Guarded with an explicit, clearly-worded stop() instead.
# Updated 10.09.2026
range_error <- function(input, expected_range, function_name)
{

  if(length(input) == 0 || all(is.na(input))){
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(input)),
        "` has no non-missing values to check against the expected range."
      ),
      call. = FALSE
    )
  }

  expected_minimum <- min(expected_range, na.rm = TRUE)
  expected_maximum <- max(expected_range, na.rm = TRUE)

  observed_minimum <- min(input, na.rm = TRUE)
  observed_maximum <- max(input, na.rm = TRUE)

  if(observed_minimum < expected_minimum){
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(input)),
        "` has a minimum value of ", observed_minimum, ".\n",
        "Expected values between ", expected_minimum, " and ", expected_maximum, "."
      ),
      call. = FALSE
    )
  }

  if(observed_maximum > expected_maximum){
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
# Stop with a clear message when an object contains `NA`. Added during a
# Phase 2 robustness review, per direct request: `typeof_error(x,
# "character", ...)` does NOT reject `NA_character_` (its `typeof()` is
# still `"character"`), so several word-level functions
# (`pluralize()`/`singularize()`/`qwerty_distance()`/`best_guess()`) had no
# actual guard against it -- confirmed each one either crashes deep inside
# with a confusing "missing value where TRUE/FALSE needed" (from an `if()`
# on an `NA`-propagated comparison) or, for `best_guess()`, silently
# returns `character(0)` as if "no correction found" rather than surfacing
# that the input itself was invalid. A single-purpose validator, matching
# the `<name>_error()` family's own convention, is clearer than repeating
# an inline `anyNA()` check at each call site.
# Updated 10.09.2026
na_error <- function(input, function_name)
{

  if(anyNA(input)){
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
# Fast, type-stable alternative to `ifelse()` -- see `swiftelse_scalar()` below
# for the length-1 fast path. Falls back to `ifelse()` itself whenever
# 'condition' contains `NA`, since direct index assignment can't propagate
# `NA` the way `ifelse()` does
# Updated 08.09.2026
swiftelse <- function(condition, true, false)
{

  condition_length <- length(condition)

  if(condition_length == 1){
    return(swiftelse_scalar(condition, true, false))
  }

  if(anyNA(condition)){
    return(ifelse(condition, true, false))
  }

  # Robustness fix: the direct-index-assignment fast path below assumes
  # `true`/`false` are each either length-1 (recycled as a scalar) or
  # already the same length as `condition` -- confirmed directly that a
  # `true`/`false` of some OTHER length silently produces wrong values
  # (out-of-range positions read back as NA) instead of `ifelse()`'s own
  # correct recycling. Falls back to `ifelse()` itself whenever that
  # assumption doesn't hold, exactly like the `anyNA(condition)` fallback
  # above.
  if(!(length(true) %in% c(1, condition_length)) || !(length(false) %in% c(1, condition_length))){
    return(ifelse(condition, true, false))
  }

  result <- vector(typeof(false), condition_length)

  if(length(true) == 1){
    result[condition] <- true
  }else{
    result[condition] <- true[condition]
  }

  not_condition <- !condition

  if(length(false) == 1){
    result[not_condition] <- false
  }else{
    result[not_condition] <- false[not_condition]
  }

  return(result)

}

#' @noRd
# The scalar branch used by `swiftelse()`
# Updated 08.09.2026
swiftelse_scalar <- function(condition, true, false)
{

  if(isTRUE(condition)){
    return(true)
  }

  return(false)

}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### INJECTABLE CONSOLE I/O ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' @noRd
# Every interactive function in this package reads user input through this
# one function instead of calling `readline()`/`menu()` directly, so the
# whole console wizard can be driven end-to-end by a scripted `reader` in
# tests instead of only manual smoke-testing (see `scripted_reader()` in
# tests/testthat/helper-scripted-reader.R). Defaults to real `readline()`.
# Updated 08.09.2026
read_input <- function(prompt = "", reader = readline)
{
  reader(prompt)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### CONSOLE MENU HELPERS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' @noRd
# Sub-routine to check if an answer provided to a console menu is appropriate
# (i.e., numeric and within response option range); re-prompts until it is
# Ported from `appropriate.answer()`. Real bug fixed: the original's
# `qwert <- c(...)[1:(length(choices[-c(1:default)]))]` reverse-ranges to
# `1:0` whenever there are zero non-default choices (`choices` has exactly
# `default` elements) -- `1:0` is `c(1, 0)`, and indexing a vector with a `0`
# silently drops that position rather than erroring, so this produced one
# bogus extra "Q" option instead of the correctly-empty QWERTY option set.
# Fixed via `seq_len()`, which is correctly empty at 0. Also dropped `first`,
# confirmed dead (assigned, never read).
# Updated 08.09.2026
appropriate_answer <- function(answer, choices, default, dictionary = FALSE, reader = readline)
{

  ret <- NA
  defaults <- paste(seq_len(default))

  if(!dictionary){

    non_default_choices <- choices[-seq_len(default)]
    qwert <- c(
      c("Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P")[seq_len(length(non_default_choices))],
      "B", "H"
    )

    app_ans <- c(defaults, qwert)
    names(app_ans) <- seq_along(app_ans)

  }else{

    app_ans <- defaults
    names(app_ans) <- seq_len(length(defaults))

  }

  while(is.na(ret)){

    if(toupper(answer) %in% app_ans){
      ret <- toupper(answer)
    }else{
      message("Inappropriate selection.")
      answer <- read_input("Please enter a new selection: ", reader = reader)
    }

  }

  ret <- as.numeric(names(which(ret == app_ans)))

  return(ret)

}

#' @noRd
# Custom menu based on Base R's `menu()`, laid out in a fixed-width 2-column
# grid. Pure formatting/printing -- it does not itself read input, callers
# pair it with `appropriate_answer()`/`read_input()` to capture a selection.
# Ported from `customMenu()`. Same reverse-range fix as `appropriate_answer()`
# above applied to its identical QWERTY-option construction.
# Updated 08.09.2026
custom_menu <- function(choices, title = NULL, default, dictionary = FALSE, help = FALSE)
{

  # The original guarded this with `if(!interactive()) stop(...)`, copied
  # from base `menu()`'s own check. Dropped: unlike base `menu()`, this
  # function never itself reads input (it only formats/prints/returns
  # text -- the actual readline() call happens separately, in
  # appropriate_answer()), so the guard was vestigial and only got in the
  # way of testing it non-interactively (binding decision #3)
  nc <- length(choices)

  if(length(title) && nzchar(title[1L])){
    cat(title[1L], "\n")
  }

  if(!dictionary){

    non_default_choices <- choices[-seq_len(default)]
    qwert <- c("Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P")[seq_len(length(non_default_choices))]

    op <- paste0(format(c(seq_len(default), qwert)), ": ", choices)

  }else{
    op <- paste0(format(seq_len(default)), ": ", choices)
  }

  if(nc > default){

    fop <- format(op)
    nw <- nchar(fop[1L], "w") + 2L
    ncol <- getOption("width") %/% nw

    if(ncol > 1L){

      if(default == 10 || default == 9){

        word <- fop[1:5]
        word <- paste0(word, c(rep.int("  ", 4), "\n"), collapse = "")

        nw <- nchar(word)
        if(substr(word, nw - 1, nw) != "\n"){
          word <- paste(substr(word, 1, nw - 2), "\n")
        }

        string <- fop[6:default]
        opts <- ifelse(default == 9, 3, 4)
        string <- paste0(string, c(rep.int("  ", opts), "\n"), collapse = "")

        ns <- nchar(string)
        if(substr(string, ns - 1, ns) != "\n"){
          string <- paste(substr(string, 1, ns - 2), "\n")
        }

      }else{

        def <- fop[seq_len(default)]
        def <- paste0(def, c(rep.int("  ", min(nc, ncol) - 1L), "\n"), collapse = "")

        n <- nchar(def)
        if(substr(def, n - 1, n) != "\n"){
          def <- paste(substr(def, 1, n - 2), "\n")
        }

      }

    }

    # 'fop' always has length > default here (guaranteed by the enclosing
    # `if(nc > default)`), so `(default + 1):length(fop)` can never reverse
    resp <- fop[(default + 1):length(fop)]
    resp[1] <- paste0("\n", resp[1], collapse = "")
    resp_len <- length(resp)

    if(resp_len > ncol){

      resp <- paste0(resp, c(rep.int("  ", min(nc, ncol) - 1L), "\n"), collapse = "")

      if((resp_len / ncol) %% 1 != 0){
        resp <- paste0(paste0(resp, collapse = ""), "\n")
      }

    }else{
      resp <- paste0(paste0(resp, collapse = ""), "\n")
    }

    if(default == 10 || default == 9){

      op <- paste0(
        style_text("Word options\n", defaults = "underline"), word,
        style_text("\nString options\n", defaults = "underline"), string,
        style_text("\nResponse options", defaults = "underline"), resp,
        collapse = ""
      )

    }else if(default == 5){

      op <- paste0(
        style_text("Word options\n", defaults = "underline"), def,
        style_text("\nResponse options", defaults = "underline"), resp,
        collapse = ""
      )

    }else{
      stop("Error in custom_menu")
    }

  }else{
    op <- c(op, "")
  }

  if(!help){
    cat("", op, sep = "\n")
  }else if(nc > default){
    # 'resp' only exists when the `nc > default` branch above ran -- the
    # original referenced it unconditionally here, crashing with "object
    # 'resp' not found" whenever `help = TRUE` was requested with no extra
    # (non-default) choices to list. Confirmed reachable: textcleaner_help()
    # calls `custom_menu(choices, default = 10, help = TRUE)` where 'choices'
    # is 5 fixed items plus a variable-length `possible` guess vector -- any
    # response with fewer than 5 guesses hit this exact crash.
    paste0("", resp, sep = "")
  }else{
    ""
  }

}

#' @noRd
# Simple yes/no console prompt, re-prompting until a recognizable answer is
# given. Ported from `yes.no.menu()`, rebuilt on `read_input()`.
# Updated 08.09.2026
yes_no_menu <- function(title = NULL, reader = readline)
{

  yes_no <- function(ans)
  {
    if(is.numeric(ans)){
      return(NA)
    }else if(is.character(ans)){

      ans <- tolower(ans)

      if(ans != "y" && ans != "yes" && ans != "n" && ans != "no"){
        return(NA)
      }else{
        return(switch(ans, "y" = 1, "yes" = 1, "n" = 2, "no" = 2))
      }

    }else{
      return(NA)
    }
  }

  title <- paste(title, "[Y/n]? ")
  ans <- read_input(title, reader = reader)

  while(is.na(yes_no(ans))){
    ans <- read_input("Inappropriate response. Try again [Y/n]. ", reader = reader)
  }

  return(yes_no(ans))

}

#' @noRd
# Prints diagnostic/GitHub-issue information when a wizard stage fails
# Ported from `error.fun()`
# Updated 08.09.2026
#' @importFrom utils packageVersion
error_report <- function(result, SUB_FUN, FUN)
{

  message(paste("\nAn error has occurred in the '", SUB_FUN, "' function of '", FUN, "':\n", sep = ""))
  cat(paste(result))
  message("\nPlease open a new issue on GitHub (bug report): https://github.com/AlexChristensen/SemNetCleaner/issues/new/choose")

  OS <- as.character(Sys.info()["sysname"])
  OSversion <- paste(as.character(Sys.info()[c("release", "version")]), collapse = " ")
  Rversion <- paste(R.version$major, R.version$minor, sep = ".")
  SNCversion <- paste(unlist(packageVersion("SemNetCleaner")), collapse = ".")
  SNDversion <- paste(unlist(packageVersion("SemNetDictionaries")), collapse = ".")

  message(paste("\nBe sure to provide the following information:\n"))

  message(style_text("To Reproduce:", defaults = "bold"))
  message(paste(" ", text_symbol("bullet"), " Function error occurred in: ", SUB_FUN, " function of ", FUN, sep = ""))

  message(style_text("\nR, SemNetCleaner, and SemNetDictionaries versions:", defaults = "bold"))
  message(paste(" ", text_symbol("bullet"), " R version: ", Rversion, sep = ""))
  message(paste(" ", text_symbol("bullet"), " SemNetCleaner version: ", SNCversion, sep = ""))
  message(paste(" ", text_symbol("bullet"), " SemNetDictionaries version: ", SNDversion, sep = ""))

  message(style_text("\nOperating System:", defaults = "bold"))
  message(paste(" ", text_symbol("bullet"), " OS: ", OS, sep = ""))
  message(paste(" ", text_symbol("bullet"), " Version: ", OSversion, sep = ""))

}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### TEXT PRESENTATION LAYER  ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' @noRd
# Colors text (8-bit ANSI color codes). Ported from `colortext()`.
# Updated 08.09.2026
color_text <- function(text, number = NULL, defaults = NULL)
{

  sys_check <- system_check()

  if(sys_check$TEXT){

    if(is.null(number) || number < 0 || number > 231){
      number <- 15
    }

    if(!is.null(defaults)){

      if(defaults == "highlight"){

        if(sys_check$RSTUDIO && requireNamespace("rstudioapi", quietly = TRUE)){
          if(rstudioapi::getThemeInfo()$dark){
            number <- 226
          }else{
            number <- 208
          }
        }else{
          number <- 208
        }

      }else{

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

    return(paste("\033[38;5;", number, "m", text, "\033[0m", sep = ""))

  }else{
    return(text)
  }

}

#' @noRd
# Stylizes text (bold, italics, underline, strikethrough). Ported from
# `styletext()`.
# Updated 08.09.2026
style_text <- function(text, defaults = c("bold", "italics", "highlight", "underline", "strikethrough"))
{

  sys_check <- system_check()

  if(sys_check$TEXT){

    if(missing(defaults)){
      number <- 0
    }else{
      number <- switch(defaults,
        bold = 1,
        italics = 3,
        underline = 4,
        highlight = 7,
        strikethrough = 9
      )
    }

    return(paste("\033[", number, ";m", text, "\033[0m", sep = ""))

  }else{
    return(text)
  }

}

#' @noRd
# Unicode text symbols (Greek letters, checkmarks, bullets, ...). Ported
# from `textsymbol()`.
# Updated 08.09.2026
text_symbol <- function(symbol = c("alpha", "beta", "chi", "delta", "eta", "gamma",
                                    "lambda", "omega", "phi", "pi", "rho", "sigma",
                                    "tau", "theta", "square root", "infinity",
                                    "check mark", "x", "bullet"))
{

  # \uXXXX escapes, not literal UTF-8 glyphs, per R CMD check's own
  # "portable packages must use only ASCII characters in R code" rule --
  # matching the untouched original's own `textsymbol()`, which already did
  # this correctly.
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

  return(sym)

}

#' @noRd
# Private cache for system_check() -- see below for why it's memoized
.system_check_cache <- new.env(parent = emptyenv())

#' @noRd
# Checks whether ANSI text styling is safe to use for the current OS/RStudio
# combination. Ported from `system.check()`.
# Optimization (microbenchmark, n = 5000): memoized after the first call --
# OS/RStudio status can't meaningfully change mid-session, so re-running
# Sys.info()/Sys.getenv() on every single color_text()/style_text() call
# (this function's only two callers, invoked pervasively across the whole
# console wizard) was pure waste. ~52us live vs ~1us cached (median), a
# ~50x win; output verified `identical()` to a fresh (uncached) computation.
# Updated 08.09.2026
system_check <- function(...)
{

  if(is.null(.system_check_cache$value)){

    OS <- unname(tolower(Sys.info()["sysname"]))
    RSTUDIO <- ifelse(Sys.getenv("RSTUDIO") == "1", TRUE, FALSE)
    TEXT <- TRUE

    if(!RSTUDIO){
      if(OS != "linux"){
        TEXT <- FALSE
      }
    }

    res <- list()
    res$OS <- OS
    res$RSTUDIO <- RSTUDIO
    res$TEXT <- TEXT

    .system_check_cache$value <- res

  }

  return(.system_check_cache$value)

}

#' @noRd
# Prints a horizontal divider line. Ported from `linebreak()`.
# Updated 08.09.2026
line_break <- function(breaks = "\n")
{
  cat(breaks, color_text(paste(rep("-", getOption("width")), collapse = ""), defaults = "message"), "\n")
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### RESPONSE / WORD PRIMITIVES  ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' @noRd
# Removes leading/trailing spaces not caught by `trimws()`. Ported from
# `rm.lead.space()`. Calls the exported `bad_response()` (R/bad_response.R,
# Phase 2) -- both live in the same package namespace, so this resolves
# correctly regardless of file-source order.
# Updated 08.09.2026
remove_leading_space <- function(word)
{

  word <- bad_response(word)

  if(!is.na(word)){
    word <- gsub("^[[:space:]]+|[[:space:]]+$", "", word)
  }

  return(word)

}

#' @noRd
# Shared singular -> plural irregular-word pairs. The original package had
# two INDEPENDENT switch()-based tables -- pluralize()'s (9 entries) and
# singularize()'s first 9 entries (of 25) -- that were, on direct
# comparison, exact mirror images of each other. Collapsed into one table
# here (used by >= 2 files, per the helpers.R convention): pluralize.R uses
# it directly; singularize.R derives its reverse lookup from it and adds
# its own 16 singularize-only entries (words the regular suffix rules
# don't handle correctly on their own -- kept local to singularize.R, no
# pluralize-side counterpart exists for them).
#
# Expanded during a follow-up review (per direct request) with two more
# categories, both verified empirically against SemNetDictionaries::
# coca.dictionary/hunspell.dictionary/general.dictionary and the real
# hunspell::hunspell_check() spell-checker before being added -- not
# guessed:
#
# 1. Invariant-plural nouns (singular and plural forms are identical),
#    added as identity mappings. This package's primary use case is
#    verbal-fluency ANIMAL naming (see the bundled `open.animals` data),
#    so these are weighted toward that domain. Needed specifically because
#    the regular suffix rules' "+s"/"+es" candidate is, for several of
#    these, coincidentally a REAL entry in coca.dictionary (confirmed:
#    "deers"/"salmons"/"elks"/"bisons"/"trouts" are all TRUE there, and
#    "elks"/"salmons"/"trouts" even pass the real hunspell spell-checker,
#    since they ARE grammatically valid English words -- just not the
#    idiomatically-preferred form for these animals). No dictionary-
#    membership check can distinguish "grammatically valid" from
#    "idiomatically standard" -- only a curated exception list can.
# 2. Words the rule engine's own first-choice candidate systematically
#    mishandles: either it collides with a coincidentally real but WRONG
#    dictionary word ("bus" -> the "-us/-i" rule's own candidate fails,
#    and the final fail-safe's "+s" attempt, "buss", happens to be a real
#    -- if archaic -- word), or the linguistically correct target isn't
#    itself common enough for any of the four dictionaries checked to
#    contain it ("walrus" -> "walruses" is real English but wasn't found
#    in coca/hunspell/cocaspell; "octopus" -> "octopi" is the form this
#    package's own pre-existing documentation already claimed, despite
#    being a folk-etymology plural rejected by every dictionary checked,
#    including hunspell_check() -- kept to match established behavior/docs
#    rather than switched to "octopuses").
#
# Entries added this pass: deer, moose, sheep, swine, salmon, trout, elk,
# bison, fish, shrimp, buffalo, species (all identity/invariant), ox ->
# oxen (a genuine classic irregular, not invariant), bus -> buses,
# walrus -> walruses, matrix -> matrices, vertex -> vertices, iris ->
# irises, hippopotamus -> hippopotamuses, platypus -> platypuses,
# octopus -> octopi.
# Updated 08.09.2026
irregular_plurals <- c(
  child = "children", goose = "geese", man = "men", woman = "women",
  tooth = "teeth", foot = "feet", mouse = "mice", person = "people",
  louse = "lice", ox = "oxen",

  deer = "deer", moose = "moose", sheep = "sheep", swine = "swine",
  salmon = "salmon", trout = "trout", elk = "elk", bison = "bison",
  fish = "fish", shrimp = "shrimp", buffalo = "buffalo", species = "species",

  bus = "buses", walrus = "walruses", matrix = "matrices", vertex = "vertices",
  iris = "irises", hippopotamus = "hippopotamuses", platypus = "platypuses",
  octopus = "octopi",

  # "genius" -> the rule engine's own "-us/-i" candidate, "genii", is a
  # real (hunspell-recognized) word -- but it's the archaic sense
  # ("guardian spirits"), not the overwhelmingly more common modern plural
  # for intelligent people. Found empirically (broadening the dictionary
  # check surfaced this as a new, unexpected-but-valid word, not a typo)
  # and overridden here in favor of the standard modern form.
  genius = "geniuses"
)

#' @noRd
# Private cache of memoized dictionary hashes, generalized beyond just
# `coca.dictionary` (per direct request, folding in the same treatment
# already applied there): `multiple_words()`/`response_splitter()`/
# `ind_word_check()` all repeat `word %in% full_dictionary`-style checks
# against WHATEVER dictionary vector was passed in (`animals.dictionary`,
# a custom vector, etc.) -- same "match() rebuilds its hash every call"
# tax `coca.dictionary` had, just not hardcoded to one specific vector.
#
# Keyed by `identical()` against a small (4-entry) most-recently-used
# list rather than some fingerprint/digest of the dictionary's content,
# because `identical()` is cheap enough to call on every lookup to matter:
# benchmarked ~1.7us when the SAME underlying object is passed repeatedly
# (the common case -- R's `identical()` short-circuits via reference
# equality before any deep comparison) and still only ~280us worst-case
# (two SEPARATELY-constructed 80K-word vectors with identical content,
# forcing a real deep compare) -- both negligible next to the ~130-220ms
# hash-build cost being avoided.
# Updated 09.09.2026
.dictionary_hash_cache <- new.env(parent = emptyenv())
.dictionary_hash_cache$entries <- list()

get_dictionary_hash <- function(dictionary)
{

  entries <- .dictionary_hash_cache$entries

  for(entry in entries){
    if(identical(entry$dictionary, dictionary)){
      return(entry$hash)
    }
  }

  # Not cached -- build it. Real bug fixed, found during a Phase 3 review:
  # `assign(word, TRUE, envir = hash_env)` silently coerces an NA entry
  # into the environment variable literally NAMED "NA", so a dictionary
  # containing a genuine missing value made the *string* "NA" falsely
  # register as a recognized word -- diverging from `%in%`'s own correct
  # semantics (`"NA" %in% c(..., NA)` is FALSE; only `NA %in% c(..., NA)`
  # is TRUE, confirmed directly). NA entries are dropped before hashing --
  # `dictionary_contains()` never needs to report a literal `NA` as
  # "found" anyway, since it already short-circuits `is.na(word)` to
  # FALSE for the word being checked, on the query side, before this is
  # ever reached. Filtered into a separate variable, NOT reassigned onto
  # `dictionary` itself -- the cache key below must stay the original
  # (unfiltered) argument, or a dictionary that actually contains NA would
  # never again compare `identical()` to itself on a later call, forcing
  # a full rebuild every single time instead of hitting the cache.
  non_na_dictionary <- dictionary[!is.na(dictionary)]

  hash_env <- new.env(hash = TRUE, parent = emptyenv(), size = length(non_na_dictionary))

  for(word in non_na_dictionary){
    assign(word, TRUE, envir = hash_env)
  }

  # Keep only the 4 most-recently-used dictionaries
  entries <- c(list(list(dictionary = dictionary, hash = hash_env)), entries)
  .dictionary_hash_cache$entries <- entries[seq_len(min(4, length(entries)))]

  return(hash_env)

}

#' @noRd
#' @importFrom stats median sd setNames
# Memoized multi-word-response split threshold (`multi_min`), used by
# `auto_spellcheck()`'s multi-word parsing stage. Found while optimizing
# Phase 4/5: `dict_lens <- unlist(lapply(full_dictionary, function(x){
# length(unlist(strsplit(x, " "))) }))` -- iterating the WHOLE dictionary
# to measure every entry's word count -- is a pure function of
# `full_dictionary` alone, recomputed unconditionally on every
# `auto_spellcheck()` call, in both the untouched original and this
# rebuild's first pass alike. Benchmarked directly: ~5ms for
# `animals.dictionary` (1,252 words), but ~500ms for `cocaspell` (~109k
# words, the default `free`-association dictionary) -- half a second of
# dead air before the multi-word stage even starts, paid again on every
# single `auto_spellcheck()` call within a session. Same MRU-cache-by-
# `identical()` pattern as `get_dictionary_hash()` above.
# Updated 09.09.2026
.multi_min_cache <- new.env(parent = emptyenv())
.multi_min_cache$entries <- list()

get_multi_min <- function(full_dictionary)
{

  entries <- .multi_min_cache$entries

  for(entry in entries){
    if(identical(entry$dictionary, full_dictionary)){
      return(entry$multi_min)
    }
  }

  dict_lens <- unlist(lapply(full_dictionary, function(x){ length(unlist(strsplit(x, " "))) }))
  multi_min <- ceiling(median(dict_lens) + 2 * sd(dict_lens))
  if(multi_min == 1){ multi_min <- 2 }

  entries <- c(list(list(dictionary = full_dictionary, multi_min = multi_min)), entries)
  .multi_min_cache$entries <- entries[seq_len(min(4, length(entries)))]

  return(multi_min)

}

#' @noRd
# Vectorized membership check against an arbitrary dictionary --
# `dictionary_contains(word, full_dictionary)` in place of
# `word %in% full_dictionary`. Accepts a scalar or vector `words` (matching
# `%in%`'s own calling convention).
#
# Size-aware dispatch, added after benchmarking found the memoized hash is
# NOT a universal win: `%in%`/`match()`'s own hash-rebuild cost only
# becomes expensive at real scale. Measured the actual crossover
# (microbenchmark, varying dictionary size against a fixed miss-word):
# raw `%in%` wins below ~2,000-5,000 words (e.g. `animals.dictionary`,
# 1,252 words: ~3.5-6.5us raw vs ~12-13us hashed -- the hash path's own
# `identical()` cache-scan + `vapply()` overhead costs MORE than `%in%`'s
# cheap rebuild at that size), while the memoized hash wins decisively
# above that (e.g. `coca.dictionary`, 80,381 words: ~355us raw vs ~17us
# hashed once cached). `3000` is a conservative threshold placed inside
# the confirmed crossover band (2,000-5,000), erring toward `%in%` for
# anything ambiguous. Below the threshold, this is a plain `%in%` call --
# `"" %in% dict` already correctly returns `FALSE` with no special-casing
# needed; the empty-string/`NA` guard below is only required for the
# hashed path's `exists()` call (see its own comment history for why).
# Updated 09.09.2026
dictionary_contains <- function(words, dictionary)
{

  if(length(dictionary) < 3000){
    return(words %in% dictionary)
  }

  hash_env <- get_dictionary_hash(dictionary)

  vapply(words, function(word){

    # `%in%` (the small-dictionary branch above, and what this function
    # replaces at every call site) tolerates a non-character `word`
    # gracefully -- coercing for comparison and simply reporting no match.
    # `exists()` does not: it errors ("invalid first argument") on
    # anything that isn't a single string. Confirmed reachable: when
    # `auto_spellcheck()`'s multi-word stage has nothing "all correct",
    # `multi_word` keeps its `list(response=, correct=)` shape into the
    # response-splitting step below, and `dictionary_contains()` gets
    # called with that whole 2-element list as `words` -- so `word` is,
    # for one `vapply()` iteration, the logical `$correct` field, not a
    # string. Only reachable with a large (>=3000-word) dictionary, since
    # the small-dictionary `%in%` branch above never hit this.
    if(!is.character(word) || is.na(word) || word == ""){
      return(FALSE)
    }

    exists(word, envir = hash_env, inherits = FALSE)

  }, logical(1), USE.NAMES = FALSE)

}

#' @noRd
# Thin, coca-specific convenience wrapper around `dictionary_contains()` --
# kept so `pluralize()`/`singularize()`'s existing call sites don't need to
# repeat `SemNetDictionaries::coca.dictionary` at every one.
# Updated 09.09.2026
coca_dictionary_contains <- function(word)
{
  dictionary_contains(word, SemNetDictionaries::coca.dictionary)
}

#' @noRd
# Private, memoized cache of the FULL recognition verdict (coca + hunspell
# combined) per word -- not just the coca hash lookup above. Optimization,
# found while profiling `best_guess()` (per direct request to look for a
# better approach there): `best_guess()` calls `pluralize()` on every
# candidate word near the edit-distance frontier, and those are
# disproportionately rare/unusual words -- exactly the ones most likely to
# fail every check and fall through to the expensive `hunspell_check()`
# fallback not once but TWICE (once for the "+s" candidate, once for
# "+es"). Confirmed via direct profiling: `best_guess("dogg", full_dictionary
# = animals.dictionary)` spent ~310ms of its ~315ms total inside a 6-word
# `pluralize()` loop, almost entirely attributable to one rare word
# ("degu") failing both hunspell attempts. Since `word_is_recognized()` is
# a pure function of `word` alone (dictionaries don't change at runtime),
# its result is safe to memoize permanently per word for the life of the
# session -- confirmed ~200x faster on a repeated check of the same word
# (~330us -> ~1.7us, microbenchmark) once cached, which matters because a
# real spellcheck session's candidate words overlap heavily across many
# `best_guess()`/`pluralize()` calls.
# Updated 09.09.2026
.word_recognition_cache <- new.env(hash = TRUE, parent = emptyenv())

word_is_recognized <- function(word)
{

  if(is.na(word) || word == ""){
    return(FALSE)
  }

  cached <- .word_recognition_cache[[word]]

  if(!is.null(cached)){
    return(cached)
  }

  # `hunspell` is a hard `Imports` dependency (promoted from `Suggests`,
  # per direct request -- `word_is_recognized()` is reachable from
  # `textcleaner()`'s own core pipeline via `pluralize()`/`singularize()`,
  # so its spell-correction quality shouldn't silently vary based on
  # whether an optional package happens to be installed) -- no
  # `requireNamespace()` guard needed, matching how every other hard
  # dependency in this package (e.g. `stringdist`) is called directly.
  result <- coca_dictionary_contains(word)

  if(!result){
    result <- isTRUE(hunspell::hunspell_check(word))
  }

  .word_recognition_cache[[word]] <- result

  return(result)

}

#' @noRd
# Memoized per-word Metaphone code, used by `best_guess()` as a
# disambiguation signal alongside its existing QWERTY-distance tiebreak
# (see `best_guess.R`'s own comment for the full priority/conflict rule).
# `phonics` is a hard `Imports` dependency (promoted from `Suggests`, per
# direct request -- matching `hunspell`, see `word_is_recognized()` above)
# -- no `requireNamespace()` guard needed. `tryCatch()` is kept: not a
# missing-dependency guard, but a defensive backstop against a genuinely
# unexpected `metaphone()` failure on some pathological input, so a
# lookup failure degrades to "no phonetic signal" rather than crashing
# `best_guess()`.
.phonetic_code_cache <- new.env(hash = TRUE, parent = emptyenv())

get_phonetic_code <- function(word)
{

  if(is.na(word) || word == ""){
    return(NA_character_)
  }

  cached <- .phonetic_code_cache[[word]]

  if(!is.null(cached)){
    return(cached)
  }

  code <- tryCatch(phonics::metaphone(word), error = function(e){ NA_character_ })

  .phonetic_code_cache[[word]] <- code

  return(code)

}

#' @noRd
# Whether two words share the same Metaphone code -- deliberately always
# `FALSE` (never `NA`) on a missing dependency or failed lookup, so this
# composes safely inside a `vapply(..., logical(1))` with no special-casing
# needed at the call site.
phonetic_match <- function(word_a, word_b)
{

  code_a <- get_phonetic_code(word_a)
  code_b <- get_phonetic_code(word_b)

  if(is.na(code_a) || is.na(code_b)){
    return(FALSE)
  }

  return(identical(code_a, code_b))

}

#' @noRd
# Memoized `SemNetDictionaries::load.monikers(dictionary)`. Found during a
# Phase 3 review: `load.monikers()` costs ~1.6ms per call (microbenchmark)
# -- not negligible, and `multiple_words()`/`ind_word_check()`/`best_guess()`
# all call it once per invocation with the SAME `dictionary` argument every
# time they're themselves called repeatedly (once per response, across a
# whole dataset, once Phase 4's auto_spellcheck() starts calling these in
# bulk). Cached with the same `identical()`-against-a-small-MRU-list
# pattern as `get_dictionary_hash()` -- `dictionary` here is typically a
# short string/vector (e.g. `"animals"`), so the cache-scan itself is
# trivially cheap regardless.
# (Checked and explicitly NOT pursued: caching `unlist(misnom)` inside
# `moniker()` itself, flagged as a candidate back in Phase 1 -- benchmarked
# at only ~1us for a real 540-entry moniker list, nowhere near worth the
# complexity. The Phase 1 estimate of ~145us was from a synthetic
# 3000-entry test list, much larger than real moniker lists turn out to be.)
# Updated 09.09.2026
.moniker_cache <- new.env(parent = emptyenv())
.moniker_cache$entries <- list()

get_monikers <- function(dictionary)
{

  entries <- .moniker_cache$entries

  for(entry in entries){
    if(identical(entry$dictionary, dictionary)){
      return(entry$monikers)
    }
  }

  monikers <- SemNetDictionaries::load.monikers(dictionary)

  entries <- c(list(list(dictionary = dictionary, monikers = monikers)), entries)
  .moniker_cache$entries <- entries[seq_len(min(4, length(entries)))]

  return(monikers)

}

#' @noRd
# Memoized `SemNetDictionaries::dictionaries(TRUE)`. Found during the same
# review: `dictionaries()` isn't a simple in-memory lookup -- it does real
# file I/O every single call (`readRDS(system.file("data", "Rdata.rds",
# package = "SemNetDictionaries"))`), confirmed by reading its source.
# `best_guess()` was calling it 4 times in one code block (already hoisted
# to 1 locally there); `ind_word_check()` calls it once per invocation, and
# both functions are themselves called per-word once Phase 4's
# auto_spellcheck() runs in bulk. The available dictionary set can't change
# mid-session (it's a static package data file), so a single cached value
# -- not a per-argument MRU list like the caches above, since this always
# takes the same effective call shape -- is sufficient.
# Updated 09.09.2026
.available_dictionaries_cache <- new.env(parent = emptyenv())

get_available_dictionaries <- function()
{

  if(is.null(.available_dictionaries_cache$value)){
    .available_dictionaries_cache$value <- SemNetDictionaries::dictionaries(TRUE)
  }

  return(.available_dictionaries_cache$value)

}

#' @noRd
# Lazily-memoized "aa".."zz" two-letter combination list -- used by
# `auto_spellcheck()`'s free-only bad/short-response-removal stage to
# identify which two-letter strings are recognized dictionary entries
# (vs. noise) and which aren't. A pure constant (built from base R's own
# `letters`, nothing else), but `apply(expand.grid(letters, letters), 1,
# paste, collapse = "", sep = "")` was being rebuilt from scratch on every
# single `auto_spellcheck(type = "free")` call regardless -- benchmarked
# at ~3.5ms each time, for a value that's identical every time.
# Updated 09.09.2026
.two_letter_combinations_cache <- new.env(parent = emptyenv())

get_two_letter_combinations <- function()
{

  if(is.null(.two_letter_combinations_cache$value)){
    .two_letter_combinations_cache$value <- apply(expand.grid(letters, letters), 1, paste, collapse = "", sep = "")
  }

  return(.two_letter_combinations_cache$value)

}

#' @noRd
# Looks up a word in a moniker (common-misspelling) list, converting between
# UK/US spelling. Ported from `moniker()`. Updated (Phase 3) to call the new
# `brit_us_conversion()` (R/brit_us_conversion.R) instead of the legacy
# `brit.us.conv()` in utils-SemNetCleaner.R.
# Updated 09.09.2026
moniker <- function(word, misnom, spelling)
{

  mis <- unlist(misnom)

  if(!is.na(match(word, mis))){
    matched <- names(mis[match(word, mis)])
    misnomed <- gsub("[[:digit:]]+", "", matched)
  }else{
    misnomed <- word
  }

  misnomed <- brit_us_conversion(vec = misnomed, spelling = spelling, dictionary = FALSE)

  return(misnomed)

}

#' @noRd
# Obtains the minimum/maximum of each contiguous run in a numeric sequence
# (used for cue/ID row-splicing in `correct_data_free()`, Phase 6). Ported
# from `seq_min_max()`, already snake_case in the original.
# Updated 08.09.2026
seq_min_max <- function(sequence)
{

  # Robustness fix: `diff(sequence)` on a length-0/1 `sequence` is itself
  # length 0, and `all(logical(0))` is vacuously TRUE, so a genuinely EMPTY
  # `sequence` fell into the "one contiguous run" branch below rather than
  # being rejected -- `min()`/`max()` on `numeric(0)` then silently return
  # `Inf`/`-Inf` (with a warning, not an error) instead of a run that
  # doesn't exist. A length-1 `sequence` is unaffected (correctly its own
  # single-element run) -- only the length-0 case needed guarding.
  if(length(sequence) == 0){
    stop("`seq_min_max()`: `sequence` must have at least one element.", call. = FALSE)
  }

  value_differences <- diff(sequence)

  if(all(value_differences == 1)){

    minimum <- min(sequence, na.rm = TRUE)
    maximum <- max(sequence, na.rm = TRUE)

  }else{

    breaks <- which(value_differences != 1)
    breaks <- c(breaks, length(sequence))
    starts <- (breaks + 1) - c(breaks[1], diff(breaks))

    split_sequences <- lapply(seq_along(breaks), function(i){
      return(sequence[starts[i]:breaks[i]])
    })

    minimum <- unlist(lapply(split_sequences, min, na.rm = TRUE))
    maximum <- unlist(lapply(split_sequences, max, na.rm = TRUE))

  }

  res <- list()
  res$min <- minimum
  res$max <- maximum

  if(exists("breaks", envir = environment())){
    res$breaks <- breaks
    res$starts <- starts
    res$lengths <- unlist(lapply(split_sequences, length))
  }

  return(res)

}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PARALLEL PROCESSING    ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' @noRd
# Run `FUN(i, ...)` over `seq_len(iterations)`, in parallel when `cores > 1`
# Ported verbatim from `../SemNeT`'s own `helpers.R` -- replaces the 5
# independently duplicated, leak-prone `parallel::makeCluster()`/
# `stopCluster()` blocks in this package's original `auto.spellcheck`/
# `.free`/`spellcheck.dictionary.free` (none guarded cleanup with
# `on.exit()`, and none floored `detectCores()/2` above 0) with one
# `future`-based helper that always restores the previous plan, even if
# `FUN` errors, and never asks for zero cores.
# Updated 08.09.2026
#
# Phase 11 additions (ported from `../SemNeT/R/helpers.R`, adapted naming --
# `SemNetCleaner.progress_file` in place of `SemNeT.progress_file`), needed
# once `textcleaner_shiny()`'s automated-pass progress bar drives this
# function from inside a `shiny::ExtendedTask`:
#
# - **Nested-worker guard**: when this call is itself running inside a
#   background `multisession` worker (the `ExtendedTask`'s own
#   `future_promise()` body, spawned from the app-level plan `global.R`
#   sets), spinning up a *second*, nested `multisession` pool here trips
#   `parallelly`'s hard localhost-worker-count guard
#   (`checkNumberOfLocalWorkers()`), since a worker process's own
#   `future::availableCores()` is deliberately capped (usually to 1) to
#   prevent exactly this kind of recursive worker explosion. Capping the
#   request to what's actually available degrades that case to sequential
#   (still correct, just not internally parallel one level deeper) while
#   leaving true top-level interactive/console use -- where
#   `availableCores()` reflects the real host -- unaffected.
# - **Progress-file channel**: `progressr`'s own cross-process relay assumes
#   the caller synchronously *waits* on the future (a plain interactive
#   `future::value()` call) -- confirmed it delivers nothing at all to a
#   `shiny::ExtendedTask`/`promises::future_promise()` caller, which never
#   blocks that way. For that case only, the Shiny app instead points
#   `options(SemNetCleaner.progress_file = <path>)` at a per-task file
#   *before* calling into a package function that reaches this point, and
#   every iteration's completion is appended there directly (a completion
#   *count*, not the raw iteration index -- `future_lapply()`'s workers
#   finish out of order under `cores > 1`, so only a count of completions so
#   far climbs monotonically). No exported function's signature needs a new
#   argument for this; ordinary interactive/console use is unaffected
#   (`getOption(...)` is `NULL`, every `progress_file`-guarded block is a
#   no-op).
parallel_process <- function(iterations, FUN, ..., cores = 1, progress = TRUE)
{

  length_error(iterations, 1, "parallel_process")
  typeof_error(iterations, "numeric", "parallel_process")
  range_error(iterations, c(1, Inf), "parallel_process")

  length_error(cores, 1, "parallel_process")
  typeof_error(cores, "numeric", "parallel_process")
  range_error(cores, c(1, Inf), "parallel_process")

  typeof_error(FUN, "function", "parallel_process")

  if(cores > 1){
    cores <- min(cores, future::availableCores())
  }

  if(cores > 1){
    old_plan <- future::plan(future::multisession, workers = cores)
  }else{
    old_plan <- future::plan(future::sequential)
  }

  on.exit(future::plan(old_plan), add = TRUE)

  progress_file <- getOption("SemNetCleaner.progress_file", default = NULL)
  progress_marker_file <- paste0(progress_file, ".progress")

  if(!is.null(progress_file)){
    try({
      writeLines(as.character(iterations), progress_file)
      writeLines(character(0), progress_marker_file)
    }, silent = TRUE)
  }

  wrapped_FUN <- function(i, ...){

    result <- FUN(i, ...)

    if(!is.null(progress_file)){
      try(cat("1", file = progress_marker_file, append = TRUE), silent = TRUE)
    }

    return(result)

  }

  if(!progress){
    return(future.apply::future_lapply(seq_len(iterations), wrapped_FUN, ..., future.seed = TRUE))
  }

  return(
    progressr::with_progress({

      report_step <- progressr::progressor(steps = iterations)

      future.apply::future_lapply(
        seq_len(iterations),
        function(i, ...){
          result <- wrapped_FUN(i, ...)
          report_step()
          return(result)
        },
        ...,
        future.seed = TRUE
      )

    })
  )

}
