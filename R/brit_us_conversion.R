#' @noRd
# Converts the spelling of a flat vector of single words between British and American
# English; `brit2us` is named by the US spelling and valued by the GB spelling
# Updated 14.10.2026
# Matches `vec` against the dictionary (not the other way around) so that every
# occurrence of a repeated word is converted
# Updated 14.10.2026
#' @noRd
# Memoized `unlist(brit2us)`; the table does not change during a session
# Updated 14.10.2026
# Cache unlisted British-US dictionary
.brit2us_unlisted_cache <- new.env(parent = emptyenv())

# Get brit2us unlisted ----
# Get unlisted British-US dictionary
# Updated 14.10.2026
get_brit2us_unlisted <- function()
{

  # Check for cached value
  if(is.null(.brit2us_unlisted_cache$value)){

    # Store unlisted dictionary
    .brit2us_unlisted_cache$value <- unlist(SemNetDictionaries::brit2us)

  }

  # Return unlisted dictionary
  return(.brit2us_unlisted_cache$value)

}

# Brit us conversion vector ----
# Updated 14.10.2026
brit_us_conversion_vector <- function(
    vec, # vector of words
    spelling = c("UK", "US"), # spelling to convert to
    dictionary = FALSE # whether vec is a dictionary
)
{

  # Check for UK spelling
  if(toupper(spelling) == "UK"){

    # Convert any US-spelled words in vec to their GB equivalents
    # Find US spellings
    matched <- match(vec, names(SemNetDictionaries::brit2us))

    # Find matched words
    found <- !is.na(matched)

    # Check for any matches
    if(any(found)){

      # Replace with UK spelling
      vec[found] <- unlist(SemNetDictionaries::brit2us[matched[found]])

    }

  }else if(toupper(spelling) == "US"){ # US spelling

    # Convert any GB-spelled words in vec to their US equivalents
    # Find UK spellings
    matched <- match(vec, get_brit2us_unlisted())

    # Find matched words
    found <- !is.na(matched)

    # Check for any matches
    if(any(found)){

      # Replace with US spelling
      vec[found] <- names(SemNetDictionaries::brit2us)[matched[found]]

    }

  }

  # Remove duplicates and alphabetize
  if(dictionary){

    # Sort unique words
    vec <- sort(unique(vec))

  }

  # Return converted words
  return(vec)

}

#' @noRd
# Brit us conversion ----
# Converts the spelling of each response word by word (responses can be multi-word strings)
# Updated 14.10.2026
brit_us_conversion <- function(
    vec, # vector of responses
    spelling = c("UK", "US"), # spelling to convert to
    dictionary # whether vec is a dictionary
)
{

  # Split responses into words
  vec <- unlist(lapply(vec, strsplit, split = " "), recursive = FALSE)

  # Loop over responses
  vec <- lapply(vec, function(x, spelling){

    # Convert words
    converted <- brit_us_conversion_vector(x, spelling = spelling)

    # Rejoin words
    conv <- paste(converted, collapse = " ")

    # Return converted response
    return(conv)

  }, spelling = spelling)

  # Check for dictionary
  if(dictionary){

    # Sort converted words
    vec <- sort(unlist(vec))

  }

  # Return converted responses
  return(vec)

}
