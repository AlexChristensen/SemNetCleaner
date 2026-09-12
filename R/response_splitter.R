#' @noRd
# Identifies and splits a multi-word response back into its individual
# words when they're already, independently, dictionary entries (e.g. a
# response accidentally recorded as one string, "catdog", is left alone,
# but "cat dog" -- already two real words -- stays split). Ported from
# `response.splitter()`. Optimization (per direct request): its
# `%in% full_dictionary` checks replaced with `dictionary_contains()`
# (helpers.R) -- a memoized-hash lookup shared with `multiple_words()`/
# `ind_word_check()`, in place of rebuilding `match()`'s internal hash
# table on every call against whatever (potentially large) dictionary was
# passed in.
# Updated 09.09.2026
response_splitter <- function(vec, full_dictionary)
{

  # Check to make sure that the vector is still a string
  if(length(vec) == 1)
  {
    # Split vector
    split_vec <- unlist(strsplit(vec, " "))

    # Remove bad responses
    split_vec <- na.omit(bad_response(split_vec))

    # Check for multiple words
    if(length(split_vec) > 1)
    {
      # Check if words in string are already in dictionary
      if(dictionary_contains(vec, full_dictionary))
      {return(vec)
      }else if(all(dictionary_contains(split_vec, full_dictionary))) # Check if words split are in dictionary
      {return(split_vec)
      }else{return(vec)} # Just return the vector
    }else{return(vec)}
  }else{return(vec)}

}
