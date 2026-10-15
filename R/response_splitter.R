#' @noRd
# Response splitter ----
# Splits a multi-word response into its words when they are dictionary entries ("cat dog"
# becomes "cat" and "dog"); a response that is itself an entry, or has a word that is not
# in the dictionary, is kept whole
# Updated 14.10.2026
response_splitter <- function(vec, full_dictionary)
{

  # Check to make sure that the vector is still a string
  if(length(vec) == 1){

    # Split vector
    split_vec <- unlist(strsplit(vec, " "))

    # Remove bad responses
    split_vec <- na.omit(bad_response(split_vec))

    # Check for multiple words
    if(length(split_vec) > 1){

      # Check if words in string are already in dictionary
      if(dictionary_contains(vec, full_dictionary)){

        return(vec) # Keep whole dictionary entry

      }else{

        # Split into the longest dictionary entries ("sea slug" + "dog"), when
        # every word is covered by one; otherwise leave the string alone
        segments <- segment_entries(split_vec, full_dictionary)

        # Return segments or leave string alone
        if(!is.null(segments)){

          return(segments)

        }else{

          return(vec)

        }

      }

    }else{

      return(vec) # Return single word

    }

  }else{

    return(vec) # Return non-string

  }

}
