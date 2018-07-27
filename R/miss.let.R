#' Missing Letter
#' @description A wrapper function for spell-checking
#' (removes each letter from the word)
#' 
#' @param word Word to remove letters
#' 
#' @return Returns a vector with each letter from the word missing
#' 
#' @examples 
#' 
#' miss.let("bombay")
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Missing letter function
miss.let <- function (word)
{
    len <- nchar(word)
    
    vec <- vector("character",length=len)

    for(i in 1:len)
    {vec[i] <- gsub(substring(word,i,i),"",word)}
    
    return(vec)
}
#----
