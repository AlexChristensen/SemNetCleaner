#' Bad Responses to NA
#' @description A wrapper function to determine whether responses are good or bad.
#' Bad responses are replaced with missing (\code{NA}). Good responses are returned.
#' 
#' @param word Character.
#' A word to be tested for whether it is bad
#' 
#' @param ... Vector.
#' Additional responses to be considered bad
#' 
#' @return If response is bad, then returns \code{NA}.
#' If response is valid, then returns the response
#' 
#' @examples
#' #bad response
#' bad.response(word = " ")
#' 
#' #good response
#' bad.response(word = "hello")
#' 
#' #make a good response bad
#' bad.response(word = "hello","hello")
#' 
#' #add additional bad responses
#' bad.response(word = "hello", c("hello","world"))
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Bad Response Function
bad.response <- function (word, ...)
{
    #other bad responses
    others <- unlist(list(...))
    
    #bad responses
    bad <- c(NA,"NA",""," ","  ",others)
    
    #if there is no longer a response
    if(length(word)==0)
    {word <- NA}
    
    for(i in 1:length(word))
    {
        #if bad, then convert to NA
        if(word[i] %in% bad)
        {word[i] <- NA}
    }
    
    return(word)
}
#----