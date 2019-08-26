#' Moniker Function
#' @description A wrapper function for spell-checking
#' (identifies monikers for a word)
#' 
#' @param word Word to check for moniker
#' 
#' @param misnom A list of monikers.
#' See \code{\link[SemNetDictionaries]{dictionaries}} for options
#' 
#' @return If \code{word} matches a moniker, then the appropriate word is returned.
#' If \code{word} does not match a moniker, then the \code{word} is returned
#' 
#' @examples 
#' moniker("possum", SemNetDictionaries::animals.moniker)
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Moniker function
moniker <- function (word, misnom)
{
    #unlist possible responses
    mis <- unlist(misnom)
        
    #if there is a match
    if(!is.na(match(word,mis)))
    {
        #then identify which word
        matched <- names(mis[match(word,mis)])

        #remove numbers
        misnomed <- gsub("[[:digit:]]+","", matched)
        
    }else{
        #return word if no moniker match
        misnomed <- word
    }
        
    return(misnomed)
}
#----