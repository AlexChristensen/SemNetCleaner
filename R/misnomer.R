#' Misnomer Fucntion
#' @description A wrapper function for spell-checking
#' (idnetifies misnomers for a word)
#' 
#' @param word Word to check for misnomer
#' 
#' @param misnom A list of misnomers.
#' Only available for "animals" currently.
#' (see \code{\link[SemNetDictionaries]{animals.misnomer}})
#' 
#' @return If \code{word} matches a misnomer, then the appropriate word is returned.
#' If \code{word} does not match a misnomer, then the \code{word} is returned
#' 
#' @examples 
#' misnomer("possum", SemNetDictionaries::animals.misnomer)
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Misnomer function
misnomer <- function (word, misnom)
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
        #return word if no misnomer match
        misnomed <- word
    }
        
    return(misnomed)
}
#----