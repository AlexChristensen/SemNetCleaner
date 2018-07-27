#' Misnomer Fucntion
#' @description A wrapper function for spell-checking
#' (idnetifies misnomers for a word)
#' 
#' @param word Word to check for misnomer
#' 
#' @param misnom Misnomer database
#' 
#' @return If \code{word} matches a misnomer, then the appropriate word is returned.
#' If \code{word} does not match a misnomer, then the \code{word} is returned
#' 
#' @examples 
#' misnomer("possum", animals.misnomer)
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Misnomer function
misnomer <- function (word, misnom)
{
    if(!is.null(misnom))
    {
        mis <- unlist(misnom)
    
        if(!is.na(match(word,mis)))
        {
            matched <- names(mis[match(word,mis)])
        
            misnomed <- gsub("[[:digit:]]+", "", matched)
        
        }else{misnomed <- word}
    
        return(misnomed)
    }else{return(word)}
}
#----