#' Makes Best Guess for Spelling Correction
#' @description A wrapper function for the best guess of a spelling mistake
#' based on the letters, the ordering of those letters, and the potential
#' for letters to be interchanged. The
#' \href{https://en.wikipedia.org/wiki/Damerau-Levenshtein_distance}{Damerau-Levenshtein distance}
#' is used to guide inferences into what word the participant was trying to spell from a dictionary
#' (see \code{\link{SemNetDictionaries}})
#' 
#' @param word Character.
#' A word to get best guess spelling options from dictionary
#' 
#' @param dictionary Character vector.
#' The dictionary to search for best guesses in.
#' See \code{\link{SemNetDictionaries}}
#' 
#' @param tolerance Numeric.
#' The distance tolerance set for automatic spell-correction purposes.
#' This function uses the function \code{\link[stringdist]{stringdist}}
#' to compute the Damerau-Levenshtein distance, which is used to determine potential best guesses
#' 
#' Unique words (i.e., \emph{n} = 1) that are within the (distance) tolerance are
#' automatically output as best guess responses, which are then passed through
#' \code{\link[SemNetCleaner]{word.check.wrapper}}. This default is based on Damerau's (1964)
#' proclamation that more than 80\% of all human misspellings can be expressed by a single error
#' (e.g., insertion, deletion, substitution, and transposition). If there is more than one word
#' that is within or below the distance tolerance, then these will be provided as potential
#' options.
#' 
#' The recommended and default distance tolerance is \code{tolerance = 1},
#' which only spell corrects a word if there is only one word with a DL distance of 1. 
#' 
#' @return The best guess(es) of the word
#' 
#' @examples
#' # Misspelled "bombay"
#' best.guess("bomba", SemNetDictionaries::animals.dictionary)
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @references 
#' Damerau, F. J. (1964).
#' A technique for computer detection and correction of spelling errors.
#' \emph{Communications of the ACM}, \emph{7}, 171-176.
#' 
#' @importFrom stats sd
#' 
#' @export
#Best Guess function
best.guess <- function (word, dictionary, tolerance = 1)
{
    #grab Damerau-Levenshtein distances
    guess <- stringdist::stringdist(word,dictionary,method="dl")
    
    #compute minimum distance
    min.guess <- min(guess)
    
    #grab guesses less than or equal to plus one of minimum
    target <- which(guess <= (min.guess+1))
    
    #target distances
    target.dist <- guess[target]
    
    #check for plural variations
    plur.var <- unlist(lapply(dictionary[target],pluralize))
    #plural distances
    plur.dist <- stringdist::stringdist(word,plur.var,method="dl")
    
    if(any(plur.dist<target.dist))
    {
        #update target distances
        target.dist[plur.dist<target.dist] <- plur.dist[plur.dist<target.dist]
    }
    
    #determine if auto-correction is possible
    if(length(which(target.dist<=tolerance))==1)
    {
        #suggest option as best guess
        bestguess <- dictionary[target[which(target.dist<=tolerance)]]
        
    }else{
        
        #order by least to greatest DL value
        target.ord <- target[order(target.dist,decreasing = FALSE)]
        
        #order guesses by guess values
        guess <- dictionary[target.ord]
        
        #grab only 10 guesses
        bestguess <- na.omit(guess[1:10])
    }
    
    return(bestguess)
}
#----