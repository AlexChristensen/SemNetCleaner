#' Multi Word Checker Wrapper
#' @description A wrapper function to spell-check responses
#' that have more than one word in them
#' 
#' @param string Character.
#' A string of words with a \code{length = 1}
#' 
#' @param dictionary A dictionary to look for word in (see examples).
#' See \code{\link{SemNetDictionaries}}
#'
#' @param tolerance Numeric.
#' The distance tolerance set for automatic spell-correction purposes.
#' This function uses the function \code{\link[stringdist]{stringdist}}
#' to compute the \href{https://en.wikipedia.org/wiki/Damerau-Levenshtein_distance}{Damerau-Levenshtein}
#' (DL) distance, which is used to determine potential best guesses.
#' 
#' Unique words (i.e., \emph{n} = 1) that are within the (distance) tolerance are
#' automatically output as \code{\link[SemNetCleaner]{best.guess}} responses, which are then passed through
#' \code{\link[SemNetCleaner]{word.check.wrapper}}. If there is more than one word
#' that is within or below the distance tolerance, then these will be provided as potential
#' options.
#' 
#' The recommended and default distance tolerance is \code{tolerance = 1},
#' which only spell corrects a word if there is only one word with a DL distance of 1. 
#' 
#' @return Either a spell-corrected response or the original response
#' 
#' @examples
#' # Returns "guinea pig"
#' multi.word.check("guinea big", SemNetDictionaries::animals.dictionary, tolerance = 1)
#' 
#' # Returns original response
#' multi.word.check("cat dog bear fish bull", SemNetDictionaries::animals.dictionary, tolerance = 1)
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Multi Word Spell-Checker
multi.word.check <- function (string, dictionary, tolerance)
{
    #split string
    spl <- unlist(strsplit(string," "))
    
    if(length(spl)>1)
    {
        #potential responses from 'best.guess' function
        pot <- best.guess(string,dictionary,tolerance)
        
        if(length(pot)==1)
        {
            #D-L Distance
            dist <- stringdist::stringdist(string,dictionary,method="dl")
            
            if(min(dist)<(length(spl)+1))
            {resp <- pot
            }else{resp <- string}
        }else{resp <- string}
    }else{resp <- string}
    
    return(resp)
}
#----