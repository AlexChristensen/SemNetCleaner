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
#' @param full.dictionary Character vector.
#' The dictionary to search for best guesses in.
#' See \code{\link{SemNetDictionaries}}
#' 
#' @param dictionary Character.
#' A dictionary from \code{\link{SemNetDictionaries}} for monikers (enhances guessing)
#' 
#' @param tolerance Numeric.
#' The distance tolerance set for automatic spell-correction purposes.
#' This function uses the function \code{\link[stringdist]{stringdist}}
#' to compute the Damerau-Levenshtein distance, which is used to determine potential best guesses
#' 
#' Unique words (i.e., \emph{n} = 1) that are within the (distance) tolerance are
#' automatically output as best guess responses. This default is based on Damerau's (1964)
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
#' best.guess("bomba", full.dictionary = SemNetDictionaries::animals.dictionary)
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @references 
#' Damerau, F. J. (1964).
#' A technique for computer detection and correction of spelling errors.
#' \emph{Communications of the ACM}, \emph{7}, 171-176.
#' 
#' @export
# Best Guess----
# Updated 12.12.2021
best.guess <- function (word, full.dictionary, dictionary = NULL, tolerance = 1)
{
    # Remove extra characters for spell-check
    word <- gsub("([-])|[[:punct:]]", "\\1", word)
    
    if(!word %in% full.dictionary)
    {
        #grab Damerau-Levenshtein distances
        guess <- as.vector(stringdist::stringdist(word, full.dictionary, method = "dl",
                                                  weight = c(d = .33, # deletion
                                                             i = .33, # insertion
                                                             s = .50, # substitution
                                                             t = .50 # transposition
                                                             )))

        #compute minimum distance
        min.guess <- min(guess)
        
        #grab guesses less than or equal to plus one of minimum
        target <- which(guess <= (min.guess+1))
        
        #target distances
        target.dist <- guess[target]
        
        #check for plural variations
        plur.var <- unlist(lapply(full.dictionary[target],pluralize))
        
        #plural distances
        plur.dist <- stringdist::stringdist(word, plur.var, method = "dl",
                                            weight = c(d = .33, # deletion
                                                       i = .33, # insertion
                                                       s = .50, # substitution
                                                       t = .50 # transposition
                                            ))
        
        if(any(plur.dist<target.dist))
        {
            #update target distances
            target.dist[plur.dist<target.dist] <- plur.dist[plur.dist<target.dist]
        }
        
        #determine if auto-correction is possible
        if(length(which(target.dist<=tolerance))==1)
        {
            #suggest option as best guess
            bestguess <- full.dictionary[target[which(target.dist<=tolerance)]]
            
        }else if(length(target.dist) != 1)
        {
            # check monikers, phoenetic and qwerty distance
            
            # check for monikers
            if(!is.null(dictionary))
            {
                if(any(dictionary %in% SemNetDictionaries::dictionaries(TRUE)[-which(SemNetDictionaries::dictionaries(TRUE) == "general")]))
                {
                    # Monikers
                    monix <- dictionary[which(dictionary %in% SemNetDictionaries::dictionaries(TRUE)[-which(SemNetDictionaries::dictionaries(TRUE) == "general")])]
                    
                    # Load monikers
                    monik <- SemNetDictionaries::load.monikers(monix)
                    
                    #grab Damerau-Levenshtein distances
                    guess.monix <- as.vector(stringdist::stringdist(word, monik, method = "dl",
                                                                    weight = c(d = 1, # deletion
                                                                               i = 1, # insertion
                                                                               s = .75, # substitution
                                                                               t = .5 # transposition
                                                                               )))
                    
                    #compute minimum distance
                    min.guess.monix <- min(guess.monix)
                    
                    #grab guesses less than or equal to plus one of minimum
                    target.monix <- which(guess.monix <= (min.guess.monix+1))
                    
                    #target distances
                    target.dist.monix <- guess.monix[target.monix]
                    
                    #identify target monikers
                    ind <- target.monix[which(target.dist.monix<=tolerance)]
                    
                    #get unique monikers
                    uniq.monix <- unique(gsub("[[:digit:]]+", "", names(monik[ind])))
                }
            }
            
            #check QWERTY distance
            dists <- unlist(lapply(full.dictionary[target[which(target.dist<=tolerance)]],
                                   function(x){qwerty.dist(x, word)}))
            
            #check if monikers exist
            if(exists("uniq.monix", envir = environment()))
            {
                #see if monikers or qwerty distance has solution
                if(length(uniq.monix) == 1 || length(which(dists <= 1)) == 1)
                {
                    if(length(uniq.monix == 1))
                    {bestguess <- uniq.monix
                    }else if(length(which(dists <= 1)) == 1)
                    {bestguess <- full.dictionary[target[which(target.dist<=tolerance)]][which(dists <= 1)]}
                }
                
            }else{
                
                #try to give back qwerty distance
                if(length(which(dists <= 1)) == 1)
                {bestguess <- full.dictionary[target[which(target.dist<=tolerance)]][which(dists <= 1)]}
            }
        }
        
        if(!exists("bestguess", envir = environment()))
        {
            #order by least to greatest DL value
            target.ord <- target[order(target.dist,decreasing = FALSE)]
            
            #order guesses by guess values (only first 10)
            if(length(target.ord) > 10)
            {bestguess <- full.dictionary[target.ord][1:10]
            }else{bestguess <- full.dictionary[target.ord]}
        }
        
    }else{bestguess <- word}
    
    return(bestguess)
}
#----