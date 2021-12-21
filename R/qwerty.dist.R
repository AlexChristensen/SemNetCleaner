#' QWERTY Distance for Same Length Words
#' 
#' @description Computes QWERTY Distance for words that have
#' the same number of characters. Distance is computed based on
#' the number of keys a character is away from another character
#' on a QWERTY keyboard
#' 
#' @param wordA Character vector.
#' Word to be compared
#' 
#' @param wordB Character vector.
#' Word to be compared
#' 
#' @return Numeric value for distance between \code{wordA} and \code{wordB}
#' 
#' @examples
#' #Identical values for Damerau-Levenshtein 
#' stringdist::stringdist("big", "pig", method="dl")
#' 
#' stringdist::stringdist("big", "bug", method="dl")
#' 
#' #Different distances for QWERTY
#' qwerty.dist("big", "pig")
#' 
#' qwerty.dist("big", "bug") # Probably meant to type "bug" 
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#QWERTY Distance----
#Updated 21.12.2021
qwerty.dist <- function(wordA, wordB)
{
  # Remove diacritic characters
  wordA <- stringi::stri_trans_general(wordA, "Latin-ASCII")
  wordB <- stringi::stri_trans_general(wordB, "Latin-ASCII")
  
  if(nchar(wordA)==nchar(wordB))
  {
    #Keyboard structure
    #Taken from <https://stackoverflow.com/questions/43946912/calculating-levenshtein-distance-permitting-qwerty-errors-in-r>
    m <- structure(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0, 1, 1, 2, 2, 3, 
                     4, 5, 6, 4, 5, 6, 7, 8, 3, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
                     2, 1, 2, 1, 2, 1, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2),
                   .Dim = c(27L,2L),
                   .Dimnames = list(c("q", "w", "e", "r", "t", "y", "u", "i","o", "p",
                                      "a", "z", "s", "x", "d", "c", "f", "b", "m", "j", "g",
                                      "h", "j", "k", "l", "v", "n"), c("x", "y")))
    
    #Compile qwerty locations
    keyb <- sweep(m, 2, c(1, -1), "*")
    keyb <- keyb[-which(duplicated(row.names(keyb))),]
    keyb["m","x"] <- 6
    
    #Add keys
    keyb <- rbind(
      keyb,
      c(4,-3), # space
      c(9,1), # hyphen
      c(9,1), # underscore
      c(10,-1), # apostrophe
      c(10,-1), # quote
      c(7,-2), # comma
      c(8,-2), # period
      c(9,-2), # slash
      c(9,-2) # question mark
    )
    row.names(keyb)[27:nrow(keyb)] <- c(
      " ", "-", "_",
      "'", '"',
      ",", ".",
      "/", "?"
    )
    
    #Initialize targets matrix
    A <- matrix(0,nrow=nchar(wordA),ncol=2)
    B <- A
    sum.dist <- vector("numeric",length=nchar(wordA))
    
    count <- 0
    
    for(i in 1:nchar(wordA))
    {
      count <- count + 1
      
      #Locations for wordA and wordB
      A[count,] <- keyb[substr(wordA,i,i),]
      B[count,] <- keyb[substr(wordB,i,i),]
      
      #Compute distances
      sum.dist[count] <- sum(abs(apply(rbind(A[count,],B[count,]),2,diff)))
    }
    
    #Compute distances
    dist <- sum(sum.dist)
  }else{dist <- NA}
  
  return(dist)
}
#----