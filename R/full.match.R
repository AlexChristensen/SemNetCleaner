#' Wrapper Match Function
#' @description A wrapper function that performs
#' the same operations as match except accounts for
#' \code{NA}
#' 
#' @param vec1 Vector.
#' Must be same length as \code{vec2}
#' 
#' @param vec2 Vector.
#' Must be same length as \code{vec1}
#' 
#' @return Returns a vector the same length
#' as the input vectors of \code{TRUE} and
#' \code{FALSE} for each element across the vectors
#' 
#' @examples
#' # Vector 1
#' vec1 <- c(NA,NA,"cat","dog",NA,0,"porcupine")
#' 
#' # Vector 2
#' vec2 <- c(NA,"bob","alice","dog","prince",0,NA)
#' 
#' # Perform match
#' full.match(vec1, vec2)
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#New Match Function
full.match <- function (vec1, vec2)
{
    #initialize boolean vector
    match.vec <- vector(length=length(vec1))
    
    for(i in 1:length(match.vec))
    {
        #if both are NA
        if(is.na(vec1[i])&&is.na(vec2[i]))
        {match.vec[i] <- TRUE
        }else if(is.na(vec2[i]))
        {match.vec[i] <- FALSE
        }else if(is.na(vec1[i]))
        {match.vec[i] <- FALSE
        }else if(vec1[i]==vec2[i])
        {match.vec[i] <- TRUE}
    }
    
    return(match.vec)
    
}
#----