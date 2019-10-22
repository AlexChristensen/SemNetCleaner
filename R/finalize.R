#' Finalize Response Matrix
#' @description Finalizes the response matrix by keeping
#' responses that are given by a certain number of people
#' 
#' @param rmat Binary matrix.
#' A \link[SemNetCleaner]{textcleaner} filtered response matrix
#' 
#' @param minCase Numeric.
#' Minimum number of cases to produce a response
#' 
#' @return A binary response matrix with responses
#' given by at least \code{minCase} people
#' 
#' @examples
#' # Toy example
#' raw <- open.animals[c(1:10),-c(1:3)]
#' 
#' # Clean and prepocess data
#' clean <- textcleaner(raw, partBY = "row", dictionary = "animals")
#' 
#' # Obtain binary data
#' bin <- clean$binary
#' 
#' # Finalize mat1
#' mat1 <- finalize(bin)
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Finalize Function----
finalize <- function (rmat, minCase = 2)
{
    fmat <- rmat[which(colSums(rmat)>=minCase)]
    
    return(fmat)
}
#----