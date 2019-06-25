#' Finalize Response Matrix
#' @description Finalizes the response matrix by keeping
#' responses that are given by two or more people
#' 
#' @param rmat Binary matrix.
#' A \link[SemNetCleaner]{textcleaner} filtered response matrix
#' 
#' @param minCase Numeric.
#' Minumum number of cases to produce a response
#' 
#' @return A binary response matrix with responses
#' given by at least \code{minCase} people
#' 
#' @examples
#' \dontrun{
#' 
#' convmat <- autoConverge(rmat)
#' }
#' 
#' finalRmat <- finalize(convmat)
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