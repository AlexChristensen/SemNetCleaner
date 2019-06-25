#' Equate Group Responses
#' @description In general, this function serves as a wrapper for
#' \code{\link[SemNetCleaner]{equate.multi}}. This function equates the
#' responses between two binary response matrices (e.g., two groups)
#' by only keeping the column names that match between both matrices.
#' 
#' @param rmatA Binary matrix.
#' A \link[SemNetCleaner]{textcleaner} filtered response matrix
#' for binary matrix A
#' 
#' @param rmatB Binary matrix. 
#' A \link[SemNetCleaner]{textcleaner} filtered response matrix
#' for binary matrix B
#' 
#' @return A list containing binary matrices that have matching column
#' names (matching response):
#' 
#' \item{rmatA}{Binary matrix with matched responses for \code{rmatA}}
#' 
#' \item{rmatB}{Binary matrix with matched responses for \code{rmatB}}
#' 
#' @examples
#' #finalize rmatA
#' finalCmat <- finalize(convmat)
#' 
#' #finalize rmatB
#' finalRmat <- finalize(rmat)
#'
#' #equate rmatA and rmatB
#' eq1 <- equate(finalCmat,finalRmat)
#' 
#' #obtain respective equated response matrices
#' eqCmat <- eq1$rmatA
#' eqRmat <- eq1$rmatB
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
# Equate----
equate <- function (rmatA, rmatB)
{
    while(length(colnames(rmatA))!=length(colnames(rmatB)))
    {
        if(length(colnames(rmatA))>=length(colnames(rmatB)))
        {rmatA<-rmatA[,(!is.na(match(colnames(rmatA),colnames(rmatB))))]
        }else if(length(colnames(rmatB))>=length(colnames(rmatA)))
        {rmatB<-rmatB[,(!is.na(match(colnames(rmatB),colnames(rmatA))))]
        }else if(all(match(colnames(rmatA),colnames(rmatB))))
        {print("Responses match")}
    }
    
    return(list(rmatA=rmatA,rmatB=rmatB))
}
#----