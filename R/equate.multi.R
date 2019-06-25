#' Equate Multiple Groups
#' @description A function to apply \code{\link[SemNetCleaner]{equate}}
#' to multiple response matrices
#' 
#' @param ... A list.
#' A list of response matrices to be equated
#' 
#' @return This function returns a list containing the
#' equated binary response matrices in the order they were input.
#' The response matrices are labeled (\code{1}, \code{2}, \code{...}, \code{n})
#' in the order in which they are input.
#' 
#' @examples 
#' #finalize rmatA
#' finalCmat <- finalize(convmat)
#' 
#' #finalize rmatB
#' finalRmat <- finalize(rmat)
#' 
#' #finalize rmatC
#' finalYmat <- finalize(rmat)
#'
#' #equate rmatA and rmatB
#' eq <- equate.multi(finalCmat,finalRmat,finalYmat)
#' 
#' #obtain respective equated response matrices
#' eqCmat <- eq$dataset1
#' eqRmat <- eq$dataset2
#' eqYmat <- eq$dataset3
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
equate.multi <- function(...)
{
    datalist <- list(...)
    
    len <- length(datalist)
    
    if(len>2)
    {
        first <- datalist[[1]]
        eq <- SemNetCleaner::equate(first,datalist[[2]])$rmatA
        
        for(i in 2:(len-1))
        {eq <- SemNetCleaner::equate(eq,datalist[[(i+1)]])$rmatA}
        
        finlist <- list()
        
        for(j in 1:len)
        {
            nam <- paste("dataset",j,sep="")
            finlist[[nam]] <- equate(eq,datalist[[j]])$rmatB
        }
    }else if(len==2)
    {
        finlist <- equate(datalist[[1]],datalist[[2]])
        names(finlist) <- c("dataset1","dataset2")
    }else{stop("Must be at least two datasets as input")}
    
    return(finlist)
}
#----