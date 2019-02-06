#' Equate Multiple Groups
#' @description An automated cleaning function for matching groups' responses.
#' Handles multiple datasets
#' 
#' @param ... Datasets to be equated
#' 
#' @return This function returns a list containing equated datasets
#' in the order they were input. They are labeled dataset (1, ..., n)
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