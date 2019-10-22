#' Detect Duplicate Matches
#' @description A wrapper function for \code{\link[SemNetCleaner]{correct.changes}}.
#' It returns the opposite values of what the name of the function suggests --
#' that is, \code{FALSE} for duplicates and \code{TRUE} for non-duplicates
#' 
#' @param tc.obj A \code{\link[SemNetCleaner]{textcleaner}} object
#' 
#' @param part Participant ID
#' 
#' @param target Target response for correcting the change
#' (see \code{old} argument in \code{\link[SemNetCleaner]{correct.changes}})
#' 
#' @return Returns \code{FALSE} for responses that have been identified
#' twice in either the participant's original responses
#' (i.e., \code{tc.obj$responses$orig}) or in their changed responses
#' (i.e., \code{tc.obj$partChanges}). Returns \code{TRUE} if response is not given
#' 
#' @examples
#' # Toy example
#' raw <- open.animals[c(1:10),-c(1:3)]
#' 
#' # Clean and prepocess data
#' clean <- textcleaner(raw, partBY = "row", dictionary = "animals")
#' 
#' # Check for duplicate match
#' dup.match(clean, 1, 1)
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Duplicate match function
dup.match <- function (tc.obj, part, target)
{
    orig <- tc.obj$responses$orig[part,]
    chn <- tc.obj$partChanges[[part]]
    
    if(!is.matrix(chn))
    {
        chn <- as.matrix(chn)
        
        if("from" %in% row.names(chn))
        {chn <- t(chn)}
    }
    
    poss <- chn[target,-1]
    
    rm.row <- chn[-target,]
    
    res <- colSums(rbind(poss %in% rm.row,poss %in% orig))
    
    return(ifelse(res>=1,FALSE,TRUE))
}
#----