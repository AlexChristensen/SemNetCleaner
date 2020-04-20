#' Binary Responses to Character Responses
#' @description Converts the binary response matrix into characters for each participant
#' 
#' @param rmat Binary matrix.
#' A binarized response matrix of verbal fluency or linguistic data
#' 
#' @param to.data.frame Boolean.
#' Should output be a data frame where participants are columns?
#' Defaults to \code{FALSE}.
#' Set to \code{TRUE} to convert output to data frame
#' 
#' @return A list containing objects for each participant and their responses
#' 
#' @examples
#' # Toy example
#' raw <- open.animals[c(1:10),-c(1:3)]
#' 
#' if(interactive())
#' {
#'   # Clean and prepocess data
#'   clean <- textcleaner(open.animals[,-c(1:2)], partBY = "row", dictionary = "animals")
#' 
#'   # Change binary response matrix to word response matrix
#'   charmat <- bin2resp(clean$responses$binary)
#' }
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Binary to Response----
bin2resp <- function (rmat, to.data.frame = FALSE)
{
    #grab response names
    name <- colnames(rmat)
    
    #number of responses
    n <- ncol(rmat)
    
    #initialize matrix
    mat <- matrix(NA,nrow=nrow(rmat),ncol=ncol(rmat))
    
    #loop for each name
    for(i in 1:n)
    {mat[,i] <- ifelse(rmat[,i]==1,name[i],NA)}
    
    #number of participants
    p <- nrow(rmat)
    
    #initialize participant list
    part <- list()
    
    #loop for each participant
    for(j in 1:p)
    {
        resps <- na.omit(mat[j,])
        attributes(resps)$na.action <- NULL
        part[[row.names(rmat)[j]]] <- resps
    }
    
    #convert output to data frame
    if(to.data.frame)
    {
        nlen <- vector("numeric",length=length(part))
        
        num <- length(nlen)
        
        for(i in 1:num)
        {nlen[i] <- length(part[[i]])}
        
        mlen <- max(nlen)
        
        part.df <- matrix("",nrow=mlen,ncol=num)
        
        for(i in 1:num)
        {
            reps <- mlen - nlen[i]
            
            part.df[,i] <- c(unlist(part[[i]]),rep("",reps))
        }
        
        part <- as.data.frame(part.df)
        colnames(part) <- row.names(rmat)
        part <- t(part)
    }
    
    return(part)
}
#----