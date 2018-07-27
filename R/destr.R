#' De-string Responses
#' @description De-string responses after performing \link[SemNetCleaner]{textcleaner}
#' 
#' @param rmat A semnetcleaner filtered response matrix
#' 
#' @param column The column number or name of the stringed response
#' 
#' @param sep Separating string character (e.g., " ", ".", ",").
#' Must be input as a character
#' 
#' @return A list containing four objects:
#' 
#' \item{rmat}{A response matrix that has been de-stringed}
#' 
#' \item{part}{The row number is supplied for each case that was affected.
#' This can be used to replicate the de-stringing process and to keep track of changes more generally}
#' 
#' \item{added}{Stringed responses that were added to the response matrix}
#' 
#' \item{removed}{Stringed responses that were removed from the response matrix}
#' 
#' @examples
#' #create example stringed responses
#' stringed <- cbind(rowSums(cbind(rmat[,c(1,2)])),convmat)
#' 
#' #change name to stringed name
#' colnames(stringed)[1] <- "alligator.ant"
#' 
#' #de-string
#' convmat <- destr(stringed, 1, ".")
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#De-string Function----
destr <- function (rmat, column, sep)
{
    if(is.character(column))
    {column <- which(colnames(rmat)==column)}
    
    part <- which(rmat[,column]==1)
    
    replace <- unlist(strsplit(colnames(rmat[column]),paste("[",sep,"]",sep="")))
    
    included <- replace
    
    output <- list()
    
    na.included <- NULL
    
    if(any(is.na(match(replace,colnames(rmat)))))
    {
        message(paste(replace[which(is.na(match(replace,colnames(rmat))))],"is not a response",sep=" ",collapse="\n"))
        
        included <- replace[-which(is.na(match(replace,colnames(rmat))))]
        na.included <- replace[which(is.na(match(replace,colnames(rmat))))]
    }
    
    rmat[match(included,colnames(rmat))][which(rmat[column]!=0),] <- 1
    
    if(!is.null(na.included))
    {
        n <- length(na.included)
        #potential additional response candidates
        pot <- replace[which(is.na(match(replace,colnames(rmat))))]
        
        added <- vector("list",length=n)
        removed <- vector("list",length=n)
        
        for(i in 1:n)
        {
            print(pot[i])
            ans <- menu(c("Yes","No"),title = "Is this a valid response")
            if(ans==1)
            {
                new <- vector(mode="numeric",length = nrow(rmat))
                rmat <- cbind(rmat,new)
                colnames(rmat)[which(colnames(rmat)=="new")] <- na.included[i]
                rmat[match(na.included[i],colnames(rmat))][which(rmat[column]!=0),] <- 1
                message(paste("A new response column has been created for: ", na.included[i]))
                
                added[i] <- na.included[i]
            }else if(ans==2)
            {
                removed[i] <- na.included[i]
            }
        }
        
        output$added <- unlist(added)
        output$removed <- unlist(removed)
    }
    
    if(is.character(column))
    {rm.col <- which(colnames(rmat)==column)
    }else if(is.numeric(column)){rm.col <- column}
    
    rmat <- rmat[,-rm.col]
    
    rmat <- rmat[,order(colnames(rmat))]
    
    output$rmat <- rmat
    output$part <- part
    
    if(!"added" %in% names(output))
    {output$added <- NA}
    
    if(!"removed" %in% names(output))
    {output$removed <- NA}
    
    return(output)
}
#----