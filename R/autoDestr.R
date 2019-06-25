#' Automated De-string of Responses
#' @description Automated de-string responses after performing
#' \code{\link[SemNetCleaner]{textcleaner}}.
#' This function is to streamlines the de-stringing of
#' like responses with other like responses (e.g., "roaches" with "cockroaches")
#' into one singular function.
#' 
#' @param rmat Binary matrix.
#' A \code{\link[SemNetCleaner]{textcleaner}} filtered response matrix
#' 
#' @param char A numeric value.
#' Minimum number of characters in a string to be
#' checked for by \code{\link[SemNetCleaner]{destr}}.
#' Defaults to \code{10}
#' 
#' @return A list containing two objects:
#' 
#' \item{rmat}{A response matrix that has been de-stringed}
#' 
#' \item{string}{A list containing the responses that were de-stringed.
#' The row number is supplied for each case that was affected.
#' This can be used to replicate the de-stringing process and to keep track of changes more generally}
#' 
#' @examples
#' #create example stringed responses
#' stringed <- cbind(rowSums(cbind(rmat[,c(1,2)])),convmat)
#' 
#' #change name to stringed name
#' colnames(stringed)[1] <- "alligator.ant"
#' 
#' \dontrun{
#' 
#' #text cleaned
#' clean <- textcleaner(rmat)
#' 
#' #automated de-string
#' convmat <- autoDestr(clean, 10)
#' }
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Automated De-string Function----
autoDestr <- function (rmat, char = 10)
{
    results <- list()
    results$string[["all ids"]] <- 1:nrow(rmat)
    
    #potential de-string candidates
    pot <- colnames(rmat)[which(nchar(colnames(rmat))>char)]
    
    for(i in 1:length(pot))
    {
        print(pot[i])
        ans <- menu(c("Yes","No"),title="De-string response?")
        
        if(ans==1)
        {
            ans2 <- menu(c("space","period","comma","backslash","other","don't de-string"),title="Seperating character:")
            
            if(ans2==1)
            {sep <- " "
            }else if(ans2==2)
            {sep <- "."
            }else if(ans2==3)
            {sep <- ","
            }else if(ans2==4)
            {sep <- "BACKSLASH"
            }else if(ans2==5)
            {sep <- readline("Type seperator: ")}
            
            if(ans2<6)
            {
                output <- destr(rmat, pot[i], sep = sep)
                rmat <- output$rmat
                results$string[[pot[i]]] <- rbind(output$part,output$added,output$removed)
                row.names(results$string[[pot[i]]]) <- c("participant","added","removed")
                
                if(any(is.na(results$string[[pot[i]]])))
                {results$string[[pot[i]]] <- as.data.frame(na.omit(results$string[[pot[i]]]))}
            }
        }
    }
    
    results$rmat <- rmat
    results$string[["all ids"]] <- NULL
    
    return(results)
}
#----