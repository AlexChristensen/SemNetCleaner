#' Combine Words Wrapper
#' @description A wrapper function to combine words that are found in dictionary
#' (e.g., "star" "fish" --> "starfish")
#' 
#' @param vec Vector.
#' A vector with words to potentially be combined
#' 
#' @param dictionary A dictionary to look for (combined) word in.
#' See \code{\link{SemNetDictionaries}}
#' 
#' @return A vector with responses combined based on dictionary entries
#' 
#' @examples
#' # Convert "star fish" to "starfish"
#' combine.responses("star fish", SemNetDictionaries::animals.dictionary)
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Combine Responses Fucntion
combine.responses <- function (vec, dictionary)
{
    if(all(!is.na(vec)))
    {
        #number of responses
        len <- length(vec)
        
        #initialize vector list
        vec.list <- list()
        
        #split strings to check for multiple responses
        for(i in 1:len)
        {vec.list[[i]] <- unlist(strsplit(vec[i],split=" "))}
        
        #initalize check vector
        check <- unlist(lapply(vec.list,length))
        
        #index responses with more than one response
        index <- which(check>1)
        
        #begin loop to potentially combine response
        for(i in index)
        {
            #continue boolean
            cont <- TRUE
            
            #identify target vector
            target <- vec.list[[i]]
            
            #remove NAs
            if(any(target=="NA"))
            {
                #identify which to remove
                rem <- which(target=="NA")
                
                #remove
                target <- target[-rem]
            }
            
            #if length equal or less than two
            if(length(target)<=2)
            {
                #check dictionary
                if(paste(target,collapse=" ") %in% dictionary)
                {
                    #put two word response into target
                    target <- paste(target,collapse=" ")
                }else{
                    
                    #temp before changing target
                    temp <- paste(target,sep="",collapse="")
                    
                    #check dictionary
                    if(temp %in% dictionary)
                    {target <- temp}#combine response
                }
            }
            
            #replace target in vec.list
            vec.list[[i]] <- target
        }
        
        #return vector
        ret.vec <- vector("character",length=len)
        
        for(i in 1:len)
        {ret.vec[i] <- paste(unlist(vec.list[[i]]),collapse=" ")}
        
        #change character "NA" to actually missing NA
        ret.vec <- ifelse(ret.vec=="NA",NA,ret.vec)
    }else{ret.vec <- vec}
    
    return(ret.vec)
}
#----