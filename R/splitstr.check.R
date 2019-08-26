#' Split String Check
#' @description A wrapper function for spell-checking
#' (ensures next word does not belong to the previous)
#' 
#' @param string Character.
#' A string of words (see examples)
#' 
#' @param split Character.
#' A character that should be used to "split"
#' the words input into the \code{string} argument.
#' Defaults to a space (\code{" "})
#' 
#' @param dictionary Dictionary to check.
#' See \code{\link{SemNetDictionaries}}
#' 
#' @param remember Character list.
#' Checks if split string has already been
#' checked (a wrapper argument for \code{\link[SemNetCleaner]{spell.check.dictionary}}).
#' Defaults to an empty list
#' 
#' @return Returns the string as is or with the selected responses merged
#' 
#' @examples 
#' # Create long word vector
#' words <- "bombay opossum guinea pig horse cow"
#' 
#' if(interactive())
#' {splitstr.check(string = words, split = " ", dictionary = SemNetDictionaries::animals.dictionary)}
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Split String Check
splitstr.check <- function (string, split = " ", dictionary, remember = list())
{
    spl <- FALSE
    
    if(length(remember)!=0)
    {
        #identify indices
        rem <- remember[-which(sapply(remember, is.null))]
        
        #identify if already changed
        uniq.chn <- unique(unlist(remember)[which(gsub("[[:digit:]]+", "", names(unlist(remember)))=="after")])
        
        #if string is in already changed, then replace
        #else search for pontential change option
        if(paste(string,collapse=" ") %in% uniq.chn)
        {vec <- paste(string,collapse=" ")
        }else{
            #identify target
            for(i in 1:length(rem))
            {
                #grab first target that matches
                if(all(rem[[i]]$before %in% string))
                {
                    #put remembered correction in vec
                    vec <- rem[[i]]$after
                    break
                }else{vec <- vector()}
            }
        }
    }else{vec <- vector()}
    
    #if split check is not remembered
    if(length(vec)==0)
    {
        #split string into a vector of words
        split.vec <- unlist(strsplit(string, paste("[",split,"]",sep="")))
        
        #number of words in vector
        len <- length(split.vec)
        
        #initialize new string
        newstring <- split.vec
        
        #initialize words in vector to remove
        remove <- vector("logical",length=len)
        
        #main function
        for(i in 1:(len-1))
        {
            #combine word and next word
            comb <- paste(split.vec[i],split.vec[i+1],collapse = " ")
            
            #check if only two words
            if(len==2)
            {
                if(all(unlist(strsplit(comb," ")) %in% dictionary) && !comb %in% dictionary)
                {
                    newstring <- unlist(strsplit(comb," "))
                }else{
                    
                    repl <- comb
                    
                    print(split.vec)
                    
                    ans <- menu(c(paste("combined: ","'",repl,"'",sep=""),paste("separated: ","'",split.vec[i],"'","'",split.vec[i+1],"'",sep="")),title=paste('\nShould "',split.vec[i],'" and "',split.vec[i+1],'" be combined or separated?',sep = ""))
                    
                    if(ans==1)
                    {
                        newstring[i] <- repl
                        remove[i + 1] <- TRUE
                    }
                }
                
                spl <- TRUE
                
            }else if(!is.na(match(comb,dictionary)))
            {
                repl <- dictionary[which(comb==dictionary)]
                
                ans <- menu(c(paste("combined: ","'",repl,"'",sep=""),paste("separated: ","'",split.vec[i],"'","'",split.vec[i+1],"'",sep="")),title=paste('\nShould "',split.vec[i],'" and "',split.vec[i+1],'" be combined or separated?',sep = ""))
                
                if(ans==1)
                {
                    newstring[i] <- repl
                    remove[i + 1] <- TRUE
                }
                
                spl <- TRUE
            }else{next}
        }
        
        if(sum(remove)!=0)
        {vec <- newstring[!remove]
        }else{
            
            if(all(newstring %in% dictionary))
            {vec <- newstring
            }else{
                vec <- unlist(strsplit(newstring,split=" "))
            }
        }
    }
    
    res <- list()
    res$vec <- vec
    res$spl <- spl
    
    return(res)
}
#----