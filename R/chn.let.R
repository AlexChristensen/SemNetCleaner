#' Change Letter
#' @description A wrapper function for spell-checking
#' (swaps each letter of the word with another nearby letter)
#' 
#' @param word Word to swap letters
#' 
#' @return Returns a vector with each letter swapped for another
#' 
#' @examples 
#' chn.let("bombae")
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Change letter function
chn.let <- function (word)
{
    split <- strsplit(word,"")[[1]]
    
    respl <- split
    
    len <- length(split)
    
    chn.list <- list()
    
    for(i in 1:len)
    {
        split <- respl
        
        if(split[i]==" ")
        {next}
        
        repl <- length(letters)
        
        chn.list[[i]] <- list()
        
        for(k in 1:repl)
        {
            split[i] <- letters[k]
            
            chn <- paste(split,collapse="")
            
            chn.list[[i]][k] <- chn
        }
    }
    
    vec <- unlist(chn.list)
    
    return(vec)
}
#----
