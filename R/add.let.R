#' Add Letter
#' @description A wrapper function for spell-checking
#' (adds additional nearby letters at each letter of the word)
#' 
#' @param word Word to swap letters
#' 
#' @return Returns a vector with all possible added letter combinations
#' 
#' @examples 
#' 
#' add.let("bombay")
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Add letter function
add.let <- function (word)
{
    split <- strsplit(word,"")[[1]]
    
    respl <- split
    
    len <- length(split)
    
    add.list <- list()
    begin.list <- list()
    
    for(i in 1:len)
    {
        split <- respl
        
        repl <- length(letters)
        
        add.list[[i]] <- list()
        
        for(k in 1:repl)
        {
            split <- respl
            
            comb <- paste(c(split[i],letters[k]),collapse="")
            
            split[i] <- comb
            
            add <- paste(split,collapse="")
            
            add.list[[i]][k] <- add
        }
        
        if(i==1)
        {
            for(j in 1:repl)
            {
                split <- respl
            
                comb <- paste(c(letters[j],split[i]),collapse="")
            
                split[i] <- comb
            
                add <- paste(split,collapse="")
                
                begin.list[[j]] <- add
            }
        }
    }
    
    beg <- unlist(begin.list)
    
    vec <- c(beg,unlist(add.list))
    
    
    return(vec)
}
#----
