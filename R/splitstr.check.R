#' Split String Check
#' @description A wrapper function for spell-checking
#' (ensures next word does not belong to the previous)
#' 
#' @param split A split string to check
#' 
#' @param base Database to check
#' 
#' @return Returns the string as is or with the selected responses merged
#' 
#' @examples 
#' #create long word vector
#' vec <- "bombay opossum guinea pig horse cow"
#' 
#' #split vector
#' split <- strsplit(vec, " ")[[1]]
#' 
#' \donttest{
#' splitstr.check(split, animals.database)
#' }
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Split String Check
splitstr.check <- function (split, base)
{
    len <- length(split)
    
    newstring <- split
    
    remove <- vector("logical",length=len)
    
    for(i in 1:(len-1))
    {
        comb <- paste(split[i],split[i+1],collapse = " ")
        
        if(!is.na(match(comb,base)))
        {
            repl <- base[which(comb==base)]
            
            print(split)
            ans <- menu(c(repl,"separate"),title=paste('Should "',split[i],'" and "',split[i+1],'" be combined?',sep = ""))
            
            if(ans==1)
            {
                newstring[i] <- repl
                remove[i + 1] <- TRUE
            }
        }else{next}
    }
    
    if(sum(remove)!=0)
    {return(newstring[!remove])
    }else{return(split)}
}
#----