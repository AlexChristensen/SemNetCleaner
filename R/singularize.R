#' Converts Words to their Singular Form
#' @description A function to de-pluralize '-s' and '-es' words. This
#' function is very basic and does not have many rules. The function
#' simply removes '-s' or '-es' from words. 
#' 
#' @param word A word
#' 
#' @return Returns the word in singular form
#' 
#' @examples
#' "fox"
#' singularize("foxes")
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Singularize
singularize <- function(word)
{
    if(!is.na(word))
    {
        word <- as.character(word)

        sing <- vector(length=length(word))

        if(length(word)>1)
        {
            for(i in 1:length(word))
            {
                Splural <- substring(word[i],nchar(word[i]),nchar(word[i]))
                ESplural <- substring(word[i],nchar(word[i])-1,nchar(word[i]))
        
                if(ESplural=="es")
                {sing[i] <- substring(word[i],1,nchar(word[i])-2)
                }else{sing[i] <- word[i]}
                
                if(Splural=="s")
                {sing[i] <- substring(word[i],1,nchar(word[i])-1)
                }else{sing[i] <- word[i]}
            }
        }else{
                Splural <- substring(word,nchar(word),nchar(word))
                ESplural <- substring(word,nchar(word)-1,nchar(word))
    
                if(ESplural=="es")
                {sing <- substring(word,1,nchar(word)-2)
                }else{sing <- word}
                
                if(Splural=="s")
                {sing <- substring(word,1,nchar(word)-1)
                }else{sing <- word}
            }
    }else(sing <- word)
    
    return(sing)
}
#----