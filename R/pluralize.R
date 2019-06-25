#' Converts Words to their Plural Form
#' @description A function to pluralize words.
#' The rules for converting words to their plural forms
#' are based on the grammar rules found here:
#' \href{https://www.grammarly.com/blog/plural-nouns/}{https://www.grammarly.com/blog/plural-nouns/}.
#' This function does not handle special cases, so particular care is necessary.
#' 
#' @param word A word
#' 
#' @return Returns the word in singular form
#' 
#' @examples
#' "dog"
#' trial[1,1]
#' 
#' "dogs"
#' pluralize(trial[1,1])
#' 
#' "fox"
#' trial[15,1]
#' 
#' "foxes"
#' pluralize(trial[15,1])
#' 
#' "wolf"
#' trial[14,2]
#' 
#' "wolves"
#' pluralize(trial[14,2])
#' 
#' "octopi"
#' pluralize("octopus")
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Pluralarize
pluralize <- function (word)
{
    #identify last two letters
    last.lets <- substr(word,nchar(word)-1,nchar(word))
    
    if(any(c("ss","sh","ch")==last.lets))
    {word <- paste(word,"es",sep="",collapse="")
    }else if(any(c("fe")==last.lets))
    {
        #remove 'fe'
        word <- substr(word,1,nchar(word)-2)
        #add 'ves'
        word <- paste(word,"ves",sep="",collapse="")
    }else if(any(c("us")==last.lets))
    {
        #remove 'us'
        word <- substr(word,1,nchar(word)-2)
        #add 'i'
        word <- paste(word,"i",sep="",collapse="")
    }else if(any(c("is")==last.lets))
    {
        #remove 'is'
        word <- substr(word,1,nchar(word)-2)
        #add 'es'
        word <- paste(word,"es",sep="",collapse="")
    }else if(any(c("on")==last.lets))
    {
        #remove 'on'
        word <- substr(word,1,nchar(word)-2)
        #add 'a'
        word <- paste(word,"a",sep="",collapse="")
    }else{
        #identify last letter
        last.let <- substr(word,nchar(word),nchar(word))
        
        #change to plural based on last letter
        if(any(c("s","x","z")==last.let))
        {word <- paste(word,"es",sep="",collapse="")
        }else if(any(c("f")==last.let))
        {
            #remove 'f'
            word <- substr(word,1,nchar(word)-1)
            #add 'ves'
            word <- paste(word,"ves",sep="",collapse="")
        }else if(any(c("y")==last.let))
        {
            if(any(c("a","e","i","o","u")==substr(word,nchar(word)-1,nchar(word)-1)))
            {word <- paste(word,"s",sep="",collapse="")
            }else{
                #remove 'y'
                word <- substr(word,1,nchar(word)-1)
                #add 'ies'
                word <- paste(word,"ies",sep="",collapse="")
            }
        }else if(any(c("o")==last.let))
        {word <- paste(word,"es",sep="",collapse="")
        }else{word <- paste(word,"s",sep="",collapse="")}
    }
    
    return(word)
}
#----