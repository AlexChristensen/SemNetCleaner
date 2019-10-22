#' Converts Words to their Plural Form
#' @description A function to change words to their plural form.
#' The rules for converting words to their plural forms
#' are based on the grammar rules found here:
#' \href{https://www.grammarly.com/blog/plural-nouns/}{https://www.grammarly.com/blog/plural-nouns/}.
#' This function handles most special cases and some irregular cases (see examples)
#' but caution is necessary. If no plural form is identified, then the original
#' word is returned.
#' 
#' @param word A word
#' 
#' @return Returns the word in singular form, unless a plural form
#' could not be found (then the original word is returned)
#' 
#' @examples
#' # Handles any prototypical cases
#' "dogs"
#' pluralize("dog")
#' 
#' "foxes"
#' pluralize("fox")
#' 
#' "wolves"
#' pluralize("wolf")
#' 
#' "octopi"
#' pluralize("octopus")
#' 
#' "taxa"
#' pluralize("taxon")
#' 
#' # And most special cases:
#' "wives"
#' pluralize("wife")
#' 
#' "roofs"
#' pluralize("roof")
#' 
#' "photos"
#' pluralize("photo")
#' 
#' # And some irregular cases:
#' "children"
#' pluralize("child")
#' 
#' "teeth"
#' pluralize("tooth")
#' 
#' "mice"
#' pluralize("mouse")
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Pluralarize
pluralize <- function (word)
{
    #original word
    orig.word <- word
    
    #general dictionary to check against
    checker <- SemNetDictionaries::general.dictionary
    
    #changed
    chn <- FALSE
    
    #irregular cases
    word <- switch(word,
                   child = "children",
                   goose = "geese",
                   man = "men",
                   woman = "women",
                   tooth = "teeth",
                   foot = "feet",
                   mouse = "mice",
                   person = "people",
                   louse = "lice")
    
    if(is.null(word))
    {word <- orig.word
    }else{chn <- TRUE}
    
    #identify last two letters
    last.lets <- substr(word,nchar(word)-1,nchar(word))
    
    if(!chn)
    {
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
            {
                word <- paste(word,"es",sep="",collapse="")
                
                if("s" == last.let)
                {
                    if(!word %in% checker)
                    {
                        #check for 'ses' ending
                        word <- paste(orig.word,"ses",sep="",collapse="")
                    }
                }else if("z" == last.let)
                {
                    if(!word %in% checker)
                    {
                        #check for 'zes' ending
                        word <- paste(orig.word,"zes",sep="",collapse="")
                    }
                }
                    
            }else if(any(c("f")==last.let))
            {
                #remove 'f'
                word <- substr(word,1,nchar(word)-1)
                #add 'ves'
                word <- paste(word,"ves",sep="",collapse="")
                
                if(!word %in% checker)
                {
                    #check for 's' ending
                    word <- paste(orig.word,"s",sep="",collapse="")
                }
                
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
            {
                word <- paste(word,"es",sep="",collapse="")
                
                if(!word %in% checker)
                {
                    #check for 's' ending
                    word <- paste(orig.word,"s",sep="",collapse="")
                }
                
            }else{word <- paste(orig.word,"s",sep="",collapse="")}
        }
    }
    
    if(!word %in% checker)
    {return(orig.word)
    }else{return(word)}
}
#----