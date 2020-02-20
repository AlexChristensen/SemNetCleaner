#' Converts Words to their Singular Form
#' @description A function to change words to their singular form.
#' The rules for converting words to their singular forms
#' are based on the \strong{\emph{inverse}} of the grammar rules found here:
#' \href{https://www.grammarly.com/blog/plural-nouns/}{https://www.grammarly.com/blog/plural-nouns/}.
#' This function handles most special cases and some irregular cases (see examples)
#' but caution is necessary. If no singular form is identified, then the original
#' word is returned.
#' 
#' @param word A word
#' 
#' @return Returns the word in singular form, unless a singular form
#' could not be found (then the original word is returned)
#' 
#' @examples
#' # Handles any prototypical cases
#' "dog"
#' singularize("dogs")
#' 
#' "fox"
#' singularize("foxes")
#' 
#' "wolf"
#' singularize("wolves")
#' 
#' "octopus"
#' singularize("octopi")
#' 
#' "taxon"
#' singularize("taxa")
#' 
#' # And most special cases:
#' "wife"
#' singularize("wives")
#' 
#' "fez"
#' singularize("fezzes")
#' 
#' "roof"
#' singularize("roofs")
#' 
#' "photo"
#' singularize("photos")
#' 
#' # And some irregular cases:
#' "child"
#' singularize("children")
#' 
#' "tooth"
#' singularize("teeth")
#' 
#' "mouse"
#' singularize("mice")
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Singularize
singularize <- function(word)
{
    #original word
    orig.word <- word
    
    #general dictionary to check against
    checker <- SemNetDictionaries::general.dictionary
    
    #changed
    chn <- FALSE
    
    #irregular cases
    word <- switch(word,
                   children = "child",
                   geese = "goose",
                   men = "man",
                   women = "woman",
                   teeth = "tooth",
                   feet = "foot",
                   mice = "mouse",
                   people = "person",
                   lice = "louse",
                   valves = "valve",
                   trees = "tree",
                   scribbles = "scribble",
                   peduncles = "peduncle",
                   mallees = "mallee",
                   panicles = "panicle",
                   ridges = "ridge",
                   petioles = "petiole",
                   angles = "angle",
                   bristles = "bristle",
                   edges = "edge",
                   fissures = "fissure",
                   sutures = "suture",
                   occurrences = "occurrence",
                   bees = "bee")
    
    if(is.null(word))
    {word <- orig.word
    }else{chn <- TRUE}
    
    #identify common plurals
    ## last three letters
    last.lets <- substr(word,nchar(word)-2,nchar(word))
    
    if(!chn)
    {
        if(any(last.lets == c("ves","ies","zes","ses")))
        {
            if(last.lets == "ves")
            {
                #remove 'ves'
                word <- substr(word,1,nchar(word)-3)
                #add 'f'
                word <- paste(word,"f",sep="",collapse="")
                
                if(!word %in% checker)
                {
                    #add 'e'
                    word <- paste(word,"e",sep="",collapse="")
                }
                
                chn <- TRUE
                
            }else if(last.lets == "ies")
            {
                #remove 'ies'
                word <- substr(word,1,nchar(word)-3)
                #add 'y'
                word <- paste(word,"y",sep="",collapse="")
                
                if(!word %in% checker)
                {
                    #check for 'ie' ending
                    word <- substr(word,1,nchar(word)-1)
                    word <- paste(word,"ie",sep="",collapse="")
                }
                
                chn <- TRUE
                
            }else if(any(last.lets == c("zes","ses")))
            {
                #remove 'zes' or 'ses'
                word <- substr(word,1,nchar(word)-3)
                chn <- TRUE
                
                if(!word %in% checker)
                {
                    word <- orig.word
                    chn <- FALSE
                }
            }
        }
    }
    
    ## last two letters
    last.lets <- substr(word,nchar(word)-1,nchar(word))
    
    if(!chn)
    {
        if(last.lets == "es")
        {
            #remove 'es'
            word <- substr(word,1,nchar(word)-2)
            
            if(!word %in% checker)
            {
                #add 'is'
                word <- paste(word,"is",sep="",collapse="")
            }
            
            chn <- TRUE
        }
    }
    
    ## last letter
    last.lets <- substr(word,nchar(word),nchar(word))
    
    if(!chn)
    {
        if(any(last.lets == c("s","i","a")))
        {
            if(last.lets == "s")
            {
                #remove 's'
                word <- substr(word,1,nchar(word)-1)
            }else if(last.lets == "i")
            {
                #remove 'i'
                word <- substr(word,1,nchar(word)-1)
                #add 'us'
                word <- paste(word,"us",sep="",collapse="")
            }else if(last.lets == "a")
            {
                #remove 'a'
                word <- substr(word,1,nchar(word)-1)
                #add 'on'
                word <- paste(word,"on",sep="",collapse="")
            }
        }
    }
    
    if(!word %in% checker)
    {return(orig.word)
    }else{return(word)}
}
#----
