#' Converts Words to their Singular Form
#' @description A function to change words to their singular form.
#' The rules for converting words to their singular forms
#' are based on the \strong{\emph{inverse}} of the grammar rules.
#' This function handles most special cases and some irregular cases (see examples)
#' but caution is necessary. If no singular form is identified, then the original
#' word is returned.
#'
#' @param word Character.
#' A word
#'
#' @param dictionary Boolean.
#' Should dictionary be used to verify word exists?
#' Default to \code{TRUE}
#'
#' @return Returns the word in singular form, unless a singular form
#' could not be found (then the original word is returned)
#'
#' @examples
#' # Handles any prototypical cases
#' # "dog"
#' singularize("dogs")
#'
#' # "fox"
#' singularize("foxes")
#'
#' # "wolf"
#' singularize("wolves")
#'
#' # "octopus"
#' singularize("octopi")
#'
#' # "taxon"
#' singularize("taxa")
#'
#' # And most special cases:
#' # "wife"
#' singularize("wives")
#'
#' # "fez"
#' singularize("fezzes")
#'
#' # "roof"
#' singularize("roofs")
#'
#' # "photo"
#' singularize("photos")
#'
#' # And some irregular cases:
#' # "child"
#' singularize("children")
#'
#' # "tooth"
#' singularize("teeth")
#'
#' # "mouse"
#' singularize("mice")
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
#Singularize
# Updated 12.16.2021
singularize <- function(word, dictionary = TRUE)
{
    #check for multiple words
    spl <- unlist(strsplit(word, " "))

    if(length(spl) > 1)
    {
        word <- spl[length(spl)]
        multiple <- TRUE
    }else{multiple <- FALSE}

    #original word
    orig.word <- word

    #general dictionary to check against
    checker <- SemNetDictionaries::coca.dictionary

    #changed
    chn <- FALSE

    #irregular cases
    word <- switch(
        word,
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
        bees = "bee",
        asses = "ass"
    )

    if(is.null(word))
    {word <- orig.word
    }else{chn <- TRUE}

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

                chn <- TRUE

                if(!word %in% checker)
                {
                    word <- orig.word
                    chn <- FALSE
                }

            }else if(last.lets == "i")
            {
                #remove 'i'
                word <- substr(word,1,nchar(word)-1)
                #add 'us'
                word <- paste(word,"us",sep="",collapse="")

                chn <- TRUE

                if(!word %in% checker)
                {
                    word <- orig.word
                    chn <- FALSE
                }

            }else if(last.lets == "a")
            {
                #remove 'a'
                word <- substr(word,1,nchar(word)-1)
                #add 'on'
                word <- paste(word,"on",sep="",collapse="")

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

            chn <- TRUE

            if(!word %in% checker)
            {
                #add 'is'
                word <- paste(word,"is",sep="",collapse="")
            }

            if(!word %in% checker)
            {
                word <- orig.word
                chn <- FALSE
            }
        }

    }

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

                if(!word %in% checker)
                {
                    word <- orig.word
                    chn <- FALSE
                }

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

                if(!word %in% checker)
                {
                    word <- orig.word
                    chn <- FALSE
                }

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

    #return word
    if(isTRUE(dictionary)){

        #check for word in dictionary
        if(!word %in% checker){
            if(isTRUE(multiple))
            {orig.word <- paste(spl, collapse = " ")}

            return(orig.word)
        }else{

            if(multiple)
            {
                spl[length(spl)] <- word
                word <- paste(spl, collapse = " ")
            }

            return(word)
        }

    }else{

        if(multiple)
        {
            spl[length(spl)] <- word
            word <- paste(spl, collapse = " ")
        }

        return(word)

    }
}
#----