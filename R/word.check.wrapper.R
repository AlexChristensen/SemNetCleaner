#' A Spell-checking wrapper
#' @description A wrapper function to spell-check with menu options
#' 
#' @param word Character.
#' A word to get spell-checked
#' 
#' @param dictionary A dictionary to look for word in (see examples).
#' See \code{\link{SemNetDictionaries}}
#' 
#' @param context Vector.
#' Defaults to \code{NULL}.
#' When a word is inside of a vector of words, then the vector
#' can be input to provide context for whether this word is spelled
#' correctly with other words. For example, "guinea" is spelled correctly
#' but will not be in \code{\link[SemNetDictionaries]{animals.dictionary}}. The vector
#' can be input to determine if "guinea pig" or "guinea fowl" is meant by the
#' participant. The word that is being checked will appear with "<<" and ">>" around
#' it in the context of other words (e.g., \code{bat dog fish <<guinea>> pig rat horse})
#' 
#' @param tolerance Numeric.
#' The distance tolerance set for automatic spell-correction purposes.
#' This function uses the function \code{\link[stringdist]{stringdist}}
#' to compute the \href{https://en.wikipedia.org/wiki/Damerau-Levenshtein_distance}{Damerau-Levenshtein}
#' (DL) distance, which is used to determine potential best guesses.
#' 
#' Unique words (i.e., \emph{n} = 1) that are within the (distance) tolerance are
#' automatically output as best guess responses, which are then passed through
#' \code{\link[SemNetCleaner]{word.check.wrapper}}. If there is more than one word
#' that is within or below the distance tolerance, then these will be provided as potential
#' options.
#' 
#' The recommended and default distance tolerace is \code{tolerance = 1},
#' which only spell corrects a word if there is only one word with a DL distance of 1. 
#' 
#' @return A menu will appear with several options. Here is what is returned with each option:
#' 
#' \item{POTENTIAL RESPONSE}{If a potential response is selected,
#' then the input word is replaced with the potential response}
#' 
#' \item{ADD TO DICTIONARY}{When selected, the input word will be
#' added to the appendix dictionary (see \code{\link[SemNetDictionaries]{append.dictionary}}).
#' The input word will be returned}
#' 
#' \item{TYPE MY OWN}{User will type their own response to replace the input
#' word. If word is not in dictionary, then user will be prompted for whether
#' they would like to add the word to their appendix dictionary
#' (see \code{\link[SemNetDictionaries]{append.dictionary}}).
#' In all cases, the typed word will be returned}
#' 
#' \item{GOOGLE IT}{"Googles" the response in question.
#' A browser will open with the Google search terms: define "RESPONSE"}
#' 
#' \item{BAD RESPONSE}{When selected, \code{NA} will be returned}
#' 
#' \item{BAD STRING}{When selected, a vector of \code{NA} the length of the
#' \code{context} vector will be returned}
#' 
#' \item{SKIP}{When selected, input word will be returned}
#' 
#' @examples
#' #word to check
#' word <- "goat"
#' 
#' \dontrun{
#' 
#' #good response, should be SKIP or "goat"
#' word.check.wrapper(word, SemNetDictionaries::animals.dictionary)
#' 
#' #response needs to be checked
#' word.check.wrapper("gost", SemNetDictionaries::animals.dictionary)
#' 
#' #word in context
#' word.check.wrapper("guinea",SemNetDictionaries::animals.dictionary, c("bat fish guinea pig rat")
#' }
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Spell-checking function
word.check.wrapper <- function (word, dictionary, context = NULL, tolerance = 1)
{
    #a check for later response for saving
    #appendix dictionary in 'spell.check.dictionary' function
    check <- FALSE
    
    if(!is.null(context))
    {target <- which(word==context)} #find word in context
    
    pot <- best.guess(word,dictionary,tolerance) #identify best words
    
    if(length(pot)!=1)
    {
        #QWERTY Distance
        qwert <- vector("numeric",length=length(pot))
        
        for(i in 1:length(pot))
        {qwert[i] <- qwerty.dist(word,pot[i])}
        
        qwert.pot <- pot[which(qwert<=tolerance)]
        
        if(length(qwert.pot)!=1)
        {
            if(is.null(context))
            {
                #print word to user
                cat(word)
            }else{
                
                #before target in context
                if(target!=1)
                {before <- context[1:(target-1)]
                }else{before <- NULL}
                
                #after target in context
                if(target!=length(context))
                {after <- context[(target+1):length(context)]
                }else{after <- NULL}
                
                #print word in context to user
                cat(paste(before,collapse=" "),paste("<<",context[target],">>",sep="",collapse=""),paste(after,collpase=" "))
            }
            
            ans <- length(pot)+3
            
            if(length(context)>1)
            {
                while(ans == length(pot)+3)
                {
                    #ask for response for options
                    ans <- menu(c(pot,"ADD TO DICTIONARY","TYPE MY OWN","GOOGLE IT","BAD RESPONSE","BAD STRING","SKIP"), title = "\n\nPotential responses:")
                    
                    if(ans <= length(pot)) #if an option is selected
                    {
                        #replace word with potential response
                        word <- pot[ans]
                    }else if(ans == length(pot)+1) #add word to dictionary
                    {
                        #add to appendix dictionary
                        suppressWarnings(SemNetDictionaries::append.dictionary(word,save.location="path",path=tempdir()))
                        
                        #load updated appendix dictionary
                        dict <- readRDS(paste(tempdir(),"appendix.dictionary.rds",sep="\\"))
                        
                        #combined with input dictionary
                        dictionary <- sort(unique(c(dictionary,dict)))
                        
                        #a check for later response for saving
                        #appendix dictionary in 'spell.check.dictionary' function
                        check <- TRUE
                        
                    }else if(ans == length(pot)+2) #have user type response
                    {
                        #get response from user
                        ans3 <- readline("Type response: ")
                        
                        if(!ans3 %in% dictionary) #check if typed response is in dictionary
                        {
                            #If not, add ask user if they want to add it to the dictionary
                            ans4 <- menu(c("Yes","No"),title=paste("Add",ans3,"to dictionary?",sep=" "))
                            
                            #If yes, add it to the dictionary
                            if(ans4 == 1)
                            {
                                #add to dictionary
                                suppressWarnings(SemNetDictionaries::append.dictionary(ans3,save.location="path",path=tempdir()))
                                
                                #load updated appendix dictionary
                                dict <- readRDS(paste(tempdir(),"appendix.dictionary.rds",sep="\\"))
                                
                                #combined with input dictionary
                                dictionary <- sort(unique(c(dictionary,dict)))
                                
                                #a check for later response for saving
                                #appendix dictionary in 'spell.check.dictionary' function
                                check <- TRUE
                            }
                        }
                        
                        word <- ans3
                    }else if(ans == length(pot)+3) #Google resposne
                    {
                        #use 'searcher' package
                        searcher::search_site(paste("dictionary"," '",context[target],"'",sep="",collpase=""),
                                              site = "google", rlang = FALSE)
                    }else if(ans == length(pot)+4) #remove response
                    {
                        #make response missing
                        word <- NA
                    }else if(ans == length(pot)+5) #remove string
                    {
                        #make string missing
                        word <- rep(NA,length(context))
                    }else if(ans == length(pot)+6) #skip response
                    {
                        #give back same response
                        word <- word
                    }
                }
                
            }else{
                
                while(ans == length(pot)+3)
                {
                    #ask for response for options
                    ans <- menu(c(pot,"ADD TO DICTIONARY","TYPE MY OWN","GOOGLE IT","BAD RESPONSE","SKIP"), title = "\n\nPotential responses:")
                    
                    if(ans <= length(pot)) #if an option is selected
                    {
                        #replace word with potential response
                        word <- pot[ans]
                    }else if(ans == length(pot)+1) #add word to dictionary
                    {
                        #add to appendix dictionary
                        suppressWarnings(SemNetDictionaries::append.dictionary(word,save.location="path",path=tempdir()))
                        
                        #load updated appendix dictionary
                        dict <- readRDS(paste(tempdir(),"appendix.dictionary.rds",sep="\\"))
                        
                        #combined with input dictionary
                        dictionary <- sort(unique(c(dictionary,dict)))
                        
                        #a check for later response for saving
                        #appendix dictionary in 'spell.check.dictionary' function
                        check <- TRUE
                        
                    }else if(ans == length(pot)+2) #have user type response
                    {
                        #get response from user
                        ans3 <- readline("Type response: ")
                        
                        if(!ans3 %in% dictionary) #check if typed response is in dictionary
                        {
                            #If not, add ask user if they want to add it to the dictionary
                            ans4 <- menu(c("Yes","No"),title=paste("Add",ans3,"to dictionary?",sep=" "))
                            
                            #If yes, add it to the dictionary
                            if(ans4 == 1)
                            {
                                #add to dictionary
                                suppressWarnings(SemNetDictionaries::append.dictionary(ans3,save.location="path",path=tempdir()))
                                
                                #load updated appendix dictionary
                                dict <- readRDS(paste(tempdir(),"appendix.dictionary.rds",sep="\\"))
                                
                                #combined with input dictionary
                                dictionary <- sort(unique(c(dictionary,dict)))
                                
                                #a check for later response for saving
                                #appendix dictionary in 'spell.check.dictionary' function
                                check <- TRUE
                            }
                        }
                        
                        word <- ans3
                    }else if(ans == length(pot)+3) #Google response
                    {
                        #use 'searcher' package
                        searcher::search_site(paste("dictionary"," '",word,"'", sep = "", collpase = ""),
                                              site = "google", rlang = FALSE)
                    }else if(ans == length(pot)+4) #remove response
                    {
                        #make response missing
                        word <- NA
                    }else if(ans == length(pot)+5) #skip response
                    {
                        #give back same response
                        word <- word
                    }
                }
            }
        }else{word <- qwert.pot}
        
    }else{word <- pot}
    
    res <- list()
    res$word <- word
    res$dict <- dictionary
    res$check <- check
    
    return(res)
}
#----