#' Spelling-check using \link{SemNetDictionaries}
#' @description A wrapper function for spell-checking text dictionaries in \code{\link{SemNetDictionaries}}
#' (combines all spell-checking wrapper functions)
#' 
#' @param check Character vector.
#' A vector of unique responses from text data
#' 
#' @param dictionary Character vector.
#' See \code{\link{SemNetDictionaries}}
#' 
#' @param part.resp Matrix or data frame.
#' Uncleaned participant response matrix
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
#' The recommended and default distance tolerance is \code{tolerance = 1},
#' which only spell corrects a word if there is only one word with a DL distance of 1. 
#' 
#' @return Returns a list containing:
#' 
#' \item{from}{A list of all responses before they were cleaned}
#' 
#' \item{to}{A list of all responses after they were cleaned}
#' 
#' \item{dict}{The updated dictionary vector}
#' 
#' \item{from.inc}{A list of only incorrect responses before they were cleaned}
#' 
#' \item{to.inc}{A list of only incorrect responses after they were cleaned}
#' 
#' @examples
#' # Toy example
#' raw <- open.animals[c(1:10),-c(1:3)]
#' 
#' if(interactive())
#' {
#'     scd <- spell.check.dictionary(check = unique(unlist(raw)),
#'     dictionary = SemNetDictionaries::animals.dictionary,
#'     part.resp = raw) 
#' }
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom SemNetDictionaries load.dictionaries
#' 
#' @export
#Spell-check for dictionary
spell.check.dictionary <- function (check, dictionary, part.resp, tolerance = 1)
{
    #load dictionaries
    full.dict <- SemNetDictionaries::load.dictionaries(dictionary)
    orig.dict <- full.dict
    
    #initialize 'from' and 'to' list for changes
    from <- as.list(check)
    to <- from
    
    #perform multi word check
    to <- lapply(to, multi.word.check, dictionary = full.dict, tolerance = 1)
    
    #initial monikers if dictionary is in 'SemNetDictionaries' package
    if(any(dictionary %in% SemNetDictionaries::dictionaries()))
    {
        #load moniker
        misnom <- SemNetDictionaries::load.monikers(dictionary)
        
        if(length(misnom)!=0)
        {
            #check for monikers
            to <- lapply(to,SemNetCleaner::moniker,misnom)
            
            #check for de-pluralized monikers
            sing.mis <- lapply(unlist(to),singularize)
            
            #check if singular moniker exists
            sing.mis <- unlist(lapply(sing.mis,SemNetCleaner::moniker,misnom))
            
            #check for monikers
            to[sing.mis %in% full.dict] <- sing.mis[sing.mis %in% full.dict]
            
        }else{to <- to}
    }else{to <- to}
    
    #initial search for words to combine
    to <- lapply(to, combine.responses, full.dict)
    
    #index correctly and incorrectly spelled responses
    ind1 <- match(unlist(to),full.dict)
    
    #check for plurals
    sing <- unlist(lapply(unlist(to),singularize))
    ind2 <- match(sing,full.dict)
    
    #correct plurals
    to[which(!is.na(ind2))] <- na.omit(full.dict[ind2])
    
    #index correct cases
    correct <- which(!is.na(ind1)|!is.na(ind2))
    
    #index incorrect cases
    incorrect <- which(is.na(ind1)&is.na(ind2))
    
    #########################
    ####BEGIN SPELL CHCEK####
    #########################
    
    ####progress bar####
    pb <- tcltk::tkProgressBar(title = "R progress bar", label = "Spell-check progress",
                  min = 0, max = length(incorrect), initial = 0, width = 300)
    invisible(tcltk::getTkProgressBar(pb))
    count <- 0
    ####progress bar####
    
    #initialize check for saving appendix dictionary
    check.vec <- vector(length=length(incorrect))
    
    #remember string split words
    rem.str.spl <- list()
    
    #remember responses
    rem.resp <- matrix(NA,nrow=1,ncol=2)
    
    for(i in incorrect)
    {
        #initialize 'wcw'
        wcw <- NULL
        
        #target response to correct
        target <- to[[i]]
        
        if(all(!is.na(bad.response(target)))) #response is not a bad response
        {
            #check for multiple responses
            multi <- unlist(strsplit(target,split = " "))
            
            #remove spaces
            if(any(multi==""))
            {
                #identify spaces
                rm.spaces <- which(multi=="")
                
                #remove spaces from multi
                multi <- multi[-rm.spaces]
            }
            
            #spell-check for multiple responses in one element
            if(length(multi)>1)
            {
                #initialize second check for saving appendix dictionary
                check.vec2 <- vector(length=length(multi))
                
                for(j in 1:length(multi))
                {
                    if(!multi[j] %in% full.dict)
                    {
                        #check for potential responses
                        wcw <- word.check.wrapper(word = multi[j],
                                                  dictionary = full.dict,
                                                  context = multi,
                                                  tolerance = tolerance,
                                                  rem.resp = rem.resp)
                        
                        if(length(wcw$word)==1)
                        {
                            #replace with word
                            multi[j] <- wcw$word
                            
                            #update dictionary if need be
                            full.dict <- wcw$dict
                            
                            #insert check for saving appendix dictionary
                            check.vec2[j] <- wcw$check
                        }else{
                            
                            #replace words
                            if(length(wcw$word)>1)
                            {multi <- wcw$word
                            }else{multi[j] <- wcw$word}
                            
                            #update dictionary if need be
                            full.dict <- wcw$dict
                            
                            #insert check for saving appendix dictionary
                            check.vec2[j] <- wcw$check
                            break
                        }
                    }
                }
                    
                
                if(all(!is.na(multi)))
                {
                        #multi before split string check
                        multi.before <- multi
                        
                        #check for whether certain words should be separated
                        multi <- splitstr.check(multi,split=" ",full.dict,rem.str.spl)
                        
                        #multi after split string check
                        multi.after <- multi$vec
                        
                        if(!multi$spl)
                        {
                            #check if multi should be combined or separated
                            if(length(multi$vec)>1)
                            {
                                if(suppressWarnings(all(multi.before==multi.after)))
                                {
                                    #if continuous string is longer than possible continuous string
                                    #in the dictionary, then separate the responses
                                    if(length(multi$vec)<max(unlist(lapply(strsplit(full.dict," "),length))))
                                    {
                                        #even if all words are spelled correctly
                                        ans <- menu(c(paste("combined: ","'",paste(multi$vec,collapse=" "),"'",sep=""),
                                                      paste("separated: ",paste("'",multi$vec,"'",collapse=" ",sep=""))),
                                                    title=paste('\nShould "',paste(multi$vec,sep="",collapse=" "), '" be combined or separated?',sep=""))
                                        
                                        if(ans == 1)
                                        {multi.after <- paste(multi$vec,collapse = " ")}
                                    }
                                }
                            }
                        }
                        
                        
                        #Check if word(s) are not in dictionary
                        if(!any(multi.after %in% full.dict))
                        {
                            #Go through each word
                            for(r in 1:length(multi.after))
                            {
                                #If word is not in dictionary, then should it be added?
                                if(!multi.after[r] %in% full.dict)
                                {
                                    #If not, add ask user if they want to add it to the dictionary
                                    ans4 <- menu(c("Yes","No"),title=paste('Add "',multi.after,'" to dictionary?',sep=""))
                                    
                                    #If yes, add it to the dictionary
                                    if(ans4 == 1)
                                    {
                                        #add to dictionary
                                        suppressWarnings(SemNetDictionaries::append.dictionary(multi.after,save.location="envir"))
                                        
                                        #load updated appendix dictionary
                                        dict <- readRDS(paste(tempdir(),"appendix.dictionary.rds",sep="\\"))
                                        
                                        #combined with input dictionary
                                        full.dict <- sort(unique(c(full.dict,dict)))
                                        
                                        #a check for later response for saving
                                        #appendix dictionary in 'spell.check.dictionary' function
                                        check.vec2 <- TRUE
                                    }
                                }
                            }
                        }
                        
                        #update list
                        rem.str.spl[[i]] <- list(before = multi.before, after = multi.after)
                }else{multi.after <- multi}
                
                #update check vec
                check.vec[i] <- any(check.vec2)
                
            }else{
                #check for potential responses
                wcw <- word.check.wrapper(word = multi,
                                          dictionary = full.dict,
                                          tolerance = tolerance,
                                          part.resp = part.resp,
                                          rem.resp = rem.resp)
                
                #replace with word
                multi <- wcw$word
                
                #multi before split string check
                multi.before <- multi
                
                #check for whether certain words should be separated
                if(!is.na(multi))
                {
                    #check for whether certain words should be separated
                    multi <- splitstr.check(multi.before,split=" ",full.dict,rem.str.spl)
                    
                    #multi after split string check
                    multi.after <- multi$vec
                }else{multi.after <- bad.response(multi)}
                
                #update dictionary if need be
                full.dict <- wcw$dict
                
                #update check vec
                check.vec[i] <- wcw$check
            }
        }else{multi.after <- bad.response(target)}
        
        
        to[[i]] <- multi.after
        
        if(!is.null(wcw))
        {rem.resp <- wcw$rem.resp}
        
        ####progress bar####
        count <- count + 1
        percent <- floor((count/length(incorrect))*100)
        info <- sprintf("%.0f%% done", percent)
        tcltk::setTkProgressBar(pb, count, sprintf("Spell-check Progress (%s)", info), info)
        ####progress bar####
    }
    
    ####progress bar####
    close(pb)
    ####progress bar####
    
    #if any words were added to the appendix dictionary,
    #then ask if dictionary should be saved
    if(any(na.omit(check.vec)))
    {
        #ask if user would like to save dictionary
        ans <- menu(c("Yes","No"),title = "Would you like to save your appendix dictionary to your computer?")
        
        #if yes,
        if(ans == 1)
        {
            #get name of dictionary from user
            ans2 <- readline("Dictionary name (no quotations): ")
            
            #identify differences between original dictionary and updated dictionary
            dict <- wcw$dict[which(is.na(match(wcw$dict,orig.dict)))]
            
            #create appendix dictionary
            SemNetDictionaries::append.dictionary(dict,
                                                  dictionary.name = ans2,
                                                  save.location = "choose",
                                                  textcleaner = TRUE)
        }else if(ans == 2)
        {
            #let user know that the dictionary data was not saved
            message("Appendix dictionary was not saved.")
        }
    }
    
    #secondary monikers if animals dictionary
    if(length(misnom)!=0)
    {
        #check for monikers
        for(i in 1:length(to))
            for(j in 1:length(to[[i]]))
        {to[[i]][j] <- SemNetCleaner::moniker(to[[i]][j],misnom)}
    }
    
    #secondary search for words to combine
    for(i in 1:length(to))
    {to[[i]] <- SemNetCleaner::combine.responses(to[[i]],full.dict)}
    
    #results list
    res <- list()
    res$from <- from
    res$to <- to
    res$dict <- full.dict
    res$from.inc <- from[incorrect]
    res$to.inc <- to[incorrect]
    
    return(res)
}
#----