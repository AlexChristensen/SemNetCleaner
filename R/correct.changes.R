#' Correct Changes from \code{\link[SemNetCleaner]{textcleaner}}
#' 
#' @description Allows corrections to changes made by \code{\link[SemNetCleaner]{textcleaner}}.
#' Some changes may have been made by accident, some changes may have been made
#' by the automated cleaning, while others may just need to be removed.
#' This function will correct any changes made in a cleaned \code{\link[SemNetCleaner]{textcleaner}}
#' object.
#' 
#' @param textcleaner.obj A \code{\link[SemNetCleaner]{textcleaner}} object
#' 
#' @param incorrect Character vector.
#' A vector of incorrect response(s) to change.
#' See the object \code{spellcheck$auto} in
#' \code{\link[SemNetCleaner]{textcleaner}} output
#' 
#' @param dictionary Character vector.
#' Can be a vector of a corpus or any text for comparison.
#' Dictionary to be used for more efficient text cleaning.
#' Defaults to \code{NULL}, which will use \code{\link[SemNetDictionaries]{general.dictionary}}
#' 
#' @details This function is used to correct mistakes that occur
#' in the cleaning process during \code{\link[SemNetCleaner]{textcleaner}}.
#' There are times when you are too deep into the text cleaning process
#' that accidentally hitting a '\code{1}' instead of a '\code{2}' does
#' not make sense to stop and start the text cleaning process over. Rather
#' when mistakes are made, a record can be kept and this function will
#' allow those mistakes to be amended.
#' 
#' Incorrect responses should be used as input. A menu will prompt the user
#' for their decision on how to manage the incorrectly cleaned response.
#' There are three potential options:
#' 
#' \itemize{
#' 
#' \item{\code{1: TYPE MY OWN}}
#' {Allows user to type their own response. If multiple responses, then 
#' commas should separate each response. Quotations are not necessary.}
#' 
#' \item{\code{2: GOOGLE IT}}
#' {"Googles" the response in question.
#' A browser will open with the Google search terms: define "RESPONSE"}
#' 
#' \item{\code{3: BAD RESPONSE}}
#' {When selected, \code{NA} will be returned}
#' 
#' }
#' 
#' @return This function returns a list containing the
#' following \code{\link[SemNetCleaner]{textcleaner}} objects, which
#' have been corrected with the user-provided changes:
#' 
#' \item{binary}{A matrix of responses where each row represents a participant
#' and each column represents a unique response. A response that a participant has provided is a '\code{1}'
#' and a response that a participant has not provided is a '\code{0}'}
#'
#' \item{responses}{A list containing two objects:
#' 
#' \itemize{
#' 
#' \item{clean.resp}
#' {A response matrix that has been spell-checked and de-pluralized with duplicates removed.
#' This can be used as a final dataset for analyses (e.g., fluency of responses)}
#' 
#' \item{orig.resp}
#' {The original response matrix that has had white spaces before and
#' after words response. Also converts all upper-case letters to lower case}
#' 
#' }
#' 
#' }
#'
#' \item{spellcheck}{A list containing three objects:
#' 
#' \itemize{
#' 
#' \item{\code{full}}
#' {All responses regardless of spell-checking changes}
#' 
#' \item{\code{auto}}
#' {Only the incorrect responses that were changed during spell-check}
#' 
#' }
#' 
#' }
#' 
#' \item{removed}{A list containing two objects: 
#' 
#' \itemize{
#' 
#' \item{\code{rows}}
#' {Identifies removed participants by their row (or column) location in the original data file}
#' 
#' \item{\code{ids}}
#' {Identifies removed participants by their ID (see argument \code{data})}
#' 
#' }
#' 
#' }
#' 
#' \item{partChanges}{A list where each participant is a list index with each
#' response that was been changed. Participants are identified by their ID (see argument \code{data}).
#' This can be used to replicate the cleaning process and to keep track of changes more generally.
#' Participants with \code{NA} did not have any changes from their original data
#' and participants with missing data are removed (see \code{removed$ids})}
#' 
#' @examples
#' # Toy example
#' raw <- open.animals[c(1:10),-c(1:3)]
#' 
#' # Clean and prepocess data
#' clean <- textcleaner(raw, partBY = "row", dictionary = "animals")
#' 
#' # Correct changes
#' if(interactive())
#' {corr.clean <- correct.changes(clean, incorrect = "rat", dictionary = "animals")}
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom utils menu
#' 
#' @export
#Correct Changes----
correct.changes <- function(textcleaner.obj, dictionary = NULL, incorrect)
{
    #Check for 'textcleaner' object
    if(class(textcleaner.obj)!="textcleaner")
    {stop("Must be a 'textcleaner' object")}
    
    #Grab dictionary
    if(is.null(dictionary))
    {full.dict <- SemNetDictionaries::load.dictionaries("general")
    }else{full.dict <- SemNetDictionaries::load.dictionaries(dictionary)}
    
    #Grab monikers
    if(any(dictionary %in% SemNetDictionaries::dictionaries(TRUE)))
    {misnom <- SemNetDictionaries::load.monikers(dictionary)}
    
    #Unique changes
    uniq <- textcleaner.obj$spellcheck$auto
    
    #Index incorrect responses
    ind <- match(incorrect,uniq$from)
    
    check <- list()
    
    for(i in 1:length(ind))
    {
        #Potential responses
        pot <- best.guess(incorrect[i], full.dict)
        
        #Initialize answer
        ans <- 2
        
        while(ans == 2)
        {
            #Changed response(s)
            chn <- uniq[which(uniq$from==incorrect[i]),-1][!is.na(bad.response(uniq[which(uniq$from==incorrect[i]),-1]))]
            
            #Print to user
            if(length(chn)>1)
            {cat("From: ",incorrect[i],"\n","To: ",paste("'",chn,"'",sep=""),sep="")
            }else{cat("From: ",incorrect[i],"\n","To: ",chn,sep="")}
            
            #Ask for correction
            ans <- menu(c("TYPE MY OWN","GOOGLE IT","BAD RESPONSE",pot), title = "\n\nPotential responses:")
            
            if(ans == 1) #TYPE MY OWN
            {
                #Have type response
                word <- readline("Use commas for multiple words (dog, fish, etc.): ")
                
                #Split by comma and space
                corr <- unlist(strsplit(word,split=", "))
            }else if(ans == 2) #GOOGLE IT
            {
                #use 'searcher' package
                searcher::search_site(paste("dictionary"," '",word,"'",sep="",collpase=""),
                                      site = "google", rlang = FALSE)
            }else if(ans == 3) #BAD RESPONSE
            {corr <- NA
            }else if(ans > 3)
            {
                #insert response
                corr <- pot[ans - 3]
                
                #check if moniker
                if(length(misnom)!=0)
                {corr <- SemNetCleaner::moniker(corr,misnom)}
            }
        }
        
        #Correct bad resposnes
        corr <- bad.response(corr)
        
        #Transfer 'spellcheck' list
        spellcheck <- textcleaner.obj$spellcheck
        
        #Change auto spelling changes
        spellcheck$auto[which(spellcheck$auto[,"from"]==incorrect[i]),2:(length(corr)+1)] <- corr
        
        #Change NA to ""
        if(any(is.na(spellcheck$auto)))
        {spellcheck$auto[which(is.na(spellcheck$auto),arr.ind=TRUE)] <- ""}
        
        #Change unique spelling changes
        spellcheck$unique[which(spellcheck$unique[,"from"]==incorrect[i]),2:(length(corr)+1)] <- corr
        
        #Change NA to ""
        if(any(is.na(spellcheck$unique)))
        {spellcheck$unique[which(is.na(spellcheck$unique),arr.ind=TRUE)] <- ""}
        
        #Change full spelling changes
        spellcheck$full[which(spellcheck$full[,"from"]==incorrect[i]),2:(length(corr)+1)] <- corr
        
        #Change NA to ""
        if(any(is.na(spellcheck$full)))
        {spellcheck$full[which(is.na(spellcheck$full),arr.ind=TRUE)] <- ""}
        
        #Remove extra corrections
        if(length(corr)<incorrect[i])
        {
            #Change incorrect columns to NA
            ##New length
            new.len <- 2:(length(corr)+1)
            
            ##Replace incorrect responses with NA
            spellcheck$auto[which(spellcheck$auto[,"from"]==incorrect[i]),-c(1,new.len)] <- ""
            spellcheck$unique[which(spellcheck$unique[,"from"]==incorrect[i]),-c(1,new.len)] <- ""
            spellcheck$full[which(spellcheck$full[,"from"]==incorrect[i]),-c(1,new.len)] <- ""
        }
        
        #Put back into 'spellcheck'
        textcleaner.obj$spellcheck <- spellcheck
        
        #Transfer 'partChanges' list
        partChanges <- textcleaner.obj$partChanges
        
        #Change participant changes
        for(j in names(partChanges))
        {
            if(!all(is.na(partChanges[[j]])))
            {
                #Make each nested object homogeneous (as a matrix)
                if(is.vector(partChanges[[j]]))
                {
                    partChanges[[j]] <- as.matrix(partChanges[[j]])
                    
                    if("from" %in% row.names(partChanges[[j]]))
                    {partChanges[[j]] <- t(partChanges[[j]])}
                }
                
                #Make sure its a matrix with column names
                if(is.null(colnames(partChanges[[j]])))
                {
                    if(ncol(partChanges[[j]]) == 1)
                    {
                        # Fix issue
                        fix <- cbind(partChanges[[j]], rep(NA, nrow(partChanges[[j]])))
                        colnames(fix) <- c("from", "to")
                        
                        partChanges[[j]] <- fix
                    }
                }
                    
                
                #Search through participants to identify which
                #participant needs the change
                if(incorrect[i] %in% partChanges[[j]][,"from"])
                {
                    check[[i]] <- c(i,j)
                    
                    #Identify incorrect response
                    target <- which(partChanges[[j]][,"from"]==incorrect[i])
                    
                    #incorrect target
                    incorrect.target <- partChanges[[j]][target,-1]
                    
                    #Idenfity whether duplicates are in the incorrect responses
                    #or if there is another corrected response
                    rem <- dup.match(textcleaner.obj, j, target)
                    
                    #Change responses that are no not longer given by the
                    #participant to 0
                    if(any(rem))
                    {textcleaner.obj$binary[j,incorrect.target[rem]] <- 0}
                    
                    #Replace incorrect response
                    if(length(corr)>ncol(partChanges[[j]]))
                    {
                        #New columns
                        new.col <- length(corr)-ncol(partChanges[[j]])+1
                        
                        #New matrix
                        new.mat <- as.matrix(cbind(partChanges[[j]],matrix("",nrow=nrow(partChanges[[j]]),ncol=new.col)))
                        
                        #Add correction
                        new.mat[target,2:(length(corr)+1)] <- corr
                        
                        #New column names
                        colnames(new.mat)[((ncol(partChanges[[j]]))+1):((ncol(partChanges[[j]]))+new.col)] <- rep("to",new.col)
                        
                        #Put back into participant changes
                        partChanges[[j]] <- as.data.frame(new.mat,stringsAsFactors = FALSE)
                    }else if(length(corr)<ncol(as.matrix(partChanges[[j]][,-1])))
                    {
                        #Change incorrect columns to NA
                        ##New length
                        new.len <- 2:(length(corr)+1)
                        
                        ##Replace incorrect responses with NA
                        partChanges[[j]][target,-c(1,new.len)] <- NA
                        
                        #Remove NAs in columns function
                        na.col <- function (df)
                        {
                            if(any(!is.na(df)))
                            {df <- df[,colSums(is.na(df))<nrow(df)]
                            }else{df <- NA}
                            
                            return(df)
                        }
                        
                        #Update 'partChanges'
                        partChanges[[j]][target,2:(length(corr)+1)] <- corr
                        partChanges[[j]] <- na.col(partChanges[[j]])
                        
                    }else{partChanges[[j]][target,2:(length(corr)+1)] <- corr}
                    
                    #Change new binary responses to one
                    new.target <- partChanges[[j]][target,-1][!is.na(partChanges[[j]][target,-1])]
                    
                    ##Clean out open spaces (i.e., "")
                    new.target <- na.omit(bad.response(new.target))
                    
                    if(length(new.target)!=0)
                    {
                        textcleaner.obj$binary[j,new.target] <- 1
                        
                        if(any(is.na(textcleaner.obj$binary[,new.target])))
                        {
                            #Change NAs to 0
                            na.target <- which(is.na(textcleaner.obj$binary[,new.target]))
                            textcleaner.obj$binary[na.target,new.target] <- 0
                        }
                    }
                }
            }
        }
        
        #Put back into 'partChanges'
        textcleaner.obj$partChanges <- partChanges
    }
    
    if(any(colSums(textcleaner.obj$binary)==0))
    {
        #Target the zero responses
        target.zero <- which(colSums(textcleaner.obj$binary)==0)
        
        #Target column names
        rm.col <- colnames(textcleaner.obj$binary)[target.zero]
        
        #Let user know column(s) will be removed
        message(paste("Response(s)",paste("'",rm.col,"'",sep="",collapse=", "),"were removed because of participants no longer giving the response"))
        
        textcleaner.obj$binary <- textcleaner.obj$binary[,-target.zero]
    }
    
    textcleaner.obj$binary <- textcleaner.obj$binary[,order(colnames(textcleaner.obj$binary))]
    
    return(textcleaner.obj)
}
#----
