#' Correct Changes from \code{\link[SemNetCleaner]{textcleaner}}
#' 
#' @description Allows corrections to changes made by \code{\link[SemNetCleaner]{textcleaner}}.
#' Some changes may have been made by accident, some changes may have been made
#' by the automated cleaning, while others may just need to be removed.
#' This function will correct any changes made in a cleaned \code{\link[SemNetCleaner]{textcleaner}}
#' object.
#' 
#' @param txt.cln.obj A \code{\link[SemNetCleaner]{textcleaner}} object
#' 
#' @param old Character vector.
#' A vector of old response(s) to change.
#' See the object \code{spellcheck$unique} in
#' \code{\link[SemNetCleaner]{textcleaner}} output
#' 
#' @details This function is used to correct mistakes that occur
#' in the cleaning process during \code{\link[SemNetCleaner]{textcleaner}}.
#' There are times when you are too deep into the text cleaning process
#' that accidentally hitting a '\code{1}' instead of a '\code{2}' does
#' not make sense to stop and start the text cleaning process over. Rather
#' when mistakes are made, a record can be kept and this function will
#' allow those mistakes to be amended.
#' 
#' Old responses should be used as input. A menu will prompt the user
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
#' \item{resposnes}{A response matrix that has been spell-checked and de-pluralized with duplicates removed.
#' This can be used as a final dataset for analyses (e.g., fluency of responses)}
#' 
#' \item{spellcheck}{A list containing two objects: \code{full} and \code{unique}. \code{full} contains
#' all responses regardless of spellcheck changes and \code{unique} contains only responses that were
#' changed during the spell-check}
#' 
#' \item{removed}{A list containing two objects: \code{rows} and \code{ids}.
#' \code{rows} identifies removed participants by their row (or column) location in the original data file
#' and \code{ids} identifies removed participants by their ID (see argument \code{data}
#' in \code{\link[SemNetCleaner]{textcleaner}})}
#' 
#' \item{partChanges}{A list where each participant is an object with their
#' responses that have been changed. Participants are identified by their ID
#' (see argument \code{data} in \code{\link[SemNetCleaner]{textcleaner}}).
#' This can be used to replicate the cleaning process and to keep track of changes more generaly.
#' Participants with \code{NA} did not have any changes from the original data
#' and participants with \code{NULL} were removed due to missing data (see \code{removed$ids})}
#' 
#' @examples
#' #load data
#' dat <- trial
#' 
#' \dontrun{
#' 
#' tc.obj <- textcleaner(dat, partBY = "col")
#' 
#' rmat <- corr.chn (tc.obj, "rat")
#' }
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom stats na.omit
#' 
#' @export
#Correct Changes----
corr.chn <- function(txt.cln.obj, old)
{
    #Unique changes
    uniq <- txt.cln.obj$spellcheck$unique
    
    #Index old responses
    ind <- match(old,uniq$from)
    
    for(i in 1:length(ind))
    {
        ans <- 2
        
        while(ans == 2)
        {
            #Changed response(s)
            chn <- uniq[which(uniq$from==old[i]),-1][!is.na(bad.response(uniq[which(uniq$from==old[i]),-1]))]
            
            #Print to user
            if(length(chn)>1)
            {cat("Old response: ",old[i],"\n","Changed response: ",paste("'",chn,"'",sep=""),sep="")
            }else{cat("Old response: ",old[i],"\n","Changed response: ",chn,sep="")}
            
            #Ask for correction
            ans <- menu(c("TYPE MY OWN","GOOGLE IT","BAD RESPONSE"), title = "\n\nPotential responses:")
            
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
            {corr <- NA}
        }
        
        #Correct bad resposnes
        corr <- bad.response(corr)
        
        #Change unique spelling changes
        txt.cln.obj$spellcheck$unique[which(txt.cln.obj$spellcheck$unique[,"from"]==old[i]),2:(length(corr)+1)] <- corr
        
        #Change NA to ""
        if(any(is.na(txt.cln.obj$spellcheck$unique)))
        {txt.cln.obj$spellcheck$unique[which(is.na(txt.cln.obj$spellcheck$unique),arr.ind=TRUE)] <- ""}
        
        #Change full spelling changes
        txt.cln.obj$spellcheck$full[which(txt.cln.obj$spellcheck$full[,"from"]==old[i]),2:(length(corr)+1)] <- corr
        
        #Change NA to ""
        if(any(is.na(txt.cln.obj$spellcheck$full)))
        {txt.cln.obj$spellcheck$full[which(is.na(txt.cln.obj$spellcheck$full),arr.ind=TRUE)] <- ""}
        
        #Change participant changes
        for(j in names(txt.cln.obj$partChanges))
        {
            #Search through participants to identify which
            #participant needs the change
            if(old[i] %in% txt.cln.obj$partChanges[[j]][,1])
            {
                #Change participant changes
                if("chn" %in% colnames(txt.cln.obj$partChanges[[j]]))
                {
                    #Transpose 
                    txt.cln.obj$partChanges[[j]] <- t(txt.cln.obj$partChanges[[j]])
                    
                    #Change column names
                    colnames(txt.cln.obj$partChanges[[j]]) <- c("from",rep("to",ncol(txt.cln.obj$partChanges[[j]])-1))
                }
                
                #Identify old response
                target <- which(txt.cln.obj$partChanges[[j]][,"from"]==old[i])
                
                #Change old binary responses to zero
                old.target <- txt.cln.obj$partChanges[[j]][target,-1][!is.na(txt.cln.obj$partChanges[[j]][target,-1])]
                
                if(length(old.target)!=0)
                {txt.cln.obj$binary[j,old.target] <- 0}
                
                #Replace old response
                if(length(corr)>ncol(txt.cln.obj$partChanges[[j]]))
                {
                    #New columns
                    new.col <- length(corr)-ncol(txt.cln.obj$partChanges[[j]])+1
                    
                    #New matrix
                    new.mat <- as.matrix(cbind(txt.cln.obj$partChanges[[j]],matrix("",nrow=nrow(txt.cln.obj$partChanges[[j]]),ncol=new.col)))
                    
                    #Add correction
                    new.mat[target,2:(length(corr)+1)] <- corr
                    
                    #New column names
                    colnames(new.mat)[((ncol(txt.cln.obj$partChanges[[j]]))+1):((ncol(txt.cln.obj$partChanges[[j]]))+new.col)] <- rep("to",new.col)
                    
                    #Put back into participant changes
                    txt.cln.obj$partChanges[[j]] <- as.data.frame(new.mat,stringsAsFactors = FALSE)
                }else{txt.cln.obj$partChanges[[j]][target,2:(length(corr)+1)] <- corr}
                
                #Change new binary responses to one
                new.target <- txt.cln.obj$partChanges[[j]][target,-1][!is.na(txt.cln.obj$partChanges[[j]][target,-1])]
                
                if(length(new.target)!=0)
                {txt.cln.obj$binary[j,new.target] <- 1}
            }
        }
    }
    
    if(any(colSums(txt.cln.obj$binary)==0))
    {
        #Target the zero responses
        target.zero <- which(colSums(txt.cln.obj$binary)==0)
        
        #Target column names
        rm.col <- colnames(txt.cln.obj$binary)[target.zero]
        
        #Let user know column(s) will be removed
        message(paste("Response(s)",paste("'",rm.col,"'",sep="",collapse=", "),"were removed because of participants no longer giving the response"))
        
        txt.cln.obj$binary <- txt.cln.obj$binary[,-target.zero]
    }
    
    txt.cln.obj$binary <- txt.cln.obj$binary[,order(colnames(txt.cln.obj$binary))]
    
    return(txt.cln.obj)
}
#----