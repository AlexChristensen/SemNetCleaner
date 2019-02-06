#' Correct Changes from \code{\link[SemNetCleaner]{textcleaner}}
#' 
#' @description Allows from correction of changes made by \code{\link[SemNetCleaner]{textcleaner}}.
#' Some changes may have been made by accident, some changes may have been made
#' by the automated cleaning, while others may simply just need to be removed.
#' This function will correct any changes made in a cleaned \code{\link[SemNetCleaner]{textcleaner}}
#' object.
#' 
#' @param txt.cln.obj A \code{\link[SemNetCleaner]{textcleaner}} object
#' 
#' @param resp A vector of response(s) to change.
#' See the object \code{spellcheck$unique} in
#' \code{\link[SemNetCleaner]{textcleaner}} output
#' 
#' @param corr How should the response(s) be corrected?
#' \code{1} = KEEP the original response.
#' \code{0} = REMOVE the response entirely.
#' These must correspond to the input for the \code{resp}
#' argument
#' 
#' @return This function returns a list containing the
#' following \code{\link[SemNetCleaner]{textcleaner}} objects, which
#' have been changed with the provided changes:
#' 
#' \item{binary}{A matrix of responses where each row represents a participant
#' and each column represents a unique response. A response that a participant has provided is a '1'
#' and a response that a participant has not provided is a '0'}
#'
#' \item{resposnes}{A response matrix that has been spell checked and de-pluralized with duplicates removed}
#' 
#' \item{spellcheck}{A list containing two objects: full and unique. \strong{full} contains
#' all responses regardless of spellcheck changes and \strong{unique} contains only responses that were
#' changed during the spell check}
#' 
#' \item{removed}{A list containing two objects: rows and ids.
#' \strong{rows} identifies removed participants by their row location in the original data file
#' and \strong{ids} identifies removed participants by their ID}
#' 
#' \item{partChanges}{A list where each participant is an object with their
#' responses that have been changed. Participants are identified by their ID.
#' This can be used to replicate the cleaning process and to keep track of changes more generaly.
#' Participants with \strong{NA} did not have any changes from the original data
#' and participants with NULL were removed due to missing data (see \emph{removed$ids})}
#' 
#' @examples
#' 
#' \donttest{
#' tc.obj <- textcleaner(trial, partBY = "col")
#' 
#' rmat <- corr.chn (tc.obj, "rat", 1)
#' }
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom stats na.omit
#' 
#' @export
#Text Cleaner----
corr.chn <- function(txt.cln.obj, resp, corr)
{
    if(length(resp)!=length(corr))
    {stop("Responses to change do not equal the corrections given. 
          The length of 'resp' must match the length of 'corr'")}
    
    part <- txt.cln.obj$partChanges
    
    part.names <- names(part)
    
    part.len <- length(part.names)
    
    for(i in 1:part.len)
    {
        part.chk <- part[[part.names[i]]]
        
        if(any(!is.na(part.chk)))
        {
            part.chk2 <- part.chk[,1]
            
            check <- any(resp %in% part.chk2)
            
            if(check)
            {
                target <- which(resp %in% part.chk2)
                
                target.len <- length(target)
                
                for(j in 1:target.len)
                {
                    part.target <- which(part.chk2==resp[target[j]])
                    
                    if(corr[target]==0)
                    {
                        num <- part.names[i]
                        
                        rem <- part[[num]][part.target,2]
                        
                        #update binary matrix
                        txt.cln.obj$binary[num,rem] <- 0
                        #update responses
                        txt.cln.obj$responses[[num]] <- txt.cln.obj$responses[[num]][-which(txt.cln.obj$responses[[num]]==rem)]
                        #update changes
                        txt.cln.obj$partChanges[[num]][part.target,2] <- NA
                        
                    }else if(corr[target]==1)
                    {
                        num <- part.names[i]
                        
                        keep <- part[[num]][part.target,1]
                        
                        #update binary matrix
                        bin <- txt.cln.obj$binary
                        
                        if(!keep %in% colnames(bin))
                        {
                            bin <- cbind(bin,rep(0,nrow(bin)))
                            
                            colnames(bin)[ncol(bin)] <- keep
                        }
                        
                        bin[num,keep] <- 1
                        
                        txt.cln.obj$binary <- bin
                        
                        #update responses
                        chn <- part[[num]][part.target,2]
                        txt.cln.obj$responses[[num]][which(txt.cln.obj$responses[[num]]==chn)] <- keep
                        
                        #update changes
                        txt.cln.obj$partChanges[[num]][part.target,2] <- keep
                    }
                }
            }
        }
    }
    
    txt.cln.obj$binary <- txt.cln.obj$binary[,order(colnames(txt.cln.obj$binary))]
    
    return(txt.cln.obj)
}
#----