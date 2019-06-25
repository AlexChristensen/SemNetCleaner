#' Automated Converge Responses
#' @description Automated \code{\link[SemNetCleaner]{converge}} function which merges
#' of columns of binarized response data with another.
#' This function streamlines the merging of like responses with other
#' like responses (e.g., "roaches" with "cockroaches")
#' into one singular function.
#' 
#' @param rmat Binary matrix.
#' A \code{\link[SemNetCleaner]{textcleaner}} filtered response matrix
#'
#' @return A list containing four objects:
#' 
#' \item{rmat}{A response matrix that has had responses converged}
#' 
#' \item{converged}{A matrix of responses showing the original response
#' (\code{from}) and the response that replaced it (\code{to}). If response
#' was not converged, then it stays the same}
#' 
#' \item{changed}{A matrix of responses that were changed. The original
#' response (\code{from}) and the response that replaced it (\code{to}).
#' If responses were removed, then \code{<NA>} is displayed}
#' 
#' \item{participant}{A list of each response with each participant affected}
#' 
#' @examples
#' rmat <- rmat
#' 
#' \dontrun{
#' 
#' #text cleaned
#' clean <- textcleaner(rmat)
#' 
#' #Automated converge
#' convmat <- autoConverge(clean)
#' }
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom utils menu
#' 
#' @export
#Automated Converge Function----
autoConverge <- function (rmat)
{
    #number of responses
    n <- ncol(rmat)
    
    #name of responses
    name <- colnames(rmat)
    
    #ids of participants
    ids <- row.names(rmat)
    
    #duplicate of original response matrix
    repmat <- rmat
    
    #initialize output list to return
    output <- list()
    
    #matrix for how response were converged
    output$converged <- matrix(NA,nrow=n,ncol=2)
    colnames(output$converged) <- c("from","to")
    
    #participants that were affected by converged responses
    output$participant[["all ids"]] <- ids
    
    #loop to go through all responses
    for(i in n:1)
    {
        #response to check for converge
        check <- colnames(rmat)[i]
        
        first <- starting.letter(check)
        
        let.chn <- TRUE
        
        while(let.chn)
        {
            #set up first letter for search
            start <- as.character(paste("^",first,sep=""))
            
            #potential converges
            pot <- colnames(repmat[grep(start,colnames(repmat),ignore.case=TRUE)])
            
            #remove check response from potential
            pot <- pot[-which(pot==check)]
            
            #display response to check for converge
            print(check)
            
            #propose responses
            ans <- menu(c(pot,"NO","RENAME","REMOVE","ANOTHER LETTER","TYPE MY OWN"),title="Converge with another response?")
            
            ####potential responses####
            if(ans <= length(pot))
            {
                #changed response
                chn <- pot[ans]
                
                #updated the duplicate response matrix
                if(check!=chn)
                {repmat <- converge(repmat,as.character(chn),as.character(check))}
                
                #input as unchanged
                output$converged[i,] <- cbind(as.character(check),as.character(chn))
                
                #identify participants this affects
                if(sum(rmat[,i]==1)!=0)
                {output$participant[[check]] <- which(rmat[,i]==1)}
            }
            
            ####NO####
            if(ans == length(pot)+1)
            {
                #input as unchanged
                output$converged[i,] <- cbind(as.character(check),as.character(check))
                
                #identify participants with responses
                if(sum(rmat[,i]==1)!=0)
                {output$participant[[check]] <- which(rmat[,i]==1)}
            }
            
            ####RENAME####
            if(ans == length(pot)+2)
            {
                #ask for new name for response
                newname <- readline("New name for response: ")
                
                #check if response matches a response already given
                if(any(newname==colnames(repmat)))
                {
                    #identify where the response is
                    conv <- colnames(repmat)[which(newname==colnames(repmat))]
                    
                    #let them know response already exists
                    message(paste("Response already exists. Converged with response:",conv))
                    
                    #input as changed to response
                    output$converged[i,] <- cbind(as.character(check),as.character(conv))
                    
                    #identify participants that this affects
                    if(sum(rmat[,i]==1)!=0)
                    {output$participant[[check]] <- which(rmat[,i]==1)}
                    
                    #updated the duplicate response matrix
                    if(check!=conv)
                    {repmat <- converge(repmat,as.character(conv),as.character(check))}
                    
                }else{
                    #if name suggested does not exist, then change it
                    #in the duplicate response matrix
                    colnames(repmat)[i] <- newname
                    
                    #let them know that it was changed
                    message(paste(check)," changed to ",newname)
                    
                    #input as changed to response
                    output$converged[i,] <- cbind(colnames(repmat)[i],newname)
                    
                    #identify participants that this affects
                    if(sum(rmat[,i]==1)!=0)
                    {output$participant[[check]] <- which(rmat[,i]==1)}
                }
            }
            
            ####REMOVE####
            if(ans == length(pot)+3)
            {
                #let them know that it was removed
                message(paste("Reponse removed:",check))
                
                #input as changed to response
                output$converged[i,] <- cbind(as.character(check),NA)
                
                #identify participants that this affects
                if(sum(rmat[,i]==1)!=0)
                {output$participant[[check]] <- which(rmat[,i]==1)}
                
                #remove response from duplicate response matrix
                repmat <- repmat[,-i]
            }
            
            ####ANOTHER LETTER####
            if(ans == length(pot)+4)
            {
                #ask for letter
                let <- readline("Type letter: ")
                
                #check if letter
                let.chk <- is.letter(let)
                
                #ask for letter
                while(!let.chk||nchar(let)>1)
                {
                    
                    let <- readline("Not a letter, please type a letter: ")
                    
                    #check if letter
                    let.chk <- is.letter(let)
                }
                
                #begin process with new letter
                first <- let
            }else{let.chn <- FALSE}
            
            ####TYPE MY OWN####
            if(ans == length(pot)+5)
            {
                #ask for response to be typed
                resp <- readline("Type response: ")
                
                #check if response already exists
                noresp <- !any(colnames(repmat)==resp)
                
                #while this response does not exist
                while(noresp)
                {
                    #duplicate response
                    orresp <- resp
                    
                    #let them know that there is not response with that name
                    #ask for new response, rename, or remove
                    ans2 <- menu(c("TYPE ANOTHER RESPONSE","RENAME","REMOVE"),title="No response with that name: ")
                    
                    #TYPE ANOTHER RESPONSE
                    if(ans2 == 1)
                    {
                        #ask for response to be typed
                        resp <- readline("Type another response: ")
                        
                        #check if response already exists
                        noresp <- !any(colnames(repmat)==resp)
                    }
                    
                    #RENAME
                    if(ans2 == 2)
                    {
                        #check if response matches a response already given
                        if(any(orresp==colnames(repmat)))
                        {
                            #identify where the response is
                            conv <- colnames(repmat)[which(orresp==colnames(repmat))]
                            
                            #let them know response already exists
                            message(paste("Response already exists. Converged with response:",conv))
                            
                            #input as changed to response
                            output$converged[i,] <- cbind(as.character(check),as.character(conv))
                            
                            #identify participants that this affects
                            if(sum(rmat[,i]==1)!=0)
                            {output$participant[[check]] <- which(rmat[,i]==1)}
                            
                            #updated the duplicate response matrix
                            if(check!=conv)
                            {repmat <- converge(repmat,as.character(conv),as.character(check))}
                            
                        }else{
                            #if name suggested does not exist, then change it
                            #in the duplicate response matrix
                            colnames(repmat)[i] <- orresp
                            
                            #let them know that it was changed
                            message(paste(check)," changed to ",orresp)
                            
                            #input as changed to response
                            output$converged[i,] <- cbind(colnames(repmat)[i],orresp)
                            
                            #identify participants that this affects
                            if(sum(rmat[,i]==1)!=0)
                            {output$participant[[check]] <- which(rmat[,i]==1)}
                        }
                        
                        #break out of no response
                        noresp <- FALSE
                        
                        #skip out of resp
                        resp <- NA
                    }
                    
                    #REMOVE
                    if(ans2 == 3)
                    {
                        #let them know that it was removed
                        message(paste("Reponse removed:",check))
                        
                        #input as changed to response
                        output$converged[i,] <- cbind(as.character(check),NA)
                        
                        #identify participants that this affects
                        if(sum(rmat[,i]==1)!=0)
                        {output$participant[[check]] <- which(rmat[,i]==1)}
                        
                        #remove response from duplicate response matrix
                        repmat <- repmat[,-i]
                        
                        #break out of no response
                        noresp <- FALSE
                        
                        #skip out of resp
                        resp <- NA
                    }
                }
                
                if(!is.na(resp))
                {
                    #let them know that it was changed
                    output$converged[i,] <- cbind(as.character(check),as.character(resp))
                    
                    #identify participants that this affects
                    if(sum(rmat[,i]==1)!=0)
                    {output$participant[[check]] <- which(rmat[,i]==1)}
                    
                    #let them know that it was changed
                    message(paste(check)," changed to ",resp)
                    
                    #updated the duplicate response matrix
                    if(check!=resp)
                    {repmat <- converge(repmat,as.character(resp),as.character(check))}
                }
            }
        }
    }
    
    output$rmat <- as.data.frame(repmat)
    output$converged <- as.data.frame(output$converged)
    
    chnResp <- which(is.na(match(output$converged$from,output$converged$to)))
    
    output$changed <- output$converged[chnResp,]
        
    output$participant <- output$participant[order(names(output$participant))]
    
    return(output)
}
#----