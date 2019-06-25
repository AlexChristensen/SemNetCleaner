#' Text Cleaner
#' @description An automated cleaning function for spell-checking, de-pluralizing,
#' removing duplicates, and binarizing text data
#' 
#' @param data Matrix or data frame.
#' A dataset of text data.
#' Participant IDs should be made to be row or
#' column names to specify whether participants
#' are by row or column (see argument \code{partBY}).
#' If no IDs are provided, then their order in the corresponding
#' row (or column is used).
#' A message will notify the user how IDs were assigned
#' 
#' @param miss Numeric or character.
#' Value for missing data.
#' Defaults to \code{99}
#' 
#' @param partBY Character.
#' Are participants by row or column?
#' Set to \code{"row"} for by row.
#' Set to \code{"col"} for by column
#' 
#' @param dictionary Character vector.
#' Can be a vector of a corpus or any text for comparison.
#' Dictionary to be used for more efficient text cleaning.
#' Defaults to \code{NULL}, which will use \code{\link[qdapDictionaries]{qdapDictionaries}}.
#' 
#' Use \code{dictionaries()} and \code{extra.dictionaries()} for more options
#' (See \code{\link{SemNetDictionaries}} for more details)
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
#' @details When working through the menu options in \code{\link[SemNetCleaner]{textcleaner}},
#' there may be mistakes. For instance, selecting to \code{REMOVE} a response when really
#' all you wanted to do was \code{RENAME} a response. There are a couple of options:
#' 
#' RECOMMENDED
#' 
#' 1. You can make a note in your \code{R} script for the change you wanted
#' to make (you can keep moving through the cleaning process).
#' After the cleaning process is through, you can check the \code{spellcheck$unique}
#' output of \code{\link[SemNetCleaner]{textcleaner}} to see what changes
#' you made. To correct any changes you made in the cleaning process,
#' you can use the \code{\link[SemNetCleaner]{corr.chn}} function
#' 
#' NOT RECOMMENDED
#' 
#' 2. You can use \code{esc} to exit out of a menu selection process.
#' This is NOT recommended because you will lose all changes that
#' you've made up to that point
#' 
#' @return This function returns a list containing the following objects:
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
#' and \code{ids} identifies removed participants by their ID (see argument \code{data})}
#' 
#' \item{partChanges}{A list where each participant is a list index with each
#' response that was been changed. Participants are identified by their ID (see argument \code{data}).
#' This can be used to replicate the cleaning process and to keep track of changes more generaly.
#' Participants with \code{NA} did not have any changes from their original data
#' and participants with missing data are removed (see \code{removed$ids})}
#' 
#' @examples
#' #load trial data
#' data <- trial
#' 
#' \dontrun{
#' 
#' rmat <- textcleaner(data, partBY = "col")
#' }
#' 
#' @references 
#' Hornik, K., & Murdoch, D. (2010).
#' Watch Your Spelling!.
#' \emph{The R Journal}, \emph{3}, 22-28.
#' doi:\href{https://doi.org/10.32614/RJ-2011-014}{10.32614/RJ-2011-014}
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom stats na.omit
#' 
#' @export
#Text Cleaner----
textcleaner <- function(data, miss = 99,
                        partBY = c("row","col"),
                        dictionary = NULL,
                        tolerance = 1)
{
    #grab column names
    col.names <- colnames(data)
  
    #make sure data is a data.frame
    data <- as.data.frame(data, stringsAsFactors = FALSE)
    
    #remove white space
    data <- apply(data,2,trimws)
    
    #convert missing value to NA
    for(i in 1:nrow(data))
        for(j in 1:ncol(data))
            if(is.na(data[i,j])){next
                }else if(data[i,j]==miss){data[i,j]<-NA} 
    
    #make participants by row
    if(partBY=="col")
    {data <- t(data)}
    
    #id column
    id.col <- which(lapply(apply(data,2,unique),length)==nrow(data))
    
    if(length(id.col)!=0)
    {
      if(length(id.col)==1)
      {
        #grab unique identifier
        ids <- data[,id.col]
        #let user know what IDs refer to
        message(paste("IDs refer to variable:"," '",col.names[id.col],"'",sep=""))
        #remove unique identifier from data
        data <- data[,-id.col]
      }
    }else{
      #make row ids
      ids <- 1:nrow(data)
      #let user know what IDs refer to
      message("IDs refer to row number")
    }
    
    #ensure data is a data frame for unlist
    data <- data.frame(data,stringsAsFactors = FALSE)
    
    #remove miscellaneous string additions from data
    for(i in 1:nrow(data))
        for(j in 1:ncol(data))
            {
                #remove punctuations
                data[i,j] <- gsub("[[:punct:]]", "", data[i,j])
                
                #remove numbers
                data[i,j] <- gsub("[[:digit:]]+", "", data[i,j])
            }
    
    #initialize duplicate data matrix (used later on in line 190)
    duplicate <- data
    
    #make all lower case and initialize vector for spell-check
    check <- tolower(unlist(data))
    
    #trim white space again
    check <- trimws(check)
    check <- unlist(lapply(check,rm.lead.space))
    
    #unique responses for efficient spell-checking
    check <- unique(check)
    
    #############################
    #### MAIN FUNCTION BEGIN ####
    #############################
    
    #perform spell-check
    if(is.null(dictionary))
    {
        #if no dictionary, then default to qdap dictionaries
        spell.check <- qdap::check_spelling_interactive(check)
        
        #initialize 'from' and 'to' list for changes
        from <- list()
        to <- list()
        
        for(i in 1:length(check))
        {
            #original response
            from[[i]] <- check[i]
            
            #changed response
            #breaks continuous strings into
            #vector of word parts
            #e.g., "fox dog cat bear"
            # --> c("fox","dog","cat","bear")
            to[[i]] <- spell.check[i]
        }
        
    }else{
        
        #if dictionaries, then perform in-house spell-check
        spell.check <- spell.check.dictionary(check,dictionary)
        
        #separate 'from' and 'to' change lists
        from <- spell.check$from
        to <- spell.check$to
    }
    
    #############################
    ##### MAIN FUNCTION END #####
    #############################
    
    
    #make into matrix
    #get maximum columns from 'to'
    max.to <-  max(unlist(lapply(to,length)))
    
    #initialize combination matrix
    comb.mat <- matrix(NA,nrow=length(from),ncol=(1+max.to))
    
    #populate comb.mat
    for(i in 1:length(from))
    {
        #place original response in first column
        comb.mat[i,1] <- from[[i]]
        
        #fill 
        comb.mat[i,2:(1+length(to[[i]]))] <- bad.response(to[[i]])
    }
    
    colnames(comb.mat) <- c("from",rep("to",max.to))
    
    #initialize spellcheck list
    spellcheck <- list()
    
    #initialize unique changes matrix
    uniqcomb <- ifelse(is.na(comb.mat),"",comb.mat)
  
    #full list of changed and unchanged responses
    spellcheck$full <- as.data.frame(uniqcomb,stringsAsFactors = FALSE)
    
    #check for rows that need to be removed
    rm.rows <- full.match(uniqcomb[,1],uniqcomb[,2])
    
    #unique list of changed and unchanged responses
    spellcheck$unique <- as.data.frame(uniqcomb[-which(rm.rows),],stringsAsFactors = FALSE)
    
    #replace incorrectly spelled responses in duplicate matrix
    #change duplicate to matrix
    dup.mat <- as.matrix(duplicate)
    
    for(i in 1:nrow(dup.mat))
    {
        #remove NAs from dup.mat
        no.na.mat <- na.omit(tolower(dup.mat[i,]))
        
        if(length(no.na.mat)!=0)
        {
          #match row responses to combination matrix
          targets <- match(no.na.mat,comb.mat[,1])
          
          #grab unique responses for that participant
          replace.vec <- unique(na.omit(unlist(as.data.frame(comb.mat[targets,-1],stringsAsFactors = FALSE))))
          
          #clear out row
          dup.mat[i,] <- rep(NA,length(dup.mat[i,]))
          
          #enter corrected responses into row
          dup.mat[i,1:length(replace.vec)] <- replace.vec
        }
    }
    
    #remove NAs
    if(any(is.na(dup.mat)))
    {
        for(i in 1:nrow(dup.mat))
        {
            #identify which responses are missing
            target.na <- which(is.na(dup.mat[i,]))
            
            #replace NAs with blank responses
            dup.mat[i,target.na] <- rep("",length(target.na))
        }
    }
    
    #replace duplicate matrix as dup.mat data frame
    duplicate <- as.data.frame(dup.mat,stringsAsFactors = FALSE)
  
    #grab unique responses only and make them all lowercase
    uni <- sort(unique(unlist(duplicate)))

    #removing missing response from unique responses
    uni.clean <- na.omit(unlist(lapply(uni,bad.response)))
  
    #initialize binary response matrix
    bin.mat <- matrix(0,nrow = nrow(duplicate), ncol = length(uni.clean))
    colnames(bin.mat) <- uni.clean
    
    #match given responses to unique responses
    for(i in 1:nrow(duplicate))
    {
        #if any response match from cleaned responses in
        #duplicate matrix, then continue
        if(any(!is.na(match(uni.clean,duplicate[i,]))))
        {
            #identify target responses
            target.resp <- which(!is.na(match(uni.clean,duplicate[i,])))
            
            #give '1' for a response given
            bin.mat[i,target.resp] <- rep(1,length(target.resp))
        }
    }
  
  #check for subjects with no responses
  if(any(rowSums(bin.mat)==0))
  {
      #identify which people do not have responses
      rems <- which(rowSums(bin.mat)==0)
      
      #let user know these rows were removed
      warning(paste(length(rems)),
              " rows were removed for zero responses\nrow(s): ",paste(rems,collapse = ", "),
              "\nsubject ID(s): ",paste(ids[rems],collapse = ", "))
      
      #remove responses from binary matrix
      bin.mat <- bin.mat[-rems,]
  }else{rems <- NA}
  
  #convert to data frame
  bin.mat <- as.data.frame(bin.mat,stringsAsFactors = FALSE)
  
  #removed list
  removed <- list()
  if(!any(is.na(rems)))
  {
    removed$rows <- rems
    removed$ids <- ids[rems]
    
    #row names to ids
    row.names(bin.mat) <- ids[-rems]
  }else{
      row.names(bin.mat) <- ids
      removed$rows <- NA
      removed$ids <- NA
      }
  
  #changed responses
  chnByPart <- list()
  for(i in 1:nrow(bin.mat))
  {
      target <- which(row.names(bin.mat)[i]==ids)
      u.vec <- unlist(data[target,])
      c.vec <- colnames(bin.mat)[which(bin.mat[i,]==1)]
      
      if(!is.na(any(u.vec == "")))
      {u.vec[which(u.vec == "")] <- NA}
      
      u.vec <- na.omit(u.vec)
      
      diff <- setdiff(u.vec,c.vec)
      
      if(length(diff)!=0)
      {
          chn <- matrix("",nrow=length(diff),ncol=max.to+1)
          
          for(j in 1:length(diff))
          {
              chn[j,1] <- na.omit(diff[j])
              target <- na.omit(comb.mat[which(tolower(diff[j])==comb.mat[,1]),-1])
              
              if(length(target)==0)
              {chn[j,2] <- NA
              }else{chn[j,2:(length(target)+1)] <- target}
          }
          
          colnames(chn) <- c("from",rep("to",max.to))
          
          #initialize remove column
          rm.col <- vector(length=ncol(chn))
          
          for(k in 1:ncol(chn))
          {rm.col[k] <- all(chn[,k]=="")}
          
          rm.col <- na.omit(rm.col)
          
          #remove "" in change columns
          if(any(rm.col))
          {chn <- chn[,-which(rm.col)]}
          
      }else{chn <- NA}
      
      val <- as.matrix(setdiff(ids,removed$ids)[i])
      
      chnByPart[[val]] <- as.data.frame(chn,stringsAsFactors = FALSE)
  }
  
  #change ids in chnByPart to names
  if(is.numeric(ids))
  {
    names(chnByPart) <- seq_along(chnByPart)
    chnByPart[sapply(chnByPart, is.null)] <- NULL
  }
  
  #results
  results <- list(
      binary = bin.mat, #binary response matrix
      responses = duplicate, #cleaned fluency matrix (with responses in the order given by the participant)
      spellcheck = spellcheck, #the 'full' and 'unique' changes from the spell-check
      removed = removed, #participants that were removed (because they had zero appropriate responses)
      partChanges = chnByPart #changes made to each participant
  )

  #make 'textcleaner' class
  class(results) <- "textcleaner"

  return(results)
}
#----