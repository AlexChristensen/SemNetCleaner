#' Text Cleaner
#' @description An automated cleaning function for spell-checking, de-pluralizing,
#' removing duplicates, and binarizing text data
#' 
#' @param data Matrix or data frame.
#' A dataset of text data.
#' Participant IDs will be automatically identified
#' if they are included.
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
#' Defaults to \code{NULL}, which will use \code{\link[SemNetDictionaries]{general.dictionary}}
#' 
#' Use \code{dictionaries()} or \code{find.dictionaries()} for more options
#' (See \code{\link{SemNetDictionaries}} for more details)
#' 
#' @param tolerance Numeric.
#' The distance tolerance set for automatic spell-correction purposes.
#' This function uses the function \code{\link[stringdist]{stringdist}}
#' to compute the \href{https://en.wikipedia.org/wiki/Damerau-Levenshtein_distance}{Damerau-Levenshtein}
#' (DL) distance, which is used to determine potential best guesses.
#' 
#' Unique words (i.e., \emph{n} = 1) that are within the (distance) tolerance are
#' automatically output as \code{\link[SemNetCleaner]{best.guess}} responses, which are then passed through
#' \code{\link[SemNetCleaner]{word.check.wrapper}}. If there is more than one word
#' that is within or below the distance tolerance, then these will be provided as potential
#' options.
#' 
#' The recommended and default distance tolerance is \code{tolerance = 1},
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
#' After the cleaning process is through, you can check the \code{spellcheck$auto}
#' output of \code{\link[SemNetCleaner]{textcleaner}} to see what changes
#' you made. To correct any changes you made in the cleaning process,
#' you can use the \code{\link[SemNetCleaner]{correct.changes}} function
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
#' \item{responses}{A list containing two objects:
#' 
#' \itemize{
#' 
#' \item{clean}
#' {A response matrix that has been spell-checked and de-pluralized with duplicates removed.
#' This can be used as a final dataset for analyses (e.g., fluency of responses)}
#' 
#' \item{original}
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
#' if(interactive())
#' {
#'     #Full test
#'     clean <- textcleaner(open.animals[,-c(1,2)], partBY = "row", dictionary = "animals")
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
    
    #remove white space
    data <- apply(data,2,trimws)
  
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
      }else{
        #make row ids
        ids <- 1:nrow(data)
        #let user know what IDs refer to
        message("IDs refer to row number")
      }
    }else{
      #make row ids
      ids <- 1:nrow(data)
      #let user know what IDs refer to
      message("IDs refer to row number")
    }
    
    #add row names to the data
    row.names(data) <- ids
    
    #convert missing value to NA
    mat.dat <- as.matrix(data)
    mat.dat <- ifelse(mat.dat==paste(miss),NA,mat.dat)
  
    #remove miscellaneous string additions from data
    mat.dat <- apply(mat.dat, 1:2, function(y) gsub(pattern="[[:punct:]]",replacement="",x=y))
    mat.dat <- apply(mat.dat, 1:2, function(y) gsub(pattern="[[:digit:]]",replacement="",x=y))

    #remove bad responses
    data <- apply(mat.dat,1:2,bad.response)
    #change NAs in data to ""
    data <- ifelse(is.na(data),"",data)
    #make trim white space again
    data <- apply(apply(data,2,trimws),1:2,rm.lead.space)
    #and make all lower case
    data <- apply(data,2,tolower)
    
    #create duplicate for later
    data <- as.data.frame(data, stringsAsFactors = FALSE)
    duplicate <- data
    
    #unique responses for efficient spell-checking
    check <- na.omit(unique(unlist(data)))
    
    #############################
    #### MAIN FUNCTION BEGIN ####
    #############################
    
    #perform spell-check
    if(is.null(dictionary))
    {
      #if no dictionary specified, then use general dictionary
      spell.check <- spell.check.dictionary(check, dictionary = "general",
                                            part.resp = duplicate, tolerance = tolerance)
      
    }else{
      #if dictionaries, then perform in-house spell-check
      spell.check <- spell.check.dictionary(check, dictionary,
                                            part.resp = duplicate, tolerance = tolerance)
    }
    
    #separate 'from' and 'to' change lists
    from <- spell.check$from
    to <- spell.check$to
    from.inc <- spell.check$from.inc
    to.inc <- spell.check$to.inc
    
    #############################
    ##### MAIN FUNCTION END #####
    #############################
    
    #let user know that data is being prepared
    message("Preparing your data...")
    
    #make into matrix
    #get maximum columns from 'to'
    max.to <-  max(unlist(lapply(to,length)))
    max.to.inc <-  max(unlist(lapply(to.inc,length)))
    
    #initialize combination matrices
    comb.mat <- matrix(NA,nrow=length(from),ncol=(1+max.to))
    comb.mat.inc <- matrix(NA,nrow=length(from.inc),ncol=(1+max.to.inc))
    
    #populate comb.mat
    for(i in 1:length(from))
    {
        #place original response in first column
        comb.mat[i,1] <- from[[i]]
        
        #fill 
        comb.mat[i,2:(1+length(to[[i]]))] <- bad.response(to[[i]])
    }
    
    #populate comb.mat
    for(i in 1:length(from.inc))
    {
      #place original response in first column
      comb.mat.inc[i,1] <- from.inc[[i]]
      
      #fill 
      comb.mat.inc[i,2:(1+length(to.inc[[i]]))] <- bad.response(to.inc[[i]])
    }
    
    colnames(comb.mat) <- c("from",rep("to",max.to))
    colnames(comb.mat.inc) <- c("from",rep("to",max.to.inc))
    
    #initialize spellcheck list
    spellcheck <- list()
    
    #initialize unique changes matrix
    uniqcomb <- ifelse(is.na(comb.mat),"",comb.mat)
    uniqcomb.inc <- ifelse(is.na(comb.mat.inc),"",comb.mat.inc)
  
    #full list of changed and unchanged responses
    spellcheck$full <- as.data.frame(uniqcomb,stringsAsFactors = FALSE)
    
    #check for rows that need to be removed
    rm.rows <- full.match(uniqcomb[,1],uniqcomb[,2])
    rm.rows.inc <- full.match(uniqcomb.inc[,1],uniqcomb.inc[,2])
    
    #unique list of changed and unchanged responses
    spellcheck$unique <- as.data.frame(uniqcomb[-which(rm.rows),],stringsAsFactors = FALSE)
    
    #unique list of spell-checked responses
    spellcheck$auto <- as.data.frame(uniqcomb.inc[-which(rm.rows.inc),],stringsAsFactors = FALSE)
    
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
  
  #changes by participant
  ##initialize participant lists
  chnByPart <- vector("list", length = length(ids))
  names(chnByPart) <- ids
  
  for(i in 1:length(ids))
  {
    # Target diff
    target.diff <- na.omit(unlist(setdiff(data[i,],duplicate[i,])))
    
    if(length(target.diff)!=0)
    {
      # Change matrix
      chn <- matrix(NA, nrow = length(target.diff), ncol = max.to + 1)
      colnames(chn) <- c("from",rep("to", max.to))
      
      # Insert old responses
      chn[,"from"] <- target.diff
      
      # Insert corrections
      for(k in 1:length(target.diff))
      {
        # Target old response in changes
        old.target <- which(spellcheck$unique[,"from"]==target.diff[k])
        
        # New response
        new.resp <- as.matrix(bad.response(spellcheck$unique[old.target,2:max.to]))
        
        # Insert into changes
        chn[k,2:max.to] <- new.resp
      }
      
      # Put into participant changes
      chnByPart[[i]] <- chn
      
    }else{chnByPart[[i]] <- NA}
    
  }
  
  #remove NAs in columns function
  na.col <- function (df)
  {
    if(any(!is.na(df)))
    {df <- df[,colSums(is.na(df))<nrow(df)]
    }else{df <- NA}
    
    return(df)
  }
  
  # Remove NA columns
  chnByPart <- lapply(chnByPart,na.col)
  
  
  #responses list
  responses <- list()
  responses$clean <- duplicate #cleaned fluency matrix (with responses in the order given by the participant)
  responses$original <- data #original fluency matrix 
  
  #results
  results <- list(
      binary = bin.mat, #binary response matrix
      responses = responses, #the cleaned and original responses
      spellcheck = spellcheck, #the 'full', 'unique', and 'auto' changes from the spell-check
      removed = removed, #participants that were removed (because they had zero appropriate responses)
      partChanges = chnByPart #changes made to each participant
  )

  #make 'textcleaner' class
  class(results) <- "textcleaner"

  return(results)
}
#----