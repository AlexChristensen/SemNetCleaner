#' Text Cleaner
#' @description An automated cleaning function for spell-checking, de-pluralizing,
#' removing duplicates, and binarizing text data
#' 
#' @param data A dataset of linguistic data
#' 
#' @param miss Value for missing data.
#' Defaults to 99
#' 
#' @param partBY Are participants by row or column?
#' Set to "row" for by row.
#' Set to "col" for by column
#' 
#' @param ID If subject IDs are included in the data file,
#' then the row or column must be specified
#' (e.g., if partBY = "row" and IDs are in the first column, then 1 should be entered)
#' 
#' @param database Database for more efficient text cleaning.
#' Defaults to NULL.
#' Can be a vector of a corpus or any text for comparison.
#' Currently, the only option is for "animals"
#' 
#' @return This function returns a list containing the following objects:
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
#' rmat <- semnetcleaner(trial, partBY = "col")
#' }
#' 
#' @references 
#' Hornik, K., & Murdoch, D. (2010).
#' Watch Your Spelling!.
#' \emph{The R Journal}, 3(2), 22-28.
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom stats na.omit
#' 
#' @export
#Text Cleaner----
textcleaner <- function(data, miss = 99,
                        partBY = c("row","col"),
                        ID = NULL,
                        database = NULL)
{
  if(!is.null(ID))
  {
      if(is.numeric(ID))
      {
          if(partBY == "row")
          {
              ids <- data[,ID]
              data <- data[,-ID]
          }else{
              ids <- data[ID,]
              data <- data[-ID,]
          }
      }else{stop("ID must be numeric")}
  }else{
      if(partBY == "row")
      {
          ids <- 1:nrow(data)
          message("IDs refer to row number")
      }else{
          ids <- 1:ncol(data)
          message("IDs refer to column number")
      }
  }
    
  #remove white space
  for(i in 1:ncol(data))
        data[,i]<-trimws(data[,i])    
      
  #convert missing value to NA
  for(i in 1:nrow(data))
      for(j in 1:ncol(data))
          if(is.na(data[i,j])){next
          }else if(data[i,j]==miss){data[i,j]<-NA} 
  
  #make participants by row
  if(partBY=="col")
  {data<-t(data)}
    
  #ensure data is a data frame for unlist
  data <- as.data.frame(data)
  
  #initialize y matrix for later
  y <- data
  
  #grab unique responses
  check <- unique(tolower(unlist(data)))
  
  #perform spell check
 
  ####OLD CODE####
  #v<-apply(data,c(2),qdap::check_spelling_interactive)
  ####OLD CODE####
  
  if(is.null(database))
  {v <- qdap::check_spelling_interactive(check)
  }else if(length(database)==1)
  {
      if(database == "animals")
      {v <- spell.check.database(check,database)}
  }else{v <- spell.check.database(check,database)}
  
  #singularize data
  #singularize <- function(x)
  #{
      #if(!is.na(x))
      #{
      #x <- as.character(x)
      #
      #sing <- vector(length=length(x))
      #
      #if(length(x)>1)
      #{
      #    for(i in 1:length(x))
      #    {
      #        Splural <- substring(x[i],nchar(x[i]),nchar(x[i]))
      #        ESplural <- substring(x[i],nchar(x[i])-1,nchar(x[i]))
      #        
      #        if(ESplural=="es")
      #        {sing[i] <- substring(x[i],1,nchar(x[i])-2)
      #        }else{sing[i] <- x[i]}
      #        if(Splural=="s")
      #        {sing[i] <- substring(x[i],1,nchar(x[i])-1)
      #        }else{sing[i] <- x[i]}
      #    }
      #}else{
      #    Splural <- substring(x,nchar(x),nchar(x))
      #    ESplural <- substring(x,nchar(x)-1,nchar(x))
      #    
      #    if(ESplural=="es")
      #    {sing <- substring(x,1,nchar(x)-2)
      #    }else{sing <- x}
      #    if(Splural=="s")
      #    {sing <- substring(x,1,nchar(x)-1)
      #    }else{sing <- x}
      #}
      
      #return(sing)
      #}else(sing <- x)
  #}
  
  #remove miscellaneous string additions
  for(i in 1:length(v))
  {
      
      v[i] <- gsub("[[:punct:]]", ".", v[i])
      v[i] <- gsub("[[:digit:]]+", "", v[i])
  }
  
  #combine check with v
  comb <- cbind(check,v)
  
  #organized changed list
  spellcheck <- list()
  
  colnames(comb) <- c("from","to")
  
  spellcheck$full <- comb[order(comb[,1]),]
  
  uniqcomb <- matrix(NA,nrow=nrow(comb),ncol=2)
  
  if(!all(is.na(comb[,2])))
  {
    for(i in 1:nrow(comb))
    {
          if(!is.na(comb[i,1]))
          {
                if(comb[i,1]!=comb[i,2]||is.na(comb[i,1]!=comb[i,2]))
                {uniqcomb[i,] <- comb[i,]}
          }
    }
      uniqchanged <- na.omit(uniqcomb)
      
      colnames(uniqchanged) <- c("from","to")
      
      spellcheck$unique <- uniqchanged[order(uniqchanged[,1]),]
  }else{spellcheck$unique <- c("none","changed")}
  
  #replace incorrectly spelled responses
  for(i in 1:nrow(y))
      for(j in 1:ncol(y))
      {
          if(!is.na(y[i,j]))
          {
              target <- which(tolower(y[i,j])==comb[,1])
              
              y[i,j] <- comb[target,2]
          }
      }
  
  
  #transform data into a writeable format
  ####OLD CODE####
  #if(is.list(v))
  #{for(i in 1:length(v))
  #      {
  #       if(is.null(v[[i]]))
  #         {v[[i]]<-data[,i]}
  #        y[,i]<-v[[i]]
  #      }
  #  }else(y<-v)
  ####OLD CODE####
  
  if(any(is.na(y)))
  for(i in 1:ncol(y))
      if(all(is.na(y[,i])))
      {y[,i]<-data[,i]}
  for(i in 1:nrow(y))
      for(j in 1:ncol(y))
          if(is.na(y[i,j]))
          {y[i,j]<-""}
      
  ####OLD CODE####
  #w<-tolower(w)
  ####OLD CODE####
  
  #transfer ids
  ####OLD CODE####
  #row.names(w) <- ids
  ####OLD CODE####
  
  #grab unique responses only and make them all lowercase
  uni<-rbind(sort(unique(tolower(unlist(apply(y,c(2),unique))))))

  #removing missing response from unique responses
  uni[uni==""]<-NA
  uni[uni==" "]<-NA
  uni[uni=="  "]<-NA
  while(any(is.na(uni)))
    for (i in 1:length(uni))
      if(is.na(uni[i])){uni<-uni[-i]}
  
  #attach unique responses to response matrix
  z<-matrix(nrow=nrow(y),ncol=length(uni)) #initialize matrix
  
  #populate response matrix
  for (i in 1:ncol(y))
  {z[,i]<-y[,i]}
 
  #initialize binary matrix
  k<-matrix(nrow=nrow(z),ncol=ncol(z))
  
  #match given responses to unique responses
      for (j in 1:nrow(k))
          if(any(!is.na(match(uni,z[j,]))))
          {k[j,]<-match(uni,z[j,])}
  k[is.na(k)]<-0
  
  #binarize matrix
  for (i in 1:ncol(k))
    for (j in 1:nrow(k))
      if (k[j,i]!=0){k[j,i]<-1}
  #column names to unique responses
  colnames(k) <- uni
  
  #check for subjects with no responses
  if(any(rowSums(k)==0))
  {
      rems <- which(rowSums(k)==0)
      warning(paste(length(rems)),
              " rows were removed for zero responses\nrow(s): ",paste(rems,collapse = ", "),
              "\nsubject ID(s): ",paste(ids[rems],collapse = ", "))
      k<-k[-rems,]
  }else{rems <- NA}
  
  #convert to data frame
  k<-as.data.frame(k)
  
  #removed list
  removed <- list()
  if(!is.na(rems))
  {
    removed$rows <- rems
    removed$ids <- ids[-rems]
    
    #row names to ids
    row.names(k) <- ids[-rems]
  }else{
      row.names(k) <- ids
      removed$rows <- NA
      removed$ids <- NA
      }
  
  #changed responses
  chnByPart <- list()
  for(i in 1:nrow(k))
  {
      target <- which(row.names(k)[i]==ids)
      u.vec <- unlist(data[target,])
      c.vec <- colnames(k)[which(k[i,]==1)]
      
      if(!is.na(any(u.vec == "")))
      {u.vec[which(u.vec == "")] <- NA}
      
      u.vec <- na.omit(u.vec)
      
      diff <- setdiff(u.vec,c.vec)
      
      if(length(diff)!=0)
      {
          chn <- matrix(0,nrow=length(diff),ncol=2)
          
          for(j in 1:length(diff))
          {
              chn[j,1] <- diff[j]
              chn[j,2] <- comb[which(tolower(diff[j])==comb[,1]),2]
          }
          
          colnames(chn) <- c("from","to")
      }else{chn <- NA}
      
      chnByPart[[setdiff(ids,removed$ids)[i]]] <- chn
  }
  
  #change ids in chnByPart to names
  if(is.numeric(ids))
  {
    names(chnByPart) <- seq_along(chnByPart)
    chnByPart[sapply(chnByPart, is.null)] <- NULL
  }

  return(
      list(
          binary=k,
          responses=y,
          spellcheck=spellcheck,
          removed=rems,
          partChanges=chnByPart)
      )
}
#----