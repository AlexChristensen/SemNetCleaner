#' Text Cleaner
#' 
#' @description An automated cleaning function for spell-checking, de-pluralizing,
#' removing duplicates, and binarizing text data
#' 
#' @param data Matrix or data frame.
#' A dataset of text data.
#' Participant IDs will be automatically identified if they are included.
#' If no IDs are provided, then their order in the corresponding
#' row (or column is used). A message will notify the user how IDs were assigned
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
#' @param continue List.
#' A result previously unfinished that still needs to be completed.
#' Allows you to continue to manually spell-check their data
#' after you've closed or errored out.
#' Defaults to \code{NULL}
#' 
#' @param walkthrough Boolean.
#' Whether a walkthrough should be provided (recommended for first time users).
#' Defaults to \code{NULL}, which will ask whether you would like a walkthrough.
#' Set to \code{TRUE} to do the walkthrough.
#' Set to \code{FALSE} to skip the walkthrough
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
# Text Cleaner----
# Updated 19.04.2020
# Major update: 19.04.2020
textcleaner <- function(data = NULL, miss = 99,
                        partBY = c("row","col"),
                        dictionary = NULL,
                        continue = NULL,
                        walkthrough = NULL)
{
  # Check if user is continuing from a previous point
  if(is.null(continue))
  {
    ## Make participants by row
    if(partBY=="col")
    {
      ### Transpose data
      data <- t(data)
      
      ### Let user know
      message("\nParticipants were made to go across the rows")
    }
    
    ## Change row names to IDs (error catch)
    id.res <- try(
      obtain.id(data),
      silent = TRUE
    )
    
    if(any(class(id.res) == "try-error"))
    {return(error.fun(id.res, "obtain.id", "textcleaner"))}
    
    data <- id.res$data
    ids <- id.res$ids
    row.names(data) <- ids
    
    ## Convert missing data to "" (returns data as matrix; error catch)
    data <- try(
      convert.miss(data, miss),
      silent = TRUE
    )
      
    if(any(class(data) == "try-error"))
    {return(error.fun(data, "convert.miss", "textcleaner"))}
    
    ## Prepare for spellcheck.dictionary (returns data as data frame)
    ### Removes punctuations and digits
    ### Removes white spaces
    ### Makes all responses lower case
    data <- try(
      prep.spellcheck.dictionary(data),
      silent = TRUE
    )
    
    if(any(class(data) == "try-error"))
    {return(error.fun(data, "prep.spellcheck.dictionary", "textcleaner"))}
    
    ## Obtain unique responses for efficient spell-checking
    uniq.resp <- na.omit(unique(unlist(data)))
    
    # Perform spell-check
    spell.check <- try(
      spellcheck.dictionary(uniq.resp = uniq.resp,
                            dictionary = ifelse(is.null(dictionary), "general", dictionary),
                            data = data, walkthrough = walkthrough),
      silent <- TRUE
    )
    
  }else if(length(continue) == 14) # Continue spell-check
  {spell.check <- spellcheck.dictionary(continue = continue)
  }else{spell.check <- continue}
  
  # Check if spell-check was stopped (either error or user stop)
  if(spell.check$stop)
  {return(spell.check)}
  
  # Let the user know that their data is being prepared
  message("\nPreparing your data...")
  
  # Intialize results to return
  res <- list()
  
  # Specify variables from spellcheck.dictionary returns
  
  ## Return dictionary if user decided to
  if("dictionary" %in% names(spell.check))
  {res$dictionary <- spell.check$dictionary}
  
  ## Re-assign data and ids variables in case of user stoppage or error
  data <- spell.check$data
  ids <- row.names(spell.check$data)
  res$responses$original <- data
  
  ## Assign spell-checking objects
  original <- spell.check$from
  checked <- spell.check$to
  
  # Create correspondence matrix (error catch)
  corr.mat <- try(
    correspondence.matrix(original, checked),
    silent = TRUE
  )
  
  if(any(class(corr.mat) == "try-error"))
  {
    error.fun(corr.mat, "correspondence.matrix", "textcleaner")
    
    return(spell.check)
  }
    
  row.names(corr.mat) <- formatC(1:nrow(corr.mat), digits = 2, flag = 0)
  res$spellcheck$correspondence <- corr.mat
  res$spellcheck$automated <- corr.mat[spell.check$auto,]
  res$spellcheck$manual <- corr.mat[spell.check$manual,]
  
  # Get spell-corrected data (error catch)
  corrected <- try(
    correct.data(data, corr.mat),
    silent = TRUE
  )
  
  if(any(class(corrected) == "try-error"))
  {
    error.fun(corrected, "correct.data", "textcleaner")
    
    return(spell.check)
  }
  
  ## Collect behavioral data
  behavioral <- corrected$behavioral
  
  ## Make sure to replace faux "NA" with real NA
  corrected$corrected[which(corrected$corrected == "NA")] <- NA
  res$responses$checked <- as.data.frame(corrected$corrected, stringsAsFactors = FALSE)
  
  ## Cleaned responses (no instrusions or perseverations)
  cleaned.list <- apply(corrected$corrected, 1, function(x){unique(na.omit(x))})
  
  max.resp <- max(unlist(lapply(cleaned.list, length)))
  
  cleaned.matrix <- t(sapply(
    lapply(cleaned.list, function(x, max.resp){
      c(x, rep(NA, max.resp - length(x)))
    }, max.resp = max.resp)
    ,rbind))
  
  colnames(cleaned.matrix) <- paste("Response_", formatC(1:ncol(cleaned.matrix),
                                                         digits = nchar(ncol(cleaned.matrix)) - 1,
                                                         flag = "0"), sep = "")
  
  res$responses$clean <- cleaned.matrix
  
  
  # Convert to binary response matrix (error catch)
  res$responses$binary <- try(
    resp2bin(corrected$corrected),
    silent = TRUE
  )
  
  if(any(class(res$responses$binary) == "try-error"))
  {
    error.fun(corrected, "resp2bin", "textcleaner")
    
    return(spell.check)
  }
  
  behavioral <- cbind(behavioral, rowSums(res$responses$binary))
  colnames(behavioral)[3] <- "Appropriate"
  res$behavioral <- as.data.frame(behavioral)

  #make 'textcleaner' class
  class(res) <- "textcleaner"

  return(res)
}
#----