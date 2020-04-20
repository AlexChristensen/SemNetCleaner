#' Correct Changes from \code{\link[SemNetCleaner]{textcleaner}}
#' 
#' @description A function that corrects changes that were made
#' automatically by \code{\link[SemNetCleaner]{textcleaner}}
#' 
#' @param textcleaner.obj Object from \code{\link[SemNetCleaner]{textcleaner}}
#' 
#' @param changes Matrix.
#' A matrix with changes made the \code{\link[SemNetCleaner]{textcleaner}}
#' object \code{$spellcheck$automated}
#' 
#' @return This function returns the corrected lists from \code{\link[SemNetCleaner]{textcleaner}}s:
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
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#' 
# Correct changes----
# Updated 19.04.2020
# Major update: 19.04.2020
correct.changes <- function(textcleaner.obj, changes)
{
  # Check if textcleaner object is input
  if(!class(textcleaner.obj) == "textcleaner")
  {stop("A 'textcleaner' class object was not input in the 'textcleaner.obj' argument")}
  
  # Store textcleaner object as result list
  res <- textcleaner.obj
  
  ## Original is used (rather than corrected) to run through same preprocessing
  ## as in textcleaner (far more efficient than actually changing through each
  ## object in the results list)
  original <- as.matrix(res$responses$original)
  
  # Create new correspondence matrix
  correspondence <- res$spellcheck$correspondence
  
  # Get number of columns between correspondence and changes matrices to match
  if(ncol(correspondence) > ncol(changes))
  {
    ## Difference in number of columns
    diff <- ncol(correspondence) - ncol(changes)
    
    ## Tack on NA columns
    for(i in 1:diff)
    {changes <- as.matrix(cbind(changes, rep(NA, nrow(changes))))}
    
  }else if(ncol(correspondence) < ncol(changes))
  {
    ## Difference in number of columns
    diff <- ncol(changes) - ncol(correspondence)
    
    ## Tack on NA columns
    for(i in 1:diff)
    {correspondence <- as.matrix(cbind(correspondence, rep(NA, nrow(correspondence))))}
  }
  
  # Update correspondence matrix
  correspondence[row.names(res$spellcheck$automated),] <- changes
  
  # Create 'from' list
  from <- as.list(correspondence[,"from"])
  
  # Create 'to' list
  to <- apply(correspondence[,grep("to", colnames(correspondence))], 1, function(x){unname(na.omit(x))})
  
  # Create correspondence matrix (error catch)
  corr.mat <- try(
    correspondence.matrix(from, to),
    silent = TRUE
  )
  
  if(any(class(corr.mat) == "try-error"))
  {return(error.fun(corr.mat, "correspondence.matrix", "correct.changes"))}
  
  ## Update with changes made by user
  res$spellcheck$automated <- changes
  
  # Get spell-corrected data (error catch)
  corrected <- try(
    correct.data(original, corr.mat),
    silent = TRUE
  )
  
  if(any(class(corrected) == "try-error"))
  {return(error.fun(corrected, "correct.data", "correct.changes"))}
  
  ## Collect behavioral data
  behavioral <- corrected$behavioral
  
  ## Make sure to replace faux "NA" with real NA
  corrected$corrected[which(corrected$corrected == "NA")] <- NA
  res$responses$corrected <- as.data.frame(corrected$corrected, stringsAsFactors = FALSE)
  
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
  {return(error.fun(res$responses$binary, "resp2bin", "correct.changes"))}
  
  behavioral <- cbind(behavioral, rowSums(res$responses$binary))
  colnames(behavioral)[3] <- "Appropriate"
  res$behavioral <- as.data.frame(behavioral)
  
  #make 'textcleaner' class
  class(res) <- "textcleaner"
  
  return(res)
}