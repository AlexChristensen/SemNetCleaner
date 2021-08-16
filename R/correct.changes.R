#' Correct Changes from \code{\link[SemNetCleaner]{textcleaner}}
#' 
#' @description A function that corrects changes that were made
#' automatically by \code{\link[SemNetCleaner]{textcleaner}}
#' 
#' @param textcleaner.obj Object from \code{\link[SemNetCleaner]{textcleaner}}
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
#' \item{\code{changes}}
#' {Only the changes made within the function \code{\link[SemNetCleaner]{correct.changes}}}
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
#' @importFrom utils edit
#' 
#' @export
#' 
# Correct changes----
# Updated 15.08.2021
# Major update: 19.04.2020
correct.changes <- function(textcleaner.obj)
{
  
  # Check if textcleaner object is input
  if(!class(textcleaner.obj) == "textcleaner")
  {stop("A 'textcleaner' class object was not input in the 'textcleaner.obj' argument")}
  
  # Store textcleaner object as result list
  res <- textcleaner.obj
  
  ## Original automated responses
  automated <- res$spellcheck$automated
  
  # Make sure automated responses are a matrix
  if(is.vector(automated)){
    automated <- t(as.matrix(automated))
  }
  
  # Get changes
  ## Check operating system
  OS <- system.check()$OS
  
  if(OS == "linux"){
    
    # Set up message to user
    cat(colortext("\nYou will now have a chance to correct the changes that", defaults = "message"))
    cat(colortext("\nwere made during the automated spell-checking process.", defaults = "message"))
    cat(colortext("\nA spreadsheet will open allowing you to manually correct", defaults = "message"))
    cat(colortext("\nthese changes.\n\n", defaults = "message"))
    
    readline("Press ENTER to continue...")
    
    cat(colortext("\nThe first column of the spreadsheet corresponds to the", defaults = "message"))
    cat(colortext("\nrow number provided in the output object `$spellcheck$correspondence`", defaults = "message"))
    cat(colortext("\n(see ?textcleaner for more information about this output).", defaults = "message"))
    
    cat(colortext("\n\nThe second column is the original response the participant provided", defaults = "message"))
    cat(colortext(paste("\nand columns 3 through", 3 + (ncol(textcleaner.obj$spellcheck$automated) - 2),
                        "are the automated spell-check responses."), defaults = "message"))
    cat(colortext('\nThese columns will have names formatted with "to_#".\n\n', defaults = "message"))
    
    readline("Press ENTER to continue...")
    
    cat(colortext(paste("\nYou should change columns 3 through", 3 + (ncol(textcleaner.obj$spellcheck$automated) - 2),
                        "by manually typing responses."), defaults = "message"))
    cat(colortext('\nFor inappropriate responses, "NA" should be typed. When finished,', defaults = "message"))
    cat(colortext('\nyou can exit this process by clicking the "X" in the top right', defaults = "message"))
    cat(colortext('\ncorner of the spreadsheet.\n\n', defaults = "message"))
    
    readline("Press ENTER to proceed with spell-check.")
    
    changes <- edit(automated)
    
  }else{
    
    # Set up message to user
    cat(colortext("\nYou will now have a chance to correct the changes that", defaults = "message"))
    cat(colortext("\nwere made during the automated spell-checking process.", defaults = "message"))
    cat(colortext("\nA spreadsheet will open allowing you to manually correct", defaults = "message"))
    cat(colortext("\nthese changes.\n\n", defaults = "message"))
    
    readline("Press ENTER to continue...")
    
    cat(colortext("\nThe first column of the spreadsheet corresponds to the", defaults = "message"))
    cat(colortext("\nrow number provided in the output object `$spellcheck$correspondence`", defaults = "message"))
    cat(colortext("\n(see ?textcleaner for more information about this output).", defaults = "message"))
    
    cat(colortext("\n\nThe second column is the original response the participant provided", defaults = "message"))
    cat(colortext(paste("\nand columns 2 through", 2 + (ncol(textcleaner.obj$spellcheck$automated) - 2),
                        "are the automated spell-check responses."), defaults = "message"))
    cat(colortext('\nThese columns will have names formatted with "to_#".\n\n', defaults = "message"))
    
    readline("Press ENTER to continue...")
    
    cat(colortext(paste("\nYou should change columns 2 through", 2 + (ncol(textcleaner.obj$spellcheck$automated) - 2),
                        "by manually typing responses."), defaults = "message"))
    cat(colortext('\nFor inappropriate responses, "NA" should be typed. When finished,', defaults = "message"))
    cat(colortext('\nyou can exit this process by clicking the "Done" in the top right', defaults = "message"))
    cat(colortext('\ncorner of the spreadsheet.\n\n', defaults = "message"))
    
    readline("Press ENTER to proceed with spell-check.")
  
    changes <- SemNetCleanerEdit() # Make changes
    automated <- as.matrix(automated) # Convert back to matrix
    changes <- as.matrix(changes) # Convert changes to matrix
    changes <- ifelse(changes == "", NA, changes) # Replace blanks with NA
  }
  
  # Get differences
  #differences <- automated[,-1] != changes[,-1]
  differences <- automated != changes
  
  ## Ensure matrix
  if(!is.matrix(differences)){
    differences <- t(as.matrix(differences))
  }
  
  # Provide changes for user
  ## Find rows that have changed
  target.changes <- which(apply(differences, 1, function(x){any(x)}))
  
  # If there are no changes, then return original object
  if(length(target.changes) == 0)
  {
    message("\nNo responses changed.")
    
    return(textcleaner.obj)
    
  }else{
    
    ## Initialize track changes
    track.changes <- list()
    
    ## Loop through changes
    for(i in 1:length(target.changes))
    {
      ## Set up change matrix
      chn.mat <- rbind(automated[target.changes[i],], changes[target.changes[i],])
      colnames(chn.mat)[-1] <- rep("to", ncol(chn.mat)-1)
      row.names(chn.mat) <- c("Previous", "Corrected")
      if(any(apply(chn.mat, 2, function(x){all(is.na(x))}))){
        chn.mat <- chn.mat[,-which(apply(chn.mat, 2, function(x){all(is.na(x))}))]
      }
      
      track.changes[[automated[target.changes[i],1]]] <- chn.mat
    }
    
    res$spellcheck$verified <- track.changes
    
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
    res$spellcheck$correspondence <- correspondence
    
    # Create 'from' list
    from <- as.list(correspondence[,"from"])
    
    # Create 'to' list
    if(any(is.na(correspondence[,grep("to", colnames(correspondence))]))){
      to <- apply(correspondence[,grep("to", colnames(correspondence))], 1, function(x){unname(na.omit(x))})
    }else{to <- correspondence[,grep("to", colnames(correspondence))]}
    
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
    
    ## Cleaned responses (no instrusions or perseverations)
    cleaned.list <- apply(corrected$corrected, 1, function(x){unique(na.omit(x))})
    
    ## Check if cleaned.list is a list
    if(!is.list(cleaned.list)){
      cleaned.list <- apply(cleaned.list, 1, as.list)
    }
    
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
  
}
