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
#' @param spelling Character vector.
#' English spelling to be used.
#' \itemize{
#' 
#' \item{\code{"UK"}}
#' {For British spelling (e.g., colour, grey, programme, theatre)}
#' 
#' \item{\code{"US"}}
#' {For American spelling (e.g., color, gray, program, theater)}
#' 
#' }
#' 
#' @param add.path Character.
#' Path to additional dictionaries to be found.
#' DOES NOT search recursively (through all folders in path)
#' to avoid time intensive search.
#' Set to \code{"choose"} to open an interactive directory explorer
#' 
#' @param keepStrings Boolean.
#' Should strings be retained or separated?
#' Defaults to \code{FALSE}.
#' Set to \code{TRUE} to retain strings as strings
#' 
#' @param allowPunctuations Character vector.
#' Allows punctuation characters to be included in responses.
#' Defaults to \code{"-"}.
#' Set to \code{"all"} to keep all punctuation characters
#' 
#' @param allowNumbers Boolean.
#' Defaults to \code{FALSE}.
#' Set to \code{TRUE} to keep numbers in text
#' 
#' @param lowercase Boolean.
#' Should words be converted to lowercase?
#' Defaults to \code{TRUE}.
#' Set to \code{FALSE} to keep words as they are
#' 
#' @param continue List.
#' A result previously unfinished that still needs to be completed.
#' Allows you to continue to manually spell-check their data
#' after you've closed or errored out.
#' Defaults to \code{NULL}
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
#' \item{\code{clean}}
#' {A response matrix that has been spell-checked and de-pluralized with duplicates removed.
#' This can be used as a final dataset for analyses (e.g., fluency of responses)}
#' 
#' \item{\code{original}}
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
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom stats na.omit
#' 
#' @export
# Text Cleaner----
# Updated 05.01.2021
# Keep strings update: 06.08.2020
# Major update: 19.04.2020
textcleaner <- function(data = NULL, miss = 99,
                        partBY = c("row","col"),
                        dictionary = NULL, spelling = c("UK", "US"),
                        add.path = NULL, keepStrings = FALSE,
                        allowPunctuations = c("-", "all"),
                        allowNumbers = FALSE, lowercase = TRUE,
                        continue = NULL#, walkthrough = NULL
                        )
{
  
  # Warning for keepStrings
  if(keepStrings){
    warning("Keeping strings intact is a new feature. There may be bugs or unexpected behavior.")
    message("\nPlease send issues to:")
    message("\nhttps://github.com/AlexChristensen/SemNetCleaner/issues")
  }
  
  
  # Check for missing arguments
  if(is.null(continue)){
    
    ## Spelling
    if(missing(spelling)){
      spelling <- "US"
      message("\nThe 'spelling' argument was not set. Using default: 'US' English spelling")
      Sys.sleep(0.5)
    }else{
      spelling <- match.arg(spelling)
    }
    
    ## Allow punctuations
    if(missing(allowPunctuations)){
      allowPunctuations <- "-"
    }else{
      allowPunctuations <- match.arg(allowPunctuations, several.ok = TRUE)
    }
    
  }
  
  # Check if user is continuing from a previous point
  if(is.null(continue))
  {
    ## Make sure data is not tibble
    data <- as.matrix(data)
    
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
      prep.spellcheck.dictionary(data, allowPunctuations, allowNumbers, lowercase),
      silent = TRUE
    )
    
    if(any(class(data) == "try-error"))
    {return(error.fun(data, "prep.spellcheck.dictionary", "textcleaner"))}
    
    ## Obtain unique responses for efficient spell-checking
    uniq.resp <- na.omit(unique(unlist(data)))
    
    # Sort out dictionaries
    if(is.null(dictionary))
    {dictionary <- "general"}
    
    # Perform spell-check
    spell.check <- try(
      spellcheck.dictionary(uniq.resp = uniq.resp,
                            dictionary = dictionary,
                            spelling = spelling,
                            add.path = add.path,
                            keepStrings = keepStrings,
                            data = data#, walkthrough = walkthrough
                            ),
      silent <- TRUE
    )
    
  }else if(length(continue) != 3) # Continue spell-check
  {spell.check <- spellcheck.dictionary(continue = continue)
  }else{spell.check <- continue}
  
  # Check if spell-check was stopped (either error or user stop)
  if(spell.check$stop)
  {return(spell.check)}
  
  # Let the user know that their data is being prepared
  message("\nPreparing your data...")
  
  # Initialize results to return
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

  # Make 'textcleaner' class
  class(res) <- "textcleaner"
  
  # Correct auto-corrections
  ## Check if there were auto-corrections
  if(length(res$spellcheck$automated) != 0){
    
    res.check <- try(correct.changes(res), silent = TRUE)
    
    if(any(class(res.check) == "try-error"))
    {
      error.fun(res, "correct.changes", "textcleaner")
      
      return(res)
    }else{res <- res.check}
    
  }else{
    
    message("\nNo auto-corrections were made. Skipping automated spell-check verification.")
    
  }
  
  # Let user know spell-check is complete
  Sys.sleep(1)
  message("\nPreprocessing complete.\n")
  Sys.sleep(2)
  
  # Let user know where to send their dictionaries and monikers
  if("dictionary" %in% names(res)){
    
    dictionary.output <- paste(
      textsymbol("bullet"),
      "Dictionary output: `OBJECT_NAME$dictionary`",
      sep = " "
    )
    
  }
  
  moniker.output <- paste(
    textsymbol("bullet"),
    "Moniker output: `OBJECT_NAME$moniker`",
    sep = " "
  )
  
  ## Save moniker object (doubles up but makes it easy for the user)
  res$moniker <- res$spellcheck$manual
  
  cat(
    
    colortext(
      
      paste(
        paste(
          "Consider submitting your",
          ifelse("dictionary" %in% names(res), " dictionary and", ""),
          " spelling corrections (i.e., monikers) to:\n\n",
          sep = ""
        ),
        "https://github.com/AlexChristensen/SemNetDictionaries/issues/new/choose\n\n",
        ifelse("dictionary" %in% names(res), paste(dictionary.output, "\n\n"), ""),
        #dictionary.output,
        moniker.output, "\n\n"
      ),
      
      defaults = "message"
      
    )
  
  )
  
  Sys.sleep(2)
  

  return(res)
}
#----