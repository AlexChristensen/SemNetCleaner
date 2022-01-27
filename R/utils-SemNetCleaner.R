#%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### MULTIPLE FUNCTIONS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Line break function
linebreak <- function(breaks = "\n"){cat(breaks, colortext(paste(rep("-", getOption("width")), collapse = ""), defaults = "message"), "\n")}

#' Removes Leading Spaces
#' 
#' @description Removes leading spaces that are not caught by \code{\link{trimws}}
#' 
#' @param word Character.
#' A word that has leading spaces that cannot be removed by \code{\link{trimws}}
#' 
#' @return Word without leading spaces
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Remove lead spaces
# Updated 10.04.2020
rm.lead.space <- function(word)
{
  word <- bad.response(word)
  
  if(!is.na(word))
  {word <- gsub("^[[:space:]]+|[[:space:]]+$", "", word)}
  
  return(word)
}

#' Changes Bad Responses to NA
#' 
#' @description A sub-routine to determine whether responses are good or bad.
#' Bad responses are replaced with missing (\code{NA}). Good responses are returned.
#' 
#' @param word Character.
#' A word to be tested for whether it is bad
#' 
#' @param ... Vector.
#' Additional responses to be considered bad
#' 
#' @return If response is bad, then returns \code{NA}.
#' If response is valid, then returns the response
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Change Bad Responses
# Updated 10.04.2020
bad.response <- function (word, ...)
{
  # Other bad responses
  others <- unlist(list(...))
  
  # Bad responses
  bad <- c(NA, "NA", "", " ", "  ", others)
  
  # If there is no longer a response
  if(length(word)==0)
  {word <- NA}
  
  for(i in 1:length(word))
  {
    #if bad, then convert to NA
    if(word[i] %in% bad)
    {word[i] <- NA}
  }
  
  return(word)
}

#' Moniker Function
#' 
#' @description A sub-routine for identifying monikers and common misspellings
#' of words
#' 
#' @param word Word to check for moniker
#' 
#' @param misnom A list of monikers.
#' See \code{\link[SemNetDictionaries]{dictionaries}} for options
#' 
#' @param spelling Character.
#' Either \code{"UK"} or \code{"US"} for their respective spelling
#' 
#' @return If \code{word} matches a moniker, then the appropriate word is returned.
#' If \code{word} does not match a moniker, then the \code{word} is returned
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Moniker
# Updated 28.11.2020
moniker <- function (word, misnom, spelling)
{
  #unlist possible responses
  mis <- unlist(misnom)
  
  #if there is a match
  if(!is.na(match(word,mis)))
  {
    #then identify which word
    matched <- names(mis[match(word,mis)])
    
    #remove numbers
    misnomed <- gsub("[[:digit:]]+","", matched)
    
  }else{
    #return word if no moniker match
    misnomed <- word
  }
  
  #convert between UK-US spelling
  misnomed <- brit.us.conv(vec = misnomed, spelling = spelling, dictionary = FALSE)
  
  return(misnomed)
}

#' Appropriate Answer Function
#' 
#' @description Sub-rountine to check if answer provided to
#' \code{\link[SemNetCleaner]{spellcheck.menu}} is appropriate
#' (i.e., numeric and within response option range)
#' 
#' @param answer Character.
#' Output from \code{\link{menu}}
#' 
#' @param ans.range Numeric vector.
#' Range of possible numeric answers
#' 
#' @param default Numeric.
#' Length of default options for menu
#' 
#' @param dictionary Boolean.
#' Changes appropriate response options when the
#' menu is for a dictionary.
#' Defaults to \code{FALSE}
#' 
#' @return Appropriate numeric response
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
#' 
# Appropriate answer
# Updated 12.04.2020
appropriate.answer <- function (answer, choices, default, dictionary = FALSE)
{
  # Set return value
  ret <- NA
  
  # Set up appropriate answer values
  defaults <- paste(1:default)
  
  if(!dictionary)
  {
    # QWERTY response options
    qwert <- c(c("Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P")[1:(length(choices[-c(1:default)]))], "B", "H")
    
    # Approriate answer values
    app.ans <- c(defaults, qwert)
    
    # Assign number names
    first <- c(1:(length(defaults) + length(qwert)))
    names(app.ans) <- 1:length(app.ans)
    
  }else{
    # Approriate answer values
    app.ans <- defaults
    
    # Assign number names
    names(app.ans) <- 1:(length(defaults))
  }
  
  while(is.na(ret))
  {
    # Check if response is appropriate
    if(toupper(answer) %in% app.ans)
    {ret <- toupper(answer)
    }else{
      
      # Let user know of why response is inappropriate
      message("Inappropriate selection.")
      
      # Have them type a new response
      answer <- readline(prompt = "Please enter a new selection: ")
    }
  }
  
  # Convert back into numeric
  ret <- as.numeric(names(which(ret == app.ans)))
  
  return(ret)
}

#' Custom menu based on Base R's \code{\link{menui}}
#' 
#' @description Custom menu based on Base R's \code{\link{menui}}
#' 
#' @param choices Character.
#' A character vector of choices
#' 
#' @param title Character.
#' A character string to be used as the title of the menu.
#' \code{NULL} is also accepted
#' 
#' @param default Numeric.
#' Length of default options for menu
#' 
#' @param dictionary Boolean.
#' Changes appropriate response options when the
#' menu is for a dictionary.
#' Defaults to \code{FALSE}
#' 
#' @return Prints a menu to the console
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
#' 
# Custom Menu
# Updated 03.01.2020
customMenu <- function (choices, title = NULL, default, dictionary = FALSE, help = FALSE) 
{
  if (!interactive()) 
  {stop("menu() cannot be used non-interactively")}
  
  nc <- length(choices)
  
  if (length(title) && nzchar(title[1L]))
  {cat(title[1L], "\n")}
  
  # Set up response options
  if(!dictionary)
  {
    # QWERTY options
    qwert <- c("Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P")[1:(length(choices[-c(1:default)]))]
    
    op <- paste0(format(c(seq_len(default),qwert)), ": ", choices)
  }else{op <- paste0(format(seq_len(default)), ": ", choices)}
  
  if (nc > default) {
    fop <- format(op)
    nw <- nchar(fop[1L], "w") + 2L
    ncol <- getOption("width")%/%nw
    if (ncol > 1L)
      
      # Set up default
      if(default == 10 || default == 9)
      {
        word <- fop[1:5]
        word <- paste0(word, c(rep.int("  ", 4), "\n"), collapse = "")
        
        # Check number of characters
        nw <- nchar(word)
        
        if(substr(word, nw-1, nw) != "\n")
        {word <- paste(substr(word, 1, nw-2), "\n")}
        
        string <- fop[6:default]
        opts <- ifelse(default == 9, 3, 4)
        string <- paste0(string, c(rep.int("  ", opts), "\n"), collapse = "")
        
        # Check number of characters
        ns <- nchar(string)
        
        if(substr(string, ns-1, ns) != "\n")
        {string <- paste(substr(string, 1, ns-2), "\n")}
        
      }else{
        def <- fop[1:default]
        def <- paste0(def, c(rep.int("  ", min(nc, ncol) - 
                                       1L), "\n"), collapse = "")
        
        # Check number of characters
        n <- nchar(def)
        
        if(substr(def, n-1, n) != "\n")
        {def <- paste(substr(def, 1, n-2), "\n")}
      }
    
    # Set up responses
    resp <- fop[(default + 1):length(fop)]
    resp[1] <- paste0("\n", resp[1], collapse = "")
    resp.len <- length(resp)
    
    # Check if length less than ncol
    if(resp.len > ncol)
    {
      resp <- paste0(resp, c(rep.int("  ", min(nc, ncol) - 1L), "\n"), collapse = "")
      
      if((resp.len / ncol) %% 1 != 0)
      {resp <- paste0(paste0(resp, collapse = ""), "\n")}
      
    }else{resp <- paste0(paste0(resp, collapse = ""), "\n")}
    
    # Set up options
    if(default == 10 || default == 9)
    {
      op <- paste0(styletext("Word options\n", defaults = "underline"), word,
                   styletext("\nString options\n", defaults = "underline"), string,
                   styletext("\nResponse options", defaults = "underline"), resp,
                   collapse = "")
      
    }else if(default == 5)
    {
      op <- paste0(styletext("Word options\n", defaults = "underline"), def,
                   styletext("\nResponse options", defaults = "underline"), resp,
                   collapse = "")
      
    }else{stop("Error in customMenu")}
    
  }else{op <- c(op, "")}
  
  if(!help)
  {cat("", op, sep = "\n")
  }else{paste0("", resp, sep = "")}
}

#' Error function
#' 
#' @description Gives necessary information for user reporting error
#' 
#' @param result Character.
#' The error from the result
#' 
#' @param SUB_FUN Character.
#' Sub-routine the error occurred in
#' 
#' @param FUN Character.
#' Main function the error occurred in
#' 
#' @return Error and message to send to GitHub
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
#' 
#' @importFrom utils packageVersion
#' 
# Error SemNetCleaner
# Updated 16.04.2020
error.fun <- function(result, SUB_FUN, FUN)
{
  # Let user know that an error has occurred
  message(paste("\nAn error has occurred in the '", SUB_FUN, "' function of '", FUN, "':\n", sep =""))
  
  # Give them the error to send to you
  cat(paste(result))
  
  # Tell them where to send it
  message("\nPlease open a new issue on GitHub (bug report): https://github.com/AlexChristensen/SemNetCleaner/issues/new/choose")
  
  # Give them information to fill out the issue
  OS <- as.character(Sys.info()["sysname"])
  OSversion <- paste(as.character(Sys.info()[c("release", "version")]), collapse = " ")
  Rversion <- paste(R.version$major, R.version$minor, sep = ".")
  SNCversion <- paste(unlist(packageVersion("SemNetCleaner")), collapse = ".")
  SNDversion <- paste(unlist(packageVersion("SemNetDictionaries")), collapse = ".")
  
  # Let them know to provide this information
  message(paste("\nBe sure to provide the following information:\n"))
  
  # To reproduce
  message(styletext("To Reproduce:", defaults = "bold"))
  message(paste(" ", textsymbol("bullet"), " Function error occurred in: ", SUB_FUN, " function of ", FUN, sep = ""))
  
  # R, SemNetCleaner, and SemNetDictionaries
  message(styletext("\nR, SemNetCleaner, and SemNetDictionaries versions:", defaults = "bold"))
  message(paste(" ", textsymbol("bullet"), " R version: ", Rversion, sep = ""))
  message(paste(" ", textsymbol("bullet"), " SemNetCleaner version: ", SNCversion, sep = ""))
  message(paste(" ", textsymbol("bullet"), " SemNetDictionaries version: ", SNDversion, sep = ""))
  
  # Desktop
  message(styletext("\nOperating System:", defaults = "bold"))
  message(paste(" ", textsymbol("bullet"), " OS: ", OS, sep = ""))
  message(paste(" ", textsymbol("bullet"), " Version: ", OSversion, sep = ""))
}

#' Levenshtein Distance Adjusted for QWERTY Keyboard
#' 
#' @description Computes the Levenshtein distance but weights the distance
#' between keys on the keyboard to provide a more accurate estimate of
#' distance when typing
#' 
#' @param ... Additional arguments
#' 
#' @return Levenshtein distance adjusted for QWERTY keyboard
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom utils adist
#' 
#' @noRd
# QWERTY Keyboard
# Updated 10.04.2020
ql.dist <- function (wordA, wordB, allowPunctuations)
{
  characters <- paste("([", paste(allowPunctuations, collapse = ""), "])|[[:punct:]]", sep = "", collapse = "")
  data <- apply(data, 2, function(y) gsub(characters, "\\1", y))
  
  # Remove diacritic characters
  wordA <- stringi::stri_trans_general(wordA, "Latin-ASCII")
  wordA <- gsub("([-])|[[:punct:]]", "\\1", wordA)
  wordB <- stringi::stri_trans_general(wordB, "Latin-ASCII")
  wordB <- gsub("([-])|[[:punct:]]", "\\1", wordB)
  
  # QWERTY Keyboard distance
  # Keyboard structure
  # Taken from <https://stackoverflow.com/questions/43946912/calculating-levenshtein-distance-permitting-qwerty-errors-in-r>
  m <- structure(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0, 1, 1, 2, 2, 3, 
                   4, 5, 6, 4, 5, 6, 7, 8, 3, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
                   2, 1, 2, 1, 2, 1, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2),
                 .Dim = c(27L,2L),
                 .Dimnames = list(c("q", "w", "e", "r", "t", "y", "u", "i","o", "p",
                                    "a", "z", "s", "x", "d", "c", "f", "b", "m", "j", "g",
                                    "h", "j", "k", "l", "v", "n"), c("x", "y")))
  
  # Compile qwerty locations
  keyb <- sweep(m, 2, c(1, -1), "*")
  
  # Add [space]
  keyb <- rbind(keyb,c(4,-3),c(9,1),c(10,-1))
  row.names(keyb)[28:30] <- c(" ","-","'")
  
  # Possible keys
  keys <- c(letters, " ", "-", "'")
  
  # Initialize distances vector
  dvec <- numeric(2)
  
  # Weighted insertions/deletions
  if(nchar(wordA) != nchar(wordB))
  {
    # Character differences
    if(nchar(wordA) > nchar(wordB))
    {
      wordC <- wordB
      wordB <- wordA
      wordA <- wordC
    }
    
    chardiff <- nchar(wordB) - nchar(wordA)
    
    # Insert letters until character difference is zero
    while(chardiff != 0)
    {
      # Number of characters in wordA
      n <- nchar(wordA)
      
      # Initialize list
      res <- list()
      
      # Loop through adding letters
      for(i in 1:n)
      {res[[i]] <- paste(substr(wordA, -1, (i-1)), keys, substr(wordA, i, n), sep = "")}
      
      # Add letters to the end
      res[[(i+1)]] <- paste(wordA, keys, sep = "")
      
      # Check Levenshtein distance
      l.dist <- sapply(lapply(res, function(x){adist(x, wordB)}), rbind)
      
      # Minimum distances
      min.dist <- unique(unlist(res)[which(l.dist == min(l.dist))])[1]
      
      # Identify earlist letter insertion
      target <- which.min(apply(l.dist,2,min))
      
      # Compute average keyboard distance from target letter to keys before and after
      if(target == 1)
      {before <- 0
      }else{before <- sum(abs(keyb[substr(min.dist,(target-1),(target-1)),] - keyb[substr(min.dist, target, target),]))}
      
      if(target == nchar(min.dist))
      {after <- 0
      }else{after <- sum(abs(keyb[substr(min.dist,(target+1),(target+1)),] - keyb[substr(min.dist, target, target),]))}
      
      average <- mean(c(before,after))
      
      # Compute penalty based on average divided by the maximum key distance
      penalty <- average / max(rowSums(sweep(keyb, 2, keyb[substr(min.dist, target, target),], "-")))
      
      # Increase distance vector for insertion/deletion
      dvec[1] <- dvec[1] + penalty
      
      # Replace wordA
      wordA <- min.dist
      
      # Character differences
      chardiff <- nchar(wordB) - nchar(wordA)
    }
  }
  
  # Weighted substitutions
  if(nchar(wordA) == nchar(wordB))
  {
    if(wordA != wordB)
    {
      # Initialize penalty vector
      penalty <- numeric(nchar(wordA))
      
      # Loop through concordant letters
      for(i in 1:nchar(wordA))
      {penalty[i] <- sum(abs(keyb[substr(wordA,i,i),] - keyb[substr(wordB, i, i),])) / max(rowSums(sweep(keyb, 2, keyb[substr(wordA, i, i),], "-")))}
      
      # Increase distance vector for substitutions
      dvec[2] <- sum(penalty)
    }
  }
  
  return(sum(dvec))
}

#' Unique Permutations
#' 
#' @description Generates all permutations of the elements of x, in a minimal-
#' change order. If x is a	positive integer,  returns all permutations
#' of the elements of seq(x). If argument "fun" is not null,  applies
#' a function given by the argument to each point. "..." are passed
#' unchanged to the function given by argument fun, if any.
#' 
#' @param ... Additional arguments
#' 
#' @return Returns a list; each component is either a permutation, or the
#' results of applying fun to a permutation.
#' 
#' @references Reingold, E.M., Nievergelt, J., Deo, N. (1977) Combinatorial
#' Algorithms: Theory and Practice. NJ: Prentice-Hall. pg. 170.
#' 
#' @author Scott D. Chasalow <Scott.Chasalow@users.pv.wau.nl>
#' 
#' @noRd
# Permutation
# DATE WRITTEN: 23 Dec 1997          LAST REVISED:  23 Dec 1997
permn<- function(x, fun = NULL, ...)
{
  if(is.numeric(x) && length(x) == 1 && x > 0 && trunc(x) == x) x <- seq(
    x)
  n <- length(x)
  nofun <- is.null(fun)
  out <- vector("list", gamma(n + 1))
  p <- ip <- seqn <- 1:n
  d <- rep(-1, n)
  d[1] <- 0
  m <- n + 1
  p <- c(m, p, m)
  i <- 1
  use <-  - c(1, n + 2)
  while(m != 1) {
    out[[i]] <- if(nofun) x[p[use]] else fun(x[p[use]], ...)
    i <- i + 1
    m <- n
    chk <- (p[ip + d + 1] > seqn)
    m <- max(seqn[!chk])
    if(m < n)
      d[(m + 1):n] <-  - d[(m + 1):n]
    index1 <- ip[m] + 1
    index2 <- p[index1] <- p[index1 + d[m]]
    p[index1 + d[m]] <- m
    tmp <- ip[index2]
    ip[index2] <- ip[m]
    ip[m] <- tmp
  }
  out
}

#' Yes/no menu
#' 
#' @description Provides Linux style yes/no menu
#' 
#' @param title Character.
#' Custom question
#'
#' @return \code{1} for \code{y} and \code{2} for \code{n}
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
#' 
#' @importFrom utils menu
#' 
# Yes/no menu
# Updated 02.01.2021
yes.no.menu <- function (title = NULL) 
{
  # function for appropriate response
  yes.no <- function (ans)
  {
    # check for numeric
    if(is.numeric(ans)){
      
      return(NA)
      
    }else if(is.character(ans)){
      
      #change to lower case
      ans <- tolower(ans)
      
      if(ans != "y" && ans != "yes" && ans != "n" && ans != "no"){
        return(NA)
      }else{
        
        return(
          switch(ans,
                 "y" = 1,
                 "yes" = 1,
                 "n" = 2,
                 "no" = 2
                 
          )
        )
        
      }
      
    }else{return(NA)}
  }
  
  # append title with Linux style yes/no
  title <- paste(title, "[Y/n]? ")
  
  # get response
  ans <- readline(prompt = title)
  
  # make sure there is an appropriate response
  while(is.na(yes.no(ans))){
    ans <- readline(prompt = "Inappropriate response. Try again [Y/n]. ")
  }
  
  return(yes.no(ans))
}

#%%%%%%%%%%%%%%%%%%%%%%%%#
#### TEXTCLEANER MAIN ####
#%%%%%%%%%%%%%%%%%%%%%%%%#

#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Updated 21.10.2021
# Keep strings update: 06.08.2020
# Major update: 19.04.2020
# Added type of task: 21.10.2021
textcleaner.fluency <- function(
  data = NULL, miss = 99, partBY = c("row","col"),
  dictionary = NULL, spelling = c("UK", "US"),
  add.path = NULL, keepStrings = FALSE,
  allowPunctuations = c("-", "all"),
  allowNumbers = FALSE, lowercase = TRUE,
  continue = NULL
)
{
  
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
    }
    
  }
  
  # Check if user is continuing from a previous point
  if(is.null(continue)){
    
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
      obtain.id(data, type = "fluency"),
      silent = TRUE
    )
    
    if(any(class(id.res) == "try-error"))
    {return(error.fun(id.res, "obtain.id", "textcleaner"))}
    
    data <- id.res$data
    ids <- id.res$ids
    row.names(data) <- ids
    
    ## Convert missing data to "" (returns data as matrix; error catch)
    data <- try(
      convert.miss(data, miss, type = "fluency"),
      silent = TRUE
    )
    
    if(any(class(data) == "try-error"))
    {return(error.fun(data, "convert.miss", "textcleaner"))}
    
    ## Prepare for spellcheck.dictionary (returns data as data frame)
    ### Removes punctuations and digits
    ### Removes white spaces
    ### Makes all responses lower case
    data <- try(
      prep.spellcheck.dictionary(data, allowPunctuations, allowNumbers, lowercase,
                                 type = "fluency"),
      silent = TRUE
    )
    
    if(any(class(data) == "try-error"))
    {return(error.fun(data, "prep.spellcheck.dictionary", "textcleaner"))}
    
    ## Obtain unique responses for efficient spell-checking
    uniq.resp <- na.omit(unique(unlist(data)))
    
    # Sort out dictionaries
    if(is.null(dictionary))
    {dictionary <- "coca"}
    
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
    
  }else if(length(continue) != 7){ # Continue spell-check
    spell.check <- spellcheck.dictionary(continue = continue)
  }else{spell.check <- continue}
  
  # Check if spell-check was stopped (either error or user stop)
  if(isTRUE(spell.check$stop))
  {return(spell.check)}
  
  # Initialize results to return
  res <- list()
  
  # Specify variables from spellcheck.dictionary returns
  
  ## Return dictionary if user decided to
  if("dictionary" %in% names(spell.check)){
    res$dictionary <- spell.check$dictionary
  }
  
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
  
  if(any(class(corr.mat) == "try-error")){
    
    error.fun(corr.mat, "correspondence.matrix", "textcleaner")
    
    return(spell.check)
  }
  
  row.names(corr.mat) <- formatC(1:nrow(corr.mat), digits = 2, flag = 0)
  res$spellcheck$correspondence <- corr.mat
  res$spellcheck$automated <- corr.mat[spell.check$auto,]
  res$spellcheck$manual <- corr.mat[spell.check$manual,]
  
  # Correct auto-corrections
  ## Check if there were auto-corrections
  if(length(res$spellcheck$automated) != 0){
    
    res.check <- try(correct.changes(res, type = "fluency"), silent = TRUE)
    
    if(any(class(res.check) == "try-error"))
    {
      error.fun(res, "correct.changes", "textcleaner")
      
      return(res)
    }else{res <- res.check}
    
  }else{
    
    message("\nNo auto-corrections were made. Skipping automated spell-check verification.")
    
  }
  
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
  
  class(res) <- c("textcleaner", "fluency")
  
  Sys.sleep(2)
  
  
  return(res)
}

#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Updated 23.11.2021
# Keep strings update: 06.08.2020
# Major update: 19.04.2020
# Added type of task: 21.10.2021

# twenty <- max(match(paste(1:20), response_matrix[,"ID"]))
# data = response_matrix[1:twenty,]
# type = "free"
# dictionary = "cocaspell"
# spelling = "US"
# miss = 99
# add.path = NULL
# keepStrings = FALSE
# allowPunctuations = "-"
# allowNumbers = FALSE
# lowercase = TRUE
# continue = NULL

textcleaner.free <- function(
  data = NULL, miss = 99,
  spelling = c("UK", "US"),
  add.path = NULL, keepStrings = FALSE,
  allowPunctuations, dictionary,
  allowNumbers = FALSE, lowercase = TRUE,
  continue = NULL
)
{
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
    }
    
  }
  
  # Check if user is continuing from a previous point
  if(is.null(continue)){
    
    ## Make sure data is not tibble
    data <- as.matrix(data)
    
    ## Change row names to IDs (error catch)
    id.res <- try(
      obtain.id(data, type = "free"),
      silent = TRUE
    )
    
    if(any(class(id.res) == "try-error")){
      return(error.fun(id.res, "obtain.id", "textcleaner"))
    }
    
    data <- id.res$data
    ids <- id.res$ids
    
    ## Convert missing data to "" (returns data as matrix; error catch)
    data <- try(
      convert.miss(data, miss, type = "free"),
      silent = TRUE
    )
    
    if(any(class(data) == "try-error")){
      return(error.fun(data, "convert.miss", "textcleaner"))
    }
    
    ## Prepare for spellcheck.dictionary (returns data as data frame)
    ### Removes punctuations and digits
    ### Removes white spaces
    ### Makes all responses lower case
    data <- try(
      prep.spellcheck.dictionary(
        data, allowPunctuations,
        allowNumbers, lowercase,
        type = "free"
      ),
      silent = TRUE
    )
    
    if(any(class(data) == "try-error")){
      return(error.fun(data, "prep.spellcheck.dictionary", "textcleaner"))
    }
    
    ## Obtain unique responses for efficient spell-checking
    uniq.resp <- na.omit(unique(unlist(data[,"Response"])))
    
    # Perform spell-check
    spell.check <- try(
      spellcheck.dictionary.free(
        uniq.resp = uniq.resp,
        dictionary = dictionary,
        spelling = spelling,
        add.path = add.path,
        keepStrings = keepStrings,
        data = data
      ),
      silent <- TRUE
    )
    
  }else if(length(continue) > 8){ # Continue spell-check
    spell.check <- spellcheck.dictionary.free(continue = continue)
  }else{
    spell.check <- continue
  }
  
  # Check if spell-check was stopped (either error or user stop)
  if(spell.check$stop){
    return(spell.check)
  }
  
  # Re-obtain IDs
  ids <- unique(spell.check$data[,"ID"])
  
  # Initialize results to return
  res <- list()
  
  ## Return dictionary if user decided to
  if("dictionary" %in% names(spell.check)){
    res$dictionary <- spell.check$dictionary
  }
  
  ## Re-assign data and ids variables in case of user stoppage or error
  data <- spell.check$data
  res$data$original <- data
  
  ## Assign spell-checking objects
  original <- spell.check$from
  checked <- spell.check$to
  
  # Create correspondence matrix (error catch)
  corr.mat <- try(
    correspondence.matrix(original, checked),
    silent = TRUE
  )
  
  if(any(class(corr.mat) == "try-error")){
    
    error.fun(corr.mat, "correspondence.matrix", "textcleaner")
    
    return(spell.check)
  }
  
  # Check corr.mat for more NAs
  if(ncol(corr.mat) > 2){
    corr.mat[,-1] <- apply(corr.mat[,-1], 1:2, function(x){
      val <- grep("NA", x)
      ifelse(length(val) == 0, x, "NA")
    })
  }else{
    corr.mat[,-1] <- unlist(
      lapply(corr.mat[,-1], function(x){
        val <- grep("NA", x)
        ifelse(length(val) == 0, x, "NA")
      })
    ) 
  }
  
  row.names(corr.mat) <- formatC(1:nrow(corr.mat), digits = 2, flag = 0)
  res$spellcheck$correspondence <- corr.mat
  res$spellcheck$automated <- corr.mat[spell.check$auto,]
  res$spellcheck$manual <- corr.mat[spell.check$manual,]
  
  # Correct auto-corrections
  ## Check if there were auto-corrections
  if(length(res$spellcheck$automated) != 0){
    
    res <- try(correct.changes(res, type = "free"), silent = TRUE)
    
    if(any(class(res) == "try-error"))
    {
      error.fun(res, "correct.changes", "textcleaner")
      
      return(res)
    }
    
    ## Replace correspondence matrix after correcting changes
    corr.mat <- res$spellcheck$correspondence
    
  }else{
    
    message("\nNo auto-corrections were made. Skipping automated spell-check verification.")
    
  }
  
  # Get spell-corrected data (error catch)
  corrected <- try(
    correct.data.free(data, corr.mat, ids),
    silent = TRUE
  )

  if(any(class(corrected) == "try-error")){
    
    error.fun(corrected, "correct.data.free", "textcleaner")

    return(spell.check)
  }

  ## Collect behavioral data
  behavioral <- corrected$behavioral

  ## Make sure to replace faux "NA" with real NA
  corrected$corrected$Response[which(corrected$corrected$Response == "NA")] <- NA
  
  ## Cleaned responses (no instrusions or perseverations)
  cleaned.list <- na.omit(corrected$corrected)
  
  ## Cleaned data
  res$data$clean <- cleaned.list
  
  ## Create frequency matrix
  ### Unique responses and cues
  unique.responses <- unique(cleaned.list$Response)
  unique.cues <- unique(cleaned.list$Cue)
  
  ### Initialize cleaned matrix
  cleaned.matrix <- matrix(
    0, nrow = length(unique.responses), ncol = length(unique.cues) 
  )
  row.names(cleaned.matrix) <- unique.responses
  colnames(cleaned.matrix) <- unique.cues
  
  # Loop through for frequencies
  for(i in 1:length(unique.cues)){
    
    frequency <- table(cleaned.list$Response[cleaned.list$Cue == unique.cues[i]])
    
    cleaned.matrix[names(frequency),i] <- frequency
    
  }
  
  res$responses$clean <- cleaned.matrix
  
  # Convert to binary response matrix (error catch)
  # res$responses$binary <- try(
  #   resp2bin(corrected$corrected),
  #   silent = TRUE
  # )
  # 
  # if(any(class(res$responses$binary) == "try-error"))
  # {
  #   error.fun(corrected, "resp2bin", "textcleaner")
  #   
  #   return(spell.check)
  # }
  
  # Compute totals
  ## Initialize vector and list
  total.vector <- vector(length = length(ids))
  #total.list <- vector("list", length(ids))
  names(total.vector) <- ids
  #names(total.list) <- ids
  
  ## Loop through for totals
  for(i in 1:length(ids)){
    
    # Target participant
    target.p <- cleaned.list[cleaned.list$ID == ids[i],]
    
    # Total overall
    total.vector[i] <- nrow(target.p)
    
    # # Target cue
    # target.c <- unique(target.p$Cue)
    # total.individual <- vector(length = length(target.c))
    # names(total.individual) <- target.c
    # 
    # for(j in 1:length(target.c)){
    #   total.individual[j] <- sum(target.p$Cue == target.c[j])
    # }
    # 
    # # Total individual
    # total.list[[i]] <- total.individual
    
  }
  
  behavioral <- cbind(behavioral, total.vector)
  colnames(behavioral)[3] <- "Appropriate"
  res$behavioral <- as.data.frame(behavioral)
  
  # Make 'textcleaner' class
  class(res) <- "textcleaner"
  
  # Let user know spell-check is complete
  message("\nPreprocessing complete.\n")
  
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
  # res$moniker <- res$spellcheck$manual
  # 
  # cat(
  #   
  #   colortext(
  #     
  #     paste(
  #       paste(
  #         "Consider submitting your",
  #         ifelse("dictionary" %in% names(res), " dictionary and", ""),
  #         " spelling corrections (i.e., monikers) to:\n\n",
  #         sep = ""
  #       ),
  #       "https://github.com/AlexChristensen/SemNetDictionaries/issues/new/choose\n\n",
  #       ifelse("dictionary" %in% names(res), paste(dictionary.output, "\n\n"), ""),
  #       #dictionary.output,
  #       moniker.output, "\n\n"
  #     ),
  #     
  #     defaults = "message"
  #     
  #   )
  #   
  # )
  
  class(res) <- c("textcleaner", "free")
  
  return(res)
}

#%%%%%%%%%%%%%%%%%%%#
#### TEXTCLEANER ####
#%%%%%%%%%%%%%%%%%%%#

#' ID identifier function
#' 
#' @description Identifies a unique column to be used as an ID
#' in \code{\link[SemNetCleaner]{textcleaner}}
#' 
#' @param data Matrix or data frame.
#' Matrix of raw data
#' 
#' @param type Character vector.
#' Type of task to be preprocessed.
#' \itemize{
#' 
#' \item{\code{"fluency"}}
#' {Verbal fluency data (e.g., categories, phonological, synonyms)}
#' 
#' \item{\code{"free"}}
#' {Free association data (e.g., cue terms or words)}
#' 
#' }
#' 
#' @return A list containing:
#' 
#' \item{data}{Data with participant IDs column removed (if applicable)}
#' 
#' \item{ids}{A vector of IDs corresponding to either a column in the \code{data}
#' or the row names of the \code{data}}
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
#' 
# ID identifier
# Updated 21.10.2021
obtain.id <- function (data, type)
{
  # Check for named column
  named.id <- c("subject", "id", "participant", "part", "sub")
  
  if(any(named.id %in% tolower(colnames(data)))){
    id.col <- na.omit(match(named.id, tolower(colnames(data))))
  }else{
    
    # Check for variable with unique values
    uniq.cols <- which(lapply(apply(data, 2, unique), length) == nrow(data))
    
    # Only one unique column variable
    if(length(uniq.cols)==1)
    {id.col <- uniq.cols}
    
  }
  
  if(type == "fluency"){
    
    if(exists("id.col", envir = environment()))
    {
      # Grab unique identifier
      ids <- data[,id.col]
      
      # Let user know what IDs refer to
      message(paste("\nIDs refer to variable:", " '", colnames(data)[id.col], "'", sep = ""))
      
      # Remove unique identifier from data
      data <- data[,-id.col]
    }else{
      # Make rows the IDs
      ids <- 1:nrow(data)
      
      # Let user know what IDs refer to
      message("\nIDs refer to row number")
    }
    
  }else if(type == "free"){
    
    if(exists("id.col", envir = environment()))
    {
      # Grab unique identifier
      ids <- unique(data[,id.col])
      
      # Let user know what IDs refer to
      message(paste("\nIDs refer to variable:", " '", colnames(data)[id.col], "'", sep = ""))
      
    }else{
      # Let user know what IDs refer to
      message("\nNo IDs were identified in data. Please name ID column as \"ID\"")
    }
    
  }
  
  # Initialize result list
  res <- list()
  res$data <- data
  res$ids <- ids

  return(res)
}

#' Convert missing data function
#' 
#' @description Converts missing data into \code{""}
#' 
#' @param data Matrix or data frame.
#' Matrix of raw data
#' 
#' @param miss Character or numeric
#' 
#' @param type Character vector.
#' Type of task to be preprocessed.
#' \itemize{
#' 
#' \item{\code{"fluency"}}
#' {Verbal fluency data (e.g., categories, phonological, synonyms)}
#' 
#' \item{\code{"free"}}
#' {Free association data (e.g., cue terms or words)}
#' 
#' }
#' 
#' @return Matrix with missing values replaced with \code{""}
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
#' 
# Convert missing data
# Updated 21.10.2021
convert.miss <- function (data, miss, type)
{
  # Convert to matrix
  data <- as.matrix(data)
  
  if(type == "fluency"){
    
    # Change missing responses to NA
    data <- ifelse(data == paste(miss), NA, data)
    
    # Change bad responses to NA
    data <- apply(data, 2, bad.response)
    
    # Change NAs in data to ""
    data <- ifelse(is.na(data), "", data)
    
  }else if(type == "free"){
    
    # Target response
    responses <- data[,"Response"]
    
    # Change missing responses to NA
    responses <- ifelse(responses == paste(miss), NA, responses)
    
    # Change bad responses to NA
    responses <- bad.response(responses)
    
    # Change NAs in data to ""
    data[,"Response"] <- ifelse(is.na(responses), "", responses)
    
  }
  
  return(data)
}

#' Prepares data for \code{spellcheck.dictionary}
#' 
#' @description Prepares data for \code{spellcheck.dictionary} by removing
#' punctuations, digits, and white spaces. Also changes all responses to lowercase
#' and converts data into a data frame
#' 
#' @param data Matrix or data frame.
#' Matrix of raw data
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
#' @param type Character vector.
#' Type of task to be preprocessed.
#' \itemize{
#' 
#' \item{\code{"fluency"}}
#' {Verbal fluency data (e.g., categories, phonological, synonyms)}
#' 
#' \item{\code{"free"}}
#' {Free association data (e.g., cue terms or words)}
#' 
#' }
#' 
#' @return Data frame prepped for \code{spellcheck.dictionary}
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
#' 
# Prep for spellcheck.dictionary
# Updated 21.10.2021
prep.spellcheck.dictionary <- function (
  data, allowPunctuations,
  allowNumbers, lowercase,
  type
)
{
  
  if(type == "fluency"){
    
    # Remove miscellaneous string additions from data
    ## Remove/allow punctuation
    if(all(allowPunctuations != "all")){
      characters <- paste("([", paste(allowPunctuations, collapse = ""), "])|[[:punct:]]", sep = "", collapse = "")
      data <- apply(data, 2, function(y) gsub(characters, "\\1", y))
    }
    
    ## Remove/allow numbers
    if(!isTRUE(allowNumbers)){
      data <- apply(data, 2, function(y) gsub("[[:digit:]]+", "", y))
    }
    
    # Remove white spaces
    data <- apply(apply(data, 2, trimws), 1:2, rm.lead.space)
    
    # Change all to lowercase
    if(isTRUE(lowercase)){
      data <- apply(data, 2, tolower)
    }
    
    # Remove new lines
    data <- apply(data, 2, function(y) gsub("[\n\r\t]",  " ", y))
    
    # Remove special characters
    ## Split words
    data <- apply(data, 2, strsplit, split = " ")
    ## Remove special characters
    data <- lapply(data, function(y){
      lapply(y, function(y) gsub("[^\x20-\x7E]", "", y))
    })
    ## Re-combine words
    data <- lapply(data, function(y){
      lapply(y, paste, sep = "", collapse = " ")
    })
    ## Back into matrix
    data <- simplify2array(data, higher = FALSE)
    ## Revert NAs
    data <- apply(data, 1:2, function(y){ifelse(y == "NA", NA, unlist(y))})
    
    # Convert to data frame
    data <- as.data.frame(data, stringsAsFactors = FALSE)
    
  }else if(type == "free"){
    
    response <- data[,"Response"]
    
    # Remove miscellaneous string additions from data
    ## Remove/allow punctuation
    if(all(allowPunctuations != "all")){
      characters <- paste("([", paste(allowPunctuations, collapse = ""), "])|[[:punct:]]", sep = "", collapse = "")
      response <- gsub(characters, "\\1", response)
    }
    
    ## Remove/allow numbers
    if(!isTRUE(allowNumbers)){
      response <- gsub("[[:digit:]]+", "", response)
    }
    
    # Remove white spaces
    response <- unlist(lapply(
      trimws(response), rm.lead.space
    ))
      
    # Change all to lowercase
    if(isTRUE(lowercase)){
      response <- tolower(response)
    }
    
    # Remove new lines
    response <- gsub("[\n\r\t]",  " ", response)
    
    # Remove special characters
    ## Split words
    response <- strsplit(response, split = " ") 
    ## Remove special characters
    response <- lapply(response, function(y){
      gsub("[^\x20-\x7E]", "", y)
    })
    ## Re-combine words
    response <- unlist(lapply(response, paste, sep = "", collapse = " "))
    ## Revert NAs
    response <- ifelse(response == "NA", NA, response)
    ## Back into matrix
    data[,"Response"] <- response
    # Convert to data frame
    data <- as.data.frame(data, stringsAsFactors = FALSE)
    
  }
  
  return(data)
}

#' Individual auto-corrects strings
#' 
#' @description A sub-rountine to spell-check responses
#' 
#' @param string Character.
#' A string of words with a \code{length = 1}
#' 
#' @param full.dict Character vector.
#' Dictionary entries
#' 
#' @param dictionary Character vector.
#' A dictionary to look for word in (see \code{\link{SemNetDictionaries}})
#' 
#' @param spelling Character.
#' Either \code{"UK"} or \code{"US"} for their respective spelling
#' 
#' @return Either a string that's been spell-corrected or the original string
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Individual Word Spell-Checker
# Updated 04.01.2020
ind.word.check <- function (string, full.dict, dictionary, spelling)
{
  # Make string only alphanumeric characters
  string <- gsub("[^[:alnum:][:space:]]", "", string)
  
  # Split string
  spl <- unlist(strsplit(string," "))
  
  # Remove bad responses
  spl <- na.omit(bad.response(spl))
  
  # Apply best guess across words
  guesses <- lapply(spl, best.guess, full.dictionary = full.dict, dictionary = dictionary, tolerance = 1)
  
  # Check for multiple best guesses
  lengths <- unlist(lapply(guesses, length))
  
  # Replace non-automated responses with the original word
  if(any(lengths > 1))
  {guesses[which(lengths > 1)] <- spl[which(lengths > 1)]}
  
  # Convert guesses back into a stringed response
  resp <- paste(unlist(guesses), collapse = " ")
  
  # Re-check for misnomers
  # Check if any dictionaries were imported from SemNetDictionaries
  if(any(dictionary %in% SemNetDictionaries::dictionaries(TRUE)))
  {resp <- moniker(word = resp, misnom = SemNetDictionaries::load.monikers(dictionary), spelling = spelling)}
  
  return(resp)
}

#' Multiple words
#' 
#' @description A sub-routine function to de-combine strings
#' that are found in dictionary (e.g., "fish dog cat" --> "fish" "dog" "cat")
#' 
#' @param vec Character vector.
#' A vector with words to potentially be de-combined
#' 
#' @param multi.min Numeric.
#' Length of multiple word resposnes in the dictionary
#' 
#' @param full.dict Character vector.
#' Dictionary entries
#' 
#' @param dictionary Character vector.
#' A dictionary to look for word in (see \code{\link{SemNetDictionaries}})
#' 
#' @param spelling Character.
#' Either \code{"UK"} or \code{"US"} for their respective spelling
#' 
#' @return A vector with responses de-combined based on dictionary entries
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom stats median sd
#' 
#' @noRd
# Multiple words
# Updated 01.12.2020
multiple.words <- function (vec, multi.min, full.dict, dictionary, spelling)
{
  # Split vector
  spl <- unlist(strsplit(vec, " "))
  
  # Remove bad responses
  spl <- na.omit(bad.response(spl))
  
  # Check for multiple words
  if(length(spl) >= multi.min)
  {
    # Identify words not in dictionary
    misspl <- which(!spl %in% full.dict)
    
    # Loop through checking for combination of words
    for(i in misspl)
    {
      # Individual check
      ind <- FALSE
      
      # Multi-word check
      multi <- vector(length = 2)
      
      ## Check first for word individually
      ind.guess <- best.guess(spl[i], full.dictionary = full.dict, dictionary = dictionary)
      
      ## Update based on individual guess
      if(length(ind.guess) == 1)
      {
        spl[i] <- ind.guess
        ind <- TRUE
      }
      
      ## Check for words before and after word
      if(i != 1)
      {
        ### Check before
        spacing <- c(paste(spl[i-1], spl[i]), paste(spl[i-1], spl[i], sep = ""))
        
        ### Convert monikers
        spacing <- unique(unlist(lapply(spacing, moniker, dictionary, spelling = spelling)))
        
        ### Check for only one solution
        if(sum(spacing %in% full.dict) == 1)
        {multi[1] <- spacing[which(spacing %in% full.dict)]}
      }
      
      if(i != length(spl))
      {
        ### Check before
        spacing <- c(paste(spl[i], spl[i+1]), paste(spl[i], spl[i+1], sep = ""))
        
        ### Convert monikers
        spacing <- unique(unlist(lapply(spacing, moniker, dictionary, spelling = spelling)))
        
        ### Check for only one solution
        if(sum(spacing %in% full.dict) == 1)
        {multi[2] <- spacing[which(spacing %in% full.dict)]}
      }
      
      # Verify singular multi-word solution
      if(multi[1] %in% full.dict && !multi[2] %in% full.dict)
      {
        ## Change response
        spl[i] <- multi[1]
        
        ## Remove previous response
        spl <- spl[-(i-1)]
        
      }else if(!multi[1] %in% full.dict && multi[2] %in% full.dict)
      {
        ## Change response
        spl[i] <- multi[2]
        
        ## Remove previous response
        spl <- spl[-(i+1)]
      }
    }
    
    # Check for common misspellings and monikers
    vec <- unlist(lapply(spl, moniker, misnom = SemNetDictionaries::load.monikers(dictionary), spelling = spelling))
  }else if(length(spl) > 1)
  {
    # Correct if only one best guess exists
    guess <- best.guess(vec, full.dictionary = full.dict, dictionary = dictionary)
    
    if(length(guess) == 1)
    {vec <- guess}
  }
  
  # Initialize result list
  res <- list()
  res$response <- unique(vec)
  res$correct <- unique(vec) %in% full.dict
  
  return(res)
}

#' Response splitter
#' 
#' @description A sub-routine function to identify and split responses in the dictionary
#' 
#' @param vec Character vector.
#' A vector with words to potentially be de-combined
#' 
#' @param full.dict Character vector.
#' Dictionary entries
#' 
#' @return Either a vector with the responses split or still combined
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Response splitter
# Updated 18.04.2020
response.splitter <- function (vec, full.dict)
{
  # Check to make sure that the vector is still a string
  if(length(vec) == 1)
  {
    # Split vector
    spl <- unlist(strsplit(vec, " "))
    
    # Remove bad responses
    spl <- na.omit(bad.response(spl))
    
    # Check for multiple words
    if(length(spl) > 1)
    {
      # Check if words in string are already in dictionary
      if(vec %in% full.dict)
      {return(vec)
      }else if(all(spl %in% full.dict)) # Check if words split are in dictionary
      {return(spl)
      }else{return(vec)} # Just return the vector
    }else{return(vec)}
  }else{return(vec)}
}

#' British-US English Conversion (Vector)
#' 
#' @description A sub-routine function to convert English spelling
#' 
#' @param vec Character vector.
#' A vector with words to be checked for English spelling
#' 
#' @param spelling Character.
#' Either \code{"UK"} or \code{"US"} for their respective spelling
#' 
#' @param dictionary Boolean.
#' If \code{TRUE}, then duplicates are removed and the words are alphabetized
#' 
#' @return British or US spellings of words
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# British-US Conversion (Vector)
# Updated 27.11.2020
brit.us.conv.vector <- function (vec, spelling = c("UK", "US"), dictionary = FALSE)
{
  if(toupper(spelling) == "UK"){
    
    # Check for any US spelling in the vector
    if(any(vec %in% names(SemNetDictionaries::brit2us))){
      
      # Target US spelling
      target.US <- which(!is.na(vec[match(names(SemNetDictionaries::brit2us), vec)]))
      
      # Get GB spelling
      spelling.GB <- unname(unlist(SemNetDictionaries::brit2us[target.US]))
    
      # Change US to GB
      vec[na.omit(match(names(SemNetDictionaries::brit2us), vec))] <- spelling.GB
      
    }
    
    
  }else if(toupper(spelling) == "US"){
    
    # Check for any GB spelling in the vector
    if(any(vec %in% SemNetDictionaries::brit2us)){
      
      # Target GB spelling
      target.GB <- which(!is.na(vec[match(SemNetDictionaries::brit2us, vec)]))
      
      # Get US spelling
      spelling.US <- names(unlist(SemNetDictionaries::brit2us[target.GB]))
      
      # Change US to GB
      vec[na.omit(match(SemNetDictionaries::brit2us, vec))] <- spelling.US
      
    }
    
  }
  
  # Remove duplicates and alphabetize
  if(dictionary){
    vec <- sort(unique(vec))
  }
  
  return(vec)
}

#' British-US English Conversion (List)
#' 
#' @description A sub-routine function to convert English spelling
#' 
#' @param vec Character vector.
#' A vector with words to be checked for English spelling
#' 
#' @param spelling Character.
#' Either \code{"UK"} or \code{"US"} for their respective spelling
#' 
#' @return British or US spellings of words
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# British-US Conversion
# Updated 27.11.2020
brit.us.conv <- function (vec, spelling = c("UK", "US"), dictionary)
{

  vec <- unlist(lapply(vec, strsplit, split = " "), recursive = FALSE)
  
  vec <- lapply(vec, function(x, spelling, dictionary){
    
    converted <- brit.us.conv.vector(x, spelling = spelling)
    
    conv <- paste(converted, collapse = " ")
    
    return(conv)
    
  }, spelling = spelling, dictionary = dictionary)
  
  if(dictionary){
    vec <- sort(unlist(vec))
  }
  
  return(vec)
}

#' Walkthrough for \code{\link[SemNetCleaner]{textcleaner}}'s Manual Spell-check
#' 
#' Provides a walkthrough of the options you can use to spell-check responses
#' 
#' @param walkthrough Boolean.
#' Whether to start the walkthrough
#' 
#' @return NULL
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
#' 
# Walkthrough
# Updated 09.06.2020
walk_through <- function(walkthrough)
{
  # Do walkthrough?
  do.it <- FALSE
  
  # Check if NULL
  if(is.null(walkthrough))
  {
    # Ask user
    customMenu(choices = c("Yes", "No"),
               title = c(paste("\nBefore starting the manual spell-check, would you like to do a walkthrough?",
                               colortext("\n(Recommended for first time users)", defaults = "message"))),
               default = 2, dictionary = TRUE)
    
    # Get response
    ans <- readline("Selection: ")
    
    # Make sure answer is appropriate
    ans <- appropriate.answer(ans, choices = 1:2, default = 2, dictionary = TRUE)
    
    if(ans == 1)
    {do.it <- TRUE
    }else{message("\nWalkthrough skipped.")}
    
  }else if(walkthrough)
  {do.it <- TRUE}
  
  # If do the walkthrough is TRUE, then do it
  if(do.it)
  {
    # Introduction to walkthrough
    cat(colortext("\nWelcome to the manual spell-check walkthrough for `textcleaner`!", defaults = "message"))
    
    cat(colortext("\n\nThe manual spell-check process is designed to allow you to", defaults = "message"))
    cat(colortext("\nhave maximum control over the responses that need spell-checking.", defaults = "message"))
    cat(colortext("\nThis walkthrough will guide you through each response option", defaults = "message"))
    cat(colortext("\nin the manual spell-check menu by using examples of responses", defaults = "message"))
    cat(colortext("\nyou might encounter.", defaults = "message"))
    
    cat(colortext("\n\nThere are generally two types of responses you might encounter:", defaults = "message"))
    cat(colortext("\na single word or multiple word response. This latter type is a", defaults = "message"))
    cat(colortext("\nbit tricky because the response could be a single response or", defaults = "message"))
    cat(colortext("\nit could be multiple responses entered as though it were a", defaults = "message"))
    cat(colortext("\nsingle response.", defaults = "message"))
    
    cat(colortext("\n\nBecause `textcleaner` spell-checks each word individually, your", defaults = "message"))
    cat(colortext("\nintervention is necessary to determine how to properly split", defaults = "message"))
    cat(colortext("\nthese responses. By demonstrating how to correct this type of", defaults = "message"))
    cat(colortext("\nresponse, you'll be prepared to correct single responses as well.", defaults = "message"))
    
    cat(colortext("\n\nThis is because these multiple word responses can be checked", defaults = "message"))
    cat(colortext("\nindividually or across all words in the response. Therefore, all", defaults = "message"))
    cat(colortext("\nexplanation for how to use the manual spell-check options of", defaults = "message"))
    cat(colortext("\n`textcleaner` are discussed in this single example.", defaults = "message"))
    
    cat(colortext("\n\nThese multiple word responses are where the `textcleaner`", defaults = "message"))
    cat(colortext("\nfunction starts and our walkthrough begins.\n\n", defaults = "message"))
    
    readline("Press ENTER to continue...")
    
    # Introduction to multiple word response
    cat(colortext("\nThe multiple word response we will use for our example was given", defaults = "message"))
    cat(colortext("\nby an actual participant:", defaults = "message"))
    
    cat("\n\n'turtle catdog elephant fish bird squiral rabbit fox deer monkey giraff'\n")
    
    cat(colortext("\nWhen this multiple word response was passed through the automated", defaults = "message"))
    cat(colortext("\nspell-check, it was changed to:", defaults = "message"))
    
    cat("\n\n'turtle' 'catdog' 'elephant' 'fish' 'bird' 'squirrel' 'rabbit' 'fox' 'deer' 'monkey' 'giraffe'\n")
    
    cat(colortext("\nThe automated spell-check handled a few errors in the original", defaults = "message"))
    cat(colortext("\nresponse. First, the words in the response were separated into", defaults = "message"))
    cat(colortext("\nindividual responses. Second, the responses 'squiral' and 'giraff'", defaults = "message"))
    cat(colortext("\nwere automatically corrected to 'squirrel' and 'giraffe',", defaults = "message"))
    cat(colortext("\nrespectively. The word 'catdog', however, was not successfully", defaults = "message"))
    cat(colortext("\nseparated and is passed on to you to be manually spell-checked.\n\n", defaults = "message"))
    
    readline("Press ENTER to continue...")
    
    cat(colortext("\nBelow is the interactive menu that will appear when manually spell-checking responses:\n", defaults = "message"))
    
    Sys.sleep(5)
    
    #-------------------------#
    ## Begin example of menu ##
    #-------------------------#
    
    original <- paste("turtle catdog elephant fish bird squiral rabbit fox deer monkey giraff")
    context <- paste("turtle", "catdog", "elephant", "fish", "bird",  "squirrel", "rabbit", "fox", "deer", "monkey", "giraffe")
    context <- unlist(strsplit(context, split = " "))
    check <- 2
    
    title <- paste(paste("\nOriginal response: ", "'", original, "'", sep = ""),
                   paste("Auto-corrected response: ", paste("'", context, "'", sep = "", collapse = " "), sep = ""),
                   paste("Response to manually spell-check: ", paste("'", colortext(context[check], defaults = "highlight"), "'", sep = ""), sep = ""),
                   sep = "\n\n")
    
    word <- c("SKIP WORD", "ADD WORD TO DICTIONARY", "TYPE MY OWN WORD", "GOOGLE WORD", "BAD WORD")
    string <- c("KEEP ORIGINAL", "KEEP AUTO-CORRECT", "TYPE MY OWN STRING", "GOOGLE STRING", "BAD STRING")
    resp <- best.guess(word = "catdog", full.dictionary = SemNetDictionaries::animals.dictionary, dictionary = "animals")
    
    choices <- c(word, string, resp)
    
    customMenu(choices = choices, title = title, default = 10)
    
    message("Press 'B' to GO BACK or 'esc' to STOP.\n")
    
    cat("Response option (accepts lowercase):\n\n")
    
    #-----------------------#
    ## End example of menu ##
    #-----------------------#
    
    readline("Press ENTER to continue...\n(make sure to expand R's Console vertically to see the full menu)")
    
    # First three lines
    cat(paste(colortext("\nAs you can see, the interactive menu contains", defaults = "message"),
              colortext(styletext("a lot", "italics"), defaults = "message"),
              colortext("of information.", defaults = "message"))
    )
    cat(colortext("\nThe purpose of this walkthrough is to get familiar with this menu", defaults = "message"))
    cat(colortext("\nby breaking it down into digestible pieces. We'll start with the", defaults = "message"))
    cat(colortext("\nfirst three lines:\n", defaults = "message"))
    
    cat(title)
    
    linebreak()
    
    cat(paste("\n", "Original response:\n",
              colortext(paste(" ", textsymbol("bullet"), " Refers to the original response that the participant typed", sep = ""), defaults = "message"),
              "\n",
              sep = "")
    )
    
    cat(paste("\n", "Auto-corrected response:\n",
              colortext(paste(" ", textsymbol("bullet"), " Refers to the response that the automated spell-check corrected to", sep = ""), defaults = "message"),
              "\n",
              sep = "")
    )
    
    cat(paste("\n", "Response to manually spell-check: '", colortext("catdog", defaults = "highlight"), "'\n",
              paste(
                colortext(paste(" ", textsymbol("bullet"), " Refers to the ", sep = ""), defaults = "message"),
                styletext(colortext("target", defaults = "message"), defaults = "italics"),
                colortext(paste(" response to be manually spell-checked", sep = ""), defaults = "message"),
                sep = ""
              ),
              "\n\n",
              sep = "")
    )
    
    readline("Press ENTER to continue...")
    
    cat(colortext("\nThese first three lines provide you with the full context of the", defaults = "message"))
    cat(colortext("\nparticipant's response to help you make a more informed decision.", defaults = "message"))
    cat(paste(colortext("\nThe ", defaults = "message"), "Auto-corrected response:",
              colortext(" is the current state of the response.", defaults = "message"),
              sep = "")
    )
    
    cat(colortext("\nThat is, if you do nothing to manually spell-check the response,", defaults = "message"))
    cat(colortext("\nthen these are the responses that will be retained. In our example,", defaults = "message"))
    cat(paste(colortext("\nwe want to change to target response: ", defaults = "message"),
              "'", colortext("catdog", defaults = "highlight"), "'",
              colortext(".", defaults = "message"),
              sep = "")
    )
    
    cat(colortext("\n\nNext, we'll move on to the possible options we have to", defaults = "message"))
    cat(colortext("\ncorrect this response.\n\n", defaults = "message"))
    
    readline("Press ENTER to continue...")
    
    # Word options
    cat(colortext("\nThe first set of options (i.e., ", defaults = "message"),
        styletext("Word options", defaults = "underline"),
        colortext(") we have are", defaults = "message"),
        sep = ""
    )
    cat(colortext("\nto correct the target word:", defaults = "message"),
        paste("'", colortext("catdog", defaults = "highlight"), "'",
              colortext(". These options allow", defaults = "message"),
              sep = "")
    )
    cat(colortext("\nyou to make a decision about how to handle this single", defaults = "message"))
    cat(colortext("\nword in the response -- that is, these options will", defaults = "message"),
        colortext(styletext("\nonly", "italics"), defaults = "message"),
        colortext("affect the target word. There are five options:", defaults = "message")
    )
    
    word.set <- paste(1:5, ": ", word, sep = "")
    
    cat(paste(styletext("\n\nWord options\n", defaults = "underline"),
              paste0(word.set, c(rep.int("  ", min(20, 5) - 1L)), sep = "", collapse = "")))
    
    linebreak()
    
    cat(paste("\n", "1: SKIP WORD\n",
              colortext(paste(" ", textsymbol("bullet"), ' Keeps word "as is" and moves on to next word to be spell-checked', sep = ""), defaults = "message"),
              "\n",
              sep = "")
    )
    
    cat(paste("\n", "2: ADD WORD TO DICTIONARY\n",
              colortext(paste(" ", textsymbol("bullet"), " Adds word to dictionary for future spell-checking", sep = ""), defaults = "message"),
              "\n",
              sep = "")
    )
    
    cat(paste("\n", "3: TYPE MY OWN WORD\n", 
              colortext(paste(" ", textsymbol("bullet"), " Allows you to type your own correction for the word", sep = ""), defaults = "message"),
              "\n",
              sep = "")
    )
    
    cat(paste("\n", "4: GOOGLE WORD\n", 
              colortext(paste(" ", textsymbol("bullet"), ' Opens your default browser and "Googles" the word', sep = ""), defaults = "message"),
              "\n",
              sep = "")
    )
    
    cat(paste("\n", "5: BAD WORD\n", 
              colortext(paste(" ", textsymbol("bullet"), " Marks the word as an inappropriate response (NA)", sep = ""), defaults = "message"),
              "\n\n",
              sep = "")
    )
    
    readline("Press ENTER to continue...")
    
    cat(colortext("\nThese options only affect the target word. But what if you want", defaults = "message"))
    cat(colortext("\nto change multiple words or the", defaults = "message"),
        colortext(styletext("entire", defaults = "italics"), defaults = "message"),
        colortext("string of responses? We", defaults = "message"))
    cat(colortext("\nmove to those options next.\n\n", defaults = "message"))
    
    readline("Press ENTER to continue...")
    
    # String options
    cat(colortext("\nThe next set of options (i.e., ", defaults = "message"),
        styletext("String options", defaults = "underline"),
        colortext(") are able to", defaults = "message"),
        sep = "")
    cat(colortext("\ncorrect the entire string of responses. These options will", defaults = "message"))
    cat(colortext("\naffect the entire string rather than just the target word.", defaults = "message"))
    cat(colortext("\nThere are also five options:", defaults = "message"))
    
    string.set <- paste(6:10, ": ", string, sep = "")
    
    cat(paste(styletext("\n\nString options\n", defaults = "underline"),
              paste0(string.set, c(rep.int("  ", min(20, 5) - 1L)), sep = "", collapse = "")))
    
    linebreak()
    
    cat(paste("\n", "6: KEEP ORIGINAL\n",
              paste(colortext(paste(" ", textsymbol("bullet"), " Reverts the string back to the", sep = ""), defaults = "message"),
                    "Original response:",
                    colortext("the participant provided.", defaults = "message")),
              "\n", sep = ""
    )
    )
    
    cat(paste("\n", "7: KEEP AUTO-CORRECT\n",
              paste(colortext(paste(" ", textsymbol("bullet"), " Keeps the string 'as is' with the", sep = ""), defaults = "message"),
                    "Auto-correct response:",
                    colortext("provided by the automated spell-check.", defaults = "message")),
              "\n", sep = ""
    )
    )
    
    cat(paste("\n", "8: TYPE MY OWN STRING\n",
              paste(colortext(paste(" ", textsymbol("bullet"), " Allows you to type your own correction for the", sep = ""), defaults = "message"),
                    "Original response:",
                    colortext("the participant provided.", defaults = "message")),
              "\n", sep = ""
    )
    )
    
    cat(paste("\n", "9: GOOGLE STRING\n",
              paste(colortext(paste(" ", textsymbol("bullet"), " Opens your default browser and 'Googles' the", sep = ""), defaults = "message"),
                    "Original response:",
                    colortext("the participant provided.", defaults = "message")),
              "\n", sep = ""
    )
    )
    
    cat(paste("\n", "10: BAD STRING\n", 
              colortext(paste(" ", textsymbol("bullet"), " Marks the entire string as an inappropriate response (NA)", sep = ""), defaults = "message"),
              "\n\n",
              sep = ""
    )
    )
    
    readline("Press ENTER to continue...")
    
    cat(colortext("\nThese options only affect the entire string. They allow you to", defaults = "message"))
    cat(colortext("\nlook beyond the target word and to make a decision about the", defaults = "message"))
    cat(colortext("\nwhole string. These options are most useful when considering", defaults = "message"))
    cat(colortext("\na multiple word response as a single response (rather than", defaults = "message"))
    cat(colortext("\nmultiple individual responses).\n\n", defaults = "message"))
    
    readline("Press ENTER to continue...")
    
    # Response options
    cat(colortext("\nThe final set of options (i.e., ", defaults = "message"),
        styletext("Response options", defaults = "underline"),
        colortext(") only affect the", defaults = "message"),
        sep = "")
    cat(colortext("\ntarget word. These options are `textcleaner`'s best guess for", defaults = "message"))
    cat(colortext("\nwhat the response nmight be. These response options offer quick", defaults = "message"))
    cat(colortext("\ncorrections or potential directions for what the user meant to", defaults = "message"))
    cat(colortext("\nBelow the", defaults = "message"),
        styletext("Response options", defaults = "underline"),
        colortext("are two convenience options and an ", defaults = "message")
    )
    cat(colortext("\ninput for your selection.", defaults = "message"))
    
    resp.set <- paste(c("Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P"), ": ", resp, sep = "")
    
    cat(paste(styletext("\n\nResponse options\n", defaults = "underline"),
              paste0(resp.set, c(rep.int("  ", min(20, 5) - 1L)), sep = "", collapse = "")))
    
    cat(colortext("\n\nPress 'B' to GO BACK, 'H' for HELP, or 'esc' to STOP.\n", defaults = "message"))
    
    cat("\nSelection (accepts lowercase): ")
    
    linebreak()
    
    cat(paste(styletext("\nResponse options\n", defaults = "underline"),
              paste(colortext(paste(" ", textsymbol("bullet"), " Potential options based on `textcleaner`'s best guess (letters correspond to the response)", sep = ""), defaults = "message")),
              "\n", sep = ""
    )
    )
    
    cat(paste("\n'B'\n",
              paste(colortext(paste(" ", textsymbol("bullet"), " Takes you back to the previous response (use if you make a mistake or just want to go back)", sep = ""), defaults = "message")),
              "\n", sep = ""
    )
    )
    
    cat(paste("\n'H'\n",
              paste(colortext(paste(" ", textsymbol("bullet"), " Takes you to the documentation of `textcleaner`", sep = ""), defaults = "message")),
              "\n", sep = ""
    )
    )
    
    cat(paste("\n'esc'\n",
              paste(colortext(paste(" ", textsymbol("bullet"), " Exits textcleaner completely but saves your output", sep = ""), defaults = "message")),
              "\n", sep = ""
    )
    )
    
    cat(paste("\nSelection (accepts lowercase): \n",
              paste(colortext(paste(" ", textsymbol("bullet"), " Where you input the option you select", sep = ""), defaults = "message")),
              "\n\n", sep = ""
    )
    )
    
    readline("Press ENTER to continue...")
    
    cat(colortext("\nThese make up all the options you'll use in `textcleaner`. They", defaults = "message"))
    cat(colortext("\nenable total control over correcting the response(s). When there", defaults = "message"))
    cat(colortext("\nis only a single word response, then the 'String options' will", defaults = "message"))
    cat(colortext("\nnot be available (because they are no longer necessary).", defaults = "message"))
    
    cat(colortext("\n\nThis concludes the walkthrough of the manual spell-check process", defaults = "message"))
    cat(colortext("\nin `textcleaner`. You're now prepared to work through your data.\n", defaults = "message"))
  }
  
  cat("\n")
  
  readline("Press ENTER to start manual spell-check...")
  
}

#' Gets Help for \code{\link[SemNetCleaner]{textcleaner}}'s Manual Spell-check
#' 
#' Provides a help for the options you can use to spell-check responses
#' 
#' @return NULL
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
#' 
# Help
# Updated 07.09.2020
textcleaner_help <- function(check, context, original, possible)
{
  linebreak(breaks = "\n\n")
  
  help_art <- c(" _            _       _                              _          _        ",
                "| |_ _____  _| |_ ___| | ___  __ _ _ __   ___ _ __  | |__   ___| |_ __   ",
                "| __/ _ \\ \\/ / __/ __| |/ _ \\/ _` | '_ \\ / _ \\ '__| | '_ \\ / _ \\ | '_ \\  ",
                "| ||  __/>  <| || (__| |  __/ (_| | | | |  __/ |    | | | |  __/ | |_) | ",
                " \\__\\___/_/\\_\\\\__\\___|_|\\___|\\__,_|_| |_|\\___|_|    |_| |_|\\___|_| .__/  ",
                "                                                                 |_| ")
  
  cat(help_art, sep = "\n")
  
  linebreak(breaks = "")
  
  Sys.sleep(2)
  
  if(!is.null(context))
  {
    cat(paste("\n", "Original string:\n\n",
              paste("'", original, "'", sep = ""), "\n\n",
              colortext(paste(" ", textsymbol("bullet"), " Refers to the original string that the participant typed", sep = ""), defaults = "message"),
              "\n",
              sep = "")
    )
    
    cat(paste("\n", "Auto-corrected string:\n\n",
              paste("'", context, "'", sep = "", collapse = " "), "\n\n",
              colortext(paste(" ", textsymbol("bullet"), " Refers to the auto-corrected string that the automated spell-check derived", sep = ""), defaults = "message"),
              "\n",
              sep = "")
    )
    
    # Choices for spell-check
    word <- c("SKIP WORD", "ADD WORD TO DICTIONARY", "TYPE MY OWN WORD", "GOOGLE WORD", "BAD WORD")
    string <- c("KEEP ORIGINAL", "KEEP AUTO-CORRECT", "TYPE MY OWN STRING", "GOOGLE STRING", "BAD STRING")
    
    choices <- c(word, string, possible)
    
    check <- context[check]
    
  }else{
    
    # Choices for spell-check
    choices <- c("SKIP", "ADD TO DICTIONARY", "TYPE MY OWN", "GOOGLE IT", "BAD WORD", possible)
    
  }
  
  cat(paste("\n", "Target word: '", colortext(check, defaults = "highlight"), "'\n",
            paste(
              colortext(paste(" ", textsymbol("bullet"), " Refers to the ", sep = ""), defaults = "message"),
              #styletext(colortext("target", defaults = "message"), defaults = "italics"),
              colortext("target", defaults = "message"),
              colortext(paste(" word to be manually spell-checked", sep = ""), defaults = "message"),
              sep = ""
            ),
            sep = "")
  )
  
  linebreak(breaks = "\n\n")
  
  cat(paste("\n", "1: SKIP WORD\n",
            colortext(paste(" ", textsymbol("bullet"), " Keeps the target word 'as is' and moves on to next word to be spell-checked", sep = ""), defaults = "message"),
            "\n",
            sep = "")
  )
  
  cat(paste("\n", "2: ADD WORD TO DICTIONARY\n",
            colortext(paste(" ", textsymbol("bullet"), " Adds the target word to dictionary for future spell-checking", sep = ""), defaults = "message"),
            "\n",
            sep = "")
  )
  
  cat(paste("\n", "3: TYPE MY OWN WORD\n", 
            colortext(paste(" ", textsymbol("bullet"), " Allows you to type your own correction for the target word", sep = ""), defaults = "message"),
            "\n",
            sep = "")
  )
  
  cat(paste("\n", "4: GOOGLE WORD\n", 
            colortext(paste(" ", textsymbol("bullet"), " Opens your default browser and 'Googles' the target word", sep = ""), defaults = "message"),
            "\n",
            sep = "")
  )
  
  cat(paste("\n", "5: BAD WORD\n", 
            colortext(paste(" ", textsymbol("bullet"), " Marks the target word as an inappropriate response (NA)", sep = ""), defaults = "message"),
            sep = "")
  )
  
  linebreak(breaks = "\n\n")
  
  if(!is.null(context))
  {
    cat(paste("\n", "6: KEEP ORIGINAL\n",
              paste(colortext(paste(" ", textsymbol("bullet"), " Reverts the string back to the", sep = ""), defaults = "message"),
                    "Original string:",
                    colortext("the participant provided", defaults = "message")),
              "\n", sep = ""
    )
    )
    
    cat(paste("\n", "7: KEEP AUTO-CORRECT\n",
              paste(colortext(paste(" ", textsymbol("bullet"), " Keeps the string 'as is' with the", sep = ""), defaults = "message"),
                    "Auto-correct string:",
                    colortext("provided by the automated spell-check", defaults = "message")),
              "\n", sep = ""
    )
    )
    
    cat(paste("\n", "8: TYPE MY OWN STRING\n",
              paste(colortext(paste(" ", textsymbol("bullet"), " Allows you to type your own correction for the", sep = ""), defaults = "message"),
                    "Original string:",
                    colortext("the participant provided", defaults = "message")),
              "\n", sep = ""
    )
    )
    
    cat(paste("\n", "9: GOOGLE STRING\n",
              paste(colortext(paste(" ", textsymbol("bullet"), " Opens your default browser and 'Googles' the", sep = ""), defaults = "message"),
                    "Original string:",
                    colortext("the participant provided", defaults = "message")),
              "\n", sep = ""
    )
    )
    
    cat(paste("\n", "10: BAD STRING\n", 
              colortext(paste(" ", textsymbol("bullet"), " Marks the entire string as an inappropriate response (NA)", sep = ""), defaults = "message"),
              sep = ""
    )
    )
    
    linebreak(breaks = "\n\n")
    
    cat(paste("\n", styletext("Response options\n", defaults = "underline"),
              customMenu(choices = choices, title = NULL, default = 10, help = TRUE), "\n",
              paste(colortext(paste(" ", textsymbol("bullet"), " Potential options based on `textcleaner`'s best guess for the target word (letters correspond to the response)", sep = ""), defaults = "message")),
              "\n", sep = ""
    )
    )
    
  }else{
    
    cat(paste("\n", styletext("Response options\n", defaults = "underline"),
              customMenu(choices = choices, title = NULL, default = 5, help = TRUE), "\n",
              paste(colortext(paste(" ", textsymbol("bullet"), " Potential options based on `textcleaner`'s best guess for the target word (letters correspond to the response)", sep = ""), defaults = "message")),
              "\n", sep = ""
    )
    )
    
  }
  
  cat(paste("\n'B'\n",
            paste(colortext(paste(" ", textsymbol("bullet"), " Takes you back to the previous response (use if you make a mistake or just want to go back)", sep = ""), defaults = "message")),
            "\n", sep = ""
  )
  )
  
  cat(paste("\n'H'\n",
            paste(colortext(paste(" ", textsymbol("bullet"), " Outputs the information you see here. For other help information, see `?textcleaner`", sep = ""), defaults = "message")),
            "\n", sep = ""
  )
  )
  
  cat(paste("\n'X'\n",
            paste(colortext(paste(" ", textsymbol("bullet"), " Exits textcleaner completely but saves your output", sep = ""), defaults = "message")),
            "\n", sep = ""
  )
  )
  
  cat(paste("\nSelection (accepts lowercase): \n",
            paste(colortext(paste(" ", textsymbol("bullet"), " Where you input the option you select", sep = ""), defaults = "message")),
            "\n\n", sep = ""
  )
  )
  
  readline("Press ENTER to get back to manual spell-check...")
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### editData package functions ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# TAKEN FROM editData VERSION 0.1.8 on December 11th, 2021
# Enables manual entry with double mouse click
#' 
#' @noRd
# Edit Data
# Updated 12.12.2021
AutomatedEdit <- function()
{
  shiny::runApp(appDir = system.file("Automated", package="SemNetCleaner"))
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### AUTOMATED SPELL-CHECK ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' Identifies words that are already spelled correctly and automatically
#' spell corrects responses
#' 
#' @description A sub-rountine to identifies correctly spelled responses
#' and automatically spell corrects responses
#' 
#' @param check List.
#' List of responses that have not been spell-checked
#' 
#' @param full.dict Character vector.
#' A vector of words imported from dictionary
#' 
#' @param dictionary Character.
#' A dictionary to look for word in (see \code{\link{SemNetDictionaries}})
#' 
#' @param spelling Character.
#' Either \code{"UK"} or \code{"US"} for their respective spelling
#' 
#' @return A list containing:
#' 
#' \item{incorrect}{Indices corresponding to responses that need to
#' be manually spell corrected}
#' 
#' \item{to}{List with spelled corrected responses from input of \code{check}}
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Automated Spell-check
# Updated 22.12.2021
auto.spellcheck <- function(check, full.dict, dictionary, spelling, keepStrings)
{
  # Change names of indices
  names(check) <- formatC(1:length(check),
                          width = nchar(as.character(length(check))),
                          format = "d", flag = "0")
  
  # Keep full indices
  full.names <- names(check)

  # Save original responses
  orig <- check
  
  #-------------------------------#
  ## Correctly spelled responses ##
  #-------------------------------#
  
  # Let user know
  message("\nIdentifying correctly spelled responses...", appendLF = FALSE)
  
  # Index correctly and incorrectly spelled responses
  ## Check if all are spelled correctly or incorrectly
  targets <- match(unlist(check),full.dict)
  
  if(all(is.na(targets))){ # All spelled incorrectly
    
    check <- orig
    auto.spell <- names(check)
    
  }else if(all(!is.na(targets))){ # All spelled correctly
    
    ind <- 1:length(check)
    names.ind <- names(check)[ind]
    
    # Message
    message("\n\nAll words passed automated spell-check. Ending spell-check...")
    
    # Initialize result list
    res <- list()
    
    res$manual <- as.numeric()
    auto <- matrix("", nrow = 0, ncol = 2)
    colnames(auto) <- c("from", "to_1")
    res$auto <- auto
    res$to <- orig
    
    return(res)
    
  }else{
    
    ## Get indices
    ind <- which(!is.na(targets))
    names.ind <- names(check)[ind]
    
    # Remove responses from original
    check <- orig[-as.numeric(names.ind)]
    auto.spell <- names(check)
  }
  
  # Let user know
  message(paste("done.\n(", length(check), " of ", length(orig), " unique responses still need to be corrected)", sep = ""))
  
  #------------------------#
  ## Pluralized responses ##
  #------------------------#
  
  # Let user know
  message("\nSingularizing responses...", appendLF = FALSE)
  
  # Index pluralized responses
  ## Singularize responses
  sing <- lapply(check, singularize, dictionary = FALSE)
  
  ## Identify responses found in dictionary
  ## Check if all are spelled correctly or incorrectly
  targets <- match(unlist(sing),full.dict)
  
  if(all(is.na(targets))){ # All spelled incorrectly
    
    sing <- check
    auto.spell <- names(sing)
    
  }else if(all(!is.na(targets))){ # All spelled correctly
    
    ## Get indices
    ind2 <- which(!is.na(targets))
    
    ## Update original responses
    orig[names(sing)[ind2]] <- sing[ind2]
    
    # Message
    message("\n\nAll words passed automated spell-check. Ending spell-check...")
    
    # Initialize result list
    res <- list()
    
    res$manual <- as.numeric()
    res$auto <- setdiff(as.numeric(names(check)), res$manual)
    res$to <- orig
    
    return(res)
    
  }else{
    
    ## Get indices
    ind2 <- which(!is.na(targets))
    
    ## Update original responses
    orig[names(sing)[ind2]] <- sing[ind2]
    
    ## Update correctly spelled indices
    names.ind <- sort(c(names.ind, names(sing)[ind2]))
    
    ## Update singularized responses
    sing <- orig[-as.numeric(names.ind)]
  }
  
  # Let user know
  message(paste("done.\n(", length(sing), " unique responses still need to be corrected)", sep = ""))
  
  #--------------------------------------------#
  ## Correct common misspellings and monikers ##
  #--------------------------------------------#
  
  if(all(dictionary == "general") | all(dictionary == "hunspell")){
    
    mons2 <- sing
    
  }else{
    
    # Check if any dictionaries were improted from SemNetDictionaries
    if(any(dictionary %in% SemNetDictionaries::dictionaries(TRUE)))
    {
      # Let user know
      message("\nAuto-correcting common misspellings and monikers...", appendLF = FALSE)
      
      # Load moniker
      monik <- SemNetDictionaries::load.monikers(dictionary)
      
      if(length(monik)!=0) # Checks in case of only using general dictionary
      {
        ## Check for monikers
        mons <- unlist(lapply(sing, moniker, monik, spelling = spelling), recursive = FALSE)
        
        ## Identify responses found in dictionary
        ## Check if all are spelled correctly or incorrectly
        targets <- match(unlist(mons),full.dict)
        
        if(all(is.na(targets))){ # All spelled incorrectly
          
          mons <- sing
          auto.spell <- names(mons)
          
        }else if(all(!is.na(targets))){ # All spelled correctly
          
          ## Get indices
          ind3 <- which(!is.na(targets))
          
          ## Update original responses
          orig[names(mons)[ind3]] <- mons[ind3]
          
          # Message
          message("\n\nAll words passed automated spell-check. Ending spell-check...")
          
          # Initialize result list
          res <- list()
          
          res$manual <- as.numeric()
          res$auto <- setdiff(as.numeric(names(sing)), res$manual)
          res$to <- orig
          
          return(res)
          
        }else{
          
          ## Get indices
          ind3 <- which(!is.na(targets))
          
          ## Update original responses
          orig[names(mons)[ind3]] <- mons[ind3]
          
          ## Update correctly spelled indices
          names.ind <- sort(c(names.ind, names(mons)[ind3]))
          
          ## Update singularized responses
          mons <- orig[-as.numeric(names.ind)]
          
        }
        
        ## Check for pluralized monikers
        mons2 <- unlist(lapply(mons, function(x, monik, spelling){moniker(singularize(x, dictionary = FALSE), monik, spelling)}, monik, spelling = spelling), recursive = FALSE)
        
        ## Identify responses found in dictionary
        ## Check if all are spelled correctly or incorrectly
        targets <- match(unlist(mons2),full.dict)
        
        if(all(is.na(targets))){ # All spelled incorrectly
          
          mons2 <- mons
          auto.spell <- names(mons2)
          
        }else if(all(!is.na(targets))){ # All spelled correctly
          
          ## Get indices
          ind4 <- which(!is.na(targets))
          
          ## Update original responses
          orig[names(mons2)[ind4]] <- mons2[ind4]
          
          # Message
          message("\n\nAll words passed automated spell-check. Ending spell-check...")
          
          # Initialize result list
          res <- list()
          
          res$manual <- as.numeric()
          res$auto <- setdiff(as.numeric(names(mons2)), res$manual)
          res$to <- orig
          
          return(res)
          
        }else{
          
          ## Get indices
          ind4 <- which(!is.na(targets))
          
          ## Update original responses
          orig[names(mons2)[ind4]] <- mons2[ind4]
          
          ## Update correctly spelled indices
          names.ind <- sort(c(names.ind, names(mons2)[ind4]))
          
          ## Update singularized responses
          mons2 <- orig[-as.numeric(names.ind)]
          
        }
        
      }

      # Let user know
      message("done.")
      
    }else{
      
      mons2 <- sing
      
    }
    
  }
  
  #------------------------------#
  ## Individualized spell-check ##
  #------------------------------#
  
  # Let user know
  message(paste("\nAttempting to auto-correct the remaining", length(mons2),"responses individually..."), appendLF = FALSE)
  
  # Message for large number of responses remaining
  message("\nUsing parallel processing to speed up individual word check...")
  
  # Number of cores
  ncores <- parallel::detectCores() / 2
  
  # Set up clusters
  cl <- parallel::makeCluster(ncores)
  
  # Functions
  # funcs <- c(
  #   "bad.response", "best.guess",
  #   "moniker"
  # )
  # 
  # # Export functions
  # parallel::clusterExport(
  #   cl = cl, funcs,
  #   envir = as.environment(asNamespace("SemNetCleaner"))
  # )
  
  # Spell-check each individual word within the list (including multiple word responses)
  ind.check <- unlist(
    pbapply::pblapply(
      mons2, ind.word.check,
      full.dict = full.dict,
      dictionary = dictionary,
      spelling = spelling,
      cl = cl
    ),
    recursive = FALSE
  )
  
  parallel::stopCluster(cl)

  ## Identify responses found in dictionary
  ## Check if all are spelled correctly or incorrectly
  targets <- match(unlist(ind.check), full.dict)
  
  if(all(is.na(targets))){ # All spelled incorrectly
    
    ind.check <- mons2
    auto.spell <- names(ind.check)
    
  }else if(all(!is.na(targets))){ # All spelled correctly
    
    ## Get indices
    ind5 <- which(!is.na(targets))
    
    ## Update original responses
    orig[names(ind.check)[ind5]] <- ind.check[ind5]
    
    # Message
    message("\n\nAll words passed automated spell-check. Ending spell-check...")
    
    # Initialize result list
    res <- list()
    
    res$manual <- as.numeric()
    res$auto <- setdiff(as.numeric(names(ind.check)), res$manual)
    res$to <- orig
    
    return(res)
    
  }else{
    
    ## Get indices
    ind5 <- which(!is.na(targets))
    
    ## Update original responses
    orig[names(ind.check)[ind5]] <- ind.check[ind5]
    
    ## Update correctly spelled indices
    names.ind <- sort(c(names.ind, names(ind.check)[ind5]))
    
    ## Update individually checked responses
    ind.check <- orig[-as.numeric(names.ind)]
    
  }
  
  # Let user know
  message(paste("(", length(ind.check), " unique responses still need to be corrected)", sep = ""))
  
  #-------------------------------------------#
  ## Parse strings with multi-word responses ##
  #-------------------------------------------#
  
  # Let user know
  message("\nParsing multi-word responses...", appendLF = FALSE)
  
  # Multiple word responses greater than in dictionary
  dict.lens <- unlist(lapply(full.dict, function(x){length(unlist(strsplit(x, " ")))}))
  
  # Set multiple word minimum in response to be considered for split
  multi.min <- ceiling(median(dict.lens) + 2 * sd(dict.lens))
  
  # Check for minimum length of 1
  if(multi.min == 1){
    multi.min <- 2
  }
  
  # Message for large number of responses remaining
  message("\nUsing parallel processing to speed up mutiple word check...")
  
  # Number of cores
  ncores <- parallel::detectCores() / 2
  
  # Set up clusters
  cl <- parallel::makeCluster(ncores)
  
  # Functions
  # funcs <- c(
  #   "bad.response", "best.guess",
  #   "moniker"
  # )
  # 
  # # Export functions
  # parallel::clusterExport(
  #   cl = cl, funcs,
  #   envir = as.environment(asNamespace("SemNetCleaner"))
  # )
  
  # Spell-check each individual word within the list (including multiple word responses)
  multi.word <- pbapply::pblapply(
    ind.check, multiple.words,
    multi.min = multi.min, full.dict = full.dict,
    dictionary = dictionary, spelling = spelling,
    cl = cl
  )
  
  parallel::stopCluster(cl)
  
  ## Identify responses found in dictionary
  ### Check responses that changed
  changed <- unlist(lapply(multi.word, function(x)
    {
      if(any(x$correct))
      {return(TRUE)
      }else{return(FALSE)}
    }))
  
  ### Grab changed responses
  responses <- unlist(multi.word, recursive = FALSE)[grep("response", names(unlist(multi.word, recursive = FALSE)))]
  
  ### Update original responses
  if(!keepStrings){
    if(length(responses[changed]) != 0){
      orig[names(multi.word)[changed]] <- responses[changed]
    }
  }
  
  ### Indices of correctly spelled responses
  ind6 <- unlist(lapply(multi.word, function(x)
  {
    if(all(x$correct))
    {return(TRUE)
    }else{return(FALSE)}
  }))
  
  if(length(names(multi.word)[ind6]) != 0){
    
    ## Update correctly spelled indices
    names.ind <- sort(c(names.ind, names(multi.word)[ind6]))
    
    ## Update checked responses
    multi.word <- orig[-as.numeric(names.ind)]
    
  }
  
  # Search through responses with more than 1 but can be individually split into separate responses
  multi.word <- lapply(multi.word, response.splitter, full.dict = full.dict)
  
  ## Identify responses found in dictionary
  ### Indices of correctly spelled responses
  ind7 <- unlist(lapply(multi.word, function(x)
  {
    if(all(x %in% full.dict))
    {return(TRUE)
    }else{return(FALSE)}
  }))
  
  if(length(multi.word[ind7]) != 0){
    
    ### Update original responses
    orig[names(multi.word)[ind7]] <- multi.word[ind7]
    
    ## Update correctly spelled indices
    names.ind <- sort(c(names.ind, names(multi.word)[ind7]))
    
    ## Update checked responses
    multi.word <- orig[-as.numeric(names.ind)]
  }
  
  # Let user know how many responses need to be spell-checked
  if(length(multi.word) != 0){
    message(paste("\nAutomated spell-checking complete.\nAbout ",length(multi.word)," responses still need to be manually spell-checked", sep = ""))
  }else{
    message("\nAll words passed automaed spell-check. Ending spell-check...")
  }
  
  # Initialize result list
  res <- list()
  
  res$manual <- as.numeric(names(multi.word))
  res$auto <- setdiff(as.numeric(full.names), res$manual)
  res$to <- orig
  
  return(res)
}

# Automated Spell-check
# Updated 27.01.2022
auto.spellcheck.free <- function(check, full.dict, dictionary, spelling, keepStrings)
{
  # Change names of indices
  names(check) <- formatC(1:length(check),
                          width = nchar(as.character(length(check))),
                          format = "d", flag = "0")
  
  # Keep full indices
  full.names <- names(check)
  
  # Save original responses
  orig <- check
  
  #-------------------------------#
  ## Correctly spelled responses ##
  #-------------------------------#
  
  # Let user know
  message("\nIdentifying correctly spelled responses...", appendLF = FALSE)
  
  # Index correctly and incorrectly spelled responses
  ## Check if all are spelled correctly or incorrectly
  targets <- match(unlist(check),full.dict)
  
  if(all(is.na(targets))){ # All spelled incorrectly
    
    check <- orig
    auto.spell <- names(check)
    
  }else if(all(!is.na(targets))){ # All spelled correctly
    
    ind <- 1:length(check)
    names.ind <- names(check)[ind]
    
    # Message
    message("\n\nAll words passed automated spell-check. Ending spell-check...")
    
    # Initialize result list
    res <- list()
    
    res$manual <- as.numeric()
    auto <- matrix("", nrow = 0, ncol = 2)
    colnames(auto) <- c("from", "to_1")
    res$auto <- auto
    res$to <- orig
    
    return(res)
    
  }else{
    
    ## Get indices
    ind <- which(!is.na(targets))
    names.ind <- names(check)[ind]
    
    # Remove responses from original
    check <- orig[-as.numeric(names.ind)]
    auto.spell <- names(check)
  }
  
  # Let user know
  message(paste("done.\n(", length(check), " of ", length(orig), " unique responses still need to be corrected)", sep = ""))
  
  #---------------------------#
  ## Short and bad responses ##
  #---------------------------#
  
  # Remove stop words
  ## stop words
  stop_words <- SemNetDictionaries::load.dictionaries("stop_words")
  ## format stop words for gsub
  stop_format <- paste("\\b", stop_words, "\\b", sep = "", collapse = "|")
  ## replace
  uniq.resp <- gsub(stop_format, "", unlist(check))
  ## make sure spaces at beginning and end are removed
  uniq.resp <- trimws(uniq.resp)
  ## remove bad words
  bad.uniq.resp <- bad.response(uniq.resp)
  bad.index <- which(is.na(bad.uniq.resp))
  
  # Remove single letters
  rm_letters <- letters[!letters %in% full.dict]
  one.index <- which(uniq.resp %in% rm_letters)
  
  # Remove two letters
  two_letters <- apply(expand.grid(letters, letters), 1, paste, collapse = "", sep = "")
  rm_letters <- two_letters[!two_letters %in% full.dict]
  two.index <- which(uniq.resp %in% rm_letters)
  
  # Concatenate bad indices
  na.index <- unique(sort(c(bad.index, one.index, two.index)))
  names(na.index) <- names(uniq.resp)[na.index]
  
  ## Update original responses
  orig[names(na.index)] <- NA
  
  ## Update correctly spelled indices
  names.ind <- unique(sort(c(names.ind, names(na.index))))
  
  # Remove responses from check
  check <- check[setdiff(names(check), names(na.index))]
  auto.spell <- names(check)
  
  #------------------------#
  ## Pluralized responses ##
  #------------------------#
  
  # Let user know
  message("\nSingularizing responses...", appendLF = FALSE)
  
  # Index pluralized responses
  ## Singularize responses
  sing <- lapply(check, singularize, dictionary = FALSE)
  
  ## Identify responses found in dictionary
  ## Check if all are spelled correctly or incorrectly
  targets <- match(unlist(sing),full.dict)
  
  if(all(is.na(targets))){ # All spelled incorrectly
    
    sing <- check
    auto.spell <- names(sing)
    
  }else if(all(!is.na(targets))){ # All spelled correctly
    
    ## Get indices
    ind2 <- which(!is.na(targets))
    
    ## Update original responses
    orig[names(sing)[ind2]] <- sing[ind2]
    
    # Message
    message("\n\nAll words passed automated spell-check. Ending spell-check...")
    
    # Initialize result list
    res <- list()
    
    res$manual <- as.numeric()
    res$auto <- setdiff(as.numeric(names(check)), res$manual)
    res$to <- orig
    
    return(res)
    
  }else{
    
    ## Get indices
    ind2 <- which(!is.na(targets))
    
    ## Update original responses
    orig[names(sing)[ind2]] <- sing[ind2]
    
    ## Update correctly spelled indices
    names.ind <- sort(c(names.ind, names(sing)[ind2]))
    
    ## Update singularized responses
    sing <- orig[-as.numeric(names.ind)]
  }
  
  # Let user know
  message(paste("done.\n(", length(sing), " unique responses still need to be corrected)", sep = ""))
  
  #--------------------------------------------#
  ## Correct common misspellings and monikers ##
  #--------------------------------------------#
  
  if(all(dictionary == "general") | all(dictionary == "hunspell")){
    
    mons2 <- sing
    
  }else{
    
    # Check if any dictionaries were improted from SemNetDictionaries
    if(any(dictionary %in% SemNetDictionaries::dictionaries(TRUE)))
    {
      # Let user know
      message("\nAuto-correcting common misspellings and monikers...", appendLF = FALSE)
      
      # Load moniker
      monik <- SemNetDictionaries::load.monikers(dictionary)
      
      if(length(monik)!=0) # Checks in case of only using general dictionary
      {
        ## Check for monikers
        mons <- unlist(lapply(sing, moniker, monik, spelling = spelling), recursive = FALSE)
        
        ## Identify responses found in dictionary
        ## Check if all are spelled correctly or incorrectly
        targets <- match(unlist(mons),full.dict)
        
        if(all(is.na(targets))){ # All spelled incorrectly
          
          mons <- sing
          auto.spell <- names(mons)
          
        }else if(all(!is.na(targets))){ # All spelled correctly
          
          ## Get indices
          ind3 <- which(!is.na(targets))
          
          ## Update original responses
          orig[names(mons)[ind3]] <- mons[ind3]
          
          # Message
          message("\n\nAll words passed automated spell-check. Ending spell-check...")
          
          # Initialize result list
          res <- list()
          
          res$manual <- as.numeric()
          res$auto <- setdiff(as.numeric(names(sing)), res$manual)
          res$to <- orig
          
          return(res)
          
        }else{
          
          ## Get indices
          ind3 <- which(!is.na(targets))
          
          ## Update original responses
          orig[names(mons)[ind3]] <- mons[ind3]
          
          ## Update correctly spelled indices
          names.ind <- sort(c(names.ind, names(mons)[ind3]))
          
          ## Update singularized responses
          mons <- orig[-as.numeric(names.ind)]
          
        }
        
        ## Check for pluralized monikers
        mons2 <- unlist(lapply(mons, function(x, monik, spelling){moniker(singularize(x, dictionary = FALSE), monik, spelling)}, monik, spelling = spelling), recursive = FALSE)
        
        ## Identify responses found in dictionary
        ## Check if all are spelled correctly or incorrectly
        targets <- match(unlist(mons2),full.dict)
        
        if(all(is.na(targets))){ # All spelled incorrectly
          
          mons2 <- mons
          auto.spell <- names(mons2)
          
        }else if(all(!is.na(targets))){ # All spelled correctly
          
          ## Get indices
          ind4 <- which(!is.na(targets))
          
          ## Update original responses
          orig[names(mons2)[ind4]] <- mons2[ind4]
          
          # Message
          message("\n\nAll words passed automated spell-check. Ending spell-check...")
          
          # Initialize result list
          res <- list()
          
          res$manual <- as.numeric()
          res$auto <- setdiff(as.numeric(names(mons2)), res$manual)
          res$to <- orig
          
          return(res)
          
        }else{
          
          ## Get indices
          ind4 <- which(!is.na(targets))
          
          ## Update original responses
          orig[names(mons2)[ind4]] <- mons2[ind4]
          
          ## Update correctly spelled indices
          names.ind <- sort(c(names.ind, names(mons2)[ind4]))
          
          ## Update singularized responses
          mons2 <- orig[-as.numeric(names.ind)]
          
        }
        
      }
      
      # Let user know
      message("done.")
      
    }else{
      
      mons2 <- sing
      
    }
    
  }
  
  #------------------------------#
  ## Individualized spell-check ##
  #------------------------------#
  
  # Let user know
  message(paste("\nAttempting to auto-correct the remaining", length(mons2),"responses individually..."), appendLF = FALSE)
  
  # Message for large number of responses remaining
  message("\nUsing parallel processing to speed up individual word check...")
  
  # Number of cores
  ncores <- parallel::detectCores() / 2
  
  # Set up clusters
  cl <- parallel::makeCluster(ncores)

  # # Functions
  # funcs <- c(
  #   "bad.response", "best.guess",
  #   "moniker"
  # )
  # 
  # # Export functions
  # parallel::clusterExport(
  #   cl = cl, funcs,
  #   envir = as.environment(asNamespace("SemNetCleaner"))
  # )
  
  # Spell-check each individual word within the list (including multiple word responses)
  ind.check <- unlist(
    pbapply::pblapply(
      mons2, ind.word.check,
      full.dict = full.dict,
      dictionary = dictionary,
      spelling = spelling,
      cl = cl
    ),
    recursive = FALSE
  )
  
  parallel::stopCluster(cl)
  
  ## Identify responses found in dictionary
  ## Check if all are spelled correctly or incorrectly
  targets <- match(unlist(ind.check), full.dict)
  
  if(all(is.na(targets))){ # All spelled incorrectly
    
    ind.check <- mons2
    auto.spell <- names(ind.check)
    
  }else if(all(!is.na(targets))){ # All spelled correctly
    
    ## Get indices
    ind5 <- which(!is.na(targets))
    
    ## Update original responses
    orig[names(ind.check)[ind5]] <- ind.check[ind5]
    
    # Message
    message("\n\nAll words passed automated spell-check. Ending spell-check...")
    
    # Initialize result list
    res <- list()
    
    res$manual <- as.numeric()
    res$auto <- setdiff(as.numeric(names(ind.check)), res$manual)
    res$to <- orig
    
    return(res)
    
  }else{
    
    ## Get indices
    ind5 <- which(!is.na(targets))
    
    ## Update original responses
    orig[names(ind.check)[ind5]] <- ind.check[ind5]
    
    ## Update correctly spelled indices
    names.ind <- sort(c(names.ind, names(ind.check)[ind5]))
    
    ## Update individually checked responses
    ind.check <- orig[-as.numeric(names.ind)]
    
  }
  
  # Let user know
  message(paste("(", length(ind.check), " unique responses still need to be corrected)", sep = ""))
  
  #-------------------------------------------#
  ## Parse strings with multi-word responses ##
  #-------------------------------------------#
  
  # Let user know
  message("\nParsing multi-word responses...", appendLF = FALSE)
  
  # Multiple word responses greater than in dictionary
  dict.lens <- unlist(lapply(full.dict, function(x){length(unlist(strsplit(x, " ")))}))
  
  # Set multiple word minimum in response to be considered for split
  multi.min <- ceiling(median(dict.lens) + 2 * sd(dict.lens))
  
  # Check for minimum length of 1
  if(multi.min == 1){
    multi.min <- 2
  }
  
  # Message for large number of responses remaining
  message("\nUsing parallel processing to speed up mutiple word check...")
  
  # Number of cores
  ncores <- parallel::detectCores() / 2
  
  # Set up clusters
  cl <- parallel::makeCluster(ncores)

  # # Functions
  # funcs <- c(
  #   "bad.response", "best.guess",
  #   "moniker"
  # )
  # 
  # # Export functions
  # parallel::clusterExport(
  #   cl = cl, funcs,
  #   envir = as.environment(asNamespace("SemNetCleaner"))
  # )
  
  # Spell-check each individual word within the list (including multiple word responses)
  multi.word <- pbapply::pblapply(
    ind.check, multiple.words,
    multi.min = multi.min, full.dict = full.dict,
    dictionary = dictionary, spelling = spelling,
    cl = cl
  )
  
  parallel::stopCluster(cl)
  
  ## Identify responses found in dictionary
  ### Check responses that changed
  changed <- unlist(lapply(multi.word, function(x)
  {
    if(any(x$correct))
    {return(TRUE)
    }else{return(FALSE)}
  }))
  
  ### Grab changed responses
  responses <- unlist(multi.word, recursive = FALSE)[grep("response", names(unlist(multi.word, recursive = FALSE)))]
  
  ### Update original responses
  if(!keepStrings){
    if(length(responses[changed]) != 0){
      orig[names(multi.word)[changed]] <- responses[changed]
    }
  }
  
  ### Indices of correctly spelled responses
  ind6 <- unlist(lapply(multi.word, function(x)
  {
    if(all(x$correct))
    {return(TRUE)
    }else{return(FALSE)}
  }))
  
  if(length(names(multi.word)[ind6]) != 0){
    
    ## Update correctly spelled indices
    names.ind <- sort(c(names.ind, names(multi.word)[ind6]))
    
    ## Update checked responses
    multi.word <- orig[-as.numeric(names.ind)]
    
  }
  
  # Search through responses with more than 1 but can be individually split into separate responses
  multi.word <- lapply(multi.word, response.splitter, full.dict = full.dict)
  
  ## Identify responses found in dictionary
  ### Indices of correctly spelled responses
  ind7 <- unlist(lapply(multi.word, function(x)
  {
    if(all(x %in% full.dict))
    {return(TRUE)
    }else{return(FALSE)}
  }))
  
  if(length(multi.word[ind7]) != 0){
    
    ### Update original responses
    orig[names(multi.word)[ind7]] <- multi.word[ind7]
    
    ## Update correctly spelled indices
    names.ind <- sort(c(names.ind, names(multi.word)[ind7]))
    
    ## Update checked responses
    multi.word <- orig[-as.numeric(names.ind)]
  }
  
  # Let user know how many responses need to be spell-checked
  if(length(multi.word) != 0){
    message(paste("\nAutomated spell-checking complete.\nAbout ",
                  length(multi.word),
                  " responses still need to be manually spell-checked", sep = ""))
  }else{
    message("\nAll words passed automaed spell-check. Ending spell-check...")
  }
  
  # Initialize result list
  res <- list()
  
  res$manual <- as.numeric(names(multi.word))
  res$auto <- setdiff(as.numeric(full.names), res$manual)
  res$to <- orig
  
  return(res)
}

#' Function to update change list in \code{\link[SemNetCleaner]{spellcheck.menu}}
#' 
#' @description Sub-rountine to update change list
#' 
#' @param change.list List.
#' Full list of changes so far
#' 
#' @param change.matrix Matrix.
#' Incoming changes to be added to \code{change.list}
#' 
#' @param current.index Numeric.
#' Index of \code{change.list} where changes need to be updated
#' 
#' @return An updated \code{change.list}
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
#' 
# Change list formatting
# Updated 12.04.2020
change.format <- function (change.list, change.matrix, current.index)
{
  # Check if all rows have been removed from list
  # (resulting in an empty list with no name)
  if(is.null(names(change.list)))
  {
    change.list <- list(change.list)
    names(change.list) <- paste(current.index)
  }
  
  # Number of columns
  n.list <- ncol(change.list[[paste(current.index)]])
  n.matrix <- ncol(change.matrix)
    
  # Number of rows
  r.list <- nrow(change.list[[paste(current.index)]])
  
  # Check if change list already exists
  if(!is.null(n.list))
  {
    # Incoming change matrix has more columns than existing change list
    if(n.list < n.matrix)
    {
      # Create new change matrix
      new.matrix <- matrix(NA, nrow = r.list + 1, ncol = n.matrix)
      colnames(new.matrix) <- colnames(change.matrix)
      
      # Loop through change list
      for(i in 1:r.list)
      {
        # Target row
        row.target <- as.vector(change.list[[paste(current.index)]][i,])
        
        # Insert into new change matrix
        new.matrix[i,length(row.target)] <- row.target
      }
      
      # Add change matrix to the end
      new.matrix <- rbind(new.matrix, change.matrix)
      
      # Update with new matrix
      change.list[[paste(current.index)]] <- new.matrix
      
      # Remove row names
      row.names(change.list[[paste(current.index)]]) <- NULL
      
    }else if(n.list > n.matrix) # Incoming change matrix has fewer columns than existing change list
    {
      # Determine difference in number of columns
      diff.cols <- n.list - n.matrix
      
      # Add on NAs to change matrix
      change.matrix <- c(as.vector(change.matrix), rep(NA, diff.cols))
      
      # Update change list
      change.list[[paste(current.index)]] <- rbind(change.list[[paste(current.index)]], change.matrix)
      
      # Remove row names
      row.names(change.list[[paste(current.index)]]) <- NULL
      
    }else{
      # Update change list
      change.list[[paste(current.index)]] <- rbind(change.list[[paste(current.index)]], change.matrix)
      
      # Remove row names
      row.names(change.list[[paste(current.index)]]) <- NULL
    }
    
  }else{change.list[[paste(current.index)]] <- change.matrix}
  
  return(change.list)
}

#%%%%%%%%%%%%%%%%%%%%%%%%#
#### SPELL-CHECK MENU ####
#%%%%%%%%%%%%%%%%%%%%%%%%#

#' Interactive Manual Spell-check
#' 
#' @description An interactive manual spell-check based on a custom base R \code{\link{menu}}
#' 
#' @param check Character.
#' Response to be manually spell-checked
#' 
#' @param context Character string.
#' A string of responses the \code{check} response is nested in.
#' Defaults to \code{NULL}
#' 
#' @param possible Character vector.
#' Possible response options (obtained from \code{\link[SemNetCleaner]{best.guess}})
#' 
#' @param original List.
#' A list of the original auto-corrected responses
#' 
#' @param current.index Numeric.
#' The current index the \code{check} response is in
#' 
#' @param changes List.
#' A list corresponding to the manual spell-check changes already made
#' 
#' @param full.dictionary Character vector.
#' A vector of correctly spelled, category appropriate responses
#' 
#' @param category Character.
#' A category for a more targeted Google search
#' 
#' @return A list containing:
#' 
#' \item{go.back}{\code{TRUE}/\code{FALSE} for whether to go back to the previous spell-checked
#' response}
#' 
#' \item{target}{The spell-checked target string or response}
#' 
#' \item{changes}{The changes list updated with the manual spell-check changes}
#' 
#' \item{full.dictionary}{The full dictionary updated with words that were manually added}
#' 
#' \item{answer}{The response input to handle the \code{check} response}
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
#' 
# Menu for Manual Spell-check
# Updated 04.01.2021
spellcheck.menu <- function (check, context = NULL, possible, original,
                             current.index, changes, full.dictionary, category,
                             dictionary, keepStrings, go.back.count)
{
  # Initialize answer
  ans <- 30
  
  # Initialize GO BACK
  go.back <- FALSE
  
  # Check if word is already in dictionary (due to dictionary updates)
  ## Check for multiple words
  if(!is.null(context)){
    dict.check <- context[check]
  }else{dict.check <- check}
  
  ## Check dictionary
  if(dict.check %in% full.dictionary){
    
    ## Message that response was cleared by previous decision
    if(!is.null(context)){
      message(paste("\nResponse '",
                    styletext(dict.check, defaults = "bold"), "' in '", styletext(paste(context, collapse = " "), defaults = "bold"),
                    "' was KEPT AS IS based on a previous manual spell-check decision",
                    sep = ""))
    }else{
      message(paste("\nResponse '",
                    styletext(dict.check, defaults = "bold"),
                    "' was KEPT AS IS based on a previous manual spell-check decision",
                    sep = ""))
    }
    
    ## Initialize result list
    res <- list()
    res$go.back <- go.back
    res$target <- ifelse(is.null(context), dict.check, context)
    res$changes <- changes
    res$full.dictionary <- full.dictionary
    res$end <- if(is.null(context)){NULL}else{FALSE}
    res$go.back.count <- go.back.count + 1
    
    # Add artificial pause for smoother feel
    if(!"general" %in% dictionary){
      Sys.sleep(1)
    }
    
    return(res)
    
  }
  
  # Set up based on context (multiple responses)
  if(!is.null(context)){
    
    # Initialize END
    end <- FALSE
    
    while(ans == 30){
      
      # Title for spell-check
      if(keepStrings){
        
        title <- paste(paste("\nOriginal string: ", "'", original, "'", sep = ""),
                       paste("Target word: ", paste("'", colortext(context[check], defaults = "highlight"), "'", sep = ""), sep = ""),
                       sep = "\n\n")
        
      }else{
        
        title <- paste(paste("\nOriginal string: ", "'", original, "'", sep = ""),
                       paste("Auto-corrected string: ", paste("'", context, "'", sep = "", collapse = " "), sep = ""),
                       paste("Target word: ", paste("'", colortext(context[check], defaults = "highlight"), "'", sep = ""), sep = ""),
                       sep = "\n\n")
        
      }
      
      # Choices for spell-check
      word <- c("SKIP WORD", "ADD WORD TO DICTIONARY", "TYPE MY OWN WORD", "GOOGLE WORD", "BAD WORD")
      if(keepStrings){
        string <- c("KEEP ORIGINAL", "TYPE MY OWN STRING", "GOOGLE STRING", "BAD STRING")
      }else{
        string <- c("KEEP ORIGINAL", "KEEP AUTO-CORRECT", "TYPE MY OWN STRING", "GOOGLE STRING", "BAD STRING")
      }
      
      choices <- c(word, string, possible)
      
      # Default length
      default <- length(c(word, string))
      
      # Output menu
      customMenu(choices = choices, title = title, default = default)
      
      # Message user
      message("Press 'B' to GO BACK, 'H' for HELP, or 'X' to EXIT.\n")
      
      # Present prompt
      ans <- readline(prompt = "Selection (accepts lowercase): ")
      
      # Check for user stoppage
      if(tolower(ans) == "x" || ans == "")
      {return("STOP")}
      
      # Original answer 
      original_ans <- tolower(ans)
      
      # Check for appropriate answer
      ans <- appropriate.answer(answer = ans, choices = choices, default = default)
      
      # Change answer for keeping strings
      if(keepStrings){
        
        # Between 7 and length of choices
        if(ans >= 7 && ans <= length(choices)){
          ans <- ans + 1
        }
        
      }
        
      # Answer options
      if(ans == 1) # SKIP WORD
      {
        
        ## Check for strings
        if(keepStrings){
          
          ## Message user
          if(check == 1){
            
            message(paste("\nResponse was SKIPPED: '",
                          styletext(context[check], defaults = "bold"),
                          " ",
                          paste(context[-check], collapse = " "),
                          "'", sep = "", collapse = " "))
            
          }else if(check == length(unlist(strsplit(context, split = " ")))){
            
            message(paste("\nResponse was SKIPPED: '",
                          paste(context[-check], collapse = " "),
                          " ",
                          styletext(context[check], defaults = "bold"),
                          "'", sep = "", collapse = " "))
            
          }else{
            
            ## Word number in string
            word.num <- which(context[check] == unlist(strsplit(context, split = " ")))
            
            message(paste("\nResponse was SKIPPED: '",
                          paste(context[1:(word.num - 1)], collapse = " "),
                          " ",
                          styletext(context[word.num], defaults = "bold"),
                          " ",
                          paste(context[(word.num + 1):length(unlist(strsplit(context, split = " ")))], collapse = " "),
                          "'", sep = "", collapse = " "))
            
          }
          
        }else{
          
          ## Message user
          message(paste("\nResponse was SKIPPED:", paste("'", context[check], "'", sep = "")))
          
        }
        
      }else if(ans == 2) # ADD WORD TO DICTIONARY
      {
        ## Add response to dictionary
        full.dictionary <- SemNetDictionaries::append.dictionary(context[check],
                                                                 full.dictionary,
                                                                 dictionary.name = "full",
                                                                 save.location = "envir", textcleaner = TRUE)
        
        ## Check for strings
        if(keepStrings){
          
          ## Message user
          if(check == 1){
            
            message(paste("\nResponse was KEPT AS: '",
                          styletext(context[check], defaults = "bold"),
                          " ",
                          paste(context[-check], collapse = " "),
                          "'", sep = "", collapse = " "))
            
          }else if(check == length(unlist(strsplit(context, split = " ")))){
            
            message(paste("\nResponse was KEPT AS: '",
                          paste(context[-check], collapse = " "),
                          " ",
                          styletext(context[check], defaults = "bold"),
                          "'", sep = "", collapse = " "))
            
          }else{
            
            ## Word number in string
            word.num <- which(context[check] == unlist(strsplit(context, split = " ")))
            
            message(paste("\nResponse was KEPT AS: '",
                          paste(context[1:(word.num - 1)], collapse = " "),
                          " ",
                          styletext(context[word.num], defaults = "bold"),
                          " ",
                          paste(context[(word.num + 1):length(unlist(strsplit(context, split = " ")))], collapse = " "),
                          "'", sep = "", collapse = " "))
            
          }
          
        }
        
      }else if(ans == 3) # TYPE MY OWN WORD
      {
        message("\nType '30' (no quotations) to go back to the other response options\n")
        
        tmo <- readline(prompt = "Use commas for multiple words (dog, fish, etc.): ")
        
        if(tmo != "30")
        {
          ## Split responses (trim white spaces)
          tmo.split <- trimws(unlist(strsplit(tmo, split = ",")))
          
          ## Insert into change matrix
          change.mat <- t(as.matrix(c(context[check], tmo.split)))
          colnames(change.mat) <- c("from", paste("to", 1:(ncol(change.mat)-1), sep = "_"))
          
          ## Update changes
          changes <- change.format(change.list = changes,
                                   change.matrix = change.mat,
                                   current.index = current.index)
          
          ## Check if typed response is in dictionary
          if(!all(tmo.split %in% full.dictionary))
          {
            ### Identify responses not in dictionary
            non.dict <- which(!tmo.split %in% full.dictionary)
            
            ### Loop through non-dictionary responses
            for(i in 1:length(non.dict))
            {
              #customMenu(choices = c("Yes", "No"),
              #           title = paste("\n",
              #                         paste("'", tmo.split[non.dict[i]], "'", sep = ""),
              #                         " was not found in the dictionary. Should ",
              #                         paste("'", tmo.split[non.dict[i]], "'", sep = ""),
              #                         " be added to the dictionary?", sep = ""),
              #           default = 2,
              #           dictionary = TRUE)
              #
              #dict.ans <- readline(prompt = "Selection: ")
              #
              #dict.ans <- appropriate.answer(answer = dict.ans, choices = c("Yes", "No"), default = 2, dictionary = TRUE)
              
              message(paste("\n'", tmo.split[non.dict[i]], "'", sep = ""),
                      " was not found in the dictionary.\n")
              
              dict.ans <- yes.no.menu(
                paste("Should ", paste("'", tmo.split[non.dict[i]], "'", sep = ""),
                      " be added to the dictionary", sep = "")
              )
              
              if(dict.ans == 1)
              {
                full.dictionary <- SemNetDictionaries::append.dictionary(tmo.split[non.dict[i]],
                                                                         full.dictionary,
                                                                         dictionary.name = "full",
                                                                         save.location = "envir", textcleaner = TRUE)
              }
            }
          }
          
          ## Check for strings
          if(keepStrings){
            
            ## Message user
            if(check == 1){
              
              message(paste("\nResponse was CHANGED TO: '",
                            styletext(paste(tmo.split, collapse = " "), defaults = "bold"),
                            " ",
                            paste(context[-check], collapse = " "),
                            "'", sep = "", collapse = " "))
              
            }else if(check == length(unlist(strsplit(context, split = " ")))){
              
              message(paste("\nResponse was CHANGED TO: '",
                            paste(context[-check], collapse = " "),
                            " ",
                            styletext(paste(tmo.split, collapse = " "), defaults = "bold"),
                            "'", sep = "", collapse = " "))
              
            }else{
              
              ## Word number in string
              word.num <- which(context[check] == unlist(strsplit(context, split = " ")))
              
              message(paste("\nResponse was CHANGED TO: '",
                            paste(context[1:(word.num - 1)], collapse = " "),
                            " ",
                            styletext(paste(tmo.split, collapse = " "), defaults = "bold"),
                            " ",
                            paste(context[(word.num + 1):length(unlist(strsplit(context, split = " ")))], collapse = " "),
                            "'", sep = "", collapse = " "))
              
            }
            
          }else{
            
            ## Message user
            message(paste("\nResponse was CHANGED TO:", paste("'", tmo.split, "'", sep = "", collapse = " ")))
            
          }
          
          ## Change responses in context
          context.change <- as.list(context)
          context.change[[check]] <- tmo.split
          context <- unlist(context.change)
          
        }else{ans <- 30}
        
      }else if(ans == 4) # GOOGLE WORD
      {
        # Use 'searcher' package
        searcher::search_site(paste(category, " '", context[check], "'", sep = "", collpase = ""),
                              site = "google", rlang = FALSE)
        
        # Renew prompt
        ans <- 30
        
      }else if(ans == 5) # BAD WORD
      {
        ## Set up change matrix
        change.mat <- t(as.matrix(c(context[check], NA)))
        colnames(change.mat) <- c("from", paste("to", 1:(ncol(change.mat)-1), sep = "_"))
        
        ## Update changes
        changes <- change.format(change.list = changes,
                                 change.matrix = change.mat,
                                 current.index = current.index)
        
        ## Check for strings
        if(keepStrings){
          
          ## Message user
          if(check == 1){
            
            message(paste("\nResponse was CHANGED TO: '",
                          styletext(NA, defaults = "bold"),
                          " ",
                          paste(context[-check], collapse = " "),
                          "'", sep = "", collapse = " "))
            
          }else if(check == length(unlist(strsplit(context, split = " ")))){
            
            message(paste("\nResponse was CHANGED TO: '",
                          paste(context[-check], collapse = " "),
                          " ",
                          styletext(NA, defaults = "bold"),
                          "'", sep = "", collapse = " "))
            
          }else{
            
            ## Word number in string
            word.num <- which(context[check] == unlist(strsplit(context, split = " ")))
            
            message(paste("\nResponse was CHANGED TO: '",
                          paste(context[1:(word.num - 1)], collapse = " "),
                          " ",
                          styletext(NA, defaults = "bold"),
                          " ",
                          paste(context[(word.num + 1):length(unlist(strsplit(context, split = " ")))], collapse = " "),
                          "'", sep = "", collapse = " "))
            
          }
          
        }else{
          
          ## Message user
          message(paste("\nResponse was CHANGED TO:", paste(NA, sep = "", collapse = " ")))
          
        }
        
        ## Change responses in context
        context.change <- as.list(context)
        context.change[[check]] <- "NA"
        context <- unlist(context.change)
        
      }else if(ans == 6) # KEEP ORIGINAL
      {
        ## Message user
        message(paste("\nString was REVERTED TO ORIGINAL:", paste("'", original, "'", sep = "")))
        
        ## Change responses in context
        context <- original
        
        ## Update END
        end <- TRUE
        
        ## Check if in dictionary
        if(!original %in% full.dictionary)
        {
          #customMenu(choices = c("Yes", "No"),
          #           title = paste("\n",
          #                         paste("'", original, "'", sep = ""),
          #                         " was not found in the dictionary. Should ",
          #                         paste("'", original, "'", sep = ""),
          #                         " be added to the dictionary?", sep = ""),
          #           default = 2,
          #           dictionary = TRUE)
          #
          #dict.ans <- readline(prompt = "Selection: ")
          #
          #dict.ans <- appropriate.answer(answer = dict.ans, choices = c("Yes", "No"), default = 2, dictionary = TRUE)
          
          message(paste("\n'", original, "'", sep = ""),
                  " was not found in the dictionary.\n")
          
          dict.ans <- yes.no.menu(
            paste("Should ",
                  paste("'", original, "'", sep = ""),
                  " be added to the dictionary", sep = "")
          )
          
          if(dict.ans == 1)
          {
            full.dictionary <- SemNetDictionaries::append.dictionary(original, 
                                                                     full.dictionary,
                                                                     dictionary.name = "full",
                                                                     save.location = "envir", textcleaner = TRUE)
          }
        }
        
      }else if(ans == 7) # KEEP AUTO-CORRECT
      {
        ## Message user
        message(paste("\nString was KEPT AS AUTO-CORRECT:", paste("'", context, "'", sep = "", collapse = " ")))
        
        ## Change responses in context
        context <- context
        
        ## Update END
        end <- TRUE
        
      }else if(ans == 8) # TYPE MY OWN STRING
      {
        message("\nType '30' (no quotations) to go back to the other response options\n")
        
        ams <- readline(prompt = "Use commas for multiple words (dog, fish, etc.): ")
        
        if(ams != "30")
        {
          ## Split responses (trim white spaces)
          ams.split <- trimws(unlist(strsplit(ams, split = ",")))
          
          ## Insert into change matrix
          change.mat <- t(as.matrix(c(paste(context, collapse = " "), ams.split)))
          colnames(change.mat) <- c("from", paste("to", 1:(ncol(change.mat)-1), sep = "_"))
          
          ## Update changes
          changes <- change.format(change.list = changes,
                                   change.matrix = change.mat,
                                   current.index = current.index)
          
          ## Check if typed response is in dictionary
          if(!all(ams.split %in% full.dictionary))
          {
            ### Identify responses not in dictionary
            non.dict <- which(!ams.split %in% full.dictionary)
            
            ### Loop through non-dictionary responses
            for(i in 1:length(non.dict))
            {
              #customMenu(choices = c("Yes", "No"),
              #           title = paste("\n",
              #                         paste("'", ams.split[non.dict[i]], "'", sep = ""),
              #                         " was not found in the dictionary. Should ",
              #                         paste("'", ams.split[non.dict[i]], "'", sep = ""),
              #                         " be added to the dictionary?", sep = ""),
              #           default = 2,
              #           dictionary = TRUE)
              #
              #dict.ans <- readline(prompt = "Selection: ")
              #  
              #dict.ans <- appropriate.answer(answer = dict.ans, choices = c("Yes", "No"), default = 2, dictionary = TRUE)
              
              message(paste("\n'", ams.split[non.dict[i]], "'", sep = ""),
                      " was not found in the dictionary.\n")
              
              dict.ans <- yes.no.menu(
                paste("Should ",
                      paste("'", ams.split[non.dict[i]], "'", sep = ""),
                      " be added to the dictionary", sep = "")
              )
              
              if(dict.ans == 1)
              {
                full.dictionary <- SemNetDictionaries::append.dictionary(ams.split[non.dict[i]],
                                                                         full.dictionary,
                                                                         dictionary.name = "full",
                                                                         save.location = "envir", textcleaner = TRUE)
              }
            }
          }
          
          ## Message user
          message(paste("\nString was CHANGED TO:", paste("'", ams.split, "'", sep = "", collapse = " ")))
          
          ## Change responses in context
          context <- ams.split
          
          ## Update END
          end <- TRUE
          
        }else{ans <- 30}
        
      }else if(ans == 9) # GOOGLE STRING
      {
        # Use 'searcher' package
        searcher::search_site(paste(category, " '", original, "'", sep = "", collpase = ""),
                              site = "google", rlang = FALSE)
        
        # Renew prompt
        ans <- 30
        
      }else if(ans == 10) # BAD STRING
      {
        ## Set up change matrix
        change.mat <- cbind(context, rep(NA, length(context)))
        colnames(change.mat) <- c("from", paste("to", 1:(ncol(change.mat)-1), sep = "_"))
        
        ## Update changes
        changes <- change.format(change.list = changes,
                                 change.matrix = change.mat,
                                 current.index = current.index)
        
        ## Message user
        message(paste("\nALL responses were CHANGED TO:", paste(NA, sep = "", collapse = " ")))
        
        ## Change responses in context
        context <- rep("NA", length(context))
        
      }else if(ans == length(choices) + 1 & original_ans == "b") # GO BACK
      {go.back <- TRUE
      }else if(ans == length(choices) + 2 & original_ans == "h") # HELP
      {
        # Get `textcleaner` documentation
        textcleaner_help(check, context, original, possible)
        
        # Renew prompt
        ans <- 30
        
      }else{# RESPONSE OPTION
        
        ## Set up change matrix
        change.mat <- t(as.matrix(c(context[check], possible[ans-10])))
        colnames(change.mat) <- c("from", paste("to",1:(ncol(change.mat)-1), sep = "_"))
        
        ## Update changes
        changes <- change.format(change.list = changes,
                                 change.matrix = change.mat,
                                 current.index = current.index)
        
        ## Check for strings
        if(keepStrings){
          
          ## Message user
          if(check == 1){
            
            message(paste("\nResponse was CHANGED TO: '",
                          styletext(possible[ans-10], defaults = "bold"),
                          " ",
                          paste(context[-check], collapse = " "),
                          "'", sep = "", collapse = " "))
            
          }else if(check == length(unlist(strsplit(context, split = " ")))){
            
            message(paste("\nResponse was CHANGED TO: '",
                          paste(context[-check], collapse = " "),
                          " ",
                          styletext(possible[ans-10], defaults = "bold"),
                          "'", sep = "", collapse = " "))
            
          }else{
            
            ## Word number in string
            word.num <- which(context[check] == unlist(strsplit(context, split = " ")))
            
            message(paste("\nResponse was CHANGED TO: '",
                          paste(context[1:(word.num - 1)], collapse = " "),
                          " ",
                          styletext(possible[ans-10], defaults = "bold"),
                          " ",
                          paste(context[(word.num + 1):length(unlist(strsplit(context, split = " ")))], collapse = " "),
                          "'", sep = "", collapse = " "))
            
          }
          
        }else{
          
          ## Message user
          message(paste("\nResponse was CHANGED TO:", paste("'", possible[ans-10], "'", sep = "")))
          
        }
        
        ## Change responses in context
        context[check] <- possible[ans-10]
      }
    }
    
    # Initialize result list
    res <- list()
    res$go.back <- go.back
    res$target <- context
    res$changes <- changes
    res$full.dictionary <- full.dictionary
    res$end <- end
    res$go.back.count <- go.back.count
    
    # Add artificial pause for smoother feel
    if(!"general" %in% dictionary){
      Sys.sleep(1)
    }
    
    return(res)
    
  }else{
    
    # Single response
    while(ans == 30)
    {
      # Title for spell-check
      title <- paste(paste("\nTarget word: ", paste("'", colortext(check, defaults = "highlight"), "'", sep = ""), sep = ""))
      
      # Choices for spell-check
      choices <- c("SKIP", "ADD TO DICTIONARY", "TYPE MY OWN", "GOOGLE IT", "BAD WORD", possible)
      
      # Save answer
      customMenu(choices = choices, title = title, default = 5)
      
      # Message user
      message("Press 'B' to GO BACK, 'H' for HELP, or 'X' to EXIT.\n")
      
      # Present prompt
      ans <- readline(prompt = "Selection (accepts lowercase): ")
      
      # Check for user stoppage
      if(tolower(ans) == "x" || ans == "")
      {return("STOP")}
      
      # Check for appropriate answer
      ans <- appropriate.answer(answer = ans, choices = choices, default = 5)
      
      # Answer options
      if(ans == 1) # SKIP
      {
        ## Message user
        message(paste("\nResponse was SKIPPED:", paste("'", check, "'", sep = "")))
        
      }else if(ans == 2) # ADD TO DICTIONARY
      {
        ## Add response to dictionary
        full.dictionary <- SemNetDictionaries::append.dictionary(check,
                                                                 full.dictionary,
                                                                 dictionary.name = "full",
                                                                 save.location = "envir", textcleaner = TRUE)
        
      }else if(ans == 3) # TYPE MY OWN
      {
        message("\nType '30' (no quotations) to go back to the other response options\n")
        
        tmo <- readline(prompt = "Use commas for multiple words (dog, fish, etc.): ")
        
        if(tmo != "30")
        {
          ## Split responses (trim white spaces)
          tmo.split <- trimws(unlist(strsplit(tmo, split = ",")))
          
          ## Insert into change matrix
          change.mat <- t(as.matrix(c(check, tmo.split)))
          colnames(change.mat) <- c("from", paste("to", 1:(ncol(change.mat)-1), sep = "_"))
          
          ## Update changes
          changes <- change.format(change.list = changes,
                                   change.matrix = change.mat,
                                   current.index = current.index)
          
          ## Check if typed response is in dictionary
          if(!all(tmo.split %in% full.dictionary))
          {
            ### Identify responses not in dictionary
            non.dict <- which(!tmo.split %in% full.dictionary)
            
            ### Loop through non-dictionary responses
            for(i in 1:length(non.dict))
            {
              #customMenu(choices = c("Yes", "No"),
              #           title = paste("\n",
              #                         paste("'", tmo.split[non.dict[i]], "'", sep = ""),
              #                         " was not found in the dictionary. Should ",
              #                         paste("'", tmo.split[non.dict[i]], "'", sep = ""),
              #                         " be added to the dictionary?", sep = ""),
              #           default = 2,
              #           dictionary = TRUE)
              #  
              #dict.ans <- readline(prompt = "Selection: ")
              #  
              #dict.ans <- appropriate.answer(answer = dict.ans, choices = c("Yes", "No"), default = 2, dictionary = TRUE)
              
              message(paste("\n'", tmo.split[non.dict[i]], "'", sep = ""),
                      " was not found in the dictionary.\n")
              
              dict.ans <- yes.no.menu(
                paste("Should ",
                      paste("'", tmo.split[non.dict[i]], "'", sep = ""),
                      " be added to the dictionary", sep = "")
              )
              
              if(dict.ans == 1)
              {
                full.dictionary <- SemNetDictionaries::append.dictionary(tmo.split[non.dict[i]],
                                                                         full.dictionary,
                                                                         dictionary.name = "full",
                                                                         save.location = "envir", textcleaner = TRUE)
              }
            }
          }
          
          ## Message user
          message(paste("\nResponse was CHANGED TO:", paste("'", tmo.split, "'", sep = "", collapse = " ")))
          
          ## Change responses in context
          check <- tmo.split
          
        }else{ans <- 30}
        
      }else if(ans == 4) # GOOGLE IT
      {
        # Use 'searcher' package
        searcher::search_site(paste(category, " '", check, "'", sep = "", collpase = ""),
                              site = "google", rlang = FALSE)
        
        # Renew prompt
        ans <- 30
        
      }else if(ans == 5) # BAD RESPONSE
      {
        ## Set up change matrix
        change.mat <- t(as.matrix(c(check, NA)))
        colnames(change.mat) <- c("from", paste("to", 1:(ncol(change.mat)-1), sep = "_"))
        
        ## Update changes
        changes <- change.format(change.list = changes,
                                 change.matrix = change.mat,
                                 current.index = current.index)
        
        ## Message user
        message(paste("\nResponse was CHANGED TO:", paste(NA, sep = "", collapse = " ")))
        
        ## Change responses in context
        check <- "NA"
        
      }else if(ans == length(choices) + 1) # GO BACK
      {go.back <- TRUE
      }else if(ans == length(choices) + 2) # HELP
      {
        # Get `textcleaner` documentation
        textcleaner_help(check, context, original, possible)
        
        # Renew prompt
        ans <- 30
        
      }else{# RESPONSE OPTION 8-17
        
        ## Set up change matrix
        change.mat <- t(as.matrix(c(check, possible[ans-5])))
        colnames(change.mat) <- c("from", paste("to",1:(ncol(change.mat)-1), sep = "_"))
        
        ## Update changes
        changes <- change.format(change.list = changes,
                                 change.matrix = change.mat,
                                 current.index = current.index)
        
        ## Message user
        message(paste("\nResponse was CHANGED TO:", paste("'", possible[ans-5], "'", sep = "")))
        
        ## Change responses in context
        check <- possible[ans-5]
      }
    }
    
    # Initialize result list
    res <- list()
    res$go.back <- go.back
    res$target <- check
    res$changes <- changes
    res$full.dictionary <- full.dictionary
    res$go.back.count <- go.back.count
    
    # Add artificial pause for smoother feel
    if(!"general" %in% dictionary){
      Sys.sleep(1)
    }
    
    return(res)
  }
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### MANUAL SPELL-CHECK ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' Spell-check using \code{\link{SemNetDictionaries}}
#' 
#' @description A sub-routine function for spell-checking text dictionaries in \code{\link{SemNetDictionaries}}
#' (combines all spell-checking sub-routines)
#' 
#' @param uniq.resp Character vector.
#' A vector of unique responses from text data
#' 
#' @param dictionary Character vector.
#' See \code{\link{SemNetDictionaries}}
#' 
#' @param spelling Character vector.
#' English spelling to be used.
#' \itemize{
#' 
#' \item{\code{"UK"}}
#' {For British spelling (e.g., colour)}
#' 
#' \item{\code{"US"}}
#' {For American spelling (e.g., color)}
#' 
#' }
#' 
#' 
#' @param add.path Character.
#' Path to additional dictionaries to be found.
#' DOES NOT search recursively (through all folders in path)
#' to avoid time intensive search
#' 
#' @param data Matrix or data frame.
#' A dataset of text data.
#' Participant IDs will be automatically identified if they are included.
#' If no IDs are provided, then their order in the corresponding
#' row (or column is used). A message will notify the user how IDs were assigned
#' 
#' @param continue List.
#' A result previously unfinished that still needs to be completed.
#' Allows user to continue to manually spell-check their data
#' after they closed or errored out of \code{\link[SemNetCleaner]{spellcheck.dictionary}}.
#' Defaults to \code{NULL}
#' 
#' @param walkthrough Boolean.
#' Whether a walkthrough should be provided (recommended for first time users).
#' Defaults to \code{NULL}, which will ask whether you would like a walkthrough.
#' Set to \code{TRUE} to do the walkthrough.
#' Set to \code{FALSE} to skip the walkthrough
#' 
#' @return Returns a list containing:
#' 
#' \item{from}{A list of all unique responses before they were cleaned}
#' 
#' \item{to}{A list of all unique responses after they were spell-checked}
#' 
#' \item{manual}{Indices for responses that were manually spell-checked}
#' 
#' \item{auto}{Indices for responses that were automatically spell-checked}
#' 
#' \item{data}{Returned in case of continue so that \code{\link[SemNetCleaner]{textcleaner}}
#' can continue with its process}
#' 
#' \item{dictionary}{Only appears \strong{if} the user requests their dictionary be
#' returned in their results}
#' 
#' \item{stop}{\code{TRUE} or \code{FALSE} for whether the process was stopped}
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom utils setTxtProgressBar txtProgressBar
#' 
#' @import SemNetDictionaries
#' 
#' @noRd
# MANUAL spell-check
# Updated 04.01.2021
spellcheck.dictionary <- function (uniq.resp = NULL, dictionary = NULL, spelling = NULL,
                                   add.path = NULL, keepStrings = NULL,
                                   data = NULL, continue = NULL#, walkthrough = NULL
                                   )
{
  # Continuation check
  if(is.null(continue))
  {
    # Set category
    categories <- SemNetDictionaries::dictionaries(TRUE)
    
    # Determine whether dictionary is in pre-defined categories
    if(any(dictionary %in% categories))
    {
      ## Target dictionaries
      target <- dictionary[na.omit(match(categories, dictionary))]
      
      ## Determine appropriate category
      if("general" %in% target)
      {category <- "dictionary"
      }else if("fruits" %in% target && "vegetables" %in% target)
      {category <- "food"
      }else if("fruits" %in% target && !"vegetables" %in% target)
      {category <- "fruits"
      }else if(!"fruits" %in% target && "vegetables" %in% target)
      {category <- "vegetables"
      }else if("jobs" %in% target)
      {category <- "jobs"
      }else if("hot" %in% target || "good" %in% target)
      {category <- "synonym"
      }else if("animals" %in% target)
      {category <- "animals"
      }else{category <- "define"}
    }else{category <- "define"}
    
    # Load dictionaries
    ## Full dictionary
    full.dictionary <- SemNetDictionaries::load.dictionaries(dictionary, add.path = add.path)
    
    ## English conversion
    message(paste("\nConverting dictionary to '", spelling, "' spelling...", sep = ""), appendLF = FALSE)
    full.dictionary <- brit.us.conv.vector(full.dictionary, spelling = spelling, dictionary = TRUE)
    message("done")
    
    ## Save original dictionary (to compare against later)
    orig.dictionary <- full.dictionary
    
    # Initialize 'from' list
    from <- as.list(uniq.resp)
    ## English conversion
    #from <- brit.us.conv(from, spelling = spelling, dictionary = FALSE)
    # Initialize 'to' list for changes
    to <- from
    
    # Perform initial spell-check
    initial <- try(
      auto.spellcheck(check = from,
                      full.dict = full.dictionary,
                      dictionary = dictionary,
                      spelling = spelling,
                      keepStrings = keepStrings),
      silent = TRUE
    )
    
    # Error check
    if(any(class(initial) == "try-error"))
    {
      error.fun(initial, "auto.spellcheck", "textcleaner")
      res <- list()
      res$stop <- TRUE
    }
    
    # Add artificial pause for smoother feel
    Sys.sleep(2)
    
    # Indices of responses that need manual correction
    ind <- initial$manual
    
    # Current responses (after auto-correction phase)
    to <- initial$to
    
    # Create duplicate current responses (for GO BACK response option)
    initial.to <- to
    
    # Organize indices so that multiple word responses are first
    # and single word responses come after
    if(keepStrings){
      multi.ind <- which(lapply(to[ind], function(x){
        length(unlist(strsplit(x, split = " ")))
      }) >= 2)
    }else{
      multi.ind <- which(lapply(to[ind], length) >= 2)
    }
    
    single.ind <- setdiff(ind, as.numeric(names(multi.ind)))
    ind <- c(as.numeric(names(multi.ind)), single.ind)
    
    # Initialize changes list
    changes <- list()
    
    # Initialize main counter
    main.count <- 1
    
    # Initialize go back counter
    go.back.count <- 1
    
    # Initialize go back reset
    go.back.reset <- FALSE
    
  }else{
    
    # Return analysis to previous state
    type <- continue$type
    dictionary <- continue$dictionary
    full.dictionary <- continue$full.dictionary
    orig.dictionary <- continue$orig.dictionary
    spelling <- continue$spelling
    category <- continue$category
    from <- continue$from
    to <- continue$to
    initial <- continue$initial
    initial.to <- continue$initial.to
    ind <- continue$ind
    changes <- continue$changes
    main.count <- continue$main.count
    go.back.count <- continue$go.back.count
    go.back.reset <- continue$go.back.reset
    if(!is.null(continue$multi.count))
    {multi.count <- continue$multi.count}
    keepStrings <- continue$keepStrings
    data <- continue$data
    
    # Do not run through walkthrough
    #walkthrough <- FALSE
  }
  
  # Check if manual spell-check is necessary
  if(length(ind) != 0){
    
    ## Check for walkthrough
    walk_through(FALSE)
    
    ## Set up progress bar (Windows only)
    if(Sys.info()["sysname"] == "Windows")
    {
      pb <- tcltk::tkProgressBar(title = "R progress bar", label = "Spell-check progress",
                                 min = 0, max = length(ind), initial = 0, width = 300)
      invisible(tcltk::getTkProgressBar(pb))
    }else{pb <- txtProgressBar(min = 0, max = length(ind), style = 3)}
    
    linebreak()
  }
  
  # Loop through for manual spell-check
  while(main.count != (length(ind) + 1))
  {
    
    # Set up target index
    i <- ind[main.count]
    
    # Obtain target response(s)
    target <- to[[i]]
    
    # Keep strings?
    if(keepStrings){
      target <- unlist(strsplit(target, split = " "))
    }
    
    # Branch based on number of words
    if(length(target) > 1)
    {
      # Check punctuation
      target.punct <- gsub("[^[:alnum:][:space:]]", "", target)
      
      # Check which words are spelled incorrectly
      check.words <- target[which(!target.punct %in% full.dictionary)]
      
      # Initialize multi count
      if(is.null(continue$multi.count))
      {multi.count <- 1
      }else{continue$multi.count <- NULL}
      
      # Check if words have been checked already
      if(length(check.words) == 0){
        
        # Increase go back count
        result$go.back.count <- go.back.count + 1
        
        if(keepStrings){
          
          ## Message that response was cleared by previous decision
          message(paste("\nResponse '",
                        styletext(paste(target, collapse = " "), defaults = "bold"),
                        "' was KEPT AS IS based on a previous manual spell-check decision",
                        sep = ""))
          
        }else{
          
          ## Message that response was cleared by previous decision
          message(paste("\nResponse '",
                        styletext(target, defaults = "bold"),
                        "' was KEPT AS IS based on a previous manual spell-check decision",
                        sep = ""))
          
        }
        
        ## Update go back count
        if(result$go.back.count == go.back.count){
          
          ## If equal then check if reset is activated
          if(go.back.reset){ ## If activated, then reset
            go.back.count <- 1
          }else{ ## If not activated, then activate
            go.back.reset <- TRUE
          }
          
        }else{
          
          ## Update count and do not reset
          go.back.count <- result$go.back.count
          go.back.reset <- FALSE
          
        }
        
        ## Line break
        linebreak()
        
      }
      
      # Loop through words that need to be checked
      while(multi.count != (length(check.words) + 1)){
        
        ## Run spell-check menu (with error capturing)
        result <- try(
          spellcheck.menu(check = which(check.words[multi.count] == target),
                          context = target,
                          possible = best.guess(target[which(check.words[multi.count] == target)],
                                                full.dictionary = full.dictionary,
                                                dictionary = dictionary),
                          original = ifelse(keepStrings,
                                            paste(target, collapse = " "),
                                            from[[i]]),
                          current.index = i,
                          changes = changes,
                          full.dictionary = full.dictionary,
                          category = category,
                          dictionary = dictionary,
                          keepStrings = keepStrings,
                          go.back.count = go.back.count),
          silent = TRUE
        )
        
        linebreak()
        
        ## Check for user stoppage or error
        if("STOP" %in% result)
        {
          # Let user know their data is being saved
          message("\nUser stopped. Saving progress...\n")
          
          # Return the results
          res <- list()
          
          # Collect necessary objects
          res$type <- "fluency"
          res$dictionary <- dictionary
          res$full.dictionary <- full.dictionary
          res$orig.dictionary <- orig.dictionary
          res$spelling <- spelling
          res$category <- category
          res$from <- from
          res$to <- to
          res$initial <- initial
          res$initial.to <- initial.to
          res$ind <- ind
          res$changes <- changes
          res$main.count <- main.count
          res$go.back.count <- go.back.count
          res$go.back.reset <- go.back.reset
          if(exists("multi.count"))
          {res$multi.count <- multi.count}
          res$data <- data
          res$keepStrings <- keepStrings
          res$stop <- TRUE
          
          class(res) <- "textcleaner"
          
          # Close progress bar
          close(pb)
          
          return(res)
          
        }else if(any(class(result) == "try-error"))
        {
          # Give error
          error.fun(result, "spellcheck.menu", "textcleaner")
          
          # Return the results
          res <- list()
          
          # Collect necessary objects
          res$type <- "fluency"
          res$dictionary <- dictionary
          res$full.dictionary <- full.dictionary
          res$orig.dictionary <- orig.dictionary
          res$spelling <- spelling
          res$category <- category
          res$from <- from
          res$to <- to
          res$initial <- initial
          res$initial.to <- initial.to
          res$ind <- ind
          res$changes <- changes
          res$main.count <- main.count
          res$go.back.count <- go.back.count
          res$go.back.reset <- go.back.reset
          if(exists("multi.count"))
          {res$multi.count <- multi.count}
          res$keepStrings <- keepStrings
          res$data <- data
          res$stop <- TRUE
          
          class(res) <- "textcleaner"
          
          # Close progress bar
          close(pb)
          
          # Let user know their data is being saved
          message("\nSaving progress...\n")
          
          Sys.sleep(2)
          
          return(res)
        }
        
        ## Update go back count
        if(result$go.back.count == go.back.count){
          
          ## If equal then check if reset is activated
          if(go.back.reset){ ## If activated, then reset
            go.back.count <- 1
          }else{ ## If not activated, then activate
            go.back.reset <- TRUE
          }
          
        }else{
          
          ## Update count and do not reset
          go.back.count <- result$go.back.count
          go.back.reset <- FALSE
          
        }
        
        ## Change with GO BACK option
        if(result$go.back)
        {
          
          ## Check if it is the first response of multiple
          ## response check
          if(multi.count == 1)
          {
            ## Check if the main response is the first response
            if(main.count != 1)
            {
              ## Revert to current responses to original
              to[[i]] <- initial.to[[i]]
              
              ## Previous response
              prev.resp <- initial.to[[ind[main.count-go.back.count]]]
              
              ## String split
              prev.resp.split <- unlist(strsplit(prev.resp, split = " "))
              
              ## Revert dictionary
              if(any(!prev.resp.split %in% full.dictionary == prev.resp.split %in% orig.dictionary)){
                
                ## Targets to remove from full dictionary
                target.rms <- which(!prev.resp.split %in% full.dictionary == prev.resp.split %in% orig.dictionary)
                
                ## Indices to remove
                ind.rms <- match(prev.resp.split[target.rms], full.dictionary)
                
                ## Update full dictionary
                full.dictionary <- full.dictionary[-ind.rms]
              }
              
              ## Revert to previous responses to original
              to[[ind[main.count-go.back.count]]] <- prev.resp
              
              ## Revert changes
              changes <- changes[-which(names(changes) == paste(ind[main.count-go.back.count]))]
              
              ## Update multiple response count
              main.count <- main.count - (go.back.count + 1)
              
              ## Reset go back count
              go.back.count <- 1
              
              break
            }else{
              
              ## Let user know they cannot go back any further
              message("\nThis is the first response. 'GO BACK' is not available.")
              
              ## Line break
              linebreak()
              
              # Add artificial pause for smoother feel
              Sys.sleep(0.5)
              
            }
          }else{
            
            ## Revert to previous response state
            target <- prev.target
            
            ## Revert full dictionary
            if(any(prev.target %in% full.dictionary & !prev.target %in% orig.dictionary))
            {
              ## Targets to remove from full dictionary
              target.rms <- which(prev.target %in% full.dictionary & !prev.target %in% orig.dictionary)
              
              ## Indices to remove
              ind.rms <- match(prev.target[target.rms], full.dictionary)
              
              ## Update full dictionary
              full.dictionary <- full.dictionary[-ind.rms]
            }
            
            ## Revert previous changes
            changes <- changes[[which(names(changes) == paste(ind[main.count]))]][-(multi.count - 1),]
            
            ## Update multiple response count
            multi.count <- multi.count - 1
          }
        }else{
          
          ## Save previous target (GO BACK response option)
          prev.target <- target
          
          ## Update changes
          target <- result$target
          changes <- result$changes
          full.dictionary <- result$full.dictionary
          
          ## Check for BAD STRING
          if(all(target == "NA")){
            result$end <- TRUE
          }
          
          ## Increase multiple response count
          if(result$end){
            multi.count <- length(check.words) + 1
          }else{multi.count <- multi.count + 1}
        }
        
      }
      
      ## Check for keeping strings
      if(keepStrings){
        target <- paste(target, collapse = " ")
      }
      
      ## Update lists
      to[[i]] <- target
      
    }else{
      
      # Single response
      ## Run spell-check menu (with error capturing)
      result <- try(
        spellcheck.menu(check = target,
                        context = NULL,
                        possible = best.guess(target, full.dictionary = full.dictionary, dictionary),
                        original = from[[i]],
                        current.index = i,
                        changes = changes,
                        full.dictionary = full.dictionary,
                        category = category,
                        dictionary = dictionary,
                        keepStrings = keepStrings,
                        go.back.count = go.back.count),
        silent = TRUE
        
      )
      
      linebreak()
      
      ## Check for user stoppage or error
      if("STOP" %in% result)
      {
        # Let user know their data is being saved
        message("\nUser stopped. Saving progress...\n")
        
        # Return the results
        res <- list()
        
        # Collect necessary objects
        res$type <- "fluency"
        res$dictionary <- dictionary
        res$full.dictionary <- full.dictionary
        res$orig.dictionary <- orig.dictionary
        res$spelling <- spelling
        res$category <- category
        res$from <- from
        res$to <- to
        res$initial <- initial
        res$initial.to <- initial.to
        res$ind <- ind
        res$changes <- changes
        res$main.count <- main.count
        res$go.back.count <- go.back.count
        res$go.back.reset <- go.back.reset
        if(exists("multi.count"))
        {res$multi.count <- multi.count}
        res$keepStrings <- keepStrings
        res$data <- data
        res$stop <- TRUE
        
        class(res) <- "textcleaner"
        
        # Close progress bar
        close(pb)
        
        Sys.sleep(2)
        
        return(res)
        
      }else if(any(class(result) == "try-error"))
      {
        # Give error
        error.fun(result, "spellcheck.menu", "textcleaner")
        
        # Return the results
        res <- list()
        
        # Collect necessary objects
        res$type <- "fluency"
        res$dictionary <- dictionary
        res$full.dictionary <- full.dictionary
        res$orig.dictionary <- orig.dictionary
        res$spelling <- spelling
        res$category <- category
        res$from <- from
        res$to <- to
        res$initial <- initial
        res$initial.to <- initial.to
        res$ind <- ind
        res$changes <- changes
        res$main.count <- main.count
        res$go.back.count <- go.back.count
        res$go.back.reset <- go.back.reset
        if(exists("multi.count"))
        {res$multi.count <- multi.count}
        res$keepStrings <- keepStrings
        res$data <- data
        res$stop <- TRUE
        res$target <- target
        
        class(res) <- "textcleaner"
        
        # Close progress bar
        close(pb)
        
        # Let user know their data is being saved
        message("\nSaving progress...\n")
        
        Sys.sleep(2)
        
        return(res)
      }
      
      ### Update go back count
      if(result$go.back.count == go.back.count){
        
        ## If equal then check if reset is activated
        if(go.back.reset){ ## If activated, then reset
          go.back.count <- 1
        }else{ ## If not activated, then activate
          go.back.reset <- TRUE
        }
        
      }else{
        
        ## Update count and do not reset
        go.back.count <- result$go.back.count
        go.back.reset <- FALSE
        
      }
      
      ## Change with GO BACK option
      if(result$go.back)
      {
        ## Check if the main response is the first response
        if(main.count != 1)
        {
          ## Revert to current responses to original
          to[[i]] <- initial.to[[i]]
          
          ## Previous response
          prev.resp <- initial.to[[ind[main.count-go.back.count]]]
          
          ## String split
          prev.resp.split <- unlist(strsplit(prev.resp, split = " "))
          
          ## Revert dictionary
          if(any(!prev.resp.split %in% full.dictionary == prev.resp.split %in% orig.dictionary)){
            
            ## Targets to remove from full dictionary
            target.rms <- which(!prev.resp.split %in% full.dictionary == prev.resp.split %in% orig.dictionary)
            
            ## Indices to remove
            ind.rms <- match(prev.resp.split[target.rms], full.dictionary)
            
            ## Update full dictionary
            full.dictionary <- full.dictionary[-ind.rms]
          }
          
          ## Revert to previous responses to original
          to[[ind[main.count-go.back.count]]] <- prev.resp
          
          ## Revert changes
          changes <- changes[-which(names(changes) == paste(ind[main.count-go.back.count]))]
          
          ## Revert dictionary
          #if(any(!initial.to[[ind[main.count-go.back.count]]] %in% full.dictionary == initial.to[[ind[main.count-go.back.count]]] %in% orig.dictionary))
          #{
          #  ## Targets to remove from full dictionary
          #  target.rms <- which(!initial.to[[ind[main.count-go.back.count]]] %in% full.dictionary == initial.to[[ind[main.count-go.back.count]]] %in% orig.dictionary)
          #  
          #  ## Indices to remove
          #  ind.rms <- match(initial.to[[ind[main.count-go.back.count]]][target.rms], full.dictionary)
          #  
          #  ## Update full dictionary
          #  full.dictionary <- full.dictionary[-ind.rms]
          #}
          
          ## Revert to previous responses to original
          #to[[ind[main.count-go.back.count]]] <- initial.to[[ind[main.count-go.back.count]]]
          
          ## Revert changes
          #changes <- changes[-which(names(changes) == paste(ind[main.count-go.back.count]))]
          
          ## Update multiple response count
          main.count <- main.count - (go.back.count + 1)
          
          ## Reset go back count
          go.back.count <- 1
          
        }else{
          
          ## Let user know they cannot go back any further
          message("This is the first response. 'GO BACK' is not available.")
          
          ## Line break
          linebreak()
          
          # Add artificial pause for smoother feel
          Sys.sleep(0.5)
          
        }
        
      }else{
        ## Update lists
        to[[i]] <- result$target
        changes <- result$changes
        full.dictionary <- result$full.dictionary
      }
    }
    
    # Update progress bar
    if(Sys.info()["sysname"] == "Windows")
    {
      percent <- floor((main.count/length(ind))*100)
      info <- suppressWarnings(sprintf(paste(main.count, "of", length(ind), "responses done"), percent))
      tcltk::setTkProgressBar(pb, main.count, sprintf("Spell-check Progress (%s)", info), info)
      
    }else{
      
      # Add spacing
      cat("\n")
      
      # Increase progress bar
      setTxtProgressBar(pb, main.count)
      
      # Add artificial pause for smoother feel
      Sys.sleep(0.10)
      
      # Add spacing
      cat("\n")
    }
    
    # Increase main count
    main.count <- main.count + 1
    
  }
  
  # Close progress bar
  if(main.count != 1){
    close(pb)
  }
  
  # Initialize final results
  final.res <- list()
  
  # Update dictionary
  if(length(orig.dictionary) != length(full.dictionary))
  {
    # Places to save
    ## Choices
    choices <- c("In my results",
                 "In my working directory",
                 "I'd like to choose the directory",
                 "Don't save it")
    
    ## Title
    title <- "\nWhere would you like to save your additional dictionary entries?"
    
    ## Menu
    customMenu(choices = choices, title = title, default = 4, dictionary = TRUE)
    
    ## User response
    ans <- readline(prompt = "Selection: ")
    
    ## Make sure it's an appropriate response
    ans <- appropriate.answer(answer = ans, choices = choices, default = 4, dictionary = TRUE)
    
    ## Branch based on answer
    if(ans == 1) # In my results
    {
      ## Save to results
      final.res$dictionary <- full.dictionary
      
      ## Let user know
      message("\nDictionary saved to your output object under '$dictionary'")
      
    }else if(ans == 2) # In my working directory
    {
      ## Obtain name
      dictionary.name <- readline(prompt = "Name for the dictionary: ")
      
      ## Check if user supplied *.dictionary
      dictionary.name <- gsub(".dictionary", "", dictionary.name)
      
      ## Save dictionary
      SemNetDictionaries::append.dictionary(full.dictionary, dictionary.name = dictionary.name,
                                            save.location = "wd", textcleaner = TRUE)
      
      ## Let user know
      message(paste("\nDictionary saved to your working directory: '", getwd(), "'", sep = ""))
      
    }else if(ans == 3) # I'd like to choose the directory
    {
      # Obtain name
      dictionary.name <- readline(prompt = "Name for the dictionary: ")
      
      # Check if user supplied *.dictionary
      dictionary.name <- gsub(".dictionary", "", dictionary.name)
      
      # Save dictionary
      SemNetDictionaries::append.dictionary(full.dictionary, dictionary.name = dictionary.name,
                                            save.location = "choose", textcleaner = TRUE)
      
    }else{message("\nDictionary was not saved")}
  }
  
  # Run ad hoc check (if necessary)
  if(main.count != 1){
    
    ## Ad hoc check for monikers
    if(any(dictionary %in% SemNetDictionaries::dictionaries(TRUE)[-which(SemNetDictionaries::dictionaries(TRUE) == "general")]))
    {
      ### Let user know
      message("\nRunning ad hoc check for common misspellings and monikers...", appendLF = FALSE)
      
      ### Target dictionaries
      target <- dictionary[na.omit(match(SemNetDictionaries::dictionaries(TRUE), dictionary))]
      
      ### Check for monikers
      for(i in 1:length(to))
        for(j in 1:length(to[[i]])){
          to[[i]][j] <- unlist(moniker(to[[i]][j], SemNetDictionaries::load.monikers(target), spelling = spelling))
        }
      
      ### Let user know
      message("done")
    }
    
  }
  
  # Collect results
  # (initialized before asking to save dictionary)
  final.res$from <- from
  final.res$to <- to
  final.res$manual <- initial$manual
  final.res$auto <- initial$auto
  final.res$data <- data
  final.res$type <- "fluency"
  final.res$stop <- FALSE
  
  class(final.res) <- "textcleaner"
  
  return(final.res)
}

#' Spell-check using \code{\link{SemNetDictionaries}}
#' 
#' @description A sub-routine function for spell-checking text dictionaries in \code{\link{SemNetDictionaries}}
#' (combines all spell-checking sub-routines)
#' 
#' @param uniq.resp Character vector.
#' A vector of unique responses from text data
#' 
#' @param dictionary Character vector.
#' See \code{\link{SemNetDictionaries}}
#' 
#' @param spelling Character vector.
#' English spelling to be used.
#' \itemize{
#' 
#' \item{\code{"UK"}}
#' {For British spelling (e.g., colour)}
#' 
#' \item{\code{"US"}}
#' {For American spelling (e.g., color)}
#' 
#' }
#' 
#' 
#' @param add.path Character.
#' Path to additional dictionaries to be found.
#' DOES NOT search recursively (through all folders in path)
#' to avoid time intensive search
#' 
#' @param data Matrix or data frame.
#' A dataset of text data.
#' Participant IDs will be automatically identified if they are included.
#' If no IDs are provided, then their order in the corresponding
#' row (or column is used). A message will notify the user how IDs were assigned
#' 
#' @param continue List.
#' A result previously unfinished that still needs to be completed.
#' Allows user to continue to manually spell-check their data
#' after they closed or errored out of \code{\link[SemNetCleaner]{spellcheck.dictionary}}.
#' Defaults to \code{NULL}
#' 
#' @param walkthrough Boolean.
#' Whether a walkthrough should be provided (recommended for first time users).
#' Defaults to \code{NULL}, which will ask whether you would like a walkthrough.
#' Set to \code{TRUE} to do the walkthrough.
#' Set to \code{FALSE} to skip the walkthrough
#' 
#' @return Returns a list containing:
#' 
#' \item{from}{A list of all unique responses before they were cleaned}
#' 
#' \item{to}{A list of all unique responses after they were spell-checked}
#' 
#' \item{manual}{Indices for responses that were manually spell-checked}
#' 
#' \item{auto}{Indices for responses that were automatically spell-checked}
#' 
#' \item{data}{Returned in case of continue so that \code{\link[SemNetCleaner]{textcleaner}}
#' can continue with its process}
#' 
#' \item{dictionary}{Only appears \strong{if} the user requests their dictionary be
#' returned in their results}
#' 
#' \item{stop}{\code{TRUE} or \code{FALSE} for whether the process was stopped}
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom utils setTxtProgressBar txtProgressBar
#' 
#' @import SemNetDictionaries
#' 
#' @noRd
# MANUAL spell-check
# Updated 04.01.2021
spellcheck.dictionary.free <- function (
  uniq.resp = NULL, dictionary = "cocaspell", spelling = NULL,
  add.path = NULL, keepStrings = NULL,
  data = NULL, continue = NULL
)
{
  # Continuation check
  if(is.null(continue)){
    
    # Initialize 'from' list
    from <- as.list(uniq.resp)
    # Change names of indices
    names(from) <- formatC(
      1:length(from),
      width = nchar(as.character(length(from))),
      format = "d", flag = "0"
    )
    ## English conversion
    #from <- brit.us.conv(from, spelling = spelling, dictionary = FALSE)
    # Initialize 'to' list for changes
    to <- from
    
    # Load dictionaries
    ## Full dictionary
    full.dictionary <- SemNetDictionaries::load.dictionaries("cocaspell")
    category <- "define"
    
    ## English conversion
    message(paste("\nConverting dictionary to '", spelling, "' spelling...", sep = ""), appendLF = FALSE)
    full.dictionary <- brit.us.conv.vector(full.dictionary, spelling = spelling, dictionary = TRUE)
    message("done")
    
    ## Save original dictionary (to compare against later)
    orig.dictionary <- full.dictionary
    
    # Perform initial spell-check
    initial <- try(
      auto.spellcheck.free(
        check = from,
        full.dict = full.dictionary,
        dictionary = dictionary,
        spelling = spelling,
        keepStrings = keepStrings
      ),
      silent = TRUE
    )
    
    # Error check
    if(any(class(initial) == "try-error")){
      error.fun(initial, "auto.spellcheck", "textcleaner")
      res <- list()
      res$stop <- TRUE
    }
    
    # Current responses (after auto-correction phase)
    ind <- initial$manual
    auto.ind <- initial$auto
    to <- initial$to
    
    # Create duplicate current responses (for GO BACK response option)
    initial.to <- to
    
    # Organize indices so that multiple word responses are first
    # and single word responses come after
    if(keepStrings){
      multi.ind <- which(lapply(to[ind], function(x){
        length(unlist(strsplit(x, split = " ")))
      }) >= 2)
    }else{
      multi.ind <- which(lapply(to[ind], length) >= 2)
    }
    
    single.ind <- setdiff(ind, as.numeric(names(multi.ind)))
    ind <- c(as.numeric(names(multi.ind)), single.ind)
    
    # Initialize changes list
    changes <- list()
    
    # Initialize main counter
    main.count <- 1
    
    # Initialize go back counter
    go.back.count <- 1
    
    # Initialize go back reset
    go.back.reset <- FALSE
    
  }else{
    
    # Return analysis to previous state
    type <- continue$type
    dictionary <- continue$dictionary
    full.dictionary <- continue$full.dictionary
    orig.dictionary <- continue$orig.dictionary
    spelling <- continue$spelling
    category <- continue$category
    from <- continue$from
    to <- continue$to
    initial <- continue$initial
    initial.to <- continue$initial.to
    ind <- continue$ind
    auto.ind <- continue$auto.ind
    changes <- continue$changes
    main.count <- continue$main.count
    go.back.count <- continue$go.back.count
    go.back.reset <- continue$go.back.reset
    if(!is.null(continue$multi.count))
    {multi.count <- continue$multi.count}
    keepStrings <- continue$keepStrings
    data <- continue$data
    
    # Do not run through walkthrough
    #walkthrough <- FALSE
  }
  
  # Check if manual spell-check is necessary
  if(length(ind) != 0){
    
    ## Check for walkthrough
    walk_through(FALSE)
    
    ## Set up progress bar (Windows only)
    if(Sys.info()["sysname"] == "Windows")
    {
      pb <- tcltk::tkProgressBar(title = "R progress bar", label = "Spell-check progress",
                                 min = 0, max = length(ind), initial = 0, width = 300)
      invisible(tcltk::getTkProgressBar(pb))
    }else{pb <- txtProgressBar(min = 0, max = length(ind), style = 3)}
    
    linebreak()
  }
  
  # Loop through for manual spell-check
  while(main.count != (length(ind) + 1)){
    
    # Set up target index
    i <- ind[main.count]
    
    # Obtain target response(s)
    target <- to[[i]]
    
    # Keep strings?
    if(keepStrings){
      target <- unlist(strsplit(target, split = " "))
      target <- na.omit(ifelse(target == "", NA, target))
    }
    
    # Branch based on number of words
    if(length(target) > 1){
      
      # Check punctuation
      target.punct <- gsub("[^[:alnum:][:space:]]", "", target)
      
      # Check which words are spelled incorrectly
      check.words <- target[which(!target.punct %in% full.dictionary)]
      
      # Initialize multi count
      if(is.null(continue$multi.count)){
        multi.count <- 1
      }else{
        continue$multi.count <- NULL
      }
      
      # Check if words have been checked already
      if(length(check.words) == 0){
        
        # Increase go back count
        result$go.back.count <- go.back.count + 1
        
        if(keepStrings){
          
          ## Message that response was cleared by previous decision
          message(paste("\nResponse '",
                        styletext(paste(target, collapse = " "), defaults = "bold"),
                        "' was KEPT AS IS based on a previous manual spell-check decision",
                        sep = ""))
          
        }else{
          
          ## Message that response was cleared by previous decision
          message(paste("\nResponse '",
                        styletext(target, defaults = "bold"),
                        "' was KEPT AS IS based on a previous manual spell-check decision",
                        sep = ""))
          
        }
        
        ## Update go back count
        if(result$go.back.count == go.back.count){
          
          ## If equal then check if reset is activated
          if(go.back.reset){ ## If activated, then reset
            go.back.count <- 1
          }else{ ## If not activated, then activate
            go.back.reset <- TRUE
          }
          
        }else{
          
          ## Update count and do not reset
          go.back.count <- result$go.back.count
          go.back.reset <- FALSE
          
        }
        
        ## Line break
        linebreak()
        
      }
      
      # Loop through words that need to be checked
      while(multi.count != (length(check.words) + 1)){
        
        ## Run spell-check menu (with error capturing)
        result <- try(
          spellcheck.menu(check = which(check.words[multi.count] == target),
                          context = target,
                          possible = best.guess(target[which(check.words[multi.count] == target)],
                                                full.dictionary = full.dictionary,
                                                dictionary = dictionary),
                          original = ifelse(keepStrings,
                                            paste(target, collapse = " "),
                                            from[[i]]),
                          current.index = i,
                          changes = changes,
                          full.dictionary = full.dictionary,
                          category = category,
                          dictionary = dictionary,
                          keepStrings = keepStrings,
                          go.back.count = go.back.count),
          silent = TRUE
        )
        
        linebreak()
        
        ## Check for user stoppage or error
        if("STOP" %in% result){
          
          # Let user know their data is being saved
          message("\nUser stopped. Saving progress...\n")
          
          # Return the results
          res <- list()
          
          # Collect necessary objects
          res$type <- "free"
          res$dictionary <- dictionary
          res$full.dictionary <- full.dictionary
          res$orig.dictionary <- orig.dictionary
          res$spelling <- spelling
          res$category <- category
          res$from <- from
          res$to <- to
          res$initial <- initial
          res$initial.to <- initial.to
          res$ind <- ind
          res$auto.ind <- auto.ind
          res$changes <- changes
          res$main.count <- main.count
          res$go.back.count <- go.back.count
          res$go.back.reset <- go.back.reset
          if(exists("multi.count"))
          {res$multi.count <- multi.count}
          res$data <- data
          res$keepStrings <- keepStrings
          res$stop <- TRUE
          
          class(res) <- "textcleaner"
          
          # Close progress bar
          close(pb)
          
          Sys.sleep(2)
          
          return(res)
          
        }else if(any(class(result) == "try-error")){
          
          # Give error
          error.fun(result, "spellcheck.menu", "textcleaner")
          
          # Return the results
          res <- list()
          
          # Collect necessary objects
          res$type <- "free"
          res$dictionary <- dictionary
          res$full.dictionary <- full.dictionary
          res$orig.dictionary <- orig.dictionary
          res$spelling <- spelling
          res$category <- category
          res$from <- from
          res$to <- to
          res$initial <- initial
          res$initial.to <- initial.to
          res$ind <- ind
          res$auto.ind <- auto.ind
          res$changes <- changes
          res$main.count <- main.count
          res$go.back.count <- go.back.count
          res$go.back.reset <- go.back.reset
          if(exists("multi.count"))
          {res$multi.count <- multi.count}
          res$keepStrings <- keepStrings
          res$data <- data
          res$stop <- TRUE
          
          class(res) <- "textcleaner"
          
          # Close progress bar
          close(pb)
          
          # Let user know their data is being saved
          message("\nSaving progress...\n")
          
          Sys.sleep(2)
          
          return(res)
        }
        
        ## Update go back count
        if(result$go.back.count == go.back.count){
          
          ## If equal then check if reset is activated
          if(go.back.reset){ ## If activated, then reset
            go.back.count <- 1
          }else{ ## If not activated, then activate
            go.back.reset <- TRUE
          }
          
        }else{
          
          ## Update count and do not reset
          go.back.count <- result$go.back.count
          go.back.reset <- FALSE
          
        }
        
        ## Change with GO BACK option
        if(result$go.back)
        {
          
          ## Check if it is the first response of multiple
          ## response check
          if(multi.count == 1)
          {
            ## Check if the main response is the first response
            if(main.count != 1)
            {
              ## Revert to current responses to original
              to[[i]] <- initial.to[[i]]
              
              ## Previous response
              prev.resp <- initial.to[[ind[main.count-go.back.count]]]
              
              ## String split
              prev.resp.split <- unlist(strsplit(prev.resp, split = " "))
              
              ## Revert dictionary
              if(any(!prev.resp.split %in% full.dictionary == prev.resp.split %in% orig.dictionary)){
                
                ## Targets to remove from full dictionary
                target.rms <- which(!prev.resp.split %in% full.dictionary == prev.resp.split %in% orig.dictionary)
                
                ## Indices to remove
                ind.rms <- match(prev.resp.split[target.rms], full.dictionary)
                
                ## Update full dictionary
                full.dictionary <- full.dictionary[-ind.rms]
              }
              
              ## Revert to previous responses to original
              to[[ind[main.count-go.back.count]]] <- prev.resp
              
              ## Revert changes
              changes <- changes[-which(names(changes) == paste(ind[main.count-go.back.count]))]
              
              ## Update multiple response count
              main.count <- main.count - (go.back.count + 1)
              
              ## Reset go back count
              go.back.count <- 1
              
              break
            }else{
              
              ## Let user know they cannot go back any further
              message("\nThis is the first response. 'GO BACK' is not available.")
              
              ## Line break
              linebreak()
              
              # Add artificial pause for smoother feel
              Sys.sleep(0.5)
              
            }
          }else{
            
            ## Revert to previous response state
            target <- prev.target
            
            ## Revert full dictionary
            if(any(prev.target %in% full.dictionary & !prev.target %in% orig.dictionary))
            {
              ## Targets to remove from full dictionary
              target.rms <- which(prev.target %in% full.dictionary & !prev.target %in% orig.dictionary)
              
              ## Indices to remove
              ind.rms <- match(prev.target[target.rms], full.dictionary)
              
              ## Update full dictionary
              full.dictionary <- full.dictionary[-ind.rms]
            }
            
            ## Revert previous changes
            changes <- changes[[which(names(changes) == paste(ind[main.count]))]][-(multi.count - 1),]
            
            ## Update multiple response count
            multi.count <- multi.count - 1
          }
        }else{
          
          ## Save previous target (GO BACK response option)
          prev.target <- target
          
          ## Update changes
          target <- result$target
          changes <- result$changes
          full.dictionary <- result$full.dictionary
          
          ## Check for BAD STRING
          if(all(target == "NA")){
            result$end <- TRUE
          }
          
          ## Increase multiple response count
          if(result$end){
            multi.count <- length(check.words) + 1
          }else{multi.count <- multi.count + 1}
        }
        
      }
      
      ## Check for keeping strings
      if(keepStrings){
        target <- paste(target, collapse = " ")
      }
      
      ## Update lists
      to[[i]] <- target
      
    }else{
      
      # Single response
      ## Run spell-check menu (with error capturing)
      result <- try(
        spellcheck.menu(check = target,
                        context = NULL,
                        possible = best.guess(target, full.dictionary = full.dictionary, dictionary),
                        original = from[[i]],
                        current.index = i,
                        changes = changes,
                        full.dictionary = full.dictionary,
                        category = category,
                        dictionary = dictionary,
                        keepStrings = keepStrings,
                        go.back.count = go.back.count),
        silent = TRUE
        
      )
      
      linebreak()
      
      ## Check for user stoppage or error
      if("STOP" %in% result)
      {
        # Let user know their data is being saved
        message("\nUser stopped. Saving progress...\n")
        
        # Return the results
        res <- list()
        
        # Collect necessary objects
        res$type <- "free"
        res$dictionary <- dictionary
        res$full.dictionary <- full.dictionary
        res$orig.dictionary <- orig.dictionary
        res$spelling <- spelling
        res$category <- category
        res$from <- from
        res$to <- to
        res$initial <- initial
        res$initial.to <- initial.to
        res$ind <- ind
        res$auto.ind <- auto.ind
        res$changes <- changes
        res$main.count <- main.count
        res$go.back.count <- go.back.count
        res$go.back.reset <- go.back.reset
        if(exists("multi.count"))
        {res$multi.count <- multi.count}
        res$keepStrings <- keepStrings
        res$data <- data
        res$stop <- TRUE
        
        class(res) <- "textcleaner"
        
        # Close progress bar
        close(pb)
        
        Sys.sleep(2)
        
        return(res)
        
      }else if(any(class(result) == "try-error"))
      {
        # Give error
        error.fun(result, "spellcheck.menu", "textcleaner")
        
        # Return the results
        res <- list()
        
        # Collect necessary objects
        res$type <- "free"
        res$dictionary <- dictionary
        res$full.dictionary <- full.dictionary
        res$orig.dictionary <- orig.dictionary
        res$spelling <- spelling
        res$category <- category
        res$from <- from
        res$to <- to
        res$initial <- initial
        res$initial.to <- initial.to
        res$ind <- ind
        res$auto.ind <- auto.ind
        res$changes <- changes
        res$main.count <- main.count
        res$go.back.count <- go.back.count
        res$go.back.reset <- go.back.reset
        if(exists("multi.count"))
        {res$multi.count <- multi.count}
        res$keepStrings <- keepStrings
        res$data <- data
        res$stop <- TRUE
        res$target <- target
        
        class(res) <- "textcleaner"
        
        # Close progress bar
        close(pb)
        
        # Let user know their data is being saved
        message("\nSaving progress...\n")
        
        Sys.sleep(2)
        
        return(res)
      }
      
      ### Update go back count
      if(result$go.back.count == go.back.count){
        
        ## If equal then check if reset is activated
        if(go.back.reset){ ## If activated, then reset
          go.back.count <- 1
        }else{ ## If not activated, then activate
          go.back.reset <- TRUE
        }
        
      }else{
        
        ## Update count and do not reset
        go.back.count <- result$go.back.count
        go.back.reset <- FALSE
        
      }
      
      ## Change with GO BACK option
      if(result$go.back)
      {
        ## Check if the main response is the first response
        if(main.count != 1)
        {
          ## Revert to current responses to original
          to[[i]] <- initial.to[[i]]
          
          ## Previous response
          prev.resp <- initial.to[[ind[main.count-go.back.count]]]
          
          ## String split
          prev.resp.split <- unlist(strsplit(prev.resp, split = " "))
          
          ## Revert dictionary
          if(any(!prev.resp.split %in% full.dictionary == prev.resp.split %in% orig.dictionary)){
            
            ## Targets to remove from full dictionary
            target.rms <- which(!prev.resp.split %in% full.dictionary == prev.resp.split %in% orig.dictionary)
            
            ## Indices to remove
            ind.rms <- match(prev.resp.split[target.rms], full.dictionary)
            
            ## Update full dictionary
            full.dictionary <- full.dictionary[-ind.rms]
          }
          
          ## Revert to previous responses to original
          to[[ind[main.count-go.back.count]]] <- prev.resp
          
          ## Revert changes
          changes <- changes[-which(names(changes) == paste(ind[main.count-go.back.count]))]
          
          ## Revert dictionary
          #if(any(!initial.to[[ind[main.count-go.back.count]]] %in% full.dictionary == initial.to[[ind[main.count-go.back.count]]] %in% orig.dictionary))
          #{
          #  ## Targets to remove from full dictionary
          #  target.rms <- which(!initial.to[[ind[main.count-go.back.count]]] %in% full.dictionary == initial.to[[ind[main.count-go.back.count]]] %in% orig.dictionary)
          #  
          #  ## Indices to remove
          #  ind.rms <- match(initial.to[[ind[main.count-go.back.count]]][target.rms], full.dictionary)
          #  
          #  ## Update full dictionary
          #  full.dictionary <- full.dictionary[-ind.rms]
          #}
          
          ## Revert to previous responses to original
          #to[[ind[main.count-go.back.count]]] <- initial.to[[ind[main.count-go.back.count]]]
          
          ## Revert changes
          #changes <- changes[-which(names(changes) == paste(ind[main.count-go.back.count]))]
          
          ## Update multiple response count
          main.count <- main.count - (go.back.count + 1)
          
          ## Reset go back count
          go.back.count <- 1
          
        }else{
          
          ## Let user know they cannot go back any further
          message("This is the first response. 'GO BACK' is not available.")
          
          ## Line break
          linebreak()
          
          # Add artificial pause for smoother feel
          Sys.sleep(0.5)
          
        }
        
      }else{
        ## Update lists
        to[[i]] <- result$target
        changes <- result$changes
        full.dictionary <- result$full.dictionary
      }
    }
    
    # Update progress bar
    if(Sys.info()["sysname"] == "Windows"){
      
      percent <- floor((main.count/length(ind))*100)
      info <- suppressWarnings(sprintf(paste(main.count, "of", length(ind), "responses done"), percent))
      tcltk::setTkProgressBar(pb, main.count, sprintf("Spell-check Progress (%s)", info), info)
      
    }else{
      
      # Add spacing
      cat("\n")
      
      # Increase progress bar
      setTxtProgressBar(pb, main.count)
      
      # Add artificial pause for smoother feel
      Sys.sleep(0.10)
      
      # Add spacing
      cat("\n")
    }
    
    # Increase main count
    main.count <- main.count + 1
    
  }
  
  # Close progress bar
  if(main.count != 1){
    close(pb)
  }
  
  # Initialize final results
  final.res <- list()
  
  # Update dictionary
  if(length(orig.dictionary) != length(full.dictionary)){
    
    # Places to save
    ## Choices
    choices <- c("In my results",
                 "In my working directory",
                 "I'd like to choose the directory",
                 "Don't save it")
    
    ## Title
    title <- "\nWhere would you like to save your additional dictionary entries?"
    
    ## Menu
    customMenu(choices = choices, title = title, default = 4, dictionary = TRUE)
    
    ## User response
    ans <- readline(prompt = "Selection: ")
    
    ## Make sure it's an appropriate response
    ans <- appropriate.answer(answer = ans, choices = choices, default = 4, dictionary = TRUE)
    
    ## Branch based on answer
    if(ans == 1) # In my results
    {
      ## Save to results
      final.res$dictionary <- full.dictionary
      
      ## Let user know
      message("\nDictionary saved to your output object under '$dictionary'")
      
    }else if(ans == 2) # In my working directory
    {
      ## Obtain name
      dictionary.name <- readline(prompt = "Name for the dictionary: ")
      
      ## Check if user supplied *.dictionary
      dictionary.name <- gsub(".dictionary", "", dictionary.name)
      
      ## Save dictionary
      SemNetDictionaries::append.dictionary(full.dictionary, dictionary.name = dictionary.name,
                                            save.location = "wd", textcleaner = TRUE)
      
      ## Let user know
      message(paste("\nDictionary saved to your working directory: '", getwd(), "'", sep = ""))
      
    }else if(ans == 3) # I'd like to choose the directory
    {
      # Obtain name
      dictionary.name <- readline(prompt = "Name for the dictionary: ")
      
      # Check if user supplied *.dictionary
      dictionary.name <- gsub(".dictionary", "", dictionary.name)
      
      # Save dictionary
      SemNetDictionaries::append.dictionary(full.dictionary, dictionary.name = dictionary.name,
                                            save.location = "choose", textcleaner = TRUE)
      
    }else{message("\nDictionary was not saved")}
  }
  
  # Run ad hoc check (if necessary)
  if(main.count != 1){
    
    ## Ad hoc check for monikers
    if(any(dictionary %in% SemNetDictionaries::dictionaries(TRUE)[-which(SemNetDictionaries::dictionaries(TRUE) == "general")])){
      
      ### Let user know
      message("\nRunning ad hoc check for common misspellings and monikers...", appendLF = FALSE)
      
      ### Target dictionaries
      target <- dictionary[na.omit(match(SemNetDictionaries::dictionaries(TRUE), dictionary))]
      
      ### Obtain monikers
      target.moniker <- SemNetDictionaries::load.monikers(target)
      
      ### Check for monikers
      # Message for large number of responses remaining
      message("\nUsing parallel processing to speed up moniker check...")
      
      # Number of cores
      ncores <- parallel::detectCores() / 2
      
      # Set up clusters
      cl <- parallel::makeCluster(ncores)
      
      # # Functions
      # funcs <- c(
      #   "moniker"
      # )
      # 
      # # Export functions
      # parallel::clusterExport(
      #   cl = cl, funcs,
      #   envir = as.environment(asNamespace("SemNetCleaner"))
      # )
      
      # Spell-check each individual word within the list (including multiple word responses)
      replace_to <- pbapply::pblapply(
        seq_along(to),
        function(i, to, target.moniker, spelling){
          unlist(lapply(to[[i]], function(x){
            
            if(!is.na(x)){
              
              unlist(moniker(
                x,
                target.moniker,
                spelling = spelling
              ))
              
            }
            
          }))
        },
        to = to, target.moniker = target.moniker, spelling = spelling,
        cl = cl
      )
      
      parallel::stopCluster(cl)
      
      # Replace to list
      to <- replace_to
  
    }
    
  }
  
  # Separate automated indices
  auto.ind <- na.omit(
    unlist(
      lapply(auto.ind, function(i){
        
        # Responses
        from_response <- from[[i]]
        to_response <- to[[i]]
        
        # Check for match
        if(all(!is.na(match(from_response, to_response)))){
          return(NA)
        }else{
          return(i)
        }
        
      })
    )
  )
  
  # Separate moniker changes
  if(main.count != 1){
    if(any(dictionary %in% SemNetDictionaries::dictionaries(TRUE)[-which(SemNetDictionaries::dictionaries(TRUE) == "general")])){
      
      auto.ind <- auto.ind[!from[auto.ind] %in% target.moniker]
      
    }
  }
  
  # Collect results
  # (initialized before asking to save dictionary)
  final.res$from <- from
  final.res$to <- to
  final.res$manual <- ind
  final.res$auto <- auto.ind
  final.res$data <- data
  final.res$type <- "free"
  final.res$stop <- FALSE
  
  class(final.res) <- "textcleaner"
  
  return(final.res)
}

#%%%%%%%%%%%%%%%%%%%%%%%#
#### CORRECT CHANGES ####
#%%%%%%%%%%%%%%%%%%%%%%%#

#' Correct Changes from \code{\link[SemNetCleaner]{textcleaner}}
#' 
#' @description A function that corrects changes that were made
#' automatically by \code{\link[SemNetCleaner]{textcleaner}}
#' 
#' @param textcleaner.obj Object from \code{\link[SemNetCleaner]{textcleaner}}
#' 
#' @param type Character vector.
#' Type of task to be preprocessed.
#' \itemize{
#' 
#' \item{\code{"fluency"}}
#' {Verbal fluency data (e.g., categories, phonological, synonyms)}
#' 
#' \item{\code{"free"}}
#' {Free association data (e.g., cue terms or words)}
#' 
#' }
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
#' @importFrom utils edit write.csv
#' 
#' @noRd
#' 
# Correct changes
# Updated 23.11.2021
# Major update: 19.04.2020
correct.changes <- function(textcleaner.obj, type = c("fluency", "free"))
{
  
  # Check if textcleaner object is input
  # if(!class(textcleaner.obj) == "textcleaner")
  # {stop("A 'textcleaner' class object was not input in the 'textcleaner.obj' argument")}
  
  # Store textcleaner object as result list
  res <- textcleaner.obj
  
  ## Original automated responses
  automated <- res$spellcheck$automated
  
  # Make sure automated responses are a matrix
  if(is.vector(automated)){
    automated <- t(as.matrix(automated))
  }
  
  # Get number of columns to add
  from_vector <- automated[,"from"]
  
  # Split string
  from_split <- strsplit(from_vector, split = " ")
  
  # Max lengths
  max_length <- max(unlist(lapply(from_split, length)), na.rm = TRUE)
  
  # Add columns for smoother Shiny experience
  for(i in 1:max_length){
    
    # Make all NA
    automated <- cbind(automated, NA)
    
    # Rename columns
    colnames(automated)[-1] <- paste("to", 1:(ncol(automated) - 1), sep = "_")
    
    # Ensure matrix
    automated <- as.matrix(automated)
  }
  
  # Write temporary file
  DIR <- tempdir()
  PATH <- paste(DIR, "automated.csv", sep = "\\")
  write.csv(automated, file = PATH, row.names = FALSE)
  
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
    
    readline("Press ENTER to continue with spell-check.")
    
    changes <- edit(automated)
    
  }else{
    
    # Set up message to user
    cat(colortext("\nYou will now have a chance to correct the changes that", defaults = "message"))
    cat(colortext("\nwere made during the AUTOMATED spell-checking process.", defaults = "message"))
    cat(colortext("\nA spreadsheet will open allowing you to manually correct", defaults = "message"))
    cat(colortext("\nthese changes.\n\n", defaults = "message"))
    
    readline("Press ENTER to continue with spell-check.")
    
    changes <- AutomatedEdit() # Make changes
    automated <- as.matrix(automated) # Convert back to matrix
    changes <- as.matrix(changes) # Convert changes to matrix
    changes <- ifelse(changes == "", NA, changes) # Replace blanks with NA
  }
  
  # Let the user know that their data is being prepared
  message("\nPreparing your data...")
  
  # Start "add column" configuration (not functional)
  
  # # Get differences
  # ## Difference in columns
  # colDiff <- ncol(changes) - ncol(automated)
  # if(colDiff != 0){
  #   
  #   # Loop through adding NA columns
  #   for(i in 1:colDiff){
  #     automated <- cbind(automated, NA)
  #   }
  #   
  #   # Rename columns
  #   colnames(automated)[-1] <- paste("to", 1:(ncol(automated) - 1), sep = "_")
  #   
  #   # Ensure matrix
  #   automated <- as.matrix(automated)
  #   
  # }
  
  # End "add column" configuration
  
  ## Obtain differences
  differences <- automated != as.matrix(changes) # ensure matrix for changes
  
  ## Ensure matrix
  if(!is.matrix(differences)){
    differences <- t(as.matrix(differences))
  }
  
  # Provide changes for user
  ## Find rows that have changed
  target.changes <- which(apply(differences, 1, function(x){any(x)}))
  
  # If there are no changes, then return original object
  if(length(target.changes) == 0){
    
    message("\nNo responses changed.")
    
    return(textcleaner.obj)
    
  }else{
    
    ## Initialize track changes
    track.changes <- list()
    
    ## Loop through changes
    for(i in 1:length(target.changes)){
      
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
    if(type == "free"){
      original <- as.matrix(res$data$original)
    }else{
      original <- as.matrix(res$responses$original)
    }
    
    # Create new correspondence matrix
    correspondence <- res$spellcheck$correspondence
    
    # Get number of columns between correspondence and changes matrices to match
    if(ncol(correspondence) > ncol(changes)){
      
      ## Difference in number of columns
      diff <- ncol(correspondence) - ncol(changes)
      
      ## Tack on NA columns
      for(i in 1:diff)
      {changes <- as.matrix(cbind(changes, rep(NA, nrow(changes))))}
      
    }else if(ncol(correspondence) < ncol(changes)){
      
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
    
    if(any(class(corr.mat) == "try-error")){
      return(error.fun(corr.mat, "correspondence.matrix", "correct.changes"))
    }
    
    ## Update with changes made by user
    res$spellcheck$automated <- changes
    
    # # Get spell-corrected data (error catch)
    # if(type == "fluency"){
    #   
    #   # Fluency data correction
    #   corrected <- try(
    #     correct.data(original, corr.mat),
    #     silent = TRUE
    #   )
    #   
    #   if(any(class(corrected) == "try-error"))
    #   {return(error.fun(corrected, "correct.data", "correct.changes"))}
    #   
    #   ## Collect behavioral data
    #   behavioral <- corrected$behavioral
    #   
    #   ## Make sure to replace faux "NA" with real NA
    #   corrected$corrected[which(corrected$corrected == "NA")] <- NA
    #   
    #   ## Cleaned responses (no instrusions or perseverations)
    #   cleaned.list <- apply(corrected$corrected, 1, function(x){unique(na.omit(x))})
    #   
    #   ## Check if cleaned.list is a list
    #   if(!is.list(cleaned.list)){
    #     cleaned.list <- apply(cleaned.list, 1, as.list)
    #   }
    #   
    #   max.resp <- max(unlist(lapply(cleaned.list, length)))
    #   
    #   cleaned.matrix <- t(sapply(
    #     lapply(cleaned.list, function(x, max.resp){
    #       c(x, rep(NA, max.resp - length(x)))
    #     }, max.resp = max.resp)
    #     ,rbind))
    #   
    #   colnames(cleaned.matrix) <- paste("Response_", formatC(1:ncol(cleaned.matrix),
    #                                                          digits = nchar(ncol(cleaned.matrix)) - 1,
    #                                                          flag = "0"), sep = "")
    #   
    #   res$responses$clean <- cleaned.matrix
    #   
    #   # Convert to binary response matrix (error catch)
    #   res$responses$binary <- try(
    #     resp2bin(corrected$corrected),
    #     silent = TRUE
    #   )
    #   
    #   if(any(class(res$responses$binary) == "try-error"))
    #   {return(error.fun(res$responses$binary, "resp2bin", "correct.changes"))}
    #   
    #   behavioral <- cbind(behavioral, rowSums(res$responses$binary))
    #   colnames(behavioral)[3] <- "Appropriate"
    #   res$behavioral <- as.data.frame(behavioral)
    #   
    #   #make 'textcleaner' class
    #   class(res) <- "textcleaner"
    #   
    # }
    
    return(res)
    
  }
  
}

#' Correspondence Matrix
#' 
#' @description A sub-routine function for matching responses pre- and post-spell-check
#' 
#' @param from List.
#' Responses prior to spell-correction
#' 
#' @param to List.
#' Spell-corrected unique responses
#' 
#' @return A matrix containing original responses (\code{"from"}) and what they were changed to (\code{"to_#})
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Correspondence matrix
# Updated 17.04.2020
correspondence.matrix <- function (from, to)
{
  # Number of responses
  n <- length(from)
  
  # Obtain widest response
  to_width <- max(unlist(lapply(to, length)))
  
  # Set up correspondence matrix
  corr.mat <- matrix(NA, nrow = n, ncol = (to_width + 1))
  colnames(corr.mat) <- c("from", paste("to_", 1:to_width, sep = ""))
  
  # Loop through responses
  for(i in 1:n)
  {
    # Set up vector to insert
    insert <- c(from[[i]], to[[i]])
    
    corr.mat[i,1:length(insert)] <- insert
  }
  
  return(corr.mat)
}

#' Spell Corrected Matrix
#' 
#' @description A sub-routine function for correcting misspelled words
#' 
#' @param data Matrix or data frame.
#' Responses prior to spell-correction
#' 
#' @param corr.mat Matrix or data frame.
#' A correspondence matrix output from \code{\link[SemNetCleaner{correspondence.matrix}]}
#' 
#' @return A list containing:
#' 
#' \item{behavioral}{A matrix containing the perseverations and intrusions for each participant}
#' 
#' \item{corrected}{The spell corrected response matrix}
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Spell Corrected Matrix
# Updated 20.08.2020
correct.data <- function (data, corr.mat)
{
  # Get number of cases
  n <- nrow(data)
  
  # Maximum number of responses that are not NA in original data
  data.max <- max(rowSums(apply(data, 2, function(x){!is.na(x)})))
  
  # Initialize data matrix
  correct.mat <- matrix(NA, nrow = n, ncol = 2 * data.max)
  
  # Initialize perseverations and intrusions matrix
  behav.mat <- matrix(0, nrow = n, ncol = 2)
  colnames(behav.mat) <- c("Perseverations", "Intrusions")
  row.names(behav.mat) <- row.names(data)
  
  # Loop through cases
  for(i in 1:n)
  {
    # Match responses to correspondence matrix
    ind <- match(data[i,!is.na(data[i,])], corr.mat[,"from"])
    
    # Obtain correspondence
    corr <- corr.mat[ind,-1]
    
    # Ensure it's a matrix
    if(!is.matrix(corr))
    {corr <- as.matrix(corr)}
    
    # Convert responses in their correct order back into data
    correct.ord <- na.omit(as.vector(t(corr)))
    
    if(length(correct.ord) > 0)
    {
      # Compute number of intrusions
      behav.mat[i,"Intrusions"] <- sum(correct.ord == "NA")
      
      # Compute number of perseverations
      behav.mat[i,"Perseverations"] <- length(correct.ord[-which(correct.ord == "NA")]) - length(unique(correct.ord[-which(correct.ord == "NA")]))
      
      # Insert into corrected matrix
      correct.mat[i,1:length(correct.ord)] <- correct.ord
      
    }else{
      
      # Compute number of intrusions
      behav.mat[i,"Intrusions"] <- 0
      
      # Compute number of perseverations
      behav.mat[i,"Perseverations"] <- 0
      
    }
    
  }
  
  # Remove columns that are all NA
  correct.mat <- correct.mat[,-which(apply(correct.mat, 2, function(x){all(is.na(x))}))]
  row.names(correct.mat) <- row.names(data)
  colnames(correct.mat) <- paste("Response_", formatC(1:ncol(correct.mat),
                                                      digits = nchar(ncol(correct.mat)) - 1,
                                                      flag = "0"), sep = "")
  
  # Initialize result list
  res <- list()
  
  # Collect results
  res$behavioral <- behav.mat
  res$corrected <- correct.mat
  
  return(res)
}

#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Spell Corrected Matrix
# Updated 20.08.2020
correct.data.free <- function (data, corr.mat, ids)
{
  # Correct matrix
  correct.mat <- data
  
  # Initialize perseverations and intrusions matrix
  behav.mat <- matrix(0, nrow = length(ids), ncol = 2)
  colnames(behav.mat) <- c("Perseverations", "Intrusions")
  row.names(behav.mat) <- ids
  
  # Loop through each participant
  for(i in 1:length(ids)){
    
    # Get target participant
    ind.p <- which(data[,"ID"] == ids[i])
    target.p <- data[ind.p,]
    
    # Loop through each cue
    cues <- unique(target.p[,"Cue"])
    
    for(j in 1:length(cues)){
      
      # Get target cue
      ind.c <- which(target.p[,"Cue"] == cues[j])
      target.c <- target.p[ind.c,]
      
      # Ensure matrix
      if(!is.data.frame(target.c) & !is.matrix(target.c)){
        target.c <- t(as.matrix(target.c))
      }
      
      # Match responses to correspondence matrix
      ind <- match(target.c[,"Response"], corr.mat[,"from"])
      
      # Obtain correspondence
      corr <- corr.mat[ind,-1]
      
      # Ensure it's a matrix
      if(!is.matrix(corr)){
        corr <- t(as.matrix(corr))
      }
      
      # Remove NA columns
      na.cols <- apply(corr, 2, function(x){all(is.na(x))})
      
      if(any(na.cols)){
        
        # Remove NA columns
        corr <- corr[,!na.cols]
        
        # Check for matrix
        if(!is.matrix(corr)){
          
          # Check for length change
          if(length(ind) != length(corr))
          {corr <- t(as.matrix(corr))}
          
        }
        
      }
      
      # Check for matrix
      if(!is.matrix(corr)){
        
        # Convert responses in their correct order back into data
        correct.ord <- as.vector(t(corr))
        
        if(length(correct.ord) > 0){
          
          # Compute number of intrusions
          behav.mat[i,"Intrusions"] <- behav.mat[i,"Intrusions"] + sum(is.na(correct.ord))
          
          # Compute number of perseverations
          behav.mat[i,"Perseverations"] <- behav.mat[i,"Perseverations"] + (length(correct.ord[-which(is.na(correct.ord))]) - length(unique(correct.ord[-which(is.na(correct.ord))])))
          
          # Insert into corrected matrix
          correct.mat[ind.p[ind.c],"Response"] <- correct.ord
          
        }else{
          
          # Compute number of intrusions
          behav.mat[i,"Intrusions"] <- behav.mat[i,"Intrusions"] + 0
          
          # Compute number of perseverations
          behav.mat[i,"Perseverations"] <- behav.mat[i,"Perseverations"] + 0
          
        }
        
      }else{
        
        # Create correct order
        correct.ord <- as.vector(t(corr[,1]))
        
        # Compute number of intrusions
        behav.mat[i,"Intrusions"] <- behav.mat[i,"Intrusions"] + sum(is.na(correct.ord))
        
        # Compute number of perseverations
        behav.mat[i,"Perseverations"] <- behav.mat[i,"Perseverations"] + (length(correct.ord[-which(is.na(correct.ord))]) - length(unique(correct.ord[-which(is.na(correct.ord))])))
        
        # Reorganize for rows to add
        add_responses <- unname(unlist(apply(corr, 1, function(x){na.omit(x)})))
        
        # Insert into corrected matrix
        correct.mat[ind.p[ind.c],"Response"] <- correct.ord
        
        # Identify row to add after
        addHere <- min(ind.p[ind.c]) - 1
        
        # Remove rows
        correct.mat <- correct.mat[-ind.p[ind.c],]
        
        # Set position
        minPosition <- min(ind.p[ind.c])
        maxPosition <- minPosition + length(add_responses) - 1
        position <- seq(minPosition, maxPosition, 1)
      
        # Create space
        correct.mat[seq(addHere + length(add_responses), nrow(correct.mat) + length(add_responses)),] <- correct.mat[seq(addHere, nrow(correct.mat)), ]
        
        # Insert values
        correct.mat[position, ] <- cbind(
          rep(ids[i], length(add_responses)),
          rep(cues[j], length(add_responses)),
          add_responses
        )
      
      }
      
    }
    
  }
  
  # Remove rows that are all NA
  correct.mat <- na.omit(correct.mat)
  
  # Initialize result list
  res <- list()
  
  # Collect results
  res$behavioral <- behav.mat
  res$corrected <- correct.mat
  
  return(res)
}

#' Update moniker lists
#' 
#' @description Sub-rountine to update moniker lists.
#' 
#' @param word Character.
#' The word for which there are monikers
#' 
#' @param monikers Character vector.
#' Monikers of the \code{word}
#' 
#' @param monk.list List.
#' A list of monikers from \code{\link{SemNetDictionaries}}
#' 
#' @return An updated moniker list
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
#' 
# Update monikers
# Updated 17.04.2020
update.monikers <- function (word, monikers, monk.list)
{
  # Get name of moniker list
  obj.name <- as.character(substitute(monk.list))
  
  # Check if word is in moniker list
  if(word %in% names(monk.list))
  {
    # Go to word in moniker list
    monk.list[[word]] <- sort(unique(c(monk.list[[word]], monikers)))
    
  }else{
    
    # Add word to moniker list
    monk.list[[paste(word)]] <- sort(unique(monikers))
    
    # Alphabetize moniker list
    monk.list <- monk.list[order(names(monk.list))]
  }
  
  #assign moniker object
  assign(obj.name, monk.list, envir = environment())
  
  #path to package on local pc
  path <- "D:/R Packages/SemNetDictionaries/data"
  
  #path to data
  data.path <- paste(path, "/", obj.name, ".Rdata", sep = "")
  
  return(monk.list)
}

#%%%%%%%%%%%%%%%%%%%%%%%%#
#### SYSTEM FUNCTIONS ####
#%%%%%%%%%%%%%%%%%%%%%%%%#

#' Colorfies Text
#' 
#' Makes text a wide range of colors (8-bit color codes)
#' 
#' @param text Character.
#' Text to color
#' 
#' @return Colorfied text
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
#' 
# Color text
# Updated 08.09.2020
colortext <- function(text, number = NULL, defaults = NULL)
{
  # Check system
  sys.check <- system.check()
  
  if(sys.check$TEXT)
  {
    # Defaults for number (white text)
    if(is.null(number) || number < 0 || number > 231)
    {number <- 15}
    
    # Check for default color
    if(!is.null(defaults))
    {
      # Adjust highlight color based on background color
      if(defaults == "highlight")
      {
        if(sys.check$RSTUDIO)
        {
          
          if(rstudioapi::getThemeInfo()$dark)
          {number <- 226
          }else{number <- 208}
          
        }else{number <- 208}
      }else{
        
        number <- switch(defaults,
                         message = 204,
                         red = 9,
                         orange = 208,
                         yellow = 11,
                         "light green" = 10,
                         green = 34,
                         cyan = 14,
                         blue = 12,
                         magenta = 13,
                         pink = 211,
        )
        
      }
    
    }
    
    return(paste("\033[38;5;", number, "m", text, "\033[0m", sep = ""))
      
  }else{return(text)}
}

#' Stylizes Text
#' 
#' Makes text bold, italics, underlined, and strikethrough
#' 
#' @param text Character.
#' Text to stylized
#' 
#' @return Sytlized text
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Style text
# Updated 08.09.2020
styletext <- function(text, defaults = c("bold", "italics", "highlight",
                                         "underline", "strikethrough"))
{
  # Check system
  sys.check <- system.check()
  
  if(sys.check$TEXT)
  {
    if(missing(defaults))
    {number <- 0
    }else{
      
      # Get number code
      number <- switch(defaults,
                       bold = 1,
                       italics = 3,
                       underline = 4,
                       highlight = 7,
                       strikethrough = 9
      )
      
    }
    
    return(paste("\033[", number, ";m", text, "\033[0m", sep = ""))
  }else{return(text)}
}

#' Text Symbols
#' 
#' Makes text symbols (star, checkmark, square root)
#' 
#' @param symbol Character.
#' 
#' @return Outputs symbol
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Symbols
# Updated 24.04.2020
textsymbol <- function(symbol = c("alpha", "beta", "chi", "delta",
                                  "eta", "gamma", "lambda", "omega",
                                  "phi", "pi", "rho", "sigma", "tau",
                                  "theta", "square root", "infinity",
                                  "check mark", "x", "bullet")
                       )
{
  # Get number code
  sym <- switch(symbol,
                alpha = "\u03B1",
                beta = "\u03B2",
                chi = "\u03C7",
                delta = "\u03B4",
                eta = "\u03B7",
                gamma = "\u03B3",
                lambda = "\u03BB,",
                omega = "\u03C9",
                phi = "\u03C6",
                pi = "\u03C0",
                rho = "\u03C1",
                sigma = "\u03C3",
                tau = "\u03C4",
                theta = "\u03B8",
                "square root" = "\u221A",
                infinity = "\u221E",
                "check mark" = "\u2713",
                x = "\u2717",
                bullet = "\u2022"
  )
  
  return(sym)
}

#' System check for OS and RSTUDIO
#' 
#' @description Checks for whether text options are available
#' 
#' @param ... Additional arguments
#' 
#' @return \code{TRUE} if text options are available and \code{FALSE} if not
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# System Check
# Updated 08.09.2020
system.check <- function (...)
{
  OS <- unname(tolower(Sys.info()["sysname"]))
  
  RSTUDIO <- ifelse(Sys.getenv("RSTUDIO") == "1", TRUE, FALSE)
  
  TEXT <- TRUE
  
  if(!RSTUDIO){if(OS != "linux"){TEXT <- FALSE}}
  
  res <- list()
  
  res$OS <- OS
  res$RSTUDIO <- RSTUDIO
  res$TEXT <- TEXT
  
  return(res)
}
