#' Checks If A Character Is A Letter
#' @description A wrapper function designed to determine whether
#' a single character is a letter
#' 
#' @param letter A single character
#' 
#' @return A \code{TRUE} or \code{FALSE} value for whether
#' the character entered is a letter
#' 
#' @examples
#' #TRUE
#' is.letter("r")
#' 
#' #FALSE
#' is.letter("5")
#' 
#' #FALSE
#' is.letter("~")
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Is letter Function----
is.letter <- function(letter) 
{
    #make sure letter is letter
    chk1 <- grepl("[[:alpha:]]", letter)
    
    #double check that it's not a numeric character
    if(is.na(suppressWarnings(as.numeric(letter))))
    {chk2 <- TRUE
    }else{chk2 <- FALSE}
    
    #passes both checks, then letter
    #fails either, then not letter
    if(chk1&&chk2)
    {TRUE
    }else{FALSE}
}
