#' All Possible Spelling Options
#' @description A wrapper function for spell-checking
#' (combines
#' \link[SemNetCleaner]{singularize},
#' \link[SemNetCleaner]{miss.let},
#' \link[SemNetCleaner]{chn.let}, and
#' \link[SemNetCleaner]{add.let} 
#' spell-checking wrapper functions)
#' 
#' @param word Word to check spelling options
#' 
#' @return Returns a vector of all possible spelling combinations from
#' several spell-checking options
#' 
#' @examples
#' #bombay
#' spell.opt("bomba")
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Spelling option function
spell.opt <- function (word)
{
    #check for singularize
    sing <- singularize(word)
    #check for space differences
    despa <- gsub(" ","",word)
    #check for missing letters
    mlets <- miss.let(word)
    #check for letter changes
    clets <- chn.let(word)
    #check for additional letters
    alets <- add.let(word)
    
    #combine unique
    comb <- unique(c(sing,despa,mlets,clets,alets))
    
    return(comb)
}
#----
