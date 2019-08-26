#' Removes Leading Spaces
#' @description Removes leading spaces that are not caught by \code{\link{trimws}}
#' 
#' @param word Character (vector). A word that has leading
#' spaces that cannot be removed by \code{\link{trimws}}
#' 
#' @return Word without leading spaces
#' 
#' @examples
#' # 'trimws' should remove lead space but doesn't
#' trimws(lead.word)
#' 
#' # 'rm.lead.space' does
#' rm.lead.space(lead.word)
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Text Cleaner----
rm.lead.space <- function(word)
{
    word <- bad.response(word)
    
    if(!is.na(word))
    {word <- gsub("^[[:space:]]+|[[:space:]]+$", "", word)}
    
    return(word)
}
#----