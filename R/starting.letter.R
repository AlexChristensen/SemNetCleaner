#' Starting Letter
#' @description A wrapper function designed to produce the first
#' letter that appears in a word, regardless of leading characters
#' 
#' @param word Character. A single word
#' 
#' @return First letter in the string
#' 
#' @examples
#' #first letter is "w"
#' starting.letter("..walrus")
#' 
#' #first letter is "r"
#' starting.letter("5rat")
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Starting Letter Function----
starting.letter <- function (word)
{
    #grab first letter
    first <- substring(word,1,1)
    
    #if first character is not a letter, then find first letter
    if(!is.letter(first))
    {
        #identify first letter in string
        for(j in 2:nchar(word))
        {
            #determine if letter
            let <- is.letter(substring(word,j,j))
            
            #if letter then break
            if(let)
            {break}
        }
        
        #grab the first letter
        first <- substring(word,j,j)
    }
    
    return(first)
}
#----