#' Max-min normalization 
#' @description A wrapper function for best guess spelling
#' 
#' @param vec Vector of potential options
#' 
#' @return A vector of normalized values ranging from 0 to 1
#' 
#' @examples 
#' #create a numeric vector list
#' num <- runif(n = 100, min = 0, max = 100)
#' 
#' #normalize the number vector
#' normalize(num)
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Max-min normalization function
normalize <- function (vec)
{
    n <- length(vec)
    
    norm <- vec
    
    for(k in 1:n)
    {norm[k] <- (vec[k] - min(vec))/(max(vec)-min(vec))}
    
    return(norm)
}
#----