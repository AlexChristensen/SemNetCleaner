#' Responses to binary matrix
#' 
#' @description Converts the response matrix to binary response matrix
#' 
#' @param resp Response matrix.
#' A response matrix of verbal fluency or linguistic data
#' 
#' @return A list containing objects for each participant and their responses
#' 
#' @examples
#' # Toy example
#' raw <- open.animals[c(1:10),-c(1:3)]
#' 
#' # Clean and prepocess data
#' clean <- textcleaner(raw, partBY = "row", dictionary = "animals")
#' 
#' # Change response matrix to binary response matrix
#' binmat <- resp2bin(clean$responses$clean)
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
# Response matrix to binary matrix
# Updated 30.03.2020
resp2bin <- function (resp)
{
  # Data matrix
  mat <- as.matrix(resp)
  
  # Replace bad responses with NA
  mat <- SemNetCleaner::bad.response(mat)
  
  # Unique responses
  uniq.resp <- sort(na.omit(unique(as.vector(mat))))
  
  # Number of cases
  n <- nrow(mat)
  
  # Initialize binary matrix
  bin.mat <- matrix(0, nrow = n, ncol = length(uniq.resp))
  colnames(bin.mat) <- uniq.resp
  row.names(bin.mat) <- row.names(resp)
  
  # Loop through and replace
  for(i in 1:n)
  {bin.mat[i,na.omit(match(mat[i,], colnames(bin.mat)))] <- 1}
  
  return(bin.mat)
}
#----