#' Converge Responses
#' @description Merge a column of binarized response data with another
#' 
#' @param rmat Binary response matrix.
#' A \link[SemNetCleaner]{textcleaner} filtered response matrix
#' 
#' @param word Character.
#' Must be column name (characters).
#' The column name (or number) that be merged \emph{into}.
#' This column will \emph{remain} in the matrix
#' 
#' @param replace Character.
#' Must be column name (characters).
#' The column name (or number) that should be merged
#' with the \code{word} column.
#' This column will be \emph{removed} from the matrix
#' 
#' @return The response matrix with the \code{word} column merged
#' and the \code{replace} column removed
#' 
#' @examples
#' # Toy example
#' raw <- open.animals[c(1:10),-c(1:3)]
#' 
#' # Clean and prepocess data
#' clean <- textcleaner(raw, partBY = "row", dictionary = "animals")
#' 
#' # Converge "golden retriever" into response of "lab"
#' rmat <- converge(clean$binary,"lab","golden retriever")
#' 
#' # "lab" remains will "golden retriever" responses are merged into
#' # "lab" and "golden retriever" is removed
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Converge Function----
converge <- function (rmat, word, replace)
{
    if(any(colnames(rmat)==replace))
    {
        if(any(colnames(rmat)==word))
        {
            for(i in 1:nrow(rmat))
                if(rmat[i,which(colnames(rmat)==replace)]==1)
                {rmat[i,which(colnames(rmat)==word)] <- 1}
            
            #coverge word to be replaced with correct word
            rmat[which(colnames(rmat)==word)]
            #remove column with spelling difference
            rmat<-rmat[-which(colnames(rmat)==replace)]
            
            return(rmat)
        }else{stop("word not found")} #produce error if word does not exist
    }else{stop("word to replace not found")} #produce error if word to replace does not exist
}
#----