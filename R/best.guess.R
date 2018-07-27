#' Best guess function
#' @description A wrapper function for best guess spelling
#' 
#' @param vec Vector of potential options
#' 
#' @param database The database to search for best guesses in
#' 
#' @return The best guess(es) of the word
#' 
#' @examples
#' 
#' best.guess("bomba", animals.database)
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom stats sd
#' 
#' @export
#Max-min normalization function
best.guess <- function (vec, database)
{
    #split of all possible resposnes
    split <- strsplit(database,"")
    #split of vector
    vecsplit <- strsplit(vec,"")[[1]]
    
    #count number of letters that the same across all possible responses
    count <- vector("numeric",length=length(split))
    for(l in 1:length(split))
    {count[l] <- length(na.omit(match(vecsplit,split[[l]])))}
    
    #count number of letters in the correct order
    ord.count <- matrix(0,nrow=length(split),ncol=length(vecsplit))
    for(u in 1:length(vecsplit))
        for(v in 1:length(split))
        {
            if(!is.na(split[[v]][u]==vecsplit[u]))
            {
                if(split[[v]][u]==vecsplit[u])
                {ord.count[v,u] <- 1}
            }else if(is.na(split[[v]][u]==vecsplit[u]))
            {ord.count[v,u] <- 0}
        }
    
    #total number in the correct order
    ord.sum <- rowSums(ord.count)
    
    #diffence in number of characters count
    diff.count <- abs(nchar(vec)-nchar(database))
    
    #combine the metrics
    ord.max <- normalize(ord.sum)
    cou.max <- normalize(count)
    comb.max <- ord.max+cou.max
    #difference responses within range
    diff.resp <- database[which(diff.count<=2)]
    #combined metrics within range
    comb.resp <- database[which(comb.max>mean(comb.max)+(2*sd(comb.max)))]
    
    #make best estimate
    best <- diff.resp[which(!is.na(match(diff.resp,comb.resp)))]
    
    #grab first letter
    first <- substring(vec,1,1)
    #grab for potential responses
    start <- as.character(paste("^",first,sep=""))
    #best guess
    guess <- best[grep(start,best,ignore.case=TRUE)]
    
    return(guess)
}
#----