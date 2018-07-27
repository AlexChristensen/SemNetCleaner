#' Spelling-check for Dababases
#' @description A wrapper function for spell-checking text databases in \code{\link{SemNetCleaner}}
#' (combines all spell-checking wrapper functions)
#' 
#' @param data Input data to spell check
#' 
#' @param database The database to spell check from
#' 
#' @return Returns a vector of cleaned response
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom utils setTxtProgressBar txtProgressBar
#' 
#' @export
#Spell-check for database
spell.check.database <- function (data, database)
{
    #number of responses
    n <- length(data)
    
    #load database
    if(length(database)==1)
    {
        if(database=="animals")
        {
            base <- data("animals.database",envir = environment())
            misnom <- data("animals.misnomer",envir = environment())
        }
    }else{
        base <- database
        misnom <- NULL
    }
    
    #corrected responses
    resp <- data
    
    #remove miscellaneous string additions
    for(i in 1:length(data))
    {
        data[i] <- gsub("[[:punct:]]", "", data[i])
        data[i] <- gsub("[[:digit:]]+", "", data[i])
    }
    
    #progress bar
    pb <- txtProgressBar(min = 1, max = length(data), style=3, char="|")
    
    #remove correctly spelled responses
    for(i in 1:n)
    {
        if(is.na(data[i])) #check if missing data
        {spl <- "SKIP"
        }else if(data[i]=="") #check if blank input
        {spl <- "SKIP"
        }else{
            if(length(strsplit(data," ")[[i]])>3) #check for stings that need to be de-stringed
            {spl <- "SPLIT"
            }else{spl <- data[i]}
        }
        
        if(spl!="SKIP"&&spl!="SPLIT")
        {
            ##check if misnomer
            mis <- misnomer(spl,misnom)
            if(spl==mis)
            {
                sing <- singularize(spl)
                singspl <- misnomer(sing,misnom)
                if(sing!=singspl)
                {spl <- singspl}
            }else{spl <- mis}
            
            #check if already matches database
            if(!is.na(match(spl,base)))
            {resp[i] <- base[which(spl==base)]
            }else{
                comb <- spell.opt(spl)
                
                if(any(!is.na(match(comb,base))))
                {
                    opts <- comb[which(!is.na(match(comb,base)))]
                    
                    if(length(opts)>1)
                    {
                        print(spl)
                        ans5 <- menu(c(opts,"CORRECT","TYPE MY OWN"),title = "Select best option")
                        
                        if(ans5<=length(opts))
                        {resp[i] <- opts[ans5]
                        }else if(ans5==(length(opts)+1))
                        {resp[i] <- resp[i]
                        }else if(ans5==(length(opts)+2))
                        {resp[i] <- readline("Type response: ")}
                    }else{resp[i] <- comb[which(!is.na(match(comb,base)))]}
                    
                }else{
                    sing <- singularize(spl)
                    comb <- spell.opt(sing)
                    
                    if(any(!is.na(match(comb,base))))
                    {
                        if(length(comb[which(!is.na(match(comb,base)))])>1)
                        {
                            opts <- comb[which(!is.na(match(comb,base)))]
                        
                            #split response
                            split <- strsplit(data[i],"")[[1]]
                            #split of options
                            optsplit <- strsplit(opts,"")
                        
                            #order count
                            ord.count <- matrix(0,nrow=length(split),ncol=length(optsplit))
                            for(u in 1:length(split))
                                for(v in 1:length(optsplit))
                                {
                                    if(!is.na(split[u]==optsplit[[v]][u]))
                                    {
                                        if(split[u]==optsplit[[v]][u])
                                        {ord.count[u,v] <- 1}
                                    }else if(is.na(split[u]==optsplit[[v]][u]))
                                    {ord.count[u,v] <- 0}
                                }
                            ord.sum <- colSums(ord.count)
 
                                opts2 <- opts[which(ord.sum==max(ord.sum))]
                                
                                print(spl)
                                ans4 <- menu(c(opts2,"CORRECT","TYPE MY OWN"),title="Select best option: ")
                                
                                if(ans4<=(length(opts2)))
                                {resp[i] <- opts2[ans4]
                                }else if(ans4==(length(opts2)+1))
                                {resp[i] <- resp[i]
                                }else if(ans4==(length(opts2)+2))
                                {resp[i] <- readline("Type response: ")}
                                
                        }else{resp[i] <- comb[which(!is.na(match(comb,base)))]}
                    }
                }
            }
            
        }else if(spl=="SPLIT")
        {
            respspl <- strsplit(data," ")[[i]]
            
            #check if blank input
            if(any(respspl==""))
            {respspl <- respspl[-which(respspl=="")]}
            
            for(j in 1:length(respspl))
            {
                ##check if misnomer
                mis <- misnomer(respspl[j],misnom)
                if(respspl[j]==mis)
                {
                    sing <- singularize(respspl[j])
                    singspl <- misnomer(sing,misnom)
                    if(sing!=singspl)
                    {respspl[j] <- singspl}
                }else{respspl[j] <- mis}
                
                #check if already matches database
                if(!is.na(match(respspl[j],base)))
                {respspl[j] <- base[which(respspl[j]==base)]
                }else{
                    comb <- spell.opt(respspl[j])
                    
                    if(any(!is.na(match(comb,base))))
                    {
                            opts <- comb[which(!is.na(match(comb,base)))]
                            
                            print(respspl[j])
                            ans3 <- menu(c(opts,"CORRECT","TYPE MY OWN"),title="Select best option: ")
                            
                            if(ans3<=(length(opts)))
                            {respspl[j] <- opts[ans3]
                            }else if(ans3==(length(opts)+1))
                            {respspl[j] <- respspl[j]
                            }else if(ans3==(length(opts)+2))
                            {respspl[j] <- readline("Type response: ")}
                    }else{
                        sing <- singularize(respspl[j])
                        comb <- spell.opt(respspl[j])
                        
                        if(any(!is.na(match(comb,base))))
                        {respspl[j] <- comb[which(!is.na(match(comb,base)))]}
                    }
                }
            }
            
            resp[i] <- paste(respspl,collapse = " ")
        }
        
        setTxtProgressBar(pb, i)
    }
    
    close(pb)
    
    #responses to be spell-check
    look <- resp[is.na(match(resp,base))]
    
    ord.look <- which(is.na(match(resp,base)))
    
    #progress bar
    pb <- txtProgressBar(min = 1, max = length(look), style = 3, char="|")
    
    for(i in 1:length(look))
    {
        if(is.na(look[i])) #check if missing data
        {spl <- "SKIP"
        }else if(look[i]=="") #check if blank input
        {spl <- "SKIP"
        }else{
            if(length(strsplit(look," ")[[i]])>3) #check for stings that need to be de-stringed
            {spl <- "SPLIT"
            }else{spl <- look[i]}
        }
        
        if(spl!="SKIP"&&spl!="SPLIT")
        {
            #best guess
            best <- best.guess(spl,base)
            
            #first two letters
            first2 <- substring(spl,1,2)
            #grab for potential responses
            start <- as.character(paste("^",first2,sep=""))
            #potential responses
            pot <- base[grep(start,base,ignore.case=TRUE)]
            
            #potential responses without best
            newpot <- setdiff(pot,best)
            
            #ask for response
            print(spl)
            if(length(best)!=0)
            {
                ans <- menu(c(best,"Other options"),title="Best guess(es): ")
                if(any(ans==(1:length(best)))){res <- best[ans]}
            }else{ans <- 1}
                
            if(ans==length(best)+1)
            {
                if(length(best)!=0)
                {print(spl)}
                ans2 <- menu(c(newpot,"TYPE MY OWN","SKIP","BAD RESPONSE"),title="Potential responses:")
                
                if(ans2<=length(newpot))
                {res <- newpot[ans2]
                }else if(ans2==length(newpot)+1)
                {res <- readline("Type response: ")
                }else if(ans2==length(newpot)+2)
                {res <- spl
                }else if(ans2==length(newpot)+3)
                {res <- "REMOVE"}
            }
        }else if(spl=="SPLIT")
        {
            
            respspl <- strsplit(look," ")[[i]]
            
            respspl <- splitstr.check(respspl,base)
            
            #check if blank input
            if(any(respspl==""))
            {respspl <- respspl[-which(respspl=="")]}
            
            for(j in 1:length(respspl))
            {
                #check if already matches database
                if(!is.na(match(respspl[j],base)))
                {respspl[j] <- base[which(respspl[j]==base)]
                }else{
                    comb <- spell.opt(respspl[j])
                    
                    if(any(!is.na(match(comb,base))))
                    {
                        if(length(comb[which(!is.na(match(comb,base)))])>1)
                        {
                            opts <- comb[which(!is.na(match(comb,base)))]
                            
                            print(respspl[j])
                            ans3 <- menu(c(opts,"CORRECT","TYPE MY OWN"),title="Select best option: ")
                            
                            if(ans3<=(length(opts)))
                            {respspl[j] <- opts[ans3]
                            }else if(ans3==(length(opts)+1))
                            {respspl[j] <- respspl[j]
                            }else if(ans3==(length(opts)+2))
                            {respspl[j] <- readline("Type response: ")}
                        }else{respspl[j] <- comb[which(!is.na(match(comb,base)))]}
                    }else{
                        sing <- singularize(respspl[j])
                        comb <- spell.opt(sing)
                        
                        if(any(!is.na(match(comb,base))))
                        {respspl[j] <- comb[which(!is.na(match(comb,base)))]
                        }else{
                            #best guess
                            best <- best.guess(respspl[j],base)
                            
                            #first two letters
                            first2 <- substring(respspl[j],1,2)
                            #grab for potential responses
                            start <- as.character(paste("^",first2,sep=""))
                            #potential responses
                            pot <- base[grep(start,base,ignore.case=TRUE)]
                            
                            #potential responses without best
                            newpot <- setdiff(pot,best)
                            
                            #ask for response
                            print(respspl[j])
                            if(length(best)!=0)
                            {
                                ans <- menu(c(best,"Other options"),title="Best guess(es): ")
                                if(any(ans==(1:length(best)))){respspl[j] <- best[ans]}
                            }else{ans <- 1}
                            
                            if(ans==length(best)+1)
                            {
                                if(length(best)!=0)
                                {print(respspl[j])}
                                ans2 <- menu(c(newpot,"TYPE MY OWN","SKIP","BAD RESPONSE"),title="Potential responses:")
                                
                                if(ans2<=length(newpot))
                                {respspl[j] <- newpot[ans2]
                                }else if(ans2==length(newpot)+1)
                                {respspl[j] <- readline("Type response: ")
                                }else if(ans2==length(newpot)+2)
                                {respspl[j] <- respspl[j]
                                }else if(ans2==length(newpot)+3)
                                {respspl[j] <- "REMOVE"}
                            }
                        }
                    }
                }
            }
            
            res <- paste(respspl,collapse = " ")
        }else if(spl=="SKIP")
        {res <- look[i]}
        
       resp[ord.look[i]] <- res
       
       setTxtProgressBar(pb, i)
    }
    
    close(pb)
    
    #check one last time for misnomers
    for(i in 1:length(resp))
    {
        split <- strsplit(resp[i]," ")[[1]]
        
        if(length(split)>1)
        {
            len <- length(split)
            
            for(j in 1:len)
            {split[j] <- misnomer(split[j],misnom)}
            
            repl <- paste(split, collapse = " ")
            
        }else{resp[i] <- misnomer(resp[i],misnom)}
    }
    
    #change "REMOVE" to NA
    for(i in 1:length(resp))
    {
        split <- strsplit(resp[i]," ")[[1]]
        
        if(length(split)>1)
        {
            if(any(split=="REMOVE"))
            {
                rem <- which(split=="REMOVE")
                
                repl <- paste(split[-rem],collapse=" ")
                
                resp[i] <- repl
            }
        }else if(split=="REMOVE"||length(split)==0)
        {resp[i] <- NA}
    }
    
    #remove lingering "SKIP"
    resp <- ifelse(resp=="SKIP",NA,resp)
    
    return(resp)
}
#----