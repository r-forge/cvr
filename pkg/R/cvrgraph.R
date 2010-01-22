sons <- function(mother, tr=cvrread()){
  sons <- c()
  sons <- tr$versionnumber[which(tr$vnbefore==mother)]
  return(sons)
} # end of function sons()

mother <- function(son, tr = cvrread()){
  mother <- tr$vnbefore[which(tr$versionnumber==son)]
  return(mother)
} # end of function mother


cousins <- function(parents){
  cousins=c()
  for (i in 1:length(parents)){
    cousins <- c(cousins, sons(parents[i]))
  }
  return(cousins)
} # end of function brothers()


generation <- function(grandma, tr=cvrread()){
  genlist <- c()
  if (length(cousins(grandma))>0){
       genlist <- rbind(genlist, cousins(grandma), generation(cousins(grandma)))
  }
  return(genlist)
} # end of function generation()

bronum <- function(genlist){
  if (!is.null(genlist)){
    bronum <- apply(genlist,1,function(x) {length(unique(x))})
  }
  else{
    bronum=0
  }
  return(bronum)
}

cvrgraph <- function(){
    parents <- 0
    genlist <- generation(parents)
    M <- c()#matrix(nrow=length(bronum(genlist)),ncol=3)
    Mtmp <- matrix(nrow=1, ncol=3)

    for (i in 1:length(bronum(genlist))){

      for (j in 1:bronum(genlist)[i]){
        Mtmp[1] <- unique(genlist[i,])[j]
        Mtmp[2] <- i

        if (j == 1) {
           Mtmp[3] <- 1
        } # end if j == 1 (first cousin)
        else{
          if (length(sons(mother(Mtmp[1])))==1){ # only son
           Mtmp[3] <- M[which(M[,1]==mother(Mtmp[1])),3] #+ max(1, bronum(generation(unique(genlist[i,1:c(j-1)]))))
         }
          else{
            Mtmp[3] <- M[which(M[,1]==mother(Mtmp[1])),3] + max(j-1, bronum(generation(unique(genlist[i,1:c(j-1)]))))
          }

        } # not the first of his cousins
        M <- rbind(M,Mtmp)
        Mtmp <- c(NA,NA,NA)

      } # for each cousin (node at the same level)

    } # for in in number of generations

    Mq <- matrix(nrow = nrow(cvrread()), ncol= nrow(cvrread()))
    
    for (i in 1:nrow(M)){
      mothery <- as.numeric(which(M[,1]==mother(M[i,1])))
      if (length(mothery)>0){
        Mq[i, mothery] <- M[i,1]
      }
    } # for i = all rows in m

    Xscale <- 1/(max(M[,3]))
    Yscale <- 1/(max(M[,2]))
    Mscaled<- cbind(M[,3]*Xscale-Xscale/2, 1-M[,2]*Yscale+Yscale/2)


    Cq <- matrix(nrow = nrow(cvrread()), ncol= nrow(cvrread()), data = 'white')
    Cq[which(M[,1]==sons(0)), 1] <- 'gray'
    
    if (exists('.cvrvn', envir= .GlobalEnv)){
	vnnow<- get('.cvrvn', envir=.GlobalEnv)
	Cq[which(M[,1]==vnnow), 1] <- 'red'
    }


    
    plotmat(Mq, name=M[,1], box.col= Cq, pos= Mscaled, curve=0, box.type="rect", box.prop=.5, box.size=1/nrow(Mq)^1.5)

} # end of function cvrgraph2

