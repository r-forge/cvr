
md5sum <- function(x) {
	md5s <- digest(x)
	return(md5s)
} # end of function md5sum

cvrinit <- function(){
	if (!exists('.cvrvn', envir= .GlobalEnv)){
		assign(".cvrvn", 0, envir = .GlobalEnv)
	}
	if (!file.exists('transformations.cvr')){
		file.create('transformations.cvr')
		}
} # end of function cvrinit

cvrread <- function(){
	transformations <- c()
	cvrinit()
	cvrfile <- file("transformations.cvr", "r")
	cvrtext <- readLines(cvrfile )
	close(cvrfile)
	if (length(cvrtext) > 1){
		recnum <- length(cvrtext)/6
		transformations <- data.frame(
				versionnumber=c(cvrtext[1+(1:recnum-1)*6]), 
				vnbefore= c(cvrtext[2+(1:recnum-1)*6]),
				md5before= c(cvrtext[3+(1:recnum-1)*6]), 
				expression= c(cvrtext[4+(1:recnum-1)*6]), 
				md5after= c(cvrtext[5+(1:recnum-1)*6]), 
				comments= c(cvrtext[6+(1:recnum-1)*6]), 
				stringsAsFactors = FALSE)
	}
	return(transformations)
} # end of function cvrread

cvrlog <- function(md5before, expression, md5after, comments){
	cvrinit()
	tr<- cvrread()
	vnbefore<- get('.cvrvn', envir=.GlobalEnv)
	versionnumber <- floor(runif(1)*10000)
	while(versionnumber %in% tr$versionnumber){ versionnumber <- floor(runif(1)*10000) }
	cat(paste(versionnumber, '\n', vnbefore, '\n', md5before, '\n', expression, '\n', md5after, '\n', comments, '\n',sep=''), file='cvr.temp')
	file.append('transformations.cvr', 'cvr.temp')
	file.remove('cvr.temp')
	assign(".cvrvn", versionnumber , envir = .GlobalEnv)
} # end of function cvrlog

cvrdo <- function(xpression, comment='uncommented'){
	cvrinit()
	origmd5 <- md5sum(ls(envir=.GlobalEnv))
	eval(parse(text=xpression), envir = .GlobalEnv)
	if (md5sum(ls(envir=.GlobalEnv))==origmd5) {
		#cat("\nunchanged!\n")
		}
	else { 
		#cat("\nChanged!\n")
		cvrlog(origmd5 , xpression, md5sum(ls()), comment)
	}
} # end of function cvrdo 




cvrrewind<- function(versionnumber){
	cvrhist <- c()
	tr <- cvrread()
	if  (versionnumber %in% tr$versionnumber){
		numstep=1
		rm(list=ls(envir=.GlobalEnv), envir=.GlobalEnv)
		rm('.cvrvn', envir=.GlobalEnv)
		cmdindex <- which(tr$versionnumber==versionnumber)
		cmdindex <- which(tr$versionnumber==versionnumber)
		cvrhist$currentvn[numstep]<- tr$versionnumber[cmdindex]
		cvrhist$vnbefore[numstep]<- tr$vnbefore[cmdindex]
		cvrhist$expression[numstep]<-tr$expression[cmdindex]
		versionnumber=cvrhist$vnbefore[numstep]
	
		numstep=numstep+1
		while(cvrhist$vnbefore[numstep-1]!=0){
			cmdindex <- which(tr$versionnumber==versionnumber)
			cvrhist$currentvn[numstep]<- tr$versionnumber[cmdindex]
			cvrhist$vnbefore[numstep]<- tr$vnbefore[cmdindex]
			cvrhist$expression[numstep]<-tr$expression[cmdindex]
			versionnumber=cvrhist$vnbefore[numstep]
			numstep=numstep+1
		}
		cvrhist<- as.data.frame(cvrhist, stringsAsFactors = FALSE)
		cvrhist <- cvrhist[nrow(cvrhist):1,]

		#assign("cvrhist", cvrhist, envir = .GlobalEnv)

		for (i in 1:nrow(cvrhist)){
			eval(parse(text=cvrhist$expression[i]), envir = .GlobalEnv)
			assign(".cvrvn", cvrhist$currentvn[i], envir = .GlobalEnv)
		}
	}
	else{
		cat('\nversion number does not exist\n')
	}

} # end of function cvrrewind

ls.inv <- function(pattern){
	listall <- ls(envir=.GlobalEnv)
	listprt <- ls(pattern=pattern, envir=.GlobalEnv)
	listinv <- c()
	if (length(listprt>0)){
		for (i in 1:length(listall)){
			if (!(listall[i] %in% listprt )){
				listinv <- c(listinv, listall[i])
			}
		}
	}
	return(listinv)
} #end of function ls.inv

cvrgraph <- function(){

	tmat <- cvrread()
	if ( nrow(tmat) > 1) {
		states.n <- nrow(tmat)+1
		states.name <- c("empty",tmat$versionnumber)
		mycommands <- tmat$comments
		for (i in 1:length(mycommands)){
			while (length(grep(" ", mycommands[i]))>0){
				mycommands[i] <- sub(" ", "_", mycommands[i])
			}
		}




		M   <- matrix(nrow=states.n,ncol=states.n,byrow=TRUE,data=0)
		col <- matrix(nrow=states.n,ncol=states.n,byrow=TRUE,data="white")
		col [which(tmat$vnbefore=="0"), 1] <- "gray" 
		if (exists('.cvrvn', envir= .GlobalEnv)){
			vnnow<- get('.cvrvn', envir=.GlobalEnv)
			col[which(tmat$versionnumber==vnnow)+1, 1] <- "red"
		}
		M[which(tmat$vnbefore=="0")+1, 1] <- mycommands[which(tmat$vnbefore=="0")]


		for (i in 2:states.n){
			M[i,which(tmat$versionnumber==tmat$vnbefore[i-1])+1] <- mycommands[i-1]
		}



		tmat <- tmat[order(tmat$vnbefore),]

		plot.yn <- length(unique(tmat$vnbefore))
		plot.xn <- length(tmat$vnbefore) - plot.yn

		plot.y <- 1

		plotM  <- matrix(nrow=states.n-1,ncol=2,byrow=TRUE,data=0)
		plotM[1,1] <- plot.x <- 1
		plotM[1,2] <- plot.y <- 2
		for (i in 2:(states.n-1)){
			print(i)
			print(tmat$versionnumber[i])
			print(length(which(tmat$vnbefore==tmat$versionnumber[i-1])))
			if (length(which(tmat$vnbefore==tmat$versionnumber[i-1]))>1){
				plot.x <- plot.x +1
				plot.y <- plot.y 
			}
			if (length(which(tmat$vnbefore==tmat$versionnumber[i-1]))==1){
				plot.x <- 1
				plot.y <- plot.y + 1
			}
			if (length(which(tmat$vnbefore==tmat$versionnumber[i-1]))<1){
				plot.x <- 1
				plot.y <- plot.y + 1
			}

			plotM[i,1] <- plot.x
			plotM[i,2] <- plot.y
	
		}
		plotM <- print(rbind(c(1,1), plotM))
		dev.new()
		Xscale <- 1/(max(plotM[,1]))
		Yscale <- 1/(max(plotM[,2]))
		plotMscaled<- cbind(plotM[,1]*Xscale-Xscale/2, 1-plotM[,2]*Yscale+Yscale/2)
		print(plotMscaled)
		print(Xscale)
		print(Yscale)
		plotmat(M, name=states.name, curve=.1, box.type="rect", box.size=1/states.n^1.5, box.prop=.5, box.col=col, pos=plotMscaled)																																	
	} 
	else{
		cat("\nOnly one change made, too few to plot... :)\n")
	}
} # end of function cvrgraph
