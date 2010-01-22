dircheck <- function(){
  if (file.access(getwd(), 6) < 0){
    cat("\nYou have no write permission in this folder:\n")
    cat(getwd())
    cat("\nPlease) set another working directory.\n")
    return(FALSE)
  }
  else{
    return(TRUE)
  }
    
} # end of function dircheck()
md5sum <- function() {
  if (dircheck()==TRUE){
        save(file = "tmpWorkspace", list=ls(envir= .GlobalEnv))
	md5s <- digest('tmpWorkspace', file=TRUE)
        file.remove('tmpWorkspace')
	return(md5s)
      }
} # end of function md5sum

cvrinit <- function(){
  if (dircheck()==TRUE){
	if (!exists('.cvrvn', envir= .GlobalEnv)){
		assign(".cvrvn", 0, envir = .GlobalEnv)
	}
	if (!file.exists('transformations.cvr')){
		file.create('transformations.cvr')
		}
  }
} # end of function cvrinit

cvrread <- function(){
	transformations <- c()
        rawtext <- c()
	cvrinit()
        if (file.info('transformations.cvr')$size >0){
           transformations <- read.table("transformations.cvr", allowEscapes=T, stringsAsFactors=F)
           names(transformations) <- c('versionnumber', 'vnbefore', 'md5before', 'expression', 'md5after', 'comments')
        }
        else{
          transformations <- data.frame()
        }        
	return(transformations)
} # end of function cvrread

cvrlog <- function(md5before, expression, md5after, comments){
  if (dircheck()==TRUE){
	cvrinit()
	tr<- cvrread()
	vnbefore<- get('.cvrvn', envir=.GlobalEnv)
	versionnumber <- floor(runif(1)*10000)
	while(versionnumber %in% tr$versionnumber){ versionnumber <- floor(runif(1)*10000) }
        cat(paste(versionnumber, '\t', vnbefore, '\t', md5before, '\t', deparse(expression), '\t', md5after, '\t', comments, '\n',sep=''), file='cvr.temp')
	file.append('transformations.cvr', 'cvr.temp')
	file.remove('cvr.temp')
	assign(".cvrvn", versionnumber , envir = .GlobalEnv)
      }
} # end of function cvrlog

cvrdo <- function(xpression, comment='uncommented'){
  if (dircheck()==TRUE){
	cvrinit()
	origmd5 <- md5sum()
        ThisErr <- try(eval(parse(text=xpression), envir = .GlobalEnv), silent=TRUE)
        if (class(ThisErr)=="try-error"){
          cat("There was an error!\n")
          cat("R says:\n")
          cat(geterrmessage())
          cat("cvrdo interrupted, no command executed, no data logged.\n")
        }
        else{ # if not error
          if (md5sum()==origmd5) {
		#cat("\nunchanged!\n")
		}
          else { 
		#cat("\nChanged!\n")
		cvrlog(origmd5 , xpression, md5sum(), comment)
          }
        } # if new error
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


