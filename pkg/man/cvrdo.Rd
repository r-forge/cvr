\name{cvrdo}
\alias{cvrdo}
\title{Executes and log a command}
\description{
  Executes a R command (one line). If R workspace is changed after the execution of this command, the line which has been run is stored in the logfile.
}
\usage{
cvrdo(xpression, comment)
}
\arguments{
  \item{xpression}{ any R command, in ". If " have to be used, put a \ before it. }
  \item{comment}{ an optional comment, which will be stored in the logfile· }
}
\details{
  expression is evaluated. In case there is an error, the execution of cvrdo is stopped and the user is warned. In case it works and modifies the R workspace, expression is logged in the transformation file. If there is no modification to the R workspace, the expression is evaluated but not logged. 
}
\author{ Andrea De Cesarei andrea.dece@gmail.com }

\seealso{ cvrlist, cvrrewind }
\examples{
rm(list=ls())
cvrdo("dummyvar <- 1:10")
print(dummyvar)
rm(dummyvar)
ls()
transformation.list <- cvrread()
cvrrewind(transformation.list$versionnumber[nrow(transformation.list)])
print(dummyvar)
}
