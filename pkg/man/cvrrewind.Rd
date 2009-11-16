\name{cvrrewind}
\alias{cvrrewind}
\title{ rewind R workspace }
\description{
  Rewind R workspace to a timepoint which has been logged in the cvr log file. 
}
\usage{
cvrrewind(versionnumber)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{versionnumber}{ the version number of the timepoint where R should be rewinded. }
}
\details{
  A list of all changes can be obtained with cvrread(). The first column contains the version numbers.
}
\value{
 The whole R workspace is rewinded to the timepoint identified by versionnumber. ALL OBJECTS in the current workspace WILL BE LOST. 
}
\author{ Andrea De Cesarei andrea.dece@gmail.com }

\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
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
