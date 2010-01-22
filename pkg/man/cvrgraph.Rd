\name{cvrgraph}
\alias{sons}
\alias{cousins}
\alias{mother}
\alias{generation}
\alias{bronum}
\title{plots a graphical representation of CVR session}
\description{
  reads the CVR log file and plots a graphical representation of all commands which have been logged.
}
\usage{
cvrgraph()
}
%- maybe also 'usage' for other objects documented here.
\details{
  A list of all changes can be obtained with cvrread(). this is an easier-to-understand representation.
}
\value{
 No new values are created, nor the dataset will be rewinded to any previous state.
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
cvrgraph()
}
