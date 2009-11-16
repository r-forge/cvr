\name{cvr-package}
\alias{cvr-package}
\alias{cvr}
\docType{package}
\title{
very simple current versioning system for R
cvr
}
\description{
A very simple current versioning system for R. It can log commands to a file, read logs and restore data as they were at any time point.
}
\details{
\tabular{ll}{
Package: \tab cvr\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2009-09-28\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
}
Using the functions cvrdo, cvrread and cvrrewing it is possible to temporally navigate the R workspace back and forth. This can be used to build a report of which transformation have been done to a dataset (e.g., input data, normalization, etc. ) or to create multiple analysis paths. 
}
\author{
Andrea De Cesarei
Maintainer: Andrea De Cesarei <andrea.dece@gmail.com>
}
\examples{
rm(list=ls())
cvrdo("dummyvar <- 1:10")
print(dummyvar)
rm(dummyvar)
ls()
transformation.list <- cvrread()
cvrrewind(transformation.list$versionnumber[1])
print(dummyvar)
}
