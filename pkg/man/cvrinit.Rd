\name{cvrinit}
\alias{cvrinit}
\alias{cvrlog}
\alias{cvrread}
\title{ cvr low-level functions }
\description{
  low-level functions to create, populate and read a log file of all transformations happened to the R workspace.
}
\usage{
cvrinit()
cvrlog(md5before, expression, md5after, comments)
cvrread()
}
\arguments{
  \item{md5before}{ md5sum of the R workspace, before the execution of the expression }
  \item{expression}{ a valid R command (one line) }
  \item{md5after}{ md5sum of the R workspace, after the execution of the expression }
  \item{comments}{ a human-readable description of what the R expression did. }
}
\value{
  cvrread returns a list of dta frame containing a list of all transformations which were logged, identified by a versionnumber.
}
\author{ Andrea De Cesarei andrea.dece@gmail.com }

\seealso{ cvrdo, cvrrewind }

