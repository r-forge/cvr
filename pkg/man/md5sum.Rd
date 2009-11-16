\name{md5sum}
\alias{md5sum}
\alias{ls.inv}
\title{ cvr utilities }
\description{
  Auxiliary functions for the cvr package
}
\usage{
md5sum(x)
ls.inv(pattern)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an R object (may also be ls() ) }
  \item{pattern}{ a pattern to be searched in the R workspace }
}
\details{
  md5sum is a wrapper to the function digest in the digest package; ls.inv returns a list of objects which DO NOT match a given pattern.
}
\value{
  md5sum resturns the md5sum of an R object; ls.inv return the list of non-matching R objects.
}
\author{ Andrea De Cesarei andrea.dece@gmail.com }
