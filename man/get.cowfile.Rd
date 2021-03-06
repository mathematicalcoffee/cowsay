% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/cowsay.r
\name{get.cowfile}
\alias{get.cowfile}
\title{Looks for a particular cow.}
\usage{
get.cowfile(cow)
}
\arguments{
\item{cow}{{string} path to a cow file to use, or name of a pre-installed cow (see
\code{\link{list.cows}})}
}
\value{
{character} the path to the first matching cow on the cowpath, or
 NULL if not found.
}
\description{
Looks for cows in this order:
}
\details{
1. If the input is a filename with extension 'cow' or 'rcow' and exists, we use it;
2. The corresponding R cow (`{cow}.rcow`) in the cowpath in order;
3. a Perl/plain cow with extension '.cow' (e.g. you downloaded a cow pack
    and have not ported them to rcows or plaincows yet)

Note that the `cow` should NOT have the extension
 (i.e. "three-eyes", not "three-eyes.cow")
}
\examples{
get.cowfile('default')
get.cowfile('three-eyes')
}
\seealso{
Other cow styles: \code{\link{cow.styles}};
  \code{\link{get.cowpaths}}; \code{\link{list.cows}}
}

