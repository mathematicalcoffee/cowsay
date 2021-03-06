% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/cowsay.r
\name{read.cow.noperl}
\alias{read.cow.noperl}
\title{Reads a Perl-style cowfile where Perl is not installed (crude substitutions)}
\usage{
read.cow.noperl(cowfile, eyes, thoughts, tongue)
}
\arguments{
\item{cowfile}{{string} path to the cowfile}

\item{eyes}{{string} characters to use for the eyes
(only the first two characters are used), e.g. "oo"}

\item{thoughts}{{string} used in drawing the speech bubble, e.g. "o" or "\\"}

\item{tongue}{{string} characters to use for the tongue
(at most the first two characters are used), e.g. "U", "\/"}
}
\value{
a character string with the cow (with embedded newlines), and
  eyes, thoughts, tongue substituted in.
}
\description{
Reads a Perl-style cowfile where Perl is not installed (crude substitutions)
}
\details{
If you are trying to read in a Perl-style cowfile but Perl is not installed,
this tries to read it in and use some EXTREMELY rudimentary regex to parse
the cow:

1. Strip the `$the_cow = <<"EOC";` and `EOC` lines if they are there
2. Replace `$eyes`, `$tongue` and `$thoughts` with the appropriate values

Any lines before `$the_cow << EOC` and after the ending `EOC` are ignored.

Everything else (including any other code) is read verbatim! So you really
don't want to be in the situation where you're using this function.
}
\seealso{
Other cowfile parsing: \code{\link{get.cow}};
  \code{\link{is.perl.cow}}; \code{\link{read.cow.perl}};
  \code{\link{read.cow.plain}}; \code{\link{read.cow.r}}
}

