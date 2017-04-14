\name{kapmeier}

\alias{kapmeier}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Implements product-limit (Kaplan-Meier) method}

\description{
Implements product-limit (Kaplan-Meier) method for time-to-event data
with censoring.
}

\usage{
kapmeier(time, status)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{time}{numeric vector with individual observation times}
  \item{status}{integer vector indicating status at the end of the
    observation time: 1 = event, 0 = censored}
}

\details{
  This function implements the product-limit method for estimating
  survival probability for time-to-event data with censoring:
  
  \preformatted{
    S(t) = product[(nj - dj) / nj] for all tj <= t,
  }
  
  where \code{tj} are event times (i.e., times at which one or more events
  occur), \code{nj} are the number at risk at time \code{tj} (by convention,
  subjects censored at time \code{tj} are considered at-risk and included in
  \code{nj}), and \code{dj} are the number of events at time \code{tj}.
  
  A primary purpose of this function was to demonstrate the use of
  available R functions to implement a simple statistical method. For example,
  \code{kapmeier} uses \code{sort}, \code{order}, \code{duplicated},
  \code{tapply}, \code{unique}, \code{cumprod}, \code{cbind}, and
  \code{dimnames}. Studying this function carefully helps one understand
  and appreciate the utility of R functions to implement simple methods.
  
  For serious survival analysis load the \code{survival} package. The
  \code{survfit} function in this package implements the product-limit method
  and much more. See examples.
}

\value{Returns an individual-level data frame}

\references{
Selvin S. Statistical Analysis of Epidemiologic Data (Monographs in
Epidemiology and Biostatistics, V. 35). Oxford University Press; 3rd
edition (May 1, 2004)
}

\author{Tomas Aragon, \email{aragon@berkeley.edu}, \url{http://www.phdata.science}}



%- ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  See also \code{\link[survival]{survfit}}
}

\examples{
##Product-limit method using 'kapmeier' function
tt <- c(1,17,20,9,24,16,2,13,10,3)
ss <- c(1,1,1,1,0,0,0,1,0,1)
round(kapmeier(tt, ss), 3)
}

\keyword{survival}
%- \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
