\name{pois.exact}

\alias{pois.exact}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Exact confidence interval for Poisson counts or rates}

\description{
  Calculates exact confidence interval for Poisson counts or rates
}

\usage{
pois.exact(x, pt = 1, conf.level = .95)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{x}{count or vector of counts}
  \item{pt}{person-time at risk (default = 1)}
  \item{conf.level}{confidence level (default = 0.95)}
}

\details{
  By default, this function calculates an exact confidence interval for
  a Poisson count \code{x} (pt = 1). To calculate an exact confidence
  interval for a crude rate (count divided by person-time at risk), set
  \code{pt} equal to the person-time at risk. Both \code{x} and
  \code{pt} can be either a number or a vector of numbers.  
}

\value{
  This function returns a n x 6 matrix with the following colnames:
  \item{x}{Poisson count}
  \item{pt}{person-time at risk}
  \item{rate}{crude rate = x/pt}
  \item{conf.level}{confidence level}
  \item{lci}{lower confidence limit}
  \item{uci}{upper confidence limit}
}

\references{none}

\author{Tomas Aragon, \email{aragon@berkeley.edu}, \url{http://www.medepi.net/aragon}}

\note{Created 2004-08-06}

%- ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{binom.exact}}, \code{\link{binom.test}}
}

\examples{
pois.exact(1:10)
}

\keyword{univar}
%- \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
