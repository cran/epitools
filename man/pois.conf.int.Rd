\name{pois.conf.int}

\alias{pois.conf.int}
\alias{pois.exact}
\alias{pois.byar}
\alias{pois.approx}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{Confidence intervals for Poisson counts or rates}

\description{
  Calculates confidence intervals for Poisson counts or rates
}

\usage{
pois.exact(x, pt = 1, conf.level = 0.95)
pois.byar(x, pt = 1, conf.level = 0.95)
pois.approx(x, pt = 1, conf.level = 0.95)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{x}{count or vector of counts}
  \item{pt}{person-time at risk (default = 1) or vector of person-times}
  \item{conf.level}{confidence level (default = 0.95) }
}

\details{
  By default, \code{pois.exact} calculates an exact confidence interval
  for a Poisson count \code{x} (\code{pt = 1}). To calculate an exact
  confidence interval for a crude rate (count divided by person-time at
  risk), set \code{pt} equal to the person-time at risk. Both \code{x}
  and \code{pt} can be either a number or a vector of numbers.

  By default, \code{pois.byar} calculates a confidence interval for a
  Poisson count \code{x} (\code{pt = 1}) using Byar's formula which
  approximates the exact method. To calculate a confidence interval for
  a crude rate (count divided by person-time at risk), set \code{pt} equal
  to the person-time at risk. Both \code{x} and \code{pt} can be either
  a number or a vector of numbers (Rothman).

  By default, \code{pois.approx} calculates an confidence interval for a
  Poisson count \code{x} (\code{pt = 1}) using a normal
  approximation to the Poisson distribution. To calculate a confidence
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
  \item{lci}{lower confidence interval limit}
  \item{uci}{upper confidence interval limit}
}

\references{
  Tomas Aragon, et al. Applied Epidemiology Using R. Available at
  \url{http://www.epitools.net} 

  Kenneth Rothman (2002), Epidemiology: An Introduction, Oxford
  University Press, 1st Edition.
}

\author{Tomas Aragon, \email{aragon@berkeley.edu}, \url{http://www.medepi.net/aragon}}

\note{Visit \url{http://www.epitools.net} for the latest.}

%- ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{binom.conf.int}}
}

\examples{
pois.exact(1:10)
pois.byar(1:10)
pois.approx(1:10)

}

\keyword{univar}
%- \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
