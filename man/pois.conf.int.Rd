\name{pois.conf.int}

\alias{pois.exact}
\alias{pois.daly}
\alias{pois.byar}
\alias{pois.approx}

\title{Confidence intervals for Poisson counts or rates}

\description{
  Calculates confidence intervals for Poisson counts or rates
}

\usage{
pois.exact(x, pt = 1, conf.level = 0.95)
pois.daly(x, pt = 1, conf.level = 0.95)
pois.byar(x, pt = 1, conf.level = 0.95)
pois.approx(x, pt = 1, conf.level = 0.95)
}

\arguments{
  \item{x}{count or vector of counts}
  \item{pt}{person-time at risk (default = 1) or vector of person-times}
  \item{conf.level}{confidence level (default = 0.95) }
}

\details{
  These functions calculate confidence intervals for a Poisson count or
  rate using an exact method (\code{pois.exact}), gamma distribution
  (\code{pois.daly}), Byar's formula (\code{pois.byar}), or normal
  approximation to the Poisson distribution (\code{pois.approx}).

  To calculate an exact confidence interval for a crude rate (count
  divided by person-time at risk), set \code{pt} equal to the
  person-time at risk. Both \code{x} and \code{pt} can be either a
  number or a vector of numbers.

  The \code{pois.daly} function gives essentially identical answers to
  the \code{pois.exact} function except when x = 0. When x = 0, for the
  upper confidence limit \code{pois.exact} returns 3.689 and
  \code{pois.daly} returns 2.996.
}

\value{
  This function returns a n x 6 matrix with the following colnames:
  \item{x}{Poisson count}
  \item{pt}{person-time at risk}
  \item{rate}{crude rate = x/pt}
  \item{lower}{lower confidence interval limit}
  \item{upper}{upper confidence interval limit}
  \item{conf.level}{confidence level}
}

\references{
  Tomas Aragon, et al. Applied Epidemiology Using R. Available at
  \url{http://www.phdata.science} 

  Leslie Day (1992), "Simple SAS macros for the calculation of exact 
  binomial and Poisson confidence limits." Comput Biol Med,
  22(5):351-361
  
  Kenneth Rothman (2002), Epidemiology: An Introduction, Oxford
  University Press, 1st Edition.
}

\author{
  Tomas Aragon, \email{aragon@berkeley.edu},
  \url{https://repitools.wordpress.com/};
  with contributions by Francis Dimzon, \email{fdimzon@yahoo.com};
  with contributions by Scott Nabity, \email{scott.nabity@sfdph.org}
}




\seealso{
\code{\link{binom.exact}}
}

\examples{
pois.exact(1:10)
pois.exact(1:10, 101:110)
pois.daly(1:10)
pois.daly(1:10, 101:110)
pois.byar(1:10)
pois.byar(1:10, 101:110)
pois.approx(1:10)
pois.approx(1:10, 101:110)
}

\keyword{univar}
