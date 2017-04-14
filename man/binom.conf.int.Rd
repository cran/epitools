\name{binom.conf.int}

\alias{binom.exact}
\alias{binom.wilson}
\alias{binom.approx}

\title{Confidence intervals for binomial counts or proportions}

\description{
  Calculates confidence intervals for binomial counts or proportions
}

\usage{
binom.exact(x, n, conf.level = 0.95)
binom.wilson(x, n, conf.level = 0.95)
binom.approx(x, n, conf.level = 0.95)
}

\arguments{
  \item{x}{number of successes in n trials, can be a vector}
  \item{n}{number of Bernoulli trials, can be a vector}
  \item{conf.level}{confidence level (default = 0.95), can be a vector}
}

\details{
  The function, \code{binom.exact}, calculates exact confidence intervals
  for binomial counts or proportions. This function uses R's
  \code{binom.test} function; however, the arguments to this function
  can be numeric vectors of any length.

  The function, \code{binom.wilson}, calculates confidence intervals for
  binomial counts or proportions using Wilson's formula which
  approximate the exact method. The arguments to this function
  can be numeric vectors of any length (Rothman).

  The function, \code{binom.approx}, calculates confidence intervals for
  binomial counts or proportions using a normal approximation to the
  binomial distribution. The arguments to this function can be numeric
  vectors of any length. 
}

\value{
  This function returns a n x 6 matrix with the following colnames:
  \item{x}{number of successes in n trials}
  \item{n}{number of Bernoulli trials}
  \item{prop}{proportion = x/n}
  \item{lower}{lower confidence interval limit}
  \item{upper}{upper confidence interval limit}
  \item{conf.level}{confidence level}
}

\references{
  Tomas Aragon, et al. Applied Epidemiology Using R. Available at
  \url{http://www.phdata.science}

  Kenneth Rothman (2002), Epidemiology: An Introduction, Oxford
  University Press, 1st Edition.
}

\author{Tomas Aragon, \email{aragon@berkeley.edu},
  \url{http://www.phdata.science}}



\seealso{
\code{\link{pois.exact}}, \code{\link[stats]{binom.test}}
}

\examples{
binom.exact(1:10, seq(10, 100, 10))
binom.wilson(1:10, seq(10, 100, 10))
binom.approx(1:10, seq(10, 100, 10))
}

\keyword{univar}
