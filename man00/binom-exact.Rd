\name{binom.exact}

\alias{binom.exact}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Exact confidence intervals for binomial counts or proportions}

\description{
  Calculates exact confidence intervals for binomial counts or proportions
}

\usage{
binom.exact(x, n, conf.level = .95)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{x}{number of successes in n trials}
  \item{n}{number of Bernoulli trials}
  \item{conf.level}{confidence level (default = 0.95)}
}

\details{
  Calculates exact confidence intervals for binomial counts or proportions.
}

\value{
  This function returns a n x 6 matrix with the following colnames:
  \item{x}{number of successes in n trials}
  \item{n}{number of Bernoulli trials}
  \item{proportion}{proportion = x/n}
  \item{conf.level}{confidence level}
  \item{lci}{lower confidence interval limit}
  \item{uci}{upper confidence interval limit}
}

\references{none}

\author{Tomas Aragon, \email{aragon@berkeley.edu}, \url{http://www.medepi.net/aragon}}

\note{none}

%- ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{binom.test}}, \code{\link{pois.exact}}
}

\examples{
binom.exact(1:10, seq(10, 100, 10))
}

\keyword{models}
%- \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
