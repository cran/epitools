\name{risk.ratio}

\alias{risk.ratio}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Risk ratio estimation and confidence intervals}

\description{
  Calculates risk ratio and confidence interval with and without a
  smallsample adjustment
}

\usage{
risk.ratio(x1, n1, x0, n0, conf.level = 0.95)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{x1}{number of events among the exposed}
  \item{n1}{number of total exposed}
  \item{x0}{number of events among the non-exposed}
  \item{n0}{number of total unexposed}
  \item{conf.level}{confidence level (default = 0.95)}
}
\details{
  This function Calculates the risk ratio and confidence interval, with
  and without a small sample adjustment, for a 2 x 2 table.
}


\value{
  \item{$data}{Original data with marginal totals included}
  \item{$p.value}{Fishers Exact Test from 'fisher.test'}
  \item{$conf.level}{Confidence level}
  \item{$crude}{Estimate and confidence limits}
  \item{$adjusted}{Estimate and confidence limits with small sample adjustment}
}

\references{
Jewell NP. Statistics for Epidemiology, 1st Edition, 2004, Chapman & Hall, pp. 81-82
}

\author{Tomas Aragon, \email{aragon@berkeley.edu}, \url{http://www.medepi.net/aragon}}

\note{Created 2004-08-06}

%- ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  See also \code{\link{odds.ratio}}, \code{\link{epitab}}
}

\examples{
risk.ratio(178, 1589, 79, 1565)
}

\keyword{models}
%- \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
