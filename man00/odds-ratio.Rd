\name{odds.ratio}

\alias{odds.ratio}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Risk ratio estimation and confidence intervals}

\description{
  Calculates odds ratio and confidence interval with and without a
  smallsample adjustment
}

\usage{
odds.ratio(ca1, co1, ca0, co0, conf.level = 0.95)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{ca1}{number of exposed cases}
  \item{co1}{number of exposed controls}
  \item{ca0}{number of unexposed cases}
  \item{co0}{number of unexposed controls}
  \item{conf.level}{confidence level (default = 0.95)}
}

\details{
  This function Calculates the odds ratio and confidence interval, with
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
Jewell NP. Statistics for Epidemiology, 1st Edition, 2004, Chapman & Hall, pp. 73-81
}

\author{Tomas Aragon, \email{aragon@berkeley.edu}, \url{http://www.medepi.net/aragon}}

\note{Created 2004-08-06}

%- ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  See also \code{\link{risk.ratio}}, \code{\link{epitab}}
}

\examples{
odds.ratio(347, 555, 20, 88)
}

\keyword{models}
%- \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
