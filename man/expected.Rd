\name{expected}

\alias{expected}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Expected values in a table}

\description{
  Assuming independence, calculates expected values in a matrix or
  table.
}

\usage{
expected(x)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{x}{is a matrix or table}
}

\details{
  Assuming independence, calculates expected values in a matrix or
  table.
}

\value{
  expected values
}

\references{
  Steve Selvin (2001), Epidemiologic Analysis: A Case-Oriented Approach,
  Oxford University Press
}

\author{Tomas Aragon, \email{aragon@berkeley.edu}, \url{http://www.phdata.science}}



%- ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  See also \code{\link[base]{margin.table}}
}

\examples{
##From Selvin, 2001, p.2
##year = year of birth
##one+ = one or more congenital defects
##one = one congenital defect
dat <- c(369, 460, 434, 434, 506, 487, 521, 518, 526, 488,
605, 481, 649, 477, 733, 395, 688, 348)

##observed
oi <- matrix(dat, nrow =2)
colnames(oi) <- 1983:1991
rownames(oi) <- c("one+", "one")

##expected
ei <- expected(oi)

##Pearson chi-square test 
chi2.T <- sum((oi - ei)^2/ei)
pchisq(q = chi2.T, df = 8, lower.tail = FALSE)

}
\keyword{manip}
%- \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
