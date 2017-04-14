\name{table.margins}

\alias{table.margins}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Marginal totals of a table}

\description{
Calculates marginal totals of a matrix, table, or array.
}

\usage{
table.margins(x)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{x}{is a matrix, table, or array}
}

\details{
Calculates marginal totals of a matrix, table, or array.
}

\value{Returns original object with marginal totals

%-  \item{comp1 }{Description of 'comp1'}
%-  \item{comp2 }{Description of 'comp2'}

}

\references{none}

\author{Tomas Aragon, \email{aragon@berkeley.edu}, \url{http://www.phdata.science}}



%- ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  See also \code{\link[base]{margin.table}}
}

\examples{
x <- matrix(1:4, 2, 2)
table.margins(x)

}
\keyword{manip}
%- \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
