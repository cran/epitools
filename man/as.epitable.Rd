\name{as.epitable}

\alias{as.epitable}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Create r x 2 contigency table (exposure levels vs. binary outcome)}

\description{
Create r x 2 contigency table for r exposure levels and 2 outcome levels
}

\usage{as.epitable(..., trow = FALSE, tcol = FALSE)}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{...}{see details}
  \item{trow}{"transpose" (reverse order of) rows of table}
  \item{tcol}{"transpose" (reverse order of) columns of table}
}

\details{
  Creates r x 2 table with r exposure levels and 2 outcome levels (No
  vs. Yes). Arguments can be either (1) four or more integers that be
  converted into r x 2 table (the number of integers must be even), (2)
  two categorical vectors (1st vector is exposure with r levels, 2nd
  vector is outcome with 2 levels), (3) r x 2 contingency table, or
  (4) single vector that be converted into r x 2 table (the number of
  integers must be even). The contingency table created by this function
  is usually used for additional analyses, for example, the
  \code{epitab} function.

  Setting \code{trow = TRUE} will reverse the order of the rows, and
  setting \code{tcol = TRUE} will reverse the order of the columns. 
}

\value{Returns r x 2 contingency table, usually for additional analyses.

%-  \item{comp1 }{Description of 'comp1'}
%-  \item{comp2 }{Description of 'comp2'}

}

\references{none}

\author{Tomas Aragon, \email{aragon@berkeley.edu}, \url{http://www.medepi.net/aragon}}

\note{Visit \url{http://www.epitools.net} for the latest.}

%- ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  See also \code{\link{table.margins}}, \code{\link[base]{margin.table}}
}

\examples{
## single vector
dat <- c(88, 20, 555, 347)
as.epitable(dat)

## 4 or more integers
as.epitable(1,2,3,4,5,6)

## single matrix
as.epitable(matrix(1:6, 3, 2))

## two categorical vectors
exposure <- factor(sample(c("Low", "Med", "High"), 100, rep=TRUE),
                   levels=c("Low", "Med", "High"))
outcome <- factor(sample(c("No", "Yes"), 100, rep=TRUE))
as.epitable(exposure, outcome)
as.epitable("Exposure Level"=exposure, "Disease"=outcome)

## reversing row and/or column order
zz <- as.epitable("Exposure Level"=exposure, "Disease"=outcome)
zz
as.epitable(zz, trow = TRUE)
as.epitable(zz, tcol = TRUE)
as.epitable(zz, trow = TRUE, tcol = TRUE)

}
\keyword{manip}
%- \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
