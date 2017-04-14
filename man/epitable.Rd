\name{epitable}

\alias{epitable}

\title{Create r x c contigency table (exposure levels vs. binary outcome)}

\description{
Create r x c contigency table for r exposure levels and c outcome levels
}

\usage{
epitable(..., ncol =2, byrow = TRUE,
         rev = c("neither", "rows", "columns", "both"))
}

\arguments{
  \item{...}{see details}
  \item{ncol}{
    number of columns = 2 (default) when a table is constructed from a
    vector or sequence of numbers 
  }
  \item{byrow}{Default is TRUE and single vector or collection of numbers is
  read in row-wise. Set to FALSE to read in column-wise.}
  \item{rev}{
    reverse order of "rows", "colums", "both", or "neither" (default)
  }
}

\details{
  Creates r x 2 table with r exposure levels and 2 outcome levels (No
  vs. Yes). Arguments can be one of the following:

  (1) four or more integers that be converted into r x 2 table (the
  number of integers must be even),

  (2) two categorical vectors (1st vector is exposure with r levels, 2nd
  vector is outcome with 2 levels), 

  (3) r x 2 contingency table, or
  
  (4) single vector that be converted into r x 2 table (the number of
  integers must be even).

  The contingency table created by this function is usually used for
  additional analyses, for example, the \code{epitab} function.
}

\value{
  Returns r x 2 contingency table, usually for additional analyses.
}

\references{none}

\author{Tomas Aragon, \email{aragon@berkeley.edu},
  \url{http://www.phdata.science}} 



\seealso{
  \code{\link{epitable}}
}

\examples{
## single vector
dat <- c(88, 20, 555, 347)
epitable(dat)

## 4 or more integers
epitable(1,2,3,4,5,6)

## single matrix
epitable(matrix(1:6, 3, 2))

## two categorical vectors
exposure <- factor(sample(c("Low", "Med", "High"), 100, rep=TRUE),
                   levels=c("Low", "Med", "High"))
outcome <- factor(sample(c("No", "Yes"), 100, rep=TRUE))
epitable(exposure, outcome)
epitable("Exposure"=exposure, "Disease"=outcome)

## reversing row and/or column order
zz <- epitable("Exposure Level"=exposure, "Disease"=outcome)
zz
epitable(zz, rev = "r")
epitable(zz, rev = "c")
epitable(zz, rev = "b")

}
\keyword{manip}
