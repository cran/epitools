\name{ratetable}

\alias{ratetable}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
  Create r x 2 count and person-time table for calculating rates
}

\description{
  Create r x 2 count and person-time table for calculating rates
}

\usage{
ratetable(..., byrow = FALSE,
          rev = c("neither", "rows", "columns", "both"))
}


\arguments{
  \item{...}{see details}
  \item{byrow}{Default is TRUE and single vector or collection of numbers is
  read in row-wise. Set to FALSE to read in column-wise.}  
  \item{rev}{
    reverse order of "rows", "colums", "both", or "neither" (default)
  }
}

\details{
  Creates r x 2 table with r exposure levels and 2 columns (counts and
  person-time exposed). Arguments can be one of the following:

  (1) r x 2 table of the following form:
  \preformatted{
                         Outcome
	     Exposure    cases pyears
	     E = 0 (ref)   a     PT0
	     E = 1         b     PT1
	   }
	     
  (2) Two numeric vectors: 1st should be vector of counts, and the 2nd
  vector should be vector of person-times at risk. For example,
  \preformatted{
    cases  <- c(a, b)
    pyears <- c(PT0, PT1)
  }
  (3) >= 4 numbers in the following order: a, PT0, b, PT1

  (4) One numeric vector of the following form: c(a, PT0, b, PT1)
}

\value{
  Returns r x 2 rate table, usually for additional analyses.
}

\references{none}

\author{Tomas Aragon, \email{aragon@berkeley.edu},
  \url{http://www.phdata.science}} 



\seealso{
  \code{\link{epitable}}
}

\examples{
##Breast cancer cases from radiation treatment for tuberculosis
##Rothman 1998, p. 238
bc0 <- 15
bc1 <- 41
py0 <- 19017
py1 <- 28010

##4 numbers
ratetable(bc0, py0, bc1, py1)

##1 vector
dat <- c(bc0, py0, bc1, py1)
ratetable(dat)

##2 vectors
cases <- c(bc0, bc1)
pyears <- c(py0, py1)
ratetable(bc.cases = cases, person.years = pyears)

##1 matrix
r238 <- matrix(c(41, 28010, 15, 19017), 2, 2)
dimnames(r238) <- list(c("BC cases", "Person-years"),
                       "Radiation" = c("Yes", "No"))
r238
r238b <- t(r238)
r238b
ratetable(r238b, rev = "r")

}
\keyword{manip}
