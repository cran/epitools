\name{ormidp.test}

\alias{ormidp.test}

\title{odds ratio test for independence (p value) for a 2x2 table}

\description{
  Test for independence using the mid-p method (Rothman 1998)
  }

  \usage{
ormidp.test(a1, a0, b1, b0, or = 1)
}

\arguments{
  \item{a1}{
    number of exposed cases
  }
  \item{a0}{
    number of unexposed cases
  }
  \item{b1}{
    number of exposed noncases (controls)
  }
  \item{b0}{
    number of unexposed noncases (controls)
  }
  \item{or}{
    odds ratio reference value (default is no association)
  }
}

\details{
  Test for independence using the mid-p method (Rothman 1998)
}


\value{
  \item{$one.sided}{one-sided p value}
  \item{$two.sided}{two-sided p value}
}

\references{
  Kenneth J. Rothman and Sander Greenland (1998), Modern Epidemiology,
  Lippincott-Raven Publishers

  Kenneth J. Rothman (2002), Epidemiology: An Introduction, Oxford
  University Press

  Nicolas P. Jewell (2004), Statistics for Epidemiology, 1st Edition,
  2004, Chapman & Hall, pp. 73-81 
} 

\author{Tomas Aragon, \email{aragon@berkeley.edu},
  \url{http://www.phdata.science}}



\seealso{
  \code{\link{tab2by2.test}}, \code{\link{oddsratio}},
  \code{\link{riskratio}} 
}

\examples{
##rothman p. 243
ormidp.test(12,2,7,9)

##jewell p. 79
ormidp.test(347,555,20,88)
}

\keyword{htest}
