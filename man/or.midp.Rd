\name{or.midp}

\alias{or.midp}

\title{Odds ratio estimation and confidence intervals using mid-p method}

\description{
  Calculates odds ratio by median-unbiased estimation and exact
  confidence interval using the mid-p method (Rothman 1998).  
}

\usage{
or.midp(x, conf.level = 0.95, byrow = TRUE, interval = c(0, 1000))
}

\arguments{
  \item{x}{input data can be 2x2 matrix or vector of length 4}
  \item{conf.level}{confidence level (default is 0.95)}
  \item{byrow}{integer vectors are read in row-wise (default)}
  \item{interval}{interval for the \code{\link{uniroot}} that finds the
    odds ratio median-unbiased estimate and mid-p exact confidence
    interval for \code{oddsratio.midp}}
}

\details{
  Calculates odds ratio by median-unbiased estimation and exact
  confidence interval using the mid-p method (Rothman 1998, p. 251).

  This function expects the following 2x2 table struture:
  \preformatted{
              exposed   not exposed
    disease  	 a1	    a0	      			
    no disease   b1	    b0
  }
  or a numeric vector of the form c(a1, a0, b1, b0).
  
  This function is used by \code{\link{oddsratio.midp}}.
}


\value{
  \item{x}{table that was used in analysis} 
  \item{data}{same table as \code{x} but with marginal totals} 
  \item{estimate}{median unbiased odds ratio} 
  \item{conf.level}{confidence level used} 
}

\references{
  Kenneth J. Rothman and Sander Greenland (1998), Modern Epidemiology,
  Lippincott-Raven Publishers
} 

\author{Tomas Aragon, \email{aragon@berkeley.edu},
  \url{http://www.phdata.science}}



\seealso{
  \code{\link{oddsratio}}
}

\examples{
##rothman p. 243
z1 <- matrix(c(12,2,7,9),2,2,byrow=TRUE)
z2 <- z1[2:1,2:1]
##jewell p. 79
z3 <- matrix(c(347,555,20,88),2,2,byrow=TRUE)
z4 <- z3[2:1,2:1]
or.midp(z1)
or.midp(z2)
or.midp(z3)
or.midp(z4)
}

\keyword{models}
%- \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
