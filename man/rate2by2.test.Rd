\name{rate2by2.test}

\alias{rate2by2.test}

\title{Comparative tests of independence in rx2 rate tables}

\description{
  Tests for independence where each row of the rx2 table is compared to
  the exposure reference level and test of independence two-sided p
  values are calculated using mid-p xxact, and normal approximation.
  }

\usage{
rate2by2.test(x, y = NULL, rr = 1, 
              rev = c("neither", "rows", "columns", "both"))
}

\arguments{
  \item{x}{
    input data can be one of the following: r x 2 table where first
    column contains disease counts and second column contains person
    time at risk; or a single numeric vector for counts followed by
    person time at risk
  }
  \item{y}{
    vector of person-time at risk; if provided, x must be a
    vector of disease counts
  }
  \item{rr}{rate ratio reference value (default is no association)}
  \item{rev}{
    reverse order of "rows", "colums", "both", or "neither" (default)
  }
}

\details{
  Tests for independence where each row of the rx2 table is compared to
  the exposure reference level and test of independence two-sided p
  values are calculated using mid-p xxact, and normal approximation.

  This function expects the following table struture:
  \preformatted{
                    counts   person-time
    exposed=0 (ref)   n00        t01
    exposed=1         n10        t11	
    exposed=2         n20        t21
    exposed=3         n30        t31
  }
  The reason for this is because each level of exposure is compared to
  the reference level.

  If the table you want to provide to this function is not in the
  preferred form, just use the \code{rev} option to "reverse" the rows,
  columns, or both. If you are providing categorical variables (factors
  or character vectors), the first level of the "exposure" variable is
  treated as the reference. However, you can set the reference of a
  factor using the \code{\link[stats]{relevel}} function.

  Likewise, each row of the rx2 table is compared to the exposure
  reference level and test of independence two-sided p values are
  calculated using mid-p exact method and normal approximation.

  This function can be used to construct a p value function
  by testing the MUE to the null hypothesis (rr=1) and alternative
  hypotheses (rr not equal to 1) to calculate two-side mid-p exact p
  values. For more detail, see Rothman.
}

\value{
  \item{x}{table that was used in analysis}
  \item{p.value}{p value for test of independence}
}

\references{
  Kenneth J. Rothman and Sander Greenland (2008), Modern Epidemiology,
  Lippincott Williams and Wilkins Publishers

  Kenneth J. Rothman (2002), Epidemiology: An Introduction, Oxford
  University Press
} 

\author{Tomas Aragon, \email{aragon@berkeley.edu},
  \url{http://www.phdata.science}}



\seealso{
  \code{\link{rateratio}},
}

\examples{
##Examples from Rothman 1998, p. 238
bc <- c(Unexposed = 15, Exposed = 41)
pyears <- c(Unexposed = 19017, Exposed = 28010)
dd <- matrix(c(41,15,28010,19017),2,2)
dimnames(dd) <- list(Exposure=c("Yes","No"), Outcome=c("BC","PYears"))
##midp
rate2by2.test(bc,pyears)
rate2by2.test(dd, rev = "r")
rate2by2.test(matrix(c(15, 41, 19017, 28010),2,2))
rate2by2.test(c(15, 41, 19017, 28010))
}

\keyword{htest}
