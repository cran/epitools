\name{tab2by2.test}

\alias{tab2by2.test}

\title{Comparative tests of independence in rx2 contigency tables}

\description{
  Tests for independence where each row of the rx2 table is compared to
  the exposure reference level and test of independence two-sided p
  values are calculated using mid-p exact, Fisher's Exact, and the
  chi-square test. 
  }

\usage{
tab2by2.test(x, y = NULL,
             correction = FALSE,
             rev = c("neither", "rows", "columns", "both"))
}

\arguments{
  \item{x}{input data can be one of the following: r x 2 table, vector
    of numbers from a contigency table (will be transformed into r x 2
    table in row-wise order), or single factor or character vector that
    will be combined with \code{y} into a table.}
  \item{y}{
    single factor or character vector that will be combined with
    \code{x} into a table (default is NULL)
  }
  \item{correction}{
    set to TRUE for Yate's continuity correction (default is FALSE)
  }
  \item{rev}{
    reverse order of "rows", "colums", "both", or "neither" (default)
  }
}

\details{
  Tests for independence where each row of the rx2 table is compared to
  the exposure reference level and test of independence two-sided p
  values are calculated using mid-p exact, Fisher's Exact, and the
  chi-square test. 

  This function expects the following table struture:
  \preformatted{
                    disease=0   disease=1
    exposed=0 (ref)    n00         n01
    exposed=1          n10         n11	
    exposed=2          n20         n21
    exposed=3          n30         n31
  }
  The reason for this is because each level of exposure is compared to
  the reference level.

  If you are providing a 2x2 table order does not matter:

  If the table you want to provide to this function is not in the
  preferred form, just use the \code{rev} option to "reverse" the rows,
  columns, or both. If you are providing categorical variables (factors
  or character vectors), the first level of the "exposure" variable is
  treated as the reference. However, you can set the reference of a
  factor using the \code{\link[stats]{relevel}} function.

  Likewise, each row of the rx2 table is compared to the exposure
  reference level and test of independence two-sided p values are
  calculated using mid-p exact, Fisher's Exact, Monte Carlo simulation,
  and the chi-square test. 
}


\value{
  \item{x}{table that was used in analysis}
  \item{p.value}{p value for test of independence}
  \item{correction}{logical specifying if continuity correction was used}
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
  \code{\link{oddsratio}}, \code{\link{riskratio}}
}

\examples{

##Case-control study assessing whether exposure to tap water
##is associated with cryptosporidiosis among AIDS patients

tapw <- c("Lowest", "Intermediate", "Highest")
outc <- c("Case", "Control")	
dat <- matrix(c(2, 29, 35, 64, 12, 6),3,2,byrow=TRUE)
dimnames(dat) <- list("Tap water exposure" = tapw, "Outcome" = outc)
tab2by2.test(dat, rev="c")

}

\keyword{htest}
