\name{rateratio}

\alias{rateratio}
\alias{rateratio.midp}
\alias{rateratio.wald}

\title{Rate ratio estimation and confidence intervals}

\description{
  Calculates rate ratio by median-unbiased estimation (mid-p),
  and unconditional maximum likelihood estimation (Wald). Confidence
  intervals are calculated using exact methods (mid-p), and
  normal approximation (Wald).
}

\usage{
rateratio(x, y = NULL,
          method = c("midp", "wald"),
          conf.level = 0.95,
          rev = c("neither", "rows", "columns", "both"),
          verbose = FALSE)
rateratio.midp(x, y = NULL,
               conf.level = 0.95,
               rev = c("neither", "rows", "columns", "both"),
               verbose = FALSE)
rateratio.wald(x, y = NULL,
               conf.level = 0.95,
               rev = c("neither", "rows", "columns", "both"),
               verbose = FALSE)
}

\arguments{
  \item{x}{
    input data can be one of the following: r x 2 table where first
    column contains disease counts and second column contains person
    time at risk; a single numeric vector of counts followed by
    person time at risk; a single numeric vector of counts combined with
    \code{y} which would be a numeric vector of corresponding person
    time at risk
  }
  \item{y}{
    numeric vector of person-time at risk; if provided, \code{x} must be
    a numeric vector of disease counts  
  }
  \item{method}{
    method for calculating rate ratio and confidence interval
  }
  \item{conf.level}{confidence level (default is 0.95)}
  \item{rev}{
    reverse order of "rows", "colums", "both", or "neither" (default)
  }
  \item{verbose}{
    set to TRUE to return more detailed results (default is
    FALSE)
  }
}

\details{
  Calculates rate ratio by median-unbiased estimation (mid-p),
  and unconditional maximum likelihood estimation (Wald). Confidence
  intervals are calculated using exact methods (mid-p), and
  normal approximation (Wald).
  
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
  calculated using mid-p exact method and normal approximation (Wald).
}


\value{
\item{x}{table that was used in analysis (verbose = TRUE)} 
\item{data}{same table as \code{x} but with marginal totals} 
\item{measure}{rate ratio and confidence interval} 
\item{conf.level}{confidence level used (verbose = TRUE)} 
\item{p.value}{p value for test of independence} 
}

\references{
  Kenneth J. Rothman, Sander Greenland, and Timothy Lash (2008), Modern Epidemiology,
  Lippincott-Raven Publishers

  Kenneth J. Rothman (2012), Epidemiology: An Introduction, Oxford
  University Press
  
} 

\author{Rita Shiau (original author), \email{rita.shiau@sfdph.org};
  Tomas Aragon, \email{aragon@berkeley.edu},
  \url{http://www.phdata.science};
  Adam Omidpanah, \email{adam.omidpanah@wsu.edu} \url{https://repitools.wordpress.com/}
}


\seealso{
  \code{\link{rate2by2.test}}, \code{\link{oddsratio}},
  \code{\link{riskratio}}, \code{\link{epitab}}
}

\examples{

##Examples from Rothman 1998, p. 238
bc <- c(Unexposed = 15, Exposed = 41)
pyears <- c(Unexposed = 19017, Exposed = 28010)
dd <- matrix(c(41,15,28010,19017),2,2)
dimnames(dd) <- list(Exposure=c("Yes","No"), Outcome=c("BC","PYears"))
##midp
rateratio(bc,pyears)
rateratio(dd, rev = "r")
rateratio(matrix(c(15, 41, 19017, 28010),2,2))
rateratio(c(15, 41, 19017, 28010))

##midp
rateratio.midp(bc,pyears)
rateratio.midp(dd, rev = "r")
rateratio.midp(matrix(c(15, 41, 19017, 28010),2,2))
rateratio.midp(c(15, 41, 19017, 28010))

##wald
rateratio.wald(bc,pyears)
rateratio.wald(dd, rev = "r")
rateratio.wald(matrix(c(15, 41, 19017, 28010),2,2))
rateratio.wald(c(15, 41, 19017, 28010))
}

\keyword{models}
