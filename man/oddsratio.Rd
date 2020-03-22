\name{oddsratio}

\alias{oddsratio}
\alias{oddsratio.midp}
\alias{oddsratio.fisher}
\alias{oddsratio.wald}
\alias{oddsratio.small}

\title{Odds ratio estimation and confidence intervals}

\description{
  Calculates odds ratio by median-unbiased estimation (mid-p),
  conditional maximum likelihood estimation (Fisher), unconditional
  maximum likelihood estimation (Wald), and small sample adjustment
  (small). Confidence intervals are calculated using exact methods
  (mid-p and Fisher), normal approximation (Wald), and normal
  approximation with small sample adjustment (small).  
}

\usage{
oddsratio(x, y = NULL,
          method = c("midp", "fisher", "wald", "small"),
          conf.level = 0.95,
          rev = c("neither", "rows", "columns", "both"),
          correction = FALSE,
          verbose = FALSE)
oddsratio.midp(x, y = NULL,
               conf.level = 0.95,
               rev = c("neither", "rows", "columns", "both"),
               correction = FALSE,
               verbose = FALSE,
               interval =  c(0, 1000))
oddsratio.fisher(x, y = NULL,
                 conf.level = 0.95,
                 rev = c("neither", "rows", "columns", "both"),
                 correction = FALSE,
                 verbose = FALSE)
oddsratio.wald(x, y = NULL,
               conf.level = 0.95,
               rev = c("neither", "rows", "columns", "both"),
               correction = FALSE,
               verbose = FALSE)
oddsratio.small(x, y = NULL,
                conf.level = 0.95,
                rev = c("neither", "rows", "columns", "both"),
                correction = FALSE,
                verbose = FALSE)
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
  \item{method}{
    method for calculating odds ratio and confidence interval
  }
  \item{conf.level}{confidence level (default is 0.95)}
  \item{rev}{
    reverse order of "rows", "colums", "both", or "neither" (default)
  }
  \item{correction}{
    set to TRUE for Yate's continuity correction (default is FALSE)
  }
  \item{verbose}{
    set to TRUE to return more detailed results (default is
    FALSE)
  }
  \item{interval}{
    interval for the \code{\link{uniroot}} that finds the
    odds ratio median-unbiased estimate and mid-p exact confidence
    interval for \code{oddsratio.midp}
  }
}

\details{
    Calculates odds ratio by median-unbiased estimation (mid-p),
  conditional maximum likelihood estimation (Fisher), unconditional
  maximum likelihood estimation (Wald), and small sample adjustment
  (small). Confidence intervals are calculated using 
  exact methods (mid-p and Fisher), normal approximation (Wald), and
  normal approximation with small sample adjustment (small). 

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

  If you are providing a 2x2 table the following table is preferred:
  \preformatted{
                    disease=0   disease=1
    exposed=0 (ref)    n00         n01
    exposed=1          n10         n11	
  }
  however, for odds ratios from 2x2 tables, the following table is
  equivalent:
  \preformatted{
                    disease=1   disease=0
    exposed=1          n11         n10
    exposed=0          n01         n00	
  }
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
  \item{x}{table that was used in analysis (verbose = TRUE)} 
  \item{data}{same table as \code{x} but with marginal totals} 
  \item{p.exposed}{proportions exposed (verbose = TRUE)} 
  \item{p.outcome}{proportions experienced outcome (verbose = TRUE)} 
  \item{measure}{risk ratio and confidence interval} 
  \item{conf.level}{confidence level used (verbose = TRUE)} 
  \item{p.value}{p value for test of independence} 
  \item{replicates}{number of replicates used in Monte Carlo simulation
    p value (verbose = TRUE)}
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
  \code{\link{tab2by2.test}}, \code{\link{riskratio}},
  \code{\link{rateratio}}, \code{\link{ormidp.test}},
  \code{\link{epitab}}
}

\examples{

##Case-control study assessing whether exposure to tap water
##is associated with cryptosporidiosis among AIDS patients

tapw <- c("Lowest", "Intermediate", "Highest")
outc <- c("Case", "Control")	
dat <- matrix(c(2, 29, 35, 64, 12, 6),3,2,byrow=TRUE)
dimnames(dat) <- list("Tap water exposure" = tapw, "Outcome" = outc)
oddsratio(dat, rev="c")
oddsratio.midp(dat, rev="c")
oddsratio.fisher(dat, rev="c")
oddsratio.wald(dat, rev="c")
oddsratio.small(dat, rev="c")

}

\keyword{models}
%- \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
