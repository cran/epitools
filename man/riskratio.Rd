\name{riskratio}

\alias{riskratio}
\alias{riskratio.wald}
\alias{riskratio.small}

\title{Risk ratio estimation and confidence intervals}

\description{
  Calculates risk ratio by unconditional maximum likelihood estimation
  (Wald), and small sample adjustment (small). Confidence intervals are
  calculated using normal approximation (Wald), and normal approximation
  with small sample adjustment (small). 
}

\usage{
riskratio(x, y = NULL,
          method = c("wald", "small"),
          conf.level = 0.95,
          rev = c("neither", "rows", "columns", "both"),
          verbose = FALSE, ...)
riskratio.wald(x, y = NULL,
               conf.level = 0.95,
               rev = c("neither", "rows", "columns", "both"),
               verbose = FALSE, ...)
riskratio.small(x, y = NULL,
                conf.level = 0.95,
                rev = c("neither", "rows", "columns", "both"),
                verbose = FALSE, ...)
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
    method for calculating risk ratio and confidence interval
  }
  \item{conf.level}{confidence level (default is 0.95)}
  \item{rev}{
    reverse order of "rows", "colums", "both", or "neither" (default)
  }
  \item{verbose}{
    set to TRUE to return more detailed results (default is
    FALSE)
  }
  \item{...}{
    passes optional arguments to \code{\link{tab2by2.test}} for
    calculatng tests of independence (p values): adding \code{correction
    = TRUE} implements Yate's continuity correction (default is FALSE),
    and adding \code{replicates = n} where \code{n} is an integer
    specifying the number of iterations (default is 2000) of the Monte
    Carlo simulation method of calculating p values. 
  }
}

\details{
  Calculates risk ratio by unconditional maximum likelihood estimation
  (Wald), and small sample adjustment (small). Confidence intervals are
  calculated using normal approximation (Wald), and normal approximation
  with small sample adjustment (small). 

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
  If the table you want to provide to this function is not in the
  preferred form, just use the \code{rev} option to "reverse" the rows,
  columns, or both. If you are providing categorical variables (factors
  or character vectors), the first level of the "exposure" variable is
  treated as the reference. However, you can set the reference of a
  factor using the \code{\link[stats]{relevel}} function.

  Likewise, each row of the rx2 table is compared to the exposure
  reference level and test of independence two-sided p values are
  calculated using Fisher's Exact, Monte Carlo simulation, and the
  chi-square test.
}


\value{
\item{x}{table that was used in analysis (verbose = TRUE)} 
\item{data}{same table as \code{x} but with marginal totals} 
\item{p.exposed}{proportions exposed (verbose = TRUE)} 
\item{p.outcome}{proportions experienced outcome (verbose = TRUE)} 
\item{measure}{risk ratio and confidence interval} 
\item{conf.level}{confidence level used (verbose = TRUE)} 
\item{p.value}{p value for test of independence} 
\item{replicates}{number of replicates used in Monte Carlo simulation p
  value (verbose = TRUE)}
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
  \url{http://www.medepi.net/aragon}}

\note{Visit \url{http://www.epitools.net} for the latest.}

\seealso{
  \code{\link{tab2by2.test}}, \code{\link{oddsratio}},
  \code{\link{rateratio}} 
}

\examples{

##Case-control study assessing whether exposure to tap water
##is associated with cryptosporidiosis among AIDS patients

tapw <- c("Lowest", "Intermediate", "Highest")
outc <- c("Case", "Control")	
dat <- matrix(c(2, 29, 35, 64, 12, 6),3,2,byrow=TRUE)
dimnames(dat) <- list("Tap water exposure" = tapw, "Outcome" = outc)
riskratio(dat, rev="c")
riskratio.wald(dat, rev="c")
riskratio.small(dat, rev="c")

}

\keyword{models}
