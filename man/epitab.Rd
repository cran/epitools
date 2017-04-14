\name{epitab}

\alias{epitab}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Epidemiologic tabulation for a cohort or case-control study}

\description{
  Calculates risks, risk ratio, odds ratio, and confidence intervals for
  epidemiologic data
}

\usage{
epitab(x, y = NULL,
       method = c("oddsratio", "riskratio", "rateratio"),
       conf.level = 0.95,
       rev = c("neither", "rows", "columns", "both"),           
       oddsratio = c("wald", "fisher", "midp", "small"),
       riskratio = c("wald", "boot", "small"),
       rateratio = c("wald", "midp"),
       pvalue = c("fisher.exact", "midp.exact", "chi2"),
       correction = FALSE,
       verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{x}{
    For odds ratio or risk ratio, input data can be one of the
    following: r x 2 table, vector of numbers from a contigency table
    (will be transformed into r x 2 table in row-wise order), or single
    factor or character vector that will be combined with \code{y} into
    a table.
    
    For rate ratio, input data can be one of the following: r x 2 table
    where first column contains disease counts and second column
    contains person time at risk; a single numeric vector of counts
    followed by person time at risk; a single numeric vector of counts
    combined with \code{y} which would be a numeric vector of
    corresponding person time at risk 
  }
  \item{y}{
    For odds ratio or risk ratio, a single factor or character vector
    that will be combined with \code{x} into a table (default is NULL)

    For rate ratio, a numeric vector of person-time at risk; if
    provided, \code{x} must be a numeric vector of disease counts
  }
  \item{method}{
    select measure of association: "oddsratio" (default), "riskratio",
    or "rateratio"
  }
  \item{conf.level}{
    confidence level (default is 0.95)
  }
  \item{rev}{
    reverse order of "rows", "colums", "both", or "neither" (default)
  }
  \item{oddsratio}{
    selection estimation method: "wald" (default), "fisher", "midp",
    "small"   
  }
  \item{riskratio}{
    selection estimation method: "wald" (default), "boot", "small"
  }
  \item{rateratio}{
    "wald" (default), "midp"
  }
  \item{pvalue}{
    "fisher.exact" (default), "midp.exact", "chi2" (normal
    approximation); for rate ratio, "fisher.exact" not calculated 
  }
  \item{correction}{
    set to TRUE for Yate's continuity correction (default is FALSE)
  }
  \item{verbose}{
    set to TRUE to return more detailed results (default is
    FALSE)
  }
}

\details{
  The \code{epitab} calculates odds ratios, risk ratios, or rate
  ratios for rx2 tables. The odds ratios are estimated using
  unconditional maximum likelihood (Wald), conditional maximum
  likelihood (Fisher), median-unbiased method (mid-p), or small-sample
  adjusted. The confidence intervals are estimated using a normal
  approximation (Wald), hypergeometric exact (Fisher), mid-p exact, or
  small sample adjusted method.

  The risk ratios are estimated using unconditional maximum
  likelihood (Wald), or small-sample adjusted. The confidence intervals
  are estimated using a normal approximation (Wald), or bootstrap
  estimation.
  
  The rate ratios are estimated using unconditional maximum likelihood
  estimation (Wald), or median unbiased method (mid-p). The confidence
  intervals are estimated using normal approximation, or mid-p exact
  method.

  Notice the expected structure of the data to be given to 'epitab':
  \preformatted{
                 Disease
  Exposure       No (ref)  Yes
   Level 1 (ref)  a         b
   Level 2        c         d
   Level 3        e         f   
   }

  This function expects the following table struture for rate ratios:
  \preformatted{
                    counts   person-time
    exposed=0 (ref)   n00        t01
    exposed=1         n10        t11	
    exposed=2         n20        t21
    exposed=3         n30        t31
  }

  If the table you want to provide to this function is not in the
  preferred form, just use the \code{rev} option to "reverse" the rows,
  columns, or both. If you are providing categorical variables (factors
  or character vectors), the first level of the "exposure" variable is
  treated as the reference. However, you can set the reference of a
  factor using the \code{\link[stats]{relevel}} function.

  Likewise, each row of the rx2 table is compared to the exposure
  reference level and test of independence two-sided p values are
  calculated using fisher exact, mid-p exact, or normal approximation
  method. 
}
\value{
  \item{tab}{primary table}
  \item{measure}{odds ratio, risk ratio, or rate ratio}
  \item{conf.level}{confidence level}
  \item{pvalue}{p value method}
  \item{x}{data input}
  \item{data}{data with margin totals}
  \item{p.exposed}{proportion exposed}
  \item{p.outcome}{proportion outcome}
  \item{p.value}{p value}
  \item{correction}{TRUE if Yate's continuity correction was used}
}

\references{
  Nicolas P Jewell, Statistics for Epidemiology, 1st Edition, 2004,
  Chapman & Hall 

  Kenneth J. Rothman and Sander Greenland (1998), Modern Epidemiology,
  Lippincott-Raven Publishers

  Kenneth J. Rothman (2002), Epidemiology: An Introduction, Oxford
  University Press
}

\author{Tomas Aragon, \email{aragon@berkeley.edu},
  \url{http://www.phdata.science}} 



%- ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{riskratio}}, \code{\link{oddsratio}}, \code{\link{rateratio}}
}

\examples{
r243 <- matrix(c(12,2,7,9), 2, 2)
dimnames(r243) <- list(Diarrhea = c("Yes", "No"),
                      "Antibody level" = c("Low", "High")
                      )
r243
r243b <- t(r243)
r243b
epitab(r243, rev = "b", verbose = TRUE)
epitab(r243, method="riskratio",rev = "b", verbose = TRUE)
epitab(matrix(c(41, 15, 28010, 19017),2,2)[2:1,],
       method="rateratio", verbose = TRUE)

}


\keyword{models}
%- \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
