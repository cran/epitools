\name{riskratio.estimation}

\alias{riskratio.estimation}
\alias{riskratio.crude}
\alias{riskratio.ss}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Risk ratio estimation and with Fisher's exact test}

\description{
  Estimates risk ratio using different methods and performs Fisher's
  exact test for testing the null of independence of rows and columns in
  a contingency table with fixed marginals. 
}

\usage{
riskratio.crude(...)
riskratio.ss(...)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{...}{A 2x2 table or equivalent. See \code{\link{as.epitable}}
    for details. }
}

\details{
  These functions calculate the risk ratio using different methods and
  performs Fisher's exact test for testing the null of independence of
  rows and columns in a contingency table with fixed marginals. These
  functions calculate the risk ratio for the following type of 2x2
  contingency table (opposite of how data is displayed in epidemiology
  textbooks, but consistent with how data is handled in statistical programs): 
  \preformatted{
                Outcome
   Exposure     No  Yes
     No (ref)    a    b
     Yes         c    d
  }
  
  The \code{riskratio.crude} function calculates the risk ratio using
  d/(c + d))/(b/(a + b).

  The \code{riskratio.ss} function calculates the risk ratio using a
  small sample (ss) adjustment (Jewell 2004):
  d/(c + d))/((b + 1)/(a + b + 1). 
}


\value{
  \item{$data}{original data}
  \item{$risks}{proportion exposed for each outcome category}
  \item{$estimate}{odds ratio}
  \item{$fishers.exact}{p value}
}

\references{
  Nicholas P. Jewell (2004), Statistics for Epidemiology, Chapman & Hall,
1st Edition    
}

\author{Tomas Aragon, \email{aragon@berkeley.edu}, \url{http://www.medepi.net/aragon}}

\note{Visit \url{http://www.epitools.net} for the latest.}

%- ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  See also \code{\link{oddsratio.estimation}}, \code{\link{as.epitable}}, \code{\link{epitab}}
}

\examples{
##From Jewell (2004), p. 79
riskratio.crude(88, 20, 555, 347)
riskratio.ss(88, 20, 555, 347)

dat <- matrix(c(88, 20, 555, 347), 2, 2, byrow = TRUE)
dimnames(dat) <- list (c("0", ">=1"), c("Controls", "Cases"))
names(dimnames(dat)) <- c("Coffee (cups/day)", "Pancreatic Cancer")
dat
riskratio.crude(dat)
riskratio.ss(dat)

}

\keyword{htest}
%- \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
