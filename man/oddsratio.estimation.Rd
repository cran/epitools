\name{oddsratio.estimation}

\alias{oddsratio.estimation}
\alias{oddsratio.crude}
\alias{oddsratio.fisher}
\alias{oddsratio.ss}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Odds ratio estimation and with Fisher's exact test}

\description{
  Estimates odds ratio using different methods and performs Fisher's
  exact test for testing the null of independence of rows and columns in
  a contingency table with fixed marginals. 
}

\usage{
oddsratio.crude(...)
oddsratio.fisher(...)
oddsratio.ss(...)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{...}{A 2x2 table or equivalent. See \code{\link{as.epitable}}
    for details. }
}

\details{
  These functions calculate the odds ratio using different methods and
  performs Fisher's exact test for testing the null of independence of
  rows and columns in a contingency table with fixed marginals. These 
  functions calculate the odds ratio for the following type of 2x2
  contingency table (opposite of how data is displayed in epidemiology
  textbooks, but consistent with how data is handled in statistical programs): 
  \preformatted{
                Outcome
   Exposure     No  Yes
     No (ref)    a    b
     Yes         c    d
  }
  
  The \code{oddsratio.crude} function calculates the odds ratio using
  the cross-product (a*d)/(b*c).

  The \code{oddsratio.fisher} function calculates the odds ratio using
  the \code{\link[stats]{fisher.test}} function. The odds ratio differs
  slightly from the crude calculation.

  The \code{oddsratio.ss} function calculates the odds ratio using a
  small sample (ss) adjustment (Jewell 2004): (a*d)/((b+1)*(c+1)). 
}


\value{
  \item{$data}{original data}
  \item{$proportion.exposed}{proportion exposed for each outcome category}
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
  See also \code{\link{riskratio.estimation}}, \code{\link{as.epitable}}, \code{\link{epitab}}
}

\examples{
##From Jewell (2004), p. 79
oddsratio.crude(88, 20, 555, 347)
oddsratio.fisher(88, 20, 555, 347)
oddsratio.ss(88, 20, 555, 347)

dat <- matrix(c(88, 20, 555, 347), 2, 2, byrow = TRUE)
dimnames(dat) <- list (c("0", ">=1"), c("Controls", "Cases"))
names(dimnames(dat)) <- c("Coffed drinking (cups/day)", "Pancreatic Cancer")
dat
oddsratio.crude(dat)
oddsratio.fisher(dat)
oddsratio.ss(dat)
}

\keyword{htest}
%- \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
