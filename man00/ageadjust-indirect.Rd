\name{ageadjust.indirect}

\alias{ageadjust.indirect}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Age standardization by indirect method}

\description{
  Calculates age standardized (adjusted) rates and "exact" confidence
  intervals using the indirect method
}

\usage{
ageadjust.indirect(count, pop, stdcount, stdpop, stdrate = NULL, conf.level =
0.95)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{count}{vector of age-specific count of events}
  \item{pop}{vector of age-specific person-years or population estimates}
  \item{stdcount}{vector of age-specific standard counts}
  \item{stdpop}{vector of age-specific standarad population}
  \item{stdrate}{vector of age-specific standard rates}
  \item{conf.level}{confidence level (default = 0.95)}
}

\details{
  Calculates age standardized (adjusted) rates and "exact" confidence
  intervals using the indirect method
}

\value{
  \item{$sir}{observed, expected, standardized incidence ratio, and
    confidence interval}
  \item{$rate}{crude.rate, adjusted rate, and confidence interval}
}

\references{
  Anderson RN, Rosenberg HM. Age Standardization of Death
  Rates: Implementation of the Year 200 Standard. National Vital
  Statistics Reports; Vol 47 No. 3. Hyattsville, Maryland: National
  Center for Health Statistics. 1998, pp. 13-19. Available at
  \url{http://www.cdc.gov/nchs/data/nvsr/nvsr49/nvsr49_09.pdf}.

  Selvin S. Statistical Analysis of Epidemiologic Data (Monographs
  in Epidemiology and Biostatistics, Vol 17), Oxford University Press, (1991)
}

\author{Tomas Aragon, \email{aragon@berkeley.edu}, \url{http://www.medepi.net/aragon}}

\note{none}

%- ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  See also \code{\link{ageadjust.direct}}
}

\examples{
dth60 <- c(141, 926, 1253, 1080, 1869, 4891, 14956, 30888, 41725, 26501,
5928)
pop60 <- c(1784033, 7065148, 15658730, 10482916, 9939972, 10563872, 9114202,
6850263, 4702482, 1874619, 330915)
dth40 <- c(45, 201, 320, 670, 1126, 3160, 9723, 17935, 22179, 13461, 2238)
pop40 <- c(906897, 3794573, 10003544, 10629526, 9465330, 8249558, 7294330,
5022499, 2920220, 1019504, 142532)
rate60 <- dth60/pop60
rate40 <- dth40/pop40
tab <- array(c(dth60, pop60, round(rate60*100000,1), dth40, pop40,
round(rate40*100000,1)), c(11,3,2))
agelabs <- c("<1", "1-4", "5-14", "15-24", "25-34", "35-44", "45-54",
"55-64", "65-74", "75-84", "85+")
dimnames(tab) <- list(agelabs, c("Deaths", "Population", "Rate"),
c("1960", "1940"))
tab
ageadjust.indirect(count = dth40, pop = pop40, stdcount = dth60, stdpop
= pop60)

}

\keyword{models}
%- \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
