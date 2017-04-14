\name{ageadjust.indirect}

\alias{ageadjust.indirect}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Age standardization by indirect method, with exact confidence intervals}

\description{
  Calculates age standardized (adjusted) rates and "exact" confidence
  intervals using the indirect method
}

\usage{
ageadjust.indirect(count, pop, stdcount, stdpop, stdrate = NULL,
conf.level = 0.95)
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
  To make valid comparisons between rates from different groups (e.g.,
  geographic area, ethnicity), one must often adjust for differences in
  age distribution to remove the confounding affect of age. When the
  number of events or rates are very small (as is often the case for
  local area studies), the normal approximation  method of calculating
  confidence intervals may give a negative number for the lower
  confidence limit. To avoid this common pitfall, one can approximate
  exact confidence intervals. This function implements this  method
  (Anderson 1998).
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
  \url{http://www.cdc.gov/nchs/data/nvsr/nvsr47/nvs47_03.pdf}.

  Steve Selvin. Statistical Analysis of Epidemiologic Data (Monographs in
  Epidemiology and Biostatistics, V. 35), Oxford University Press; 3rd
  edition (May 1, 2004) 
}

\author{Tomas Aragon, \email{aragon@berkeley.edu},
  \url{http://www.phdata.science}. Thanks to Giles Crane
  (\email{giles.crane@doh.state.nj.us}) for reporting error in
  'ageadjust.indirect' function.}

\note{Visit \url{https://repitools.wordpress.com/} for the latest}

%- ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  See also \code{\link{ageadjust.direct}}
}

\examples{
##From Selvin (2004)
##enter data
dth60 <- c(141, 926, 1253, 1080, 1869, 4891, 14956, 30888,
41725, 26501, 5928)

pop60 <- c(1784033, 7065148, 15658730, 10482916, 9939972,
10563872, 9114202, 6850263, 4702482, 1874619, 330915)

dth40 <- c(45, 201, 320, 670, 1126, 3160, 9723, 17935,
22179, 13461, 2238)

pop40 <- c(906897, 3794573, 10003544, 10629526, 9465330,
8249558, 7294330, 5022499, 2920220, 1019504, 142532)

##calculate age-specific rates
rate60 <- dth60/pop60
rate40 <- dth40/pop40

#create array for display
tab <- array(c(dth60, pop60, round(rate60*100000,1), dth40, pop40,
round(rate40*100000,1)),c(11,3,2))
agelabs <- c("<1", "1-4", "5-14", "15-24", "25-34", "35-44", "45-54",
"55-64", "65-74", "75-84", "85+")
dimnames(tab) <- list(agelabs,c("Deaths", "Population", "Rate"),
c("1960", "1940"))
tab

##implement direct age standardization using 'ageadjust.direct'
dsr <- ageadjust.direct(count = dth40, pop = pop40, stdpop = pop60)
round(100000*dsr, 2) ##rate per 100,000 per year

##implement indirect age standardization using 'ageadjust.indirect'
isr <- ageadjust.indirect(count = dth40, pop = pop40,
                          stdcount = dth60, stdpop = pop60)
round(isr$sir, 2)         ##standarized incidence ratio
round(100000*isr$rate, 1) ##rate per 100,000 per year

}

\keyword{models}
%- \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
