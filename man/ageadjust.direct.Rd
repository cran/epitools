\name{ageadjust.direct}

\alias{ageadjust.direct}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Age standardization by direct method, with exact confidence intervals}

\description{
  Calculates age standardized (adjusted) rates and "exact" confidence
  intervals using the direct method
}

\usage{
ageadjust.direct(count, pop, rate = NULL, stdpop, conf.level = 0.95)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{count}{vector of age-specific count of events}
  \item{pop}{vector of age-specific person-years or population estimates}
  \item{rate}{vector of age-specific rates}
  \item{stdpop}{vector of age-specific standarad population}
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
  (Fay 1997).

  Original function written by TJ Aragon, based on Anderson,
  1998. Function re-written and improved by MP Fay, based on Fay 1998.
}

\value{
  \item{crude.rate}{crude (unadjusted) rate}
  \item{adj.rate}{age-adjusted rate}
  \item{lci}{lower confidence interval limit}
  \item{uci}{upper confidence interval limit}
}

\references{
  Fay MP, Feuer EJ. Confidence intervals for directly standardized
  rates: a method based on the gamma distribution. Stat Med. 1997 Apr
  15;16(7):791-801. PMID: 9131766

  Steve Selvin. Statistical Analysis of Epidemiologic Data (Monographs in
  Epidemiology and Biostatistics, V. 35), Oxford University Press; 3rd
  edition (May 1, 2004)

  Anderson RN, Rosenberg HM. Age Standardization of Death
  Rates: Implementation of the Year 200 Standard. National Vital
  Statistics Reports; Vol 47 No. 3. Hyattsville, Maryland: National
  Center for Health Statistics. 1998, pp. 13-19. Available at
  \url{http://www.cdc.gov/nchs/data/nvsr/nvsr47/nvs47_03.pdf}.
}

\author{Michael P. Fay, \email{mfay@niaid.nih.gov}; Tomas Aragon,
  \email{aragon@berkeley.edu}, \url{http://www.phdata.science}}



%- ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  See also \code{\link{ageadjust.indirect}}
}

\examples{
## Data from Fleiss, 1981, p. 249 
population <- c(230061, 329449, 114920, 39487, 14208, 3052,
72202, 326701, 208667, 83228, 28466, 5375, 15050, 175702,
207081, 117300, 45026, 8660, 2293, 68800, 132424, 98301, 
46075, 9834, 327, 30666, 123419, 149919, 104088, 34392, 
319933, 931318, 786511, 488235, 237863, 61313)
population <- matrix(population, 6, 6, 
dimnames = list(c("Under 20", "20-24", "25-29", "30-34", "35-39",
"40 and over"), c("1", "2", "3", "4", "5+", "Total")))
population
count <- c(107, 141, 60, 40, 39, 25, 25, 150, 110, 84, 82, 39,
3, 71, 114, 103, 108, 75, 1, 26, 64, 89, 137, 96, 0, 8, 63, 112,
262, 295, 136, 396, 411, 428, 628, 530)
count <- matrix(count, 6, 6, 
dimnames = list(c("Under 20", "20-24", "25-29", "30-34", "35-39",
"40 and over"), c("1", "2", "3", "4", "5+", "Total")))
count

### Use average population as standard
standard<-apply(population[,-6], 1, mean)
standard

### This recreates Table 1 of Fay and Feuer, 1997
birth.order1<-ageadjust.direct(count[,1],population[,1],stdpop=standard)
round(10^5*birth.order1,1)

birth.order2<-ageadjust.direct(count[,2],population[,2],stdpop=standard)
round(10^5*birth.order2,1)

birth.order3<-ageadjust.direct(count[,3],population[,3],stdpop=standard)
round(10^5*birth.order3,1)

birth.order4<-ageadjust.direct(count[,4],population[,4],stdpop=standard)
round(10^5*birth.order4,1)

birth.order5p<-ageadjust.direct(count[,5],population[,5],stdpop=standard)
round(10^5*birth.order5p,1)

}


\keyword{models}
%- \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
