\name{as.week}

\alias{as.week}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Convert date-time object in 'disease week'}

\description{
  Converts date-time object into "disease week" with values of 0 to 53
  and values 1 to 53  
}

\usage{
as.week(x, format = "\%Y-\%m-\%d", first.day = "Sunday", before = 7, after = 7)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{x}{character vector of dates to be converted into 'disease weeks'}
  \item{format}{default is standard date 'YYYY-mm-dd'}
  \item{first.day}{first day of first weeks starts with either 'Sunday'
    (default) or 'Monday'}
  \item{before}{number of days before minimum date in x; used in
    creating vector of possible dates of which x is a subset}
  \item{after}{number of days after maximum date in x; used in
    creating vector of possible dates of which x is a subset}
}

\details{
  In public health, reportable diseases are often reported by 'disease
  week' (either week of reporting or week of symptom onset). In R, weeks
  are numbered from 0 to 53 in the same year. The first day of week 1
  starts with either the first Sunday or Monday of the year. The
  'as.week' functions generates weeks numbered from 1 to 53. The week
  before week 1 takes on the value (52 or 53) from the last week of the
  previous year.
}

\value{
  \item{$jdates}{'Date' vector created from x}
  \item{$jweeks0}{'disease week' vector created from x; week before week
    1 takes the value of 0 in same year (default in R)}
  \item{$jweeks}{'disease week' vector created from x; week before week 1 takes the value of 52 or 53 from previous year}
  \item{$jstratum}{unique strata values for each week in jweeks}
  \item{$jindex}{unique values for unique dates in x}
  \item{$cdates}{range of possible calendar dates of which jdates is a subset}
  \item{$cweeks0}{range of possible calendar weeks0 of which jweeks0 is a subset}
  \item{$cweeks}{range of possible calendar weeks of which jweeks is a subset}
  \item{$cstratum}{unique strata values for each week in cweeks}
  \item{$cindex}{unique values for unique dates in cdates; i.e., 1:length(cdates)}
}

\references{none}

\author{Tomas Aragon, \email{aragon@berkeley.edu}, \url{http://www.medepi.net/aragon}}

\note{none}

\seealso{See also \code{\link{as.Date}}, \code{\link{strptime}}, \code{\link{DateTimeClasses}}}

\examples{
mydates <- c("1/1/04", "1/2/04", "1/7/04", "1/14/04", "8/18/04");
as.week(mydates, format = "\%m/\%d/\%y")
}

\keyword{manip}
