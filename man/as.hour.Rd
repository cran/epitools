\name{as.hour}

\alias{as.hour}
\title{Convert date-time object into hour units}
\description{
  Convert date-time object into hour or half-hour units
}

\usage{
as.hour(x, mindt, maxdt, half.hour = FALSE)
}

\arguments{
  \item{x}{
Date-time object in standard format: for example, "2004-12-23 08:27:00", "2004-12-23 08:27", "2004-12-23"
  }
  \item{mindt}{
[required] Date-time object in standard format that will form the lower
boundary of the hour or half-hour time categories. \code{mindt} must
less than or equal to the minimum value in \code{x}, and must be rounded
off to the nearst hour for hour categories (e.g., HH:00:00) or rounded
off to the nearest half-hour for half-hour categories (e.g., HH:30:00).
  }
  \item{maxdt}{
[required] Date-time object in standard format that will form the upper
boundary of the hour or half-hour time categories. \code{maxdt} must
greater than or equal to the minimum value in \code{x}, and must be rounded
off to the nearst hour for hour categories (e.g., HH:00:00) or rounded
off to the nearest half-hour for half-hour categories (e.g., HH:30:00).
  }
  \item{half.hour}{
Set to TRUE for half-hour categories.
  }

}

\details{
This function (1) converts standard date-time objects into 1-hour or
1/2-hour categories, and (2) generates levels for range of values that
that the new 1-hour or 1/2-hour categories can take. These levels are
use for converting x into a factor and for providing names for labeling
the x-axis in plot. This function is used by \code{epicurves.hours}.
}

\value{
  \item{$ct}{
    Date-time object that contains the number of seconds since the beginning
    of 1970 as a numeric vector and produced by \code{\link{as.POSIXct}}. You
      can use \code{\link{as.POSIXlt}} to convert this output in human legible
      (already done by this function).
    }
    \item{$sec}{seconds}
\item{$min}{minutes}
\item{$hour}{hours (0-23)}
\item{$hour12}{hours (1-12)}
\item{$stratum}{number of hours or 1/2 hours since beginning of 1970}
\item{$stratum2}{
factor (categorical variable) with number of hours of 1/2 hours since
beginning of 1970 using \code{$cstratum} as the levels
}
\item{$stratum3}{
factor (categorical variable) in standard date-time format indicating
number of hours or 1/2 hours since beginning of 1970 using
\code{$cstratum2} as the levels 
}
\item{$cstratum}{levels for creating \code{$stratum2} factor}
\item{$cstratum2}{levels for creating \code{$stratum3} factor}
\item{$csec}{seconds from \code{$cstratum2}}
\item{$cmin}{minutes from \code{$cstratum2}}
\item{$chour}{hours from \code{$cstratum2} in 24-hour format}
\item{$chour12}{hours from \code{$cstratum2} in 12-hour format}
\item{$campm}{corresponding 'AM' or 'PM' for \code{$chour12}}
\item{$campm2}{corresponding 'am' or 'pm' for \code{$chour12}}
\item{$cweekday}{day of the week for \code{$cstratum2}}
\item{$cwkday}{abbreviated day of the week for \code{$cstratum2}}
\item{$cmday}{day of the month for \code{$cstratum2}}
\item{$cmonth}{month for \code{$cstratum2}}
\item{$cmon}{abbreviated month for \code{$cstratum2}}
\item{$cyear}{year for \code{$cstratum2}}
\item{$half.hour}{
FALSE (default) for 1-hour categories; TRUE for
1/2-hour categories}

}

\references{none}

\author{Tomas Aragon, \email{aragon@berkeley.edu},
  \url{http://www.phdata.science}} 



\seealso{
  epitools: \code{\link{as.month}}, \code{\link{epicurve.dates}}
  
  \code{\link[base]{as.Date}}, \code{\link[base]{strptime}},
  \code{\link[base]{DateTimeClasses}}}

\examples{

dates <- c("1/1/04", "1/2/04", "1/3/04", "1/4/04", "1/5/04",
"1/6/04", "1/7/04", "1/8/04", "1/9/04", "1/10/04", NA, "1/12/04",
"1/14/04", "3/5/04", "5/5/04", "7/6/04", "8/18/04", "12/13/05",
"1/5/05", "4/6/05", "7/23/05", "10/3/05")
aw <- as.week(dates, format = "\%m/\%d/\%y")
aw

aw2 <- as.week(dates, format = "\%m/\%d/\%y", sunday= FALSE)
aw2

aw3 <- as.week(dates, format = "\%m/\%d/\%y", min.date="2003-01-01")
aw3

}

\keyword{chron}
