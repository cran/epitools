\name{epidate}

\alias{epidate}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Convert dates into multiple legible formats}

\description{
  Convert character vector of dates into multiple legible formats.  
}

\usage{
epidate(x, format = "\%m/\%d/\%Y", cal.dates = FALSE,
        before = 7, after = 7, sunday = TRUE)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{x}{character vector of dates to be converted}
  \item{format}{format of character vector of dates}
  \item{cal.dates}{Calendar dates that contains \code{x}, starting 7
    days 'before' (default) until 7 days 'after' \code{x}}
  \item{before}{defines lower limit of \code{cal.dates}: default is 7
    days before earliest date in \code{x}}
  \item{after}{defines upper limit of \code{cal.dates}: default is 7
    days after latest date in \code{x}}
  \item{sunday}{first day of the week is either Sunday (default) or Monday}  
}

\details{
  Dates can come in many formats (e.g., November 12, 2001, 12Nov01,
  11/12/2001, 11/12/01, 2001-11-12) and need to be converted into other
  formats for data analysis, graphical displays, generating reports,
  etc.

  There is tremendous flexibility in converting any character vector
  with sufficient information to be converted into a unique date. For
  complete options for the \code{format} option see
  \code{\link[base]{strptime}}.
}

\value{
  \item{dates}{dates wtih date-time class}
  \item{julian}{number of days since 1970-01-01}
  \item{mday}{day of the month: 1-31}
  \item{mon}{month of the year: 0-11}
  \item{month}{month: January, February, March, ...}
  \item{month2}{month: Jan, Feb, Mar, ...}
  \item{firstday}{first day of the week: Sunday or Monday}
  \item{week}{week of the year: 0-53}
  \item{year}{year: YYYY}
  \item{yr}{year: YY}
  \item{wday}{day of the week: 0-6}
  \item{weekday}{weekday: Monday, Tuesday, Wednesday, ...}
  \item{wkday}{weekday: Mon, Tue, Wed, ...}
  \item{yday}{day of the year: 0-365}
  \item{quarter}{quarter of the year: Q1, Q2, Q3, Q4}
  \item{cdates}{Calendar dates that contains \code{dates}}
  \item{cjulian}{Julian calendar dates}
}

\references{none}

\author{Tomas Aragon, \email{aragon@berkeley.edu}, \url{http://www.phdata.science}}



\seealso{
  \code{epitools}: \code{\link{as.week}}

  \code{\link[base]{DateTimeClasses}} to learn about date-time classes
  
  \code{\link[base]{format.Date}} to convert character vector of dates into
  calendar dates with date-time class (done by \code{epidate})

  \code{\link[base]{strptime}} to convert date-time character strings
  into a date-time class 
}

\examples{
#x <- c("12/1/03", "11/2/03", NA, "1/7/04", "1/14/04", "8/18/04")
#epidate(x, format = "\%m/\%d/\%y")
#epidate(x, format = "\%m/\%d/\%y", TRUE)
#
###convert vector of disease weeks into vector of mid-week dates
#dwk <- sample(0:53, 100, replace = TRUE)
#wk2date <- paste(dwk, "/", "Wed", sep="")
#wk2date[1:10]
#wk2date2 <- epidate(wk2date, format = "\%U/\%a")
#wk2date2$dates[1:20]
}

\keyword{chron}
