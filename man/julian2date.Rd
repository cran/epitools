\name{julian2date}

\alias{julian2date}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Convert a julian date into standard a date format}

\description{
  Convert a julian date into a standard calendar date format
}

\usage{
julian2date(x)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{x}{julian date; that is, the number of days since day 0 (default is
    1970-01-01)}
}

\details{
  In R, the \code{julian} function converts a date-time object into a
  Julian date: the number of day since day 0 (default is
  1970-01-01). However, there is no function, without loading another
  package, that converts a Julian date back into a date object. The
  \code{julian2date} function does this conversion.
}

\value{
  Return standard calendar date format.
}

\references{none}

\author{Tomas Aragon, \email{aragon@berkeley.edu}, \url{http://www.phdata.science}}



\seealso{
  \code{\link[base]{format.Date}}, \code{\link[base]{weekdays}}
}
\examples{
mydates <- c("1/1/04", "1/2/04", "1/7/04", "1/14/04", "8/18/04");
mydates <- as.Date(mydates, format = "\%m/\%d/\%y")
mydates
myjulian <- julian(mydates)
myjulian
julian2date(myjulian)

}

\keyword{chron}
