\name{expand.table}

\alias{expand.table}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Expand contingency table into individual-level data set}

\description{
Expands contingency table or array into individual-level data set.
}

\usage{
expand.table(x)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{x}{table or array with \code{dimnames(x)} and
    \code{names(dimnames(x))}
  }
}

\details{
  For educational purposes, one may want to convert a multi-dimensional
  contingency table into an individual-level data frame. In R,
  multi-dimensional contigency tables are represented by arrays. An
  array can be created using the \code{array} command, or the
  \code{table} command with 3 or more vectors (usually fields from a data
  frame).
  
  It is this array, \code{x}, that is processed by
  \code{expand.table}. In order to generate a data frame,
  \code{expand.table} needs to process the field names and the possible
  values for each field. The array x must have dimension names [i.e.,
  \code{dimnames(x)}] and field names [i.e.,
  \code{names(dimnames(x))}]. The \code{expand.table} function converts 
  \code{names(dimnames(x))} to field names and the \code{dimnames(x)} to
  factor levels for each field. Study the examples.
  
  An \code{ftable} object, say \code{ftab}, can be expanded using
  \code{expand.table(as.table(ftab))}.
  
  Study the Titanic example to compare how a data frame can contain
  either individual-level data or group-level data.
  
}

\value{Returns an individual-level data frame
%-  \item{comp1 }{Description of 'comp1'}
%-  \item{comp2 }{Description of 'comp2'}
}

\references{none}

\author{Tomas Aragon, \email{aragon@berkeley.edu},
  \url{http://www.phdata.science}; Daniel Wollschlaeger,
  \email{dwoll@psychologie.uni-kiel.de},
  \url{http://www.uni-kiel.de/psychologie/dwoll/}
}

%- ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link[base]{expand.grid}}
}

\examples{
##Creating array using 'array' function and expanding it
tab <- array(1:8, c(2, 2, 2))
dimnames(tab) <- list(c("No","Yes"), c("No","Yes"), c("No","Yes"))
names(dimnames(tab)) <- c("Exposure", "Disease", "Confounder")
tab
df <- expand.table(tab)
df

##Creating array using 'table' function and expanding it
tab2 <- table(Exposure = df$Exp, Disease = df$Dis, Confounder = df$Conf)
expand.table(tab2)

##Expanding ftable object
ftab2 <- ftable(tab2)
ftab2
expand.table(as.table(ftab2))

##Convert Titanic data into individual-level data frame
data(Titanic)
expand.table(Titanic)[1:20,]

##Convert Titanic data into group-level data frame
as.data.frame(Titanic)

}

\keyword{manip}
%- \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
