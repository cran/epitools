\name{colors.plot}

\alias{colors.plot}
\alias{colors.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Plots R's 657 named colors for selection}

\description{
Plots R's 657 named colors for selection
}

\usage{
colors.plot(locator = FALSE, cex.axis = 0.7)
colors.matrix()
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \code{colors.plot}:
  \item{locator}{activates 'locator' for interactive selection of color
    names (default is FALSE)}
  \item{cex.axis}{change size of axes labels}

  \code{colors.matrix} has no arguments.
}

\details{
  The \code{colors.plot} function plots R's 657 named colors. If
  \code{locator=TRUE} then you can interactively point and click to
  select the colors for which you want names. To end selection, right
  click on the mouse and select 'Stop', then R returns the selected
  color names.

  The \code{colors.matrix} function is used by \code{colors.plot} to
  create the matrix of color names that corresponds to the graph created
  by \code{colors.plot}. \code{colors.matrix} can be used alone to
  create the matrix of name without generating a plot. To see the matrix
  it must be assigned an object name and then displayed.
}

\value{
  \code{colors.plot} generates plot with R colors and, when
  \code{locator=TRUE}, returns matrix with graph coordinates and
  names of colors selected

  \code{colors.matrix} quietly returns matrix of names 
}
  
%-  \item{comp1 }{Description of 'comp1'}
%-  \item{comp2 }{Description of 'comp2'}


\references{
none
}

\author{Tomas Aragon, \email{aragon@berkeley.edu}, \url{http://www.phdata.science}}



%- ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{colorbrewer.display}}, \code{\link{colorbrewer.palette}},
  \code{\link{colorbrewer.data}}
  
  \code{\link[grDevices]{colors}}
}

\examples{
##creates matrix with color names
cm <- colors.matrix()
cm[1:3, 1:3]

##generates plot
colors.plot()

##generates plot and activates 'locator'
##don't run
##colors.plot(TRUE)

}

\keyword{color}
%- \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
