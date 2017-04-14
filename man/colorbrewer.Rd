\name{colorbrewer}


\alias{colorbrewer.display}
\alias{colorbrewer.palette}
\alias{colorbrewer.data}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Display and create ColorBrewer palettes}

\description{
Display and create ColorBrewer palettes based on Cindy Brewer's website
at www.colorbrewer.org. 
}

\usage{
colorbrewer.display(nclass = 5,
                    type = c("qualitative", "sequential", "diverging"),
                    col.bg = "white")
colorbrewer.palette(nclass = 5,
                    type = c("qualitative", "sequential", "diverging"),
                    palette = letters[1:18])
colorbrewer.data()
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{nclass}{number of classes or categories to be compared
    graphically} 
  \item{type}{select either 'qualitative' (default), 'sequential', or
    'diverging'}
  \item{col.bg}{set background color (default is white)} 
  \item{palette}{select palette (letter) from displayed plot} 
}

\details{
  These R functions includes color specifications and designs developed
  by Cynthia Brewer (\url{http://www.colorbrewer.org}). For more
  details on color selection please visit this excellent site.
  
  First, select the number of classes or categories to be
  compared (\code{nclass}). Second, select the \code{type} of comparison
  (qualitative  vs. sequential vs. diverging). Third, use
  \code{colorbrewer.display} to display the available ColorBrewer
  palette for a given type and number of classes. Fourth, using the
  \code{colorbrewer.palette} function, create a color palette for use in
  R graphics functions (e.g, col = mypal, where mypal was created from
  \code{colorbrewer.palette}).

  Note that you can change the background color.

  ColorBrewer is Copyright (c) 2002 Cynthia Brewer, Mark Harrower, and
  The Pennsylvania State University. All rights reserved. The
  ColorBrewer palettes have been included in this R package with
  permission of the copyright holder. Copyright and license information
  at \url{http://www.colorbrewer.org}.

  These functions for \code{epitools} were created to make the
  ColorBrewer palettes readily  available to \code{epitools} users, and
  to have the same 3-step selection order as the
  \url{www.colorbrewer.org} site. A more visually appealing display of
  the ColorBrewer schemes is available in the \code{RColorBrewer} package.
  
}

\value{
  \code{colorbrewer.display} displays ColorBrewer selection and invisibly
  returns data that corresponds to graphical display

  \code{colorbrewer.palette} returns \code{rgb} vector palette  
}
  
%-  \item{comp1 }{Description of 'comp1'}
%-  \item{comp2 }{Description of 'comp2'}


\references{
ColorBrewer, by Cynthia Brewer, Pennsylvanis State University,
\email{cbrewer@psu.edu}, \url{http://www.colorbrewer.org} accessed on
2004-11-26 
}

\author{
  Tomas Aragon, \email{aragon@berkeley.edu},
  \url{http://www.phdata.science}
}



%- ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{epitools} package: \code{\link{colors.plot}}
}

\examples{
##display available palettes for given nclass and type
colorbrewer.display(9, "sequential")

##change background to blue
colorbrewer.display(9, "sequential", "blue")

##display available palettes for given nclass and type,
##but also display RGB numbers to create your own palette
cbrewer.9s <- colorbrewer.display(9, "sequential")
cbrewer.9s


##Display and use ColorBrewer palette
##first, display and choose palette (letter)
colorbrewer.palette(10, "q")

##second, extract and use ColorBrewer palette
mycolors <- colorbrewer.palette(nclass = 10, type = "q", palette = "b")
xx <- 1:10
yy <- outer(1:10, 1:10, "*")
matplot(xx,yy, type="l", col = mycolors, lty = 1, lwd = 4)

}

\keyword{color}
%- \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
