"as.epitable" <-
  function(..., trow = FALSE, tcol = FALSE){
    lx <- list(...)
    ## r x 2 table
    if(length(lx)==1 && is.matrix(lx[[1]]) && nrow(lx[[1]])>=2 && ncol(lx[[1]])==2)
      {
        x <- lx[[1]] 
        if(is.null(dimnames(lx[[1]])))
          {
            nr <- nrow(x)
            rn <- paste("Level", 1:nr, sep="")
            cn <- c("Level1", "Level2")
            dimnames(x) <- list(Exposure = rn, Outcome = cn)      
          }
      }
    ## 2 vectors
    if(length(lx)==2 &&
       (is.vector(lx[[1]]) || is.factor(lx[[1]])) &&
       (is.vector(lx[[2]]) || is.factor(lx[[2]]))
       )
      {
        x <- table(lx[[1]], lx[[2]]) 
        if(nrow(x)<2 || ncol(x)!=2)
          {
            stop("must be r x 2 table, where r >= 2")
          }
        if(is.null(names(lx)))
          {
            names(dimnames(x)) <- c("Exposure", "Outcome")
          } else names(dimnames(x)) <- names(lx)
      }
    ## >=4 numbers
    is.even <- function(x){ifelse(x%%2==0, TRUE, FALSE)}
    if(length(lx)>=4 && all(sapply(list(1,2,3,4,5),is.numeric)) && is.even(length(lx)) && all(sapply(lx,length)==1)) 
      {
        x <- matrix(sapply(lx,as.vector), ncol = 2, byrow = TRUE)
        nr <- nrow(x)
        rn <- paste("Level", 1:nr, sep="")
        cn <- c("Level1", "Level2")
        dimnames(x) <- list(Exposure = rn, Outcome = cn)
      }
    ## 1 vector
    if(length(lx)==1 && is.vector(lx[[1]]) && is.numeric(lx[[1]]) && is.even(length(lx[[1]])))
      {
        x <- matrix(lx[[1]], ncol = 2, byrow = TRUE)
        nr <- nrow(x)
        rn <- paste("Level", 1:nr, sep="")
        cn <- c("Level1", "Level2")
        dimnames(x) <- list(Exposure = rn, Outcome = cn)
      }
    nrx <- nrow(x)
    if(trow==TRUE && tcol==FALSE) finalx <- x[nrx:1,]
    if(trow==FALSE && tcol==TRUE) finalx <- x[,2:1]
    if(trow==TRUE && tcol==TRUE) finalx <- x[nrx:1,2:1]
    if(trow==FALSE && tcol==FALSE) finalx <- x  
    finalx
  }
