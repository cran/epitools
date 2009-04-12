"expand.table" <-
  function(x){
    if(is.null(dimnames(x))==TRUE) stop("must have dimnames")
    if(any(names(dimnames(x))=="")) stop("must have names")
    tablevars <- expand.grid(rev(dimnames(x)))
    ftablex <- ftable(x)
    counts <- as.vector(t(ftablex[,1:ncol(ftablex)]))
    expansion.index <- rep(1:nrow(tablevars),counts)
    newdat <- tablevars[expansion.index,]
    row.names(newdat) <- 1:nrow(newdat)
    revnames <- rev(names(newdat))
    newdat[,revnames]
}
