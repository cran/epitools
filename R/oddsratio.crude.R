"oddsratio.crude" <-
  function(...){
    x <- as.epitable(...)
    if(any((dim(x)==c(2,2))==FALSE)) stop("must be 2x2 table")
    aa <- x[1,1]
    bb <- x[1,2]
    cc <- x[2,1]
    dd <- x[2,2]
    exp.prop <- c(cc/(aa+cc),dd/(bb+dd))
    names(exp.prop) <- paste(names(dimnames(x)[2]),"=",colnames(x))
    estimate <- (aa*dd)/(bb*cc)
    names(estimate) <- "crude (no adjustment)"
    pv <- fisher.test(x)$p.value
    names(pv) <- "p value"
    list(data = x, proportion.exposed = exp.prop,
         estimate = estimate, fishers.exact = pv)
}
