"oddsratio.fisher" <-
  function(...){
    x <- as.epitable(...)
    if(any((dim(x)==c(2,2))==FALSE)) stop("must be 2x2 table")
    aa <- x[1,1]
    bb <- x[1,2]
    cc <- x[2,1]
    dd <- x[2,2]
    exp.prop <- c(cc/(aa+cc),dd/(bb+dd))
    names(exp.prop) <- paste(names(dimnames(x)[2]),"=",colnames(x))
    ftest <- fisher.test(x)
    estimate <- ftest$estimate
    names(estimate) <- "from 'fisher.test'"
    pv <- ftest$p.value
    names(pv) <- "p value"
    list(data = x, proportion.exposed = exp.prop,
         estimate = estimate, fishers.exact = pv)
}

