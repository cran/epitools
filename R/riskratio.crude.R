"riskratio.crude" <-
  function(...){
    x <- as.epitable(...)
    if(any((dim(x)==c(2,2))==FALSE)) stop("must be 2x2 table")
    cases0 <- x[1,2]
    cases1 <- x[2,2]
    N0 <- x[1,1] + x[1,2]
    N1 <- x[2,1] + x[2,2]
    risks <- c(cases1/N1, cases0/N0)
    names(risks) <- paste(names(dimnames(x)[1]),"=",rownames(x))
    estimate <- (cases1/N1)/(cases0/N0)
    names(estimate) <- "crude (no adjustment)"
    pv <- fisher.test(x)$p.value
    names(pv) <- "p value"
    list(data = x, risks = risks, estimate = estimate, fishers.exact = pv)
}
