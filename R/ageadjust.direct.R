"ageadjust.direct" <-
  function(count, pop, rate = NULL, stdpop, conf.level = 0.95){
  if(missing(count)==TRUE & !missing(pop)==TRUE & is.null(rate)==TRUE)
    count <- rate * pop
  if(missing(pop)==TRUE & !missing(count)==TRUE & is.null(rate)==TRUE)
    pop <- count/rate
  if(is.null(rate)==TRUE & !missing(count)==TRUE & !missing(pop)==TRUE)
    rate <- count/pop
  zv <- qnorm(0.5*(1+conf.level))
  alpha <- 1 - conf.level

  cruderate <- sum(count)/sum(pop)
  stdwt <- stdpop/sum(stdpop)
  dsr <- sum(stdwt * rate)
  dsr.var <- sum((stdwt^2)*(count/pop^2))

  ##gamma approximation
  x2divvar <- (dsr^2)/dsr.var
  lcf <- qgamma(alpha/2,shape=x2divvar,scale=1)/x2divvar
  ucf <- qgamma(1-alpha/2,shape=x2divvar+1,scale=1)/x2divvar
  gamma.lci <- dsr*lcf
  gamma.uci <- dsr*ucf

  c(crude.rate = cruderate, adj.rate = dsr, lci = gamma.lci, uci=gamma.uci)
}
