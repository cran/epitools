"epitab" <-
function(..., conf.level = 0.95, trow = FALSE, tcol = FALSE){
  x <- as.epitable(..., trow = trow, tcol = tcol)
  nr <- nrow(x)
  outcome.distrib <- sweep(x, 1, apply(x,1,sum),"/")
  exposure.distrib <- sweep(x, 2, apply(x,2,sum),"/")
  x2 <- table.margins(x)
  rr <- rr.ss <- matrix(NA, nr = nr, nc = 3)
  for(i in 1:nr){
    rr.tmp <- risk.ratio(x1 = x2[i,2], n1 = x2[i,3], x0 = x2[1,2], n0 =x2[1,3], conf.level = conf.level)
    rr[i,] <- rr.tmp$crude
    rr.ss[i,] <- rr.tmp$adjusted
  }
  rr[1, 2:3] <- NA
  rr.ss[1, ] <- NA
  colnames(rr) <- c("rr", "lci", "uci")
  colnames(rr.ss) <- c("rr.ss", "lci.ss", "uci.ss")
  
  or <- or.ss <- matrix(NA, nr = nr, nc = 3)
  for(i in 1:nr){
    or.tmp <- odds.ratio(ca1 = x2[i,2], co1 = x2[i,1], ca0 = x2[1,2], co0 = x2[1,1], conf.level = conf.level)
    or[i,] <- or.tmp$crude
    or.ss[i,] <- or.tmp$adjusted  
  }
  or[1, 2:3] <- NA
  or.ss[1, ] <- NA
  colnames(or) <- c("or", "lci", "uci")
  colnames(or.ss) <- c("or.ss", "lci.ss", "uci.ss")
  pv <- rep(NA, nr-1)
  for(i in 2:nrow(x)){
    pv[i] <- fisher.test(x[1:i,])$p.value
  }
    
  attributes(outcome.distrib) <- attributes(x)
  
  cohort <- cbind(Noncases = x2[1:nr,1],
                  Prop = round(outcome.distrib[1:nr,1],2),
                  Cases = x2[1:nr,2],
                  Prop = round(outcome.distrib[1:nr,2],2),
                  rr,
                  rr.ss,
                  p.value = signif(pv,4)
                  )
  cn <- colnames(cohort)
  cn[cn=="Noncases"] <- colnames(x)[1]
  cn[cn=="Cases"] <- colnames(x)[2]
  colnames(cohort) <- cn
  names(dimnames(cohort)) <- names(dimnames(x))

  case.control <- cbind(Controls = x2[1:nr,1],
                        Prop = round(exposure.distrib[1:nr,1],2),
                        Cases = x2[1:nr,2],
                        Prop = round(exposure.distrib[1:nr,2],2),
                        or,
                        or.ss,
                        p.value = signif(pv,4)
                        )
  ccn <- colnames(case.control)
  ccn[ccn=="Controls"] <- colnames(x)[1]
  ccn[ccn=="Cases"] <- colnames(x)[2]
  colnames(case.control) <- ccn
  names(dimnames(case.control)) <- names(dimnames(x))
  
  list(data = x2,
       risks = outcome.distrib,
       exposure.distrib = exposure.distrib,
       conf.level = conf.level,
       cohort.analysis = cohort,
       case.control.analysis = case.control
       )
}
