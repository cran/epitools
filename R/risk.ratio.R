"risk.ratio" <-
function(x1, n1, x0, n0, conf.level = 0.95){
  ss <- 1 #small sample adjustment 
  risk1 <- x1/n1
  risk0 <- x0/n0
  RR <- risk1/risk0
  RRss <- (x1/n1)/((x0+ss)/(n0+ss))
  logRR <- log(RR)
  logRRss <- log(RRss)
  SElogRR <- sqrt(((n1-x1)/(x1*n1)) + ((n0-x0)/(x0*n0)))
  SElogRRss <- sqrt(((n1-x1)/(x1*n1)) + ((n0-x0)/((x0+ss)*(n0+ss))))
  Z <- qnorm(0.5*(1 + conf.level))
  conf.int <- exp(logRR + c(-1, 1)*Z*SElogRR)
  conf.int.ss <- exp(logRRss + c(-1, 1)*Z*SElogRRss)
  tab <- matrix(c(x1, n1-x1, x0, n0-x0), 2, 2, byrow = TRUE)
  pv <- fisher.test(tab)$p.value
  tab <- cbind(tab,rowSums(tab))
  tab <- rbind(tab,colSums(tab))
  dimnames(tab) <- list(Exposure = c("Yes", "No", "Total"),
                        Disease = c("Yes", "No", "Total"))
  list(data = tab,
       p.value = pv,
       conf.level = conf.level,
       crude = cbind(
         rr = RR,
         lci = conf.int[1],
         uci = conf.int[2]
         ),
       adjusted = cbind(
         rr = RRss,
         lci = conf.int.ss[1],
         uci = conf.int.ss[2]
         )
       )
}
