"odds.ratio" <-
function(ca1, co1, ca0, co0, conf.level = 0.95){
  ss <- 1 
  OR <- (ca1*co0)/((co1)*(ca0))
  ORss <- (ca1*co0)/((co1+ss)*(ca0+ss))
  logOR <- log(((ca1)*(co0)) / ((co1)*(ca0)))
  logORss <- log(((ca1+ss/2)*(co0+ss/2)) /
                 ((co1+ss/2)*(ca0+ss/2)))
  SElogOR <- sqrt((1/(ca1)) + (1/(co1)) +
                  (1/(ca0)) + (1/(co0)))
  SElogORss <- sqrt((1/(ca1+ss/2)) + (1/(co1+ss/2)) +
                    (1/(ca0+ss/2)) + (1/(co0+ss/2)))
  Z <- qnorm(0.5*(1 + conf.level))
  conf.int <- exp(logOR + c(-1, 1)*Z*SElogOR)
  conf.int.ss <- exp(logORss + c(-1, 1)*Z*SElogORss)
  tab <- matrix(c(ca1, co1, ca0, co0), 2, 2, byrow = TRUE)
  pv <- fisher.test(tab)$p.value
  tab <- cbind(tab,rowSums(tab))
  tab <- rbind(tab,colSums(tab))
  dimnames(tab) <- list(Exposure = c("Yes", "No", "Total"),
                        Outcome = c("Case", "Control", "Total"))  
  list(data = tab,
       p.value = pv,
       conf.level = conf.level,
       crude = cbind(
         or = OR,
         lci = conf.int[1],
         uci = conf.int[2]
         ),
       adjusted = cbind(
         or = ORss,
         lci = conf.int.ss[1],
         uci = conf.int.ss[2]
         )
       )
}
