"pois.exact" <-
function(x, pt = 1, conf.level = 0.95){
  xc <- cbind(x,conf.level)
  results <- matrix(NA,nrow(xc),6)
  f1 <- function(x,ans,alpha=alp) {ppois(x,ans)-alpha/2}
  f2 <- function(x,ans,alpha=alp) 1-ppois(x,ans)+dpois(x,ans)-alpha/2
  for(i in 1:nrow(xc)){
    alp <- 1-xc[i,2]
    interval <- c(0,xc[i,1]*5+4)
    uci <- uniroot(f1,interval=interval,x=xc[i,1])$root/pt
    if(xc[i,1]==0){
      lci <- 0
    } else {
      lci <- uniroot(f2,interval=interval,x=xc[i,1])$root/pt
    } 
    results[i,] <- c(xc[i,1],pt,xc[i,1]/pt,xc[i,2],lci,uci) 
  }
  colnames <- c("x", "pt", "rate", "conf.level", "lci", "uci")
  dimnames(results) <- list(NULL, colnames)
  as.data.frame(results)
}
