"binom.approx" <-
  function(x, n, conf.level = 0.95) {
  # x = number of successes
  # n = number of trials
  # do calculations
    Z <- qnorm(0.5*(1 + conf.level))
    SE.R <- sqrt(x * (n - x) / (n^3))
    R.lci <- x/n - Z*SE.R
    R.uci <- x/n + Z*SE.R
  # collect results into one object
    data.frame(x = x, n = n, prop = x/n, conf.level = conf.level, lci = R.lci, uci = R.uci)  
}
