probratio <- function(object, parm, subset, method=c('ML', 'delta', 'bootstrap'), scale=c('linear', 'log'), level=0.95, 
                    seed, NREPS=100, ...) {
  if (length(match('glm', class(object))) < 0) 
    stop('Non GLM input to "object"')
  if (family(object)$family != 'binomial')
    stop('object not a logistic regression model')
  nc <- length(cf <- coef(object))
  if (missing(subset))
    subset <- T
  if (missing(parm))
    parm <- seq(2, nc)
  cf <- cf[parm]
  
  method <- match.arg(method, c('ML', 'delta', 'bootstrap'))
  
  scale <- match.arg(scale, c('linear', 'log'))
  if (is.na(scale))
    stop('scale cannot take values outside of linear or log')
  if (scale == 'linear') {
    f <- function(x) x[2]/x[1]
    name <- c('Relative risk')
    null <- 1
  } else {
    name <- c('Log relative risk')
    null <- 0
    f <- function(x)log(x[2]) - log(x[1])
  }
  cilevel <- c({1-level}/2, 1-{1-level}/2)
  ciname <- paste0(c('Lower', 'Upper'), ' ', formatC(100*cilevel, format='f', digits=1), '% CI')
    
  if (method == 'ML') {
    newfit <- glm(object, family=binomial(link=log), subset=subset, ...)
    out <- coef(summary(newfit))[parm, , drop=F]
    
    if (scale == 'log') {
      out <- cbind(out[, 1], out[, 2], out[, 3], out[, 4], out[, 1] + qnorm((1-level)/2)*out[, 2], out[, 1] + qnorm(1-(1-level)/2)*out[, 2])
    } else {
      val <- exp(out[, 1])
      se <- val * out[, 2]
      out <- cbind(val, se, z <- abs(val-1)/se, pnorm(z, lower.tail = F)*2, 
                   val + qnorm((1-level)/2)*se, val + qnorm(1-(1-level)/2)*se)
    }
    colnames(out) <- c(name, 'Std. Error', 'Z-value', 'p-value', ciname)
    return(out)
  }
  
  Mod1 <- Mod0 <- model.matrix(object)[subset, ]
  n <- nrow(Mod0)
  Nvec <- matrix(rep(c(1/n,0,0,1/n),each=n), n*2, 2)
    
  if (method == 'delta') {
    if (scale == 'linear') {
      df <- deriv( ~y/x, c('x', 'y'))
    } else {
      df <- deriv(~log(y) - log(x), c('x', 'y'))
    }
    
    out <- sapply(parm, function(p) {
      Mod0[, p] <- 0
      Mod1[, p] <- 1
      Mod <- rbind(Mod0, Mod1)
      allpreds <- family(object)$linkinv(Mod %*% coef(object))
      avgpreds <- t(Nvec)%*% allpreds
      val <- f(avgpreds)
      V <- sweep(chol(vcov(object)) %*% t(Mod), allpreds*(1-allpreds), '*', MARGIN = 2) %*% Nvec
      V <- t(V)%*%V
      dxdy <- matrix(attr(eval(df, list('x'=avgpreds[1], 'y'=avgpreds[2])), 'gradient'))
      se <- sqrt(t(dxdy)%*%V%*%dxdy)
      out <- c(val, se, z <- abs({val-null}/se), pnorm(z, lower.tail=F)*2, val + qnorm(cilevel[1])*se, val + qnorm(cilevel[2])*se)
      names(out) <- c(name, 'Std. Error', 'Z-value', 'p-value', ciname)
      out
    })
    out <- t(out)
    rownames(out) <- names(cf)
    return(out)
  } ## endif delta
  
  if (method == 'bootstrap') {
    if (missing(seed))
      stop('seed must be supplied by the user when obtaining results from random number generation')
    set.seed(seed)
    out <- replicate(NREPS, {
      
      index <- sample(1:n, n, replace=T)
      Mod <- model.matrix(object)[subset, ][index, ]
      newbeta <- glm.fit(Mod, object$y[index], family=binomial())$coef
      out <- sapply(parm, function(p) {
        Mod1 <- Mod0 <- Mod
        Mod1[, p] <- 1
        Mod0[, p] <- 0
        Mod <- rbind(Mod0, Mod1)
        newpreds <- family(object)$linkinv(Mod %*% newbeta)
        f(t(Nvec) %*% newpreds)
      })
      out
    })
    
    
    if (length(parm) == 1) {
    out <- c(val <- mean(out), se <- sd(out), z <- abs({val - null}/se), pnorm(z, lower.tail = F)*2,
             val + qnorm((1-level)/2)*se, val + qnorm(1-(1-level)/2)*se)
    names(out) <- c(name, 'Std. Error', 'Z-value', 'p-value', ciname)
    } else {
      out <- cbind(val <- rowMeans(out), se <- apply(out, 1, sd), z <- abs({val - null}/se), pnorm(z, lower.tail = F)*2,
                   val + qnorm(cilevel[1])*se, val + qnorm(cilevel[2])*se)
      colnames(out) <- c(name, 'Std. Error', 'Z-value', 'p-value', ciname)  
      rownames(out) <- names(cf)
    }
    
    return(out)
  }
}

