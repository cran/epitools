"oddsratio" <-
  function(x, y = NULL,
           method = c("midp", "fisher", "wald", "small"),
           conf.level = 0.95,
           rev = c("neither", "rows", "columns", "both"),
           verbose = FALSE, ...){
    if(is.matrix(x) && !is.null(y)){stop("y argument should be NULL")}
    if(is.null(y)){
      x <- epitable(x, rev = rev)
    } else {
      x <- epitable(x, y, rev = rev)
    }
    method <- match.arg(method)
    if(method=="midp"){
      rr <- oddsratio.midp(x, conf.level = conf.level,
                             verbose = verbose, ...)
    }    
    if(method=="fisher"){
      rr <- oddsratio.fisher(x, conf.level = conf.level,
                             verbose = verbose, ...)
    }
    if(method=="wald"){
      rr <- oddsratio.wald(x, conf.level = conf.level,
                           verbose = verbose, ...)
    }
    if(method=="small"){
      rr <- oddsratio.small(x, conf.level = conf.level,
                            verbose = verbose, ...)
    }
    rr
  }
