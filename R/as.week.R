"as.week" <-
function(x, format = "%Y-%m-%d", first.day = "Sunday", before = 7, after = 7){
  if(first.day == "Sunday") {fday <- "%U"}
  if(first.day == "Monday") (fday <- "%W")
  if(first.day != "Sunday" & first.day != "Monday") stop("ERROR: must specify 'first.day' as either \"Sunday\" (default) or \"Monday\"")
  jdates <- as.Date(x, format = format)
  names(jdates) <- as.character(jdates)
  jweeks <- jweeks0 <- format(jdates, format = fday)
  mindate <- min(jdates, na.rm=TRUE)
  maxdate <- max(jdates, na.rm=TRUE)  
  cdates <- seq(mindate - before, maxdate + after, by = 1)
  names(cdates) <- as.character(cdates)
  cweeks <- cweeks0 <- format(cdates, format = fday)
  names(cweeks) <- names(cdates)
  for(i in 1:length(cweeks)){
    if(cweeks[i]=="00"){cweeks[i] <- cweeks[i-1]}
  }
  j <- 1
  cstratum <- c(1, rep(NA, length(cweeks)-1))
  for(i in 2:length(cweeks)){
    if(cweeks[i]==cweeks[i-1]){
      cstratum[i] <- j
    }
    else {
      j <- j + 1
      cstratum[i] <- j
    }
  }
  names(cstratum) <- names(cdates)
  jweeks <- cweeks[names(jdates)]
  jstratum <- cstratum[names(jdates)]
  cnum <- 1:length(cdates)
  names(cnum) <- names(cdates)
  jnum <- cnum[names(jdates)]
  list(jdates=unname(jdates),
       jweeks0=unname(as.numeric(jweeks0)),
       jweeks=unname(as.numeric(jweeks)),
       jstratum=unname(as.numeric(jstratum)),
       jindex=unname(jnum),
       cdates=unname(cdates),
       cweeks0=unname(as.numeric(cweeks0)),
       cweeks=unname(as.numeric(cweeks)),
       cstratum=unname(as.numeric(cstratum)),
       cindex=unname(cnum))
}
