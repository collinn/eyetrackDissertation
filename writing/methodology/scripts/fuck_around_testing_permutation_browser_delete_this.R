getT2 <- function(x, idx, group, whole = FALSE, addVar = TRUE, ip = FALSE) {
  x[[group]] <- x[[group]][idx]

  ## Stuff I need to  call the function (gross)
  timeName <- attr(x, "call")$time
  TIME <- attributes(x)$time
  ff <- makeCurveFun(x)

  fit_s <- split(x, by = group)

  ## Do i sample with the added variation?
  if (addVar == FALSE) {
    mvl <- lapply(fit_s, function(y) {
      cc <- coef(y)
      cl <- apply(cc, 1, function(z) {
        z <- as.list(z)
        z[[timeName]] <- TIME
        do.call(ff, z)
      })
      mm <- rowMeans(cl)
      vv <- apply(cl, 1, var)
      vvn <- vv/nrow(cc)
      list(mean = mm, nvar = vvn, nn = nrow(cc), curveList = cl)
    })
  } else {
    mvl <- lapply(fit_s, function(y) {
      ## Weird that this becomes a list
      cc <- apply(y, 1, function(z) {
        rmvnorm(1, coef(z$fit), vcov(z$fit))
      }) |> t()
      #cc <- coef(y)
      cl <- apply(cc, 1, function(z) {
        z <- as.list(z)
        z[[timeName]] <- TIME
        do.call(ff, z)
      })
      mm <- rowMeans(cl)
      vv <- apply(cl, 1, var)
      vvn <- vv/nrow(cc)
      list(mean = mm, nvar = vvn, nn = nrow(cc), curveList = cl)
    })
  }


  ## Guess I don't need a function for this
  x <- mvl[[1]]; y <- mvl[[2]]
  xm <- x$mean; xv <- x$nvar
  ym <- y$mean; yv <- y$nvar

  ## If paired, this will be same as y$nn
  clx <- x$curveList; cly <- y$curveList

  if (!ip) {
    Tt <- abs(xm-ym) / sqrt(yv + xv)
  } else {
    dif <- clx - cly
    dd <- rowMeans(dif)
    vv <- apply(dif, 1, var)
    vv <- vv / x$nn
    Tt <- abs(dd) / sqrt(vv)
  }

  Tt <- t.test(clx, cly, paired = TRUE)$statistic

  ifelse(whole, return(Tt), return(max(Tt)))
}

tnull2 <- parApply(cl, permmat, 2, function(y) {
  bidx <- bool2idx(y)
  getT2(x, bidx, group = pgroups, ip)
})
