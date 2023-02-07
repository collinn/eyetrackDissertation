
library(data.table)
library(bdots)
library(ggplot2)

## This file is going to be for examining parameter coverage and pointwise coverage

dat <- readRDS("argon/rds_files/sim3.rds")
boot <- readRDS("argon/rds_boot/boot3.rds")

dd <- dat[[1]]
bb <- boot[[1]]

getParMeans <- function(dd) {
  pa <- colMeans(dd$data$parsA)
  #pb <- colMeans(dd$data$parsB)
  #return(list(parsA = pa, parsB = pb))
}

getParDist <- function(bb, dd) {
  sa <- bb$singleMeans$parA
  ma <- bb$manyMeans$parA

  aa <- getParMeans(dd)

  return(list(sa, ma))
}

parQuants <- function(x, tp) {
  qq <- Map(function(y, p) {
    ff <- ecdf(y)
    ff(p)
  }, y = split(x, col(x)), p = tp)
  setNames(unlist(qq), names(tp))
}
# parQuants(x, tp)

curveQuants <- function(x, tp) {
  time <- seq(0, 1600, by = 4)
  cc <- apply(x, 1, function(y) {
    eyetrackSim:::logistic_f(y, time)
  })
  actualCurve <- eyetrackSim:::logistic_f(tp, time)
  cc_ecdf <- apply(cc, 1, ecdf)
  tt <- Map(function(ac, ec) {
    ec(ac)
  }, ac = actualCurve, ec = cc_ecdf) |> unlist()
  tt
}

test <- parQuants(sa, aa)
test2 <- curveQuants(sa, aa)
test3 <- parQuants(ma, aa)
test23 <- curveQuants(ma, aa)

### Parameter stuff first
ddat <- copy(dat)
dd <- dat
bb <- boot


## Takes dd, a list of raw data
## takes bb, a list of bootstrap data
getParCoverage <- function(dd, bb) {
  ## First, get rid of fit stuff
  dd <- lapply(dd, `[[`, 2)
  dd <- lapply(dd, function(x) list(pa = colMeans(x$parsA), pb = colMeans(x$parsB)))

  dda <- lapply(dd, `[[`, 1)
  ddb <- lapply(dd, `[[`, 2)

  ## Now get boot stuff
  bb_s <- lapply(bb, `[[`, 1)
  bb_sa <- lapply(bb_s, `[[`, "parA")
  bb_sb <- lapply(bb_s, `[[`, "parB")

  bb_m <- lapply(bb, `[[`, 2)
  bb_ma <- lapply(bb_m, `[[`, "parA")
  bb_mb <- lapply(bb_m, `[[`, "parB")

  combineforPar <- function(z, n) {
    w <- Map(parQuants, x = z, tp = n)
    w <- Reduce(cbind, w)
    mm <- apply(w, 1, function(a) {
      sum(a > 0.05 & a < 0.95) / length(a)
    })
    mm
  }

  ## results
  sa <- combineforPar(bb_sa, dda)
  sb <- combineforPar(bb_sb, ddb)
  ma <- combineforPar(bb_ma, dda)
  mb <- combineforPar(bb_mb, ddb)

  list(singleMeans = list(parA = sa, parB = sb),
       manyMeans = list(parA = ma, parB = mb))
}

res <- getParCoverage(dat, boot)

