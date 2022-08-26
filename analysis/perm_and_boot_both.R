library(data.table)
library(bdots)
library(eyetrackSim)
library(mvtnorm)

## Let's start by creating distribution for each group
ci <- as.data.table(ci)
ci <- ci[LookType == "Target", ]
fit <- bdotsFit(data = ci,
                y = "Fixations",
                subject = "Subject",
                time = "Time",
                group = "protocol", curveType = logistic())

groupDist <- lapply(split(fit, by = "protocol"), function(x) {
  cc <- coef(x)
  vv <- var(cc)
  cc <- colMeans(cc)
  # Got get these in right spot
  cc[1] <- abs(cc[1])
  cc[2] <- pmin(cc[2], 1)
  list(mean = cc, sigma = vv)
})

## For a single group?
#' @param n number of subjects
#' @param trials number of trials
#' @param pars list of mean and sigma
createData <- function(n = 25, trials = 10, pars, gp = "A") {
  ## Constant
  time <- seq(0, 2000, by = 4)
  newpars <- do.call(rmvnorm, as.list(c(n, pars)))
  ## no negatives or greater than one please
  newpars[, 1] <- abs(newpars[, 1])
  newpars[, 2] <- pmin(newpars[, 2], 1)
  spars <- split(newpars, row(newpars))
  dts <- lapply(seq_len(n), function(x) {
    pp <- spars[[x]]
    dt <- data.table(id = x,
                     time = time,
                     group = gp,
                     true = eyetrackSim:::logistic_f(pp, time))
    dt[, fixations := mean(rbinom(trials, 1, true)), by = time]
  })
  dts <- rbindlist(dts)
  return(list(dts = dts, pars = newpars))
}

dts <- Map(function(x, y) {
  createData(n = 10, trials = 20, pars = x, gp = y)
}, x = groupDist, y = list("A", "B"))

## Change ID
dts[[2]]$dts$id <- dts[[2]]$dts$id + 25
dt <- rbindlist(lapply(dts, `[[`, 1))

## starting parameters
parsA <- groupDist$CI$mean
parsB <- groupDist$NH$mean

## Ok bdots
fit <- bdotsFit(data = dt,
                group = "group",
                y = "fixations",
                time = "time",
                subject = "id",
                curveType = logistic())


###### Ok, everything above is setup

## Functions for perm test

getT <- function(x, idx, whole = FALSE) {
  x$group <- x$group[idx]
  fit_s <- split(x, by = "group")
  mvl <- lapply(fit_s, function(x) {
    cc <- coef(x)
    time <- seq(0, 2000, by = 4)
    cl <- apply(cc, 1, function(y) eyetrackSim:::logistic_f(y, time))
    mm <- rowMeans(cl)
    vv <- apply(cl, 1, var)
    vvn <- vv/nrow(cc)
    list(mean = mm, nvar = vvn)
  })
  
  ## Guess I don't need a function for this
  x <- mvl[[1]]; y <- mvl[[2]]
  xm <- x$mean; xv <- x$nvar
  ym <- y$mean; yv <- y$nvar
  Tt <- abs(xm-ym) / sqrt(yv + xv)
  
  ifelse(whole, return(Tt), return(max(Tt)))
}

getTDist <- function(x, P = 1000) {
  n <- nrow(x)
  permmat <- replicate(P, sample(seq_len(n), n))
  tvec <- getT(x, seq_len(n), whole = TRUE)
  tnull <- apply(permmat, 2, function(y) {
    getT(x, y)
  })
}


# x is fitted object, b is bootstrap
bsPars <- function(x, b = 1000) {
  bsPars2 <- function(x) {
    idx <- sample(seq_len(nrow(x)), replace = TRUE)
    xn <- x[idx, ]
    xn$splitvar <- seq_len(nrow(x))
    xns <- split(xn, by = "splitvar")
    xpar <- vapply(xns, function(z) {
      rmvnorm(1, coef(z), vcov(z$fit[[1]]))
    }, numeric(4))
    rowMeans(xpar)
  }
  tt <- replicate(b, bsPars2(x), simplify = TRUE) |> t()
}

plotBoot <- function(x) {
  A <- x$curveList$A$curveMat
  B <- x$curveList$B$curveMat
  
  qa <- apply(B, 2, function(q) quantile(q, probs = c(0.025, 0.975)))
  qa <- cbind(t(qa), colMeans(A))
  qb <- apply(B, 2, function(q) quantile(q, probs = c(0.025, 0.975)))
  qb <- cbind(t(qb), colMeans(B))
  
  matplot(qa, ylim = c(0, 1), type = 'l', lty = c(2, 2, 1), lwd = 2, col = 'navy', 
          main = "bdotsBoot")
  matlines(qb, lty = c(2, 2, 1), lwd = 2, col = "brown")
  
  st <- x$sigTime
  time <- attr(x, "bdObjAttr")$time
  if (!is.null(st)) {
    for (i in seq_len(nrow(st))) {
      rr <- st[i, ]
      abline(v = which(time == rr[1]), lty = 2, col = 'red')
      abline(v = which(time == rr[2]), lty = 2, col = 'red')
    }
  }
}


### Do the work
fit <- bdotsFit(data = dt,
                group = "group",
                y = "fixations",
                time = "time",
                subject = "id",
                curveType = logistic())
## From bootstrap
bsa <- bsPars(fit[group == "A", ], 1000)
bsb <- bsPars(fit[group == "B", ], 1000)
time <- seq(0, 2000, by = 4)
cla <- apply(bsa, 1, function(p) eyetrackSim:::logistic_f(p, time))
clb <- apply(bsb, 1, function(p) eyetrackSim:::logistic_f(p, time))

qa <- apply(cla, 1, function(q) quantile(q, probs = c(0.025, 0.975)))
qa <- cbind(t(qa), rowMeans(cla))

qb <- apply(clb, 1, function(q) quantile(q, probs = c(0.025, 0.975)))
qb <- cbind(t(qb), rowMeans(clb))

actual_t <- getT(fit, seq_len(nrow(fit)), whole = TRUE)
t500 <- getTDist(fit, 1000)
tq <- quantile(t500, probs = 0.975)

plot(actual_t, type = 'l')
abline(h = tq, lty = 2, col='red')

## Indices where significant diff based on null
# (here, i know its a sequence but i need to tighten this for general use)
idx <- which(actual_t > tq)
sig_time <- time[idx]

boot <- bdotsBoot(y ~ group(A, B), fit)
plot(boot, plotDiffs = FALSE)

par(mfrow = c(1, 2))
matplot(qa, ylim = c(0, 1), type = 'l', lty = c(2, 2, 1), lwd = 2, col = 'navy', 
        main = "bs intervals and perm diff")
matlines(qb, lty = c(2, 2, 1), lwd = 2, col = "brown")
abline(v = min(idx), col = 'red', lty = 2) # use idx bc curves on idx scale not time
abline(v = max(idx), col = 'red', lty = 2)
plotBoot(boot)


