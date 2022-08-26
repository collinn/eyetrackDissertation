
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

### Everything above this is just setup (same as for coverage simulator)
### Everything below this is related to permutation tests for differences

## assuming we have a fitted object to begin
P <- 500 # number of permutations
n <- nrow(fit)

# each column a permutation
permmat <- replicate(P, sample(seq_len(n), n))

curperm <- permmat[, 2]


## Actually, start here with T(t) function

idx <- permmat[, 2]
x <- fit
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


## Insignificant difference between distribution of 500 and 1000

t500 <- getTDist(fit, 500)
t1000 <- getTDist(fit, 1000)
hist(t1000)
hist(t500, col = 'blue', add = TRUE)
abline(v = quantile(t500, probs = 0.975), col = 'blue', lwd = 2)
abline(v = quantile(t1000, probs = 0.975), col = 'red', lwd = 2)

quantile(t500, probs = 0.975)
quantile(t1000, probs = 0.975)































