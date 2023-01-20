library(bdots)
library(eyetrackSim)
library(mvtnorm)

## Start by generating an empirical distribution
ci <- as.data.table(ci)
ci <- ci[LookType == "Target", ]
fit <- bdotsFit(data = ci,
                y = "Fixations",
                subject = "Subject",
                time = "Time",
                group = "protocol", curveType = logistic())

## Except this time I only need one empirical dist
## Here I am using BOTH TD/ND to create more variability
cc <- coef(fit)
vv <- var(cc)
cc <- colMeans(cc)
cc[1] <- abs(cc[1])
cc[2] <- pmin(cc[2], 1)
pars <- list(mean = cc, sigma = vv)

## Will always create twice what N is (so two groups by default)
createData <- function(n = 25, trials = 10, pars, paired = FALSE, pairMagnitude = 0.1) {
  time <- seq(0, 2000, by = 4)
  newpars <- do.call(rmvnorm, as.list(c(n, pars)))
  newpars[,1] <- abs(newpars[,1]) # need base > 0
  newpars[,2] <- pmin(newpars[,2], 1) # need peak < 1
  spars <- split(newpars, row(newpars))
  dts1 <- lapply(seq_len(n), function(x) {
    pp <- spars[[x]]
    dt <- data.table(id = x,
                     time = time,
                     group = "A",
                     true = eyetrackSim:::logistic_f(pp, time))
    dt[, fixations := mean(rbinom(trials, 1, true)), by = time]
  })
  
  dts1 <- rbindlist(dts1)
  
  ## Then we make our parameters for group 2
  if (paired) {
    ## Basically just repeat above, exact same distribution
    newpars2 <- do.call(rmvnorm, as.list(c(n, pars)))
    newpars2[,1] <- abs(newpars2[,1]) # need base > 0
    newpars2[,2] <- pmin(newpars2[,2], 1) # need peak < 1
  } else {
    ## Keep the original pars from newpars
    orig_pars <- newpars
    ## Then make one with mean 0
    pars2 <- pars
    pars2$mean[] <- 0
    pars2$sigma <- pars2$sigma*pairMagnitude
    ## This gets the variance
    varpars <- do.call(rmvnorm, as.list(c(n, pars2)))
    ## And then we make our paired parameters
    newpars2 <- orig_pars + varpars
    newpars2[,1] <- abs(newpars2[,1]) # need base > 0
    newpars2[,2] <- pmin(newpars2[,2], 1) # need peak < 1
  }
  
  spars2 <- split(newpars2, row(newpars2))
  dts2 <- lapply(seq_len(n), function(x) {
    pp <- spars2[[x]]
    dt <- data.table(id = x,
                     time = time,
                     group = "B",
                     true = eyetrackSim:::logistic_f(pp, time))
    dt[, fixations := mean(rbinom(trials, 1, true)), by = time]
  })
  dts2 <- rbindlist(dts2)
  dts <- rbindlist(list(dts1, dts2))

  return(list(dts = dts, parsA = newpars, parsB = newpars2))
}

tt <- createData(n = 2, pars = pars, paired = TRUE, pairMagnitude = 0.01)
