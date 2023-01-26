library(bdots)
library(eyetrackSim)
library(mvtnorm)

## Start by generating an empirical distribution
ci <- as.data.table(ci)
ci <- ci[LookType == "Target", ]
#ci <- ci[LookType == "Target" & protocol != "NH", ]
fit <- bdotsFit(data = ci,
                y = "Fixations",
                subject = "Subject",
                time = "Time",
                group = "protocol", curveType = logistic())

## Except this time I only need one empirical dist
## Here I am using BOTH TD/ND to create more variability
cc <- coef(fit[])
vv <- var(cc)
cc <- colMeans(cc)
cc[1] <- abs(cc[1])
cc[2] <- pmin(cc[2], 1)
pars <- list(mean = cc, sigma = vv)

## Will always create twice what N is (so two groups by default)
createData <- function(n = 25, trials = 10, pars, paired = FALSE, pairMag = 0.05) {
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
  if (!paired) {
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
    pars2$sigma <- pars2$sigma*pairMag
    ## This gets the variance
    varpars <- do.call(rmvnorm, as.list(c(n, pars2)))
    ## And then we make our paired parameters
    newpars2 <- orig_pars + varpars
    newpars2[,1] <- abs(newpars2[,1]) # need base > 0
    newpars2[,2] <- pmin(newpars2[,2], 1) # need peak < 1
  }
  spars2 <- split(newpars2, row(newpars2))
  # id not correct for paired here
  dts2 <- lapply(seq_len(n) + n, function(x) {
    pp <- spars2[[x - n]]
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



runSim <- function(n = 25, trials = 100, pars,
                   paired = FALSE, pairedMag = 0.05) {

  tt <- createData(n, trials, pars, paired, pairedMag)

  dat <- tt$dts
  fit <- bdotsFit(subject = "id",
                  time = "time",
                  y = "fixations",
                  group = "group",
                  dat = dat,
                  curveType = logistic(),
                  cores = 7L)

  boot <- bdotsBoot(formula = y ~ group(A,B), bdObj = fit, Niter = 250, cores = 7L)
  suppressMessages(bootp <- bdotsBoot(formula = y ~ group(A,B), bdObj = fit, Niter = 250,
                     permutation = TRUE, skipDist = TRUE, cores = 7L))

  list(bootSig = boot$sigTime, permSig = bootp$sigTime)
}

#
#sims <- replicate(1000, runSim(pars = pars))
#sim2 <- replicate(500, runSim(n = 10, pars = pars))
#sim3 <- replicate(500, runSim(n = 50, pars = pars))
#sim4 <- replicate(500, runSim(trials = 300, pars = pars))

## These are all unpaired

## Prep this for argon submission
idx <- as.numeric(commandArgs(TRUE))

## Standard with 100 trials, try with n = 10, 25, 50, 100

N <- 2

if (idx == 1) {
  sim1 <- replicate(N, runSim(n = 10, pars = pars))
  sim2 <- replicate(N, runSim(n = 25, pars = pars))
  sim3 <- replicate(N, runSim(n = 50, pars = pars))
  sim4 <- replicate(N, runSim(n = 100, pars = pars))
  save.image(file = "sim_tie_1.RData")
} else if (idx == 2) {
  sim5 <- replicate(N, runSim(n = 10, trials = 300, pars = pars))
  sim6 <- replicate(N, runSim(n = 25, trials = 300, pars = pars))
  sim7 <- replicate(N, runSim(n = 50, trials = 300, pars = pars))
  sim8 <- replicate(N, runSim(n = 100, trials = 300, pars = pars))
  save.image(file = "sim_tie_2.RData")
} else {
  sim9 <- replicate(N, runSim(n = 10, trials = 50, pars = pars))
  sim10 <- replicate(N, runSim(n = 25, trials = 50, pars = pars))
  sim11 <- replicate(N, runSim(n = 50, trials = 50, pars = pars))
  sim12 <- replicate(N, runSim(n = 100, trials = 50, pars = pars))
  save.image(file = "sim_tie_3.RData")
}



## Then try with 300 trials, n = 10, 25, 50, 100


## Then try with 50 trials, n = 10, 25, 50, 100



save.image(file = "sim_tie_practice.RData")

#load("sim_tie_practice.RData")
#load("sim_tie.RData")

#btie <- apply(sims, 2, function(y) !is.null(y[[1]])) |> mean()
#ptie <- apply(sims, 2, function(y) !is.null(y[[2]])) |> mean()

#
# tie <- function(x) {
#   a <- apply(x, 2, function(y) !is.null(y[[1]])) |> mean()
#   b <- apply(x, 2, function(y) !is.null(y[[2]])) |> mean()
#   c(boot = a, perm = b)
# }
#
# # standard
# tie(sim1)
# tie(sim2)
# tie(sim3)
# tie(sim4)
#
# # more trials
# tie(sim5)
# tie(sim6)
# tie(sim7)
# tie(sim8)
#
# # less trials
# tie(sim9)
# tie(sim10)
# tie(sim11)
# tie(sim12)
