
library(eyetrackSim)
library(bdots)

## run one iteration of sim

## For logistic, idx = 1:3 for different delay
runSim_lg <- function(idx = 1) {

  ## Because of active binding
  if (exists("od")) rm(od)

  ## Which delay to use
  if (idx == 1) {
    od <- 0
  } else if (idx == 2) {
    nn <- function() rnorm(1, 200, 15) #abs(rnorm(1, 200, sd = 30))
    makeActiveBinding("od", nn, .GlobalEnv)
  } else if (idx == 3) {
    wb <- function() rweibull(1, shape = 1.8, scale = 224.9)
    makeActiveBinding("od", wb, .GlobalEnv)
  }

  getRmvIdx <- function(ff) {
    rr <- coef(ff)
    idx <- which(rr[,4] <= 0 | rr[,2] < rr[,1] | rr[,3] < 0)
    idx
  }

  ## Create group 1
  gp1 <- runSim_compare(nsub = 25, group = "A", alt = FALSE, omDelay = od)

  ## Create group 2
  gp2 <- runSim_compare(nsub = 25, group = "B", alt = TRUE, omDelay = od)


  fixate <- rbind(gp1$trialData, gp2$trialData)
  onset <- rbind(gp1$fixations, gp2$fixations)

  ## Oops gotta do this
  if (idx != 1) {
    fixate[, times := times - 200L]
    onset[, starttime := starttime - 200L]
  }

  fit_onset <- bdotsFit(onset,
                        subject = "id",
                        time = "starttime",
                        group = "group",
                        y = "looks",
                        curveType = logistic_sac(startSamp = 12),
                        cores = 4)

  fit_fix <- bdotsFit(fixate,
                      subject = "id",
                      time = "times",
                      group = "group",
                      y = "looks",
                      curveType = logistic_sac(startSamp = 12),
                      cores = 4)

  idx1 <- getRmvIdx(fit_onset)
  idx2 <- getRmvIdx(fit_fix)
  idx <- union(idx1, idx2)
  keepidx <- setdiff(1:50, idx)

  fit_onset <- fit_onset[keepidx, ]
  fit_fix <- fit_fix[keepidx, ]

  onset_st <- bdotsBoot(y ~ group(A, B), fit_onset, cores = 4)$sigTime
  fix_st <- bdotsBoot(y ~ group(A, B), fit_fix, cores = 4)$sigTime
  return(list(onset_sig = onset_st, fix_sig = fix_st))
}


## For doubleGuass, idx = 4:6 for different delay
runSim_dg <- function(idx = 4) {

  ## Because of active binding
  if (exists("od")) rm(od)

  ## Which delay to use
  if (idx == 4) {
    od <- 0
  } else if (idx == 5) {
    nn <- function() rnorm(1, 200, 15) #abs(rnorm(1, 200, sd = 30))
    makeActiveBinding("od", nn, .GlobalEnv)
  } else if (idx == 6) {
    wb <- function() rweibull(1, shape = 1.8, scale = 224.9)
    makeActiveBinding("od", wb, .GlobalEnv)
  }

  getRmvIdx <- function(ff) {
    rr <- coef(ff)
    idx <- which(rr[,3] < 10 | rr[,4] < 10 | rr[,2] < rr[,5] | rr[,2] < rr[,6])
    idx
  }

  ## Create group 1
  gp1 <- runSim_compare(nsub = 25, group = "A", alt = FALSE,
                        omDelay = od, fnct = "doubleGauss")

  ## Create group 2
  gp2 <- runSim_compare(nsub = 25, group = "B", alt = TRUE,
                        omDelay = od, fnct = "doubleGauss")


  fixate <- rbind(gp1$trialData, gp2$trialData)
  onset <- rbind(gp1$fixations, gp2$fixations)


  ## Oops gotta do this
  if (idx != 4) {
    fixate[, times := times - 200L]
    onset[, starttime := starttime - 200L]
  }


  fit_onset <- bdotsFit(onset,
                        subject = "id",
                        time = "starttime",
                        group = "group",
                        y = "looks",
                        curveType = doubleGauss_sac(startSamp = 12),
                        cores = 4)

  fit_fix <- bdotsFit(fixate,
                      subject = "id",
                      time = "times",
                      group = "group",
                      y = "looks",
                      curveType = doubleGauss_sac(startSamp = 12),
                      cores = 4)

  idx1 <- getRmvIdx(fit_onset)
  idx2 <- getRmvIdx(fit_fix)
  idx <- union(idx1, idx2)
  keepidx <- setdiff(1:50, idx)

  fit_onset <- fit_onset[keepidx, ]
  fit_fix <- fit_fix[keepidx, ]

  onset_st <- bdotsBoot(y ~ group(A, B), fit_onset, cores = 4)$sigTime
  fix_st <- bdotsBoot(y ~ group(A, B), fit_fix, cores = 4)$sigTime
  return(list(onset_sig = onset_st, fix_sig = fix_st))
}

N <- 1000

idx <- as.numeric(commandArgs(TRUE))

if (idx %in% 1:3) {
  res <- replicate(n = N, expr = runSim_lg(idx))
} else {
  res <- replicate(n = N, expr = runSim_dg(idx))
}

rf <- paste0("rds_files/sim", idx, ".rds")

saveRDS(res, rf)
#
# tt <- replicate(n = N, expr = runSim_lg(idx = 1))
# tt2 <- replicate(n = N, expr = runSim_lg(idx = 2))
# tt3 <- replicate(n = N, expr = runSim_lg(idx = 3))
#
# tt4 <- replicate(n = N, expr = runSim_dg(idx = 4))
# tt5 <- replicate(n = N, expr = runSim_dg(idx = 5))
# tt6 <- replicate(n = N, expr = runSim_dg(idx = 6))
