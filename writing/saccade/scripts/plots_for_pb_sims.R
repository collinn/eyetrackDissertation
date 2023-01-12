
# need to check bias of parameters
# need to look at representative curves

library(eyetrackSim)
library(bdots)
library(ggplot2)
library(gridExtra)

## This needs to be updated with correct data
load("~/dissertation/writing/saccade/data/index.RData")


subsetSim <- function(ss, idx) {
  ss$trialData <- ss$trialData[id %in% idx, ]
  ss$fixations <- ss$fixations[id %in% idx, ]
  pp <- ss$subPars
  pp$pars <- pp$pars[id %in% idx, ]
  pp$em <- pp$em[id  %in% idx, ]
  pp$emT <- pp$emT[id  %in% idx, ]
  ss$subPars <- pp
  ss
}

getParBias <- function(ss, ff) {
  tp <- as.matrix(ss$subPars$pars[, 2:5])
  fp <- coef(ff)
  bb <- suppressWarnings(melt(as.data.table(tp-fp))) #obs bias
}

## Starting with fixed (0 delay)
rr <- coef(fit_sac_fixed2)
idx <- which(rr[,4] == 0 | rr[,2] < rr[,1] | rr[,3] < 0)
idx <- setdiff(1:1000, idx)

fsim <- subsetSim(sim_fixed, idx)
ff <- fit_fix_fixed[idx, ]
ss <- fit_sac_fixed2[idx, ]


bb <- getParBias(fsim, ff)
pdf("../img/fixation_fixed_par_bias.pdf", 
    width = 6.5, height = 4.5)
ggplot(bb, aes(x = value)) + geom_histogram(bins=40) +
  geom_vline(xintercept = 0, color = 'red') +
  facet_wrap(~variable, scales = "free") +
  labs(y = "Number of Runs", x = "Bias") +
  theme_bw() +
  #theme_bw(base_size = 16) +
  ggtitle("Parameter Bias, Fixation Fixed Delay")
dev.off()


## Biasa with saccade
bb2 <- getParBias(fsim, ss)
pdf("../img/saccade_fixed_par_bias.pdf", 
    width = 6.5, height = 4.5)
ggplot(bb2, aes(x = value)) + geom_histogram(bins=40) +
  geom_vline(xintercept = 0, color = 'red') +
  facet_wrap(~variable, scales = "free") +
  labs(y = "Number of Runs", x = "Bias") +
  #theme_bw(base_size = 16) +
  theme_bw() +
  ggtitle("Parameter Bias, Saccade Fixed Delay")
dev.off()

## Now with random delay
rr <- coef(fit_sac_random)
idx <- which(rr[,4] == 0 | rr[,2] < rr[,1] | rr[,3] < 0)
idx <- setdiff(1:1000, idx)

fsim <- subsetSim(sim_random, idx)
ff <- fit_fix_random[idx, ]
ss <- fit_sac_random[idx, ]

bb3 <- getParBias(fsim, ff)
pdf("../img/fixation_random_par_bias.pdf", 
    width = 6.5, height = 4.5)
ggplot(bb3, aes(x = value)) + geom_histogram(bins=40) +
  geom_vline(xintercept = 0, color = 'red') +
  facet_wrap(~variable, scales = "free") +
  labs(y = "Number of Runs", x = "Bias") +
  #theme_bw(base_size = 16) +
  theme_bw() +
  ggtitle("Parameter Bias, Fixation Random Delay")
dev.off()

bb4 <- getParBias(fsim, ss)
pdf("../img/saccade_random_par_bias.pdf", 
    width = 6.5, height = 4.5)
ggplot(bb4, aes(x = value)) + geom_histogram(bins=40) +
  geom_vline(xintercept = 0, color = 'red') +
  facet_wrap(~variable, scales = "free") +
  labs(y = "Number of Runs", x = "Bias") +
  #theme_bw(base_size = 16) +
  theme_bw() +
  ggtitle("Parameter Bias, Saccade Random Delay")
dev.off()


###################################################
## Time for representative curves for each group ##
###################################################

## NOTE: F < G < S fucks up ordering on the legend

## Starting with fixed

truep <- subsetSim(sim_fixed, 1:10)$subPars$pars[, 2:5] |> as.matrix()
sacp <- coef(fit_sac_fixed2)[1:10, ]
fixp <- coef(fit_fix_fixed)[1:10, ]

getCurve <- function(pp, type) {
  pp <- split(pp, seq_len(nrow(pp)))
  ff <- function(p) logistic_f(p, 0:2000)
  tc <- lapply(pp, function(x) data.table(curve = ff(x), Curve = type, time = 0:2000))
  for (i in seq_along(tc)) {
    tc[[i]]$id <- i
  }
  tc <- rbindlist(tc)
}

trueCurve <- getCurve(truep, "Generating")
sacCurve <- getCurve(sacp, "Saccade")
fixCurve <- getCurve(fixp, "Fixation")

fixxy <- rbindlist(list(trueCurve, fixCurve))

pdf("../img/fixation_fixed_pb_curves.pdf", 
    width = 6.5, height = 4.5)
ggplot(fixxy, aes(time, curve, color = Curve)) + 
  geom_line(lwd = 1) + facet_wrap(~id, nrow = 2) + 
  theme_bw() + labs(y = "Activation", x = "Time") + 
  ggtitle("Representative Curves, Fixation, Fixed Delay")
dev.off()


saccy <- rbindlist(list(trueCurve, sacCurve))

pdf("../img/saccade_fixed_pb_curves.pdf", 
    width = 6.5, height = 4.5)
ggplot(saccy, aes(time, curve, color = Curve)) + 
  geom_line(lwd = 1) + facet_wrap(~id, nrow = 2) + 
  theme_bw() + labs(y = "Activation", x = "Time") + 
  ggtitle("Representative Curves, Saccade, Fixed Delay")
dev.off()



#### Now with random delay

# dumb workaround since i have wrong bdots thing here
rr <- coef(fit_sac_random)
idx <- which(rr[,4] == 0 | rr[,2] < rr[,1] | rr[,3] < 0)
idx <- setdiff(1:1000, idx)
idx <- idx[1:10] # still just get first 10

truep <- subsetSim(sim_random, idx)$subPars$pars[, 2:5] |> as.matrix()
sacp <- coef(fit_sac_random)[idx, ]
fixp <- coef(fit_fix_random)[idx, ]

trueCurve <- getCurve(truep, "Generating")
sacCurve <- getCurve(sacp, "Saccade")
fixCurve <- getCurve(fixp, "Fixation")

fixxy <- rbindlist(list(trueCurve, fixCurve))

pdf("../img/fixation_random_pb_curves.pdf", 
    width = 6.5, height = 4.5)
ggplot(fixxy, aes(time, curve, color = Curve)) + 
  geom_line(lwd = 1) + facet_wrap(~id, nrow = 2) + 
  theme_bw() + labs(y = "Activation", x = "Time") + 
  ggtitle("Representative Curves, Fixation, Random Delay")
dev.off()


saccy <- rbindlist(list(trueCurve, sacCurve))


## Some of these are incredibly obviously incorrect
pdf("../img/saccade_random_pb_curves.pdf", 
    width = 6.5, height = 4.5)
ggplot(saccy, aes(time, curve, color = Curve)) + 
  geom_line(lwd = 1) + facet_wrap(~id, nrow = 2) + 
  theme_bw() + labs(y = "Activation", x = "Time") + 
  ggtitle("Representative Curves, Saccade, Random Delay")
dev.off()










