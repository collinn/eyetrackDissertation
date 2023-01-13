
# need to check bias of parameters
# need to look at representative curves
# also look at average mise between each group

# Maybe I'll try this again with FBS+T, the saccade clearly wins but by
# less than it did previously

library(eyetrackSim)
library(bdots)
library(ggplot2)
library(gridExtra)

## This needs to be updated with correct data
load("~/dissertation/writing/saccade/data/pb_data_sim.RData")


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

getRmvIdx <- function(ff) {
  rr <- coef(ff)
  idx <- which(rr[,4] == 0 | rr[,2] < rr[,1] | rr[,3] < 0)
  idx
}

## Starting with fixed (0 delay)
rr <- coef(fit_sac_fixed)
idx1 <- which(rr[,4] == 0 | rr[,2] < rr[,1] | rr[,3] < 0)
rr <- coef(fit_fix_fixed)
idx2 <- which(rr[,4] == 0 | rr[,2] < rr[,1] | rr[,3] < 0)
idx <- setdiff(1:1000, c(idx1, idx2))


fsim <- subsetSim(sim_fixed, idx)
ff <- fit_fix_fixed[idx, ]
ss <- fit_sac_fixed[idx, ]


### ORRRR Maybe let's grid arrange these so they combined
bb <- getParBias(fsim, ff)
bb2 <- getParBias(fsim, ss)

p1 <- ggplot(bb, aes(x = value)) + geom_histogram(bins=40) +
  geom_vline(xintercept = 0, color = 'red') +
  facet_wrap(~variable, scales = "free") +
  labs(y = "Number of Runs", x = "Bias") +
  theme_bw() +
  #theme_bw(base_size = 16) +
  ggtitle("Fixation Parameter Bias, Fixed Delay")

p2 <- ggplot(bb2, aes(x = value)) + geom_histogram(bins=40) +
  geom_vline(xintercept = 0, color = 'red') +
  facet_wrap(~variable, scales = "free") +
  labs(y = "Number of Runs", x = "Bias") +
  #theme_bw(base_size = 16) +
  theme_bw() +
  ggtitle("Saccade Parameter Bias, Fixed Delay")

pdf("../img/fixed_delay_par_bias.pdf", 
    width = 6.5, height = 7.5)
grid.arrange(p1, p2, ncol = 1)
dev.off()


## Now with random delay (lost about 10%)
rr <- coef(fit_sac_random)
idx1 <- which(rr[,4] == 0 | rr[,2] < rr[,1] | rr[,3] < 0)
rr <- coef(fit_fix_random)
idx2 <- which(rr[,4] == 0 | rr[,2] < rr[,1] | rr[,3] < 0)
idx <- setdiff(1:1000, c(idx1, idx2))

fsim <- subsetSim(sim_random, idx)
ff <- fit_fix_random[idx, ]
ss <- fit_sac_random[idx, ]

bb3 <- getParBias(fsim, ff)
bb4 <- getParBias(fsim, ss)

p1 <- ggplot(bb3, aes(x = value)) + geom_histogram(bins=40) +
  geom_vline(xintercept = 0, color = 'red') +
  facet_wrap(~variable, scales = "free") +
  labs(y = "Number of Runs", x = "Bias") +
  #theme_bw(base_size = 16) +
  theme_bw() +
  ggtitle("Fixation Parameter Bias, Random Delay")

p2 <- ggplot(bb4, aes(x = value)) + geom_histogram(bins=40) +
  geom_vline(xintercept = 0, color = 'red') +
  facet_wrap(~variable, scales = "free") +
  labs(y = "Number of Runs", x = "Bias") +
  #theme_bw(base_size = 16) +
  theme_bw() +
  ggtitle("Saccade Parameter Bias, Random Delay")


pdf("../img/random_delay_par_bias.pdf", 
    width = 6.5, height = 7.5)
grid.arrange(p1, p2, ncol = 1)
dev.off()

###################################################
## Time for representative curves for each group ##
###################################################

## NOTE: F < G < S fucks up ordering on the legend

## Starting with fixed

## Get not fucked up ones first
rr <- coef(fit_sac_fixed)
idx1 <- which(rr[,4] == 0 | rr[,2] < rr[,1] | rr[,3] < 0)
rr <- coef(fit_fix_fixed)
idx2 <- which(rr[,4] == 0 | rr[,2] < rr[,1] | rr[,3] < 0)

## Only need first 10
idx <- setdiff(1:1000, c(idx1, idx2))[1:10]

truep <- subsetSim(sim_fixed, idx)$subPars$pars[, 2:5] |> as.matrix()
sacp <- coef(fit_sac_fixed)[idx, ]
fixp <- coef(fit_fix_fixed)[idx, ]

getCurve <- function(pp, type) {
  pp <- split(pp, seq_len(nrow(pp)))
  #rrr <- seq(0, 2000, by = 200)
  rrr <- 0:2000
  ff <- function(p) logistic_f(p, rrr)
  tc <- lapply(pp, function(x) data.table(curve = ff(x), Curve = type, time = rrr))
  for (i in seq_along(tc)) {
    tc[[i]]$id <- i
  }
  tc <- rbindlist(tc)
}

trueCurve <- getCurve(truep, "Generating")
sacCurve <- getCurve(sacp, "Saccade")
fixCurve <- getCurve(fixp, "Fixation")

## OR we combine above
plotty <- rbindlist(list(trueCurve, fixCurve, sacCurve))

pdf("../img/fixed_pb_curves.pdf",
    width = 7.5, height = 4)
ggplot(plotty, aes(time, curve, color = Curve)) +
  geom_line(linewidth = 1) + facet_wrap(~id, nrow = 2) +
  theme_bw() + labs(y = "Activation", x = "Time") +
  ggtitle("Representative Curves, Fixed Delay") + 
  scale_x_discrete(limits = c(0, 750, 1500))
dev.off()

#### Now with random delay

# dumb workaround since i have wrong bdots thing here
rr <- coef(fit_sac_random)
idx1 <- which(rr[,4] == 0 | rr[,2] < rr[,1] | rr[,3] < 0)
rr <- coef(fit_fix_random)
idx2 <- which(rr[,4] == 0 | rr[,2] < rr[,1] | rr[,3] < 0)
idx <- setdiff(1:1000, c(idx1, idx2))[1:10]

truep <- subsetSim(sim_random, idx)$subPars$pars[, 2:5] |> as.matrix()
sacp <- coef(fit_sac_random)[idx, ]
fixp <- coef(fit_fix_random)[idx, ]

trueCurve <- getCurve(truep, "Generating")
sacCurve <- getCurve(sacp, "Saccade")
fixCurve <- getCurve(fixp, "Fixation")

plotty <- rbindlist(list(trueCurve, fixCurve, sacCurve))

pdf("../img/random_pb_curves.pdf",
    width = 7.5, height = 4)
ggplot(plotty, aes(time, curve, color = Curve)) +
  geom_line(linewidth = 1) + facet_wrap(~id, nrow = 2) +
  theme_bw() + labs(y = "", x = "Time") +
  ggtitle("Representative Curves, Random Delay") + 
  scale_x_discrete(limits = c(0, 750, 1500))
dev.off()


####### Let's make tables, yo
mise <- function(fp, tp) {
  times <- 0:2000
  fp <- split(fp, 1:nrow(fp))
  tp <- split(tp, 1:nrow(tp))
  
  mv <- Map(function(x, y) {
    g <- function(tt) {
      (logistic_f(x, tt) - logistic_f(y, tt))^2
    }
    integrate(g, lower = min(times), upper = max(times))$value
  }, fp, tp)
  mv <- unlist(mv, use.names = FALSE)
}


## Takes fp fitted function and tp true function pars
idx1 <- getRmvIdx(fit_sac_fixed)
idx2 <- getRmvIdx(fit_fix_fixed)
idx <- setdiff(1:1000, union(idx1, idx2))

fixpars <- coef(fit_fix_fixed)[idx, ]
sacpars <- coef(fit_sac_fixed)[idx, ]
truepars <- subsetSim(sim_fixed, idx)$subPars$pars[, 2:5] |> as.matrix()

fp <- fixpars
tp <- truepars

## For fixed delay
rr1 <- mise(fixpars, truepars)
rr2 <- mise(sacpars, truepars)

## Random delay
idx1 <- getRmvIdx(fit_sac_random)
idx2 <- getRmvIdx(fit_fix_random)
idx <- setdiff(1:1000, union(idx1, idx2))

fixpars <- coef(fit_fix_random)[idx, ]
sacpars <- coef(fit_sac_random)[idx, ]
truepars <- subsetSim(sim_random, idx)$subPars$pars[, 2:5] |> as.matrix()

rr3 <- mise(fixpars, truepars)
rr4 <- mise(sacpars, truepars)


library(xtable)
## This is comically retarded
ss1 <- setnames(transpose(data.table(as.numeric(summary(rr1)))), names(summary(rr1)))
ss2 <- setnames(transpose(data.table(as.numeric(summary(rr2)))), names(summary(rr2)))
ss3 <- setnames(transpose(data.table(as.numeric(summary(rr3)))), names(summary(rr3)))
ss4 <- setnames(transpose(data.table(as.numeric(summary(rr4)))), names(summary(rr4)))

ss <- rbindlist(list(ss1, ss2, ss3, ss4))
nn <- data.table(Curve = c("Fixation", "Saccade", "Fixation", "Saccade"), 
                 Delay = c("Fixed", "Fixed", "Random", "Random"))
ss <- cbind(nn, ss)
print(xtable(ss), include.rownames=FALSE)
