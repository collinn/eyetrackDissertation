
# need to check bias of parameters
# need to look at representative curves
# also look at average mise between each group

# Maybe I'll try this again with FBS+T, the saccade clearly wins but by
# less than it did previously

library(bdots)
library(ggplot2)
library(gridExtra)
library(eyetrackSim)

#load("~/dissertation/writing/saccade/data/pb_data_sim.RData")
#load("~/packages/eyetrackSim/analysis/pb_data_sim_no_fbst.RData")
#load("~/packages/eyetrackSim/analysis/pb_data_sim_fbst.RData")
# load("~/packages/eyetrackSim/analysis/pb_data_sim_fbst_no_start_par.RData")
# load("~/packages/eyetrackSim/analysis/pb_data_sim_no_fbst_no_start_par.RData")
#
# load("~/packages/eyetrackSim/analysis/pb_data_sim_fbst_normal.RData")
load("~/packages/eyetrackSim/analysis/pb_data_sim_no_fbst_normal.RData")
load("~/packages/eyetrackSim/analysis/no_min_pb_data_sim_no_fbst_normal.RData")


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
  bb <- suppressWarnings(melt(as.data.table(fp-tp))) #obs bias
}

getRmvIdx <- function(ff) {
  rr <- coef(ff)
  idx <- which(rr[,4] <= 0 | rr[,2] < rr[,1] | rr[,3] < 0)
  idx
}

biasPlot <- function(fix, sac, sim, tit, xint = 0) {
  idx1 <- getRmvIdx(fix)
  idx2 <- getRmvIdx(sac)
  idx <- setdiff(seq_len(nrow(fix)), c(idx1, idx2))

  print(length(idx) / nrow(fix))

  fsim <- subsetSim(sim, idx)
  ff <- fix[idx, ]
  ss <- sac[idx, ]

  bb <- getParBias(fsim, ff)
  bb2 <- getParBias(fsim, ss)

  p1 <- ggplot(bb, aes(x = value)) + geom_histogram(bins=40) +
    geom_vline(xintercept = xint, color = 'red') +
    facet_wrap(~variable, scales = "free") +
    labs(y = "Number of Runs", x = "Bias") +
    theme_bw() +
    #theme_bw(base_size = 16) +
    ggtitle(paste0("Parameter Bias for Proportions Method, ", tit))

  p2 <- ggplot(bb2, aes(x = value)) + geom_histogram(bins=40) +
    geom_vline(xintercept = xint, color = 'red') +
    facet_wrap(~variable, scales = "free") +
    labs(y = "Number of Runs", x = "Bias") +
    #theme_bw(base_size = 16) +
    theme_bw() +
    ggtitle(paste0("Parameter Bias for Look Onset Method, ", tit))
  return(list(p1, p2))
}

sampleCurvePlot <- function(fix, sac, sim, tit) {
  idx1 <- getRmvIdx(fix)
  idx2 <- getRmvIdx(sac)
  idx <- setdiff(seq_len(nrow(fix)), c(idx1, idx2))[11:20]
  # get pars
  truep <- subsetSim(sim, idx)$subPars$pars[, 2:5] |> as.matrix()
  fixp <- coef(fix[idx, ])
  sacp <- coef(sac[idx, ])

  getCurve <- function(pp, type) {
    pp <- split(pp, seq_len(nrow(pp)))
    rrr <- 0:2000
    ff <- function(p) logistic_f(p, rrr)
    tc <- lapply(pp, function(x) data.table(curve = ff(x), Curve = type, time = rrr))
    for (i in seq_along(tc)) {
      tc[[i]]$id <- i
    }
    tc <- rbindlist(tc)
  }
  trueCurve <- getCurve(truep, "True")
  sacCurve <- getCurve(sacp, "Look Onset")
  fixCurve <- getCurve(fixp, "Proportion")
  plotty <- rbindlist(list(trueCurve, fixCurve, sacCurve))

  pp <- ggplot(plotty, aes(time, curve, color = Curve, linetype = Curve)) +
    geom_line(linewidth = 1) +
    scale_linetype_manual(values = c("solid", "solid", "twodash")) +
    scale_color_manual(values = c("steelblue", "tomato", "black")) +
    facet_wrap(~id, nrow = 2) + theme_bw() + labs(y = "Activation", x = "Time") +
    ggtitle(paste0("Representative Curves, ", tit)) +
    suppressWarnings(scale_x_discrete(limits = c(0, 750, 1500))) + theme(legend.position = "bottom")

  pp
}

makeStatTable <- function(fix, sac, sim, tit, r2=FALSE) {

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

  get_r2 <- function(fp, tp) {
    times <- 0:2000
    fp <- split(fp, 1:nrow(fp))
    tp <- split(tp, 1:nrow(tp))

    rr <- Map(function(x, y) {
      yy <- logistic_f(y, times)
      yh <- logistic_f(x, times)
      sst <- sum((yy-mean(yy))^2)
      sse <- sum((yy - yh)^2)
      rr2 <- 1 - sse/sst
    }, fp, tp)
    rr <- unlist(rr, use.names = FALSE)
  }


  idx1 <- getRmvIdx(fix)
  idx2 <- getRmvIdx(sac)
  idx <- setdiff(seq_len(nrow(fix)), c(idx1, idx2))

  print(length(idx) / nrow(fix))

  # get pars
  truep <- subsetSim(sim, idx)$subPars$pars[, 2:5] |> as.matrix()
  fixp <- coef(fix[idx, ])
  sacp <- coef(sac[idx, ])

  if (r2 == TRUE) {
    mm1 <- get_r2(fixp, truep)
    mm2 <- get_r2(sacp, truep)
  } else {
    mm1 <- mise(fixp, truep)
    mm2 <- mise(sacp, truep)
  }
  ss1 <- setnames(transpose(data.table(as.numeric(summary(mm1)))), names(summary(mm1)))
  ss2 <- setnames(transpose(data.table(as.numeric(summary(mm2)))), names(summary(mm2)))
  ss <- rbindlist(list(ss1, ss2))

  nn <- data.table(Curve = c("Proportion", "Look Onset"),
                   Delay = rep(tit, 2))
  cbind(nn, ss)
}

############### END MAKING FUNCTIONS #################################

# for testing
# fix <- fit_fix_no_delay
# sac <- fit_sac_no_delay
# sim <- sim_no_delay
# tit <- ""
## Eh, not what we really are trying to go for here
ndtabr <- makeStatTable(fit_fix_no_delay, fit_sac_no_delay,
                       sim_no_delay, tit = "No Delay", r2 = TRUE)

unftabr <- makeStatTable(fit_fix_uniform, fit_sac_uniform,
                        sim_uniform, tit = "Uniform Delay", r2 = TRUE)

wbtabr <- makeStatTable(fit_fix_weibull, fit_sac_weibull,
                       sim_weibull, tit = "Weibull Delay", r2 = TRUE)
#
# bbtabr <- makeStatTable(fit_fix_beta, fit_sac_beta,
#                        sim_beta, tit = "Beta Delay", r2 = TRUE)
#
# nmtabr <- makeStatTable(fit_fix_normal, fit_sac_normal,
#                        sim_normal, tit = "Normal Delay", r2 = TRUE)
#
# misetabr <- rbindlist(list(ndtabr, unftabr, wbtabr, bbtabr, nmtabr))[order(Curve), ]
# misetabr

#print(xtable::xtable(misetab, label = "Summary of MISE across simulations"), include.rownames=FALSE)

## Or with MISE
ndtab <- makeStatTable(fit_fix_no_delay, fit_sac_no_delay,
                       sim_no_delay, tit = "No Delay", r2 = FALSE)

unftab <- makeStatTable(fit_fix_uniform, fit_sac_uniform,
                        sim_uniform, tit = "Uniform Delay", r2 = FALSE)

wbtab <- makeStatTable(fit_fix_weibull, fit_sac_weibull,
                       sim_weibull, tit = "Weibull Delay", r2 = FALSE)

bbtab <- makeStatTable(fit_fix_beta, fit_sac_beta,
                       sim_beta, tit = "Beta Delay", r2 = FALSE)

nmtab <- makeStatTable(fit_fix_normal, fit_sac_normal,
                       sim_normal, tit = "Normal Delay", r2 = FALSE)

misetab <- rbindlist(list(ndtab, unftab, wbtab))[order(Curve), ]
misetab
print(xtable::xtable(misetab, label = "Summary of MISE across simulations"), include.rownames=FALSE)
#print(xtable::xtable(misetab[, c(1:2, 4:5, 7)], label = "Summary of MISE across simulations"), include.rownames=FALSE)

misetab <- rbindlist(list(ndtab, nmtab, wbtab))[order(Curve), ]
misetab
print(xtable::xtable(misetab, label = "Summary of MISE across simulations"), include.rownames=FALSE)
#print(xtable::xtable(misetab[, c(1:2, 4:5, 7)], label = "Summary of MISE across simulations"), include.rownames=FALSE)



########################################################################


## No delay
pp <- biasPlot(fit_fix_no_delay,
               fit_sac_no_delay,
               sim_no_delay,
               tit = "No Delay",
               xint = 0)

# pdf("../img/no_delay_par_bias.pdf")
# grid.arrange(pp[[1]], pp[[2]])
# dev.off()

pdf("../img/no_delay_par_bias_onset.pdf", width = 6, height = 3)
pp[[2]]
dev.off()

pdf("../img/no_delay_par_bias_proportion.pdf", width = 6, height = 3)
pp[[1]]
dev.off()




## Now with Uniform delay (lost about 10%)
pp <- biasPlot(fit_fix_uniform,
               fit_sac_uniform,
               sim_uniform,
               tit = "Uniform Delay",
               xint = 0)

# pdf("../img/uniform_delay_par_bias.pdf")
# grid.arrange(pp[[1]], pp[[2]])
# dev.off()

pdf("../img/uniform_delay_par_bias_proportion.pdf", width = 6, height = 3)
pp[[1]]
dev.off()

pdf("../img/uniform_delay_par_bias_onset.pdf", width = 6, height = 3)
pp[[2]]
dev.off()

## Normal
pp <- biasPlot(fit_fix_normal,
               fit_sac_normal,
               sim_uniform,
               tit = "Normal Delay",
               xint = 0)

pdf("../img/normal_delay_par_bias_proportion.pdf", width = 6, height = 3)
pp[[1]]
dev.off()

pdf("../img/uniform_delay_par_bias_onset.pdf", width = 6, height = 3)
pp[[2]]
dev.off()

## Now with Weibull delay (lost about 10%)
pp <- biasPlot(fit_fix_weibull,
               fit_sac_weibull,
               sim_weibull,
               tit = "Weibull Delay",
               xint = 0)

# pdf("../img/weibull_delay_par_bias.pdf")
# grid.arrange(pp[[1]], pp[[2]])
# dev.off()

pdf("../img/weibull_delay_par_bias_proportion.pdf", width = 6, height = 3)
pp[[1]]
dev.off()

pdf("../img/weibull_delay_par_bias_onset.pdf", width = 6, height = 3)
pp[[2]]
dev.off()



###################################################
## Time for representative curves for each group ##
###################################################

## NOTE: F < G < S fucks up ordering on the legend

## Starting with fixed
pp1 <- sampleCurvePlot(fit_fix_no_delay,
                      fit_sac_no_delay,
                      sim_no_delay,
                      tit = "No Delay")
pdf("../img/rep_curves_no_delay.pdf", width = 7, height = 4)
pp1
dev.off()

pp2 <- sampleCurvePlot(fit_fix_uniform,
                      fit_sac_uniform,
                      sim_uniform,
                      tit = "Uniform Delay")
pdf("../img/rep_curves_uniform_delay.pdf", width = 7, height = 4)
pp2
dev.off()

pp3 <- sampleCurvePlot(fit_fix_weibull,
                      fit_sac_weibull,
                      sim_weibull,
                      tit = "Weibull Delay")
pdf("../img/rep_curves_weibull_delay.pdf", width = 7, height = 4)
pp3
dev.off()
#
# grid.arrange(pp, pp3)
#
####### Let's make tables, yo




## Why does weibull look better?
unf <- function() abs(rnorm(1, 200, sd = 30)) #runif(1, min = 100, max = 300)
makeActiveBinding("unf_rv", unf, .GlobalEnv)

wb <- function() rweibull(1, shape = 1.8, scale = 224.9)
makeActiveBinding("wb_rv", wb, .GlobalEnv)

# dist of looks
rfT <- function() rgamma(1, 334.02**2/184.70**2, scale = 184.7**2/334.02)

x <- replicate(10e5, wb())
z <- replicate(10e5, unf())
y <- replicate(10e5, rfT())

pdf("../img/dist_of_vars_example.pdf", width = 7, height = 4)
par(mfrow = c(1,2))
hist(y-z, main = "dist of fixations - length\nuniform")
hist(y-x, main = "dist of fixation - length\nweibull")
dev.off()
