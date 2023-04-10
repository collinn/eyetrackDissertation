
# need to check bias of parameters
# need to look at representative curves
# also look at average mise between each group

library(bdots)
library(ggplot2)
library(gridExtra)
library(eyetrackSim)
library(ggpubr)

load("~/packages/eyetrackSim/analysis/pb_data.RData")


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

biasPlot <- function(fix, sac, sim, tit, xint = 0, trim = 0) {
  idx1 <- getRmvIdx(fix)
  idx2 <- getRmvIdx(sac)
  idx <- setdiff(seq_len(nrow(fix)), c(idx1, idx2))

  print(length(idx) / nrow(fix))

  fsim <- subsetSim(sim, idx)
  ff <- fix[idx, ]
  ss <- sac[idx, ]

  bb <- getParBias(fsim, ff)
  bb2 <- getParBias(fsim, ss)

  ## If trim is not 0, we keep that quantile
  if (trim) {
    bb <- lapply(split(bb, by = "variable"), function(x) {
      qq <- quantile(x$value, probs = c(1-trim, trim))
      x <- x[value > qq[1] & value < qq[2], ]
    }) |> rbindlist()
    bb2 <- lapply(split(bb2, by = "variable"), function(x) {
      qq <- quantile(x$value, probs = c(1-trim, trim))
      x <- x[value > qq[1] & value < qq[2], ]
    }) |> rbindlist()
  }

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
  idx <- setdiff(seq_len(nrow(fix)), c(idx1, idx2))[11:16]
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
    scale_color_manual(values = c("#619CFF", "#00BA38", "black")) +
    facet_wrap(~id, nrow = 2) + theme_bw() + labs(y = "Activation", x = "Time") +
    ggtitle(paste0("Representative Curves, ", tit)) +
    suppressWarnings(scale_x_discrete(limits = c(0, 750, 1500))) + theme(legend.position = "bottom")

  pp
}

biasBarPlot <- function(fix, sac, sim, tit, trim = 0,  legend = "yes", nn = 10) {
  idx1 <- getRmvIdx(fix)
  idx2 <- getRmvIdx(sac)
  idx <- setdiff(seq_len(nrow(fix)), c(idx1, idx2))

  print(length(idx) / nrow(fix))

  fsim <- subsetSim(sim, idx)
  ff <- fix[idx, ]
  ss <- sac[idx, ]

  bb <- getParBias(fsim, ff)
  bb2 <- getParBias(fsim, ss)

  ## If trim is not 0, we keep that quantile
  if (trim) {
    bb <- lapply(split(bb, by = "variable"), function(x) {
      qq <- quantile(x$value, probs = c(1-trim, trim))
      x <- x[value > qq[1] & value < qq[2], ]
    }) |> rbindlist()
    bb2 <- lapply(split(bb2, by = "variable"), function(x) {
      qq <- quantile(x$value, probs = c(1-trim, trim))
      x <- x[value > qq[1] & value < qq[2], ]
    }) |> rbindlist()
  }


  bb$Method <- "Proportion"
  bb2$Method <- "Look Onset"
  dat <- rbind(bb2, bb)
  dat$Bias <- dat$value
  dat[, variable := stringr::str_to_title(variable)]
  dat$variable <- ifelse(dat$variable == "Cross", "Crossover", dat$variable)
  dat$nvar <- factor(dat$variable, levels = c("Mini", "Peak", "Slope", "Crossover"))
  #dat$Method <- factor(dat$Method, levels = c("Look Onset", "Proportion"))
  dat$Method <- factor(dat$Method, levels = c("Proportion", "Look Onset"))

  pp <- ggplot(dat, aes(x = Bias, y = Method, fill = Method)) +
    geom_boxplot() +
    geom_vline(xintercept = 0, color = 'red', linetype = 'dashed') +
    facet_wrap(~nvar, scales = "free") + theme_bw() +
    scale_fill_manual(values = c("#619CFF", "#00BA38"),
                      breaks = c("Look Onset", "Proportion")) +
    ggtitle(tit)

  if (legend == "none") {
    pp <- pp + theme(legend.position = "none", text = element_text(size = nn))
  }
  return(pp)

}


makeStatTable <- function(fix, sac, sim, tit, r2=FALSE, justres = FALSE) {

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



  if (justres) {
    return(list(fix = mm1, sac = mm2))
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
fix <- fit_fix_no_delay
sac <- fit_sac_no_delay
sim <- sim_no_delay
tit <- ""


## Or with MISE
ndtab <- makeStatTable(fit_fix_no_delay, fit_sac_no_delay,
                       sim_no_delay, tit = "No Delay", justres = FALSE)

wbtab <- makeStatTable(fit_fix_weibull, fit_sac_weibull,
                       sim_weibull, tit = "Weibull Delay", justres= FALSE)

nmtab <- makeStatTable(fit_fix_normal, fit_sac_normal,
                       sim_normal, tit = "Normal Delay", justres= FALSE)


misetab <- rbindlist(list(ndtab, nmtab, wbtab))[order(Curve), ]
misetab
print(xtable::xtable(misetab[, c(1:2, 4:5, 7)], label = "Summary of MISE across simulations"), include.rownames=FALSE)
#print(xtable::xtable(misetab[, c(1:2, 4:5, 7)], label = "Summary of MISE across simulations"), include.rownames=FALSE)


## How about neato histograms for this?
## Or with MISE
ndtab <- makeStatTable(fit_fix_no_delay, fit_sac_no_delay,
                       sim_no_delay, tit = "No Delay", r2 = TRUE)

wbtab <- makeStatTable(fit_fix_weibull, fit_sac_weibull,
                       sim_weibull, tit = "Weibull Delay", r2 = TRUE)


nmtab <- makeStatTable(fit_fix_normal, fit_sac_normal,
                       sim_normal, tit = "Normal Delay", r2 = TRUE)

misetab <- rbindlist(list(ndtab, nmtab, wbtab))[order(Curve), ]
misetab
print(xtable::xtable(misetab[, c(1:2, 4:5, 7)], caption = "Summary of R2 across simulations"), include.rownames=FALSE)

########################################################################


## No delay
pdf("../img/no_delay_bar_plot.pdf", width = 7, height = 4)
biasBarPlot(fit_fix_no_delay,
            fit_sac_no_delay,
            sim_no_delay, tit = "No Delay", trim = 0.99)
dev.off()


## Now with Weibull delay (lost about 10%)
pdf("../img/weibull_delay_bar_plot.pdf", width = 7, height = 4)
biasBarPlot(fit_fix_weibull,
               fit_sac_weibull,
               sim_weibull,
               tit = "Weibull Delay", trim = 0.99)
dev.off()

## Now with normal delay (lost about 10%)
pdf("../img/normal_delay_bar_plot.pdf", width = 7, height = 4)
biasBarPlot(fit_fix_normal,
            fit_sac_normal,
            sim_normal,
            tit = "Normal Delay", trim = 0.99)
dev.off()



###############################################################################
## Bar plots for Defense
ndtab <- makeStatTable(fit_fix_no_delay, fit_sac_no_delay,
                       sim_no_delay, tit = "No Delay", r2 = FALSE, justres = TRUE)

dt <- data.table(Method = rep(c("Look Onset", "Proportion"), each = 993),
                 MISE = c(ndtab$sac, ndtab$fix))

wbtab <- makeStatTable(fit_fix_weibull, fit_sac_weibull,
                       sim_weibull, tit = "Weibull Delay", r2 = FALSE, justres = TRUE)
dt2 <- data.table(Method = rep(c("Look Onset", "Proportion"), each = length(wbtab$sac)),
                  MISE = c(wbtab$sac, wbtab$fix))


dt$Delay <- "No Delay"
dt2$Delay <- "Weibull Delay"

dat <- rbind(dt, dt2)

dat[, Median := median(MISE), by = .(Method, Delay)]

dat <- dat[, .(Method, Delay, Median)]
dat <- unique(dat)

pdf("../../../defense/img/mise_bar.pdf", width = 4.5, height = 3.5)
ggplot(dat, aes(x = Method, y = Median, fill = Method)) +
  geom_bar(stat = "identity") + facet_wrap(~Delay) + theme_bw() +
  theme(legend.position = "none") + ylab("MISE") + #ggtitle("Median MISE by Method") +
  scale_fill_manual(values =  c("#619CFF", "#00BA38")) + xlab("")
dev.off()


## Try that with normal?
ndtab <- makeStatTable(fit_fix_no_delay, fit_sac_no_delay,
                       sim_no_delay, tit = "No Delay", r2 = FALSE, justres = TRUE)

dt <- data.table(Method = rep(c("Look Onset", "Proportion"), each = 993),
                 MISE = c(ndtab$sac, ndtab$fix))

nmtab <- makeStatTable(fit_fix_normal, fit_sac_normal,
                       sim_normal, tit = "Weibull Delay", r2 = FALSE, justres = TRUE)
dt2 <- data.table(Method = rep(c("Look Onset", "Proportion"), each = length(nmtab$sac)),
                  MISE = c(nmtab$sac, nmtab$fix))


dt$Delay <- "No Delay"
dt2$Delay <- "Normal Delay"

dat <- rbind(dt, dt2)

dat[, Median := median(MISE), by = .(Method, Delay)]

dat <- dat[, .(Method, Delay, Median)]
dat <- unique(dat)

pdf("../../../defense/img/normal_mise_bar.pdf", width = 4.5, height = 3.5)
ggplot(dat, aes(x = Method, y = Median, fill = Method)) +
  geom_bar(stat = "identity") + facet_wrap(~Delay) + theme_bw() +
  theme(legend.position = "none") + ylab("MISE") + #ggtitle("Median MISE by Method") +
  scale_fill_manual(values =  c("#619CFF", "#00BA38")) + xlab("")
dev.off()
################################################################################

## Also for defense
## No delay
pdf("~/dissertation/defense/img/no_delay_bar_plot.pdf", width = 4.5, height = 3.5)
biasBarPlot(fit_fix_no_delay,
            fit_sac_no_delay,
            sim_no_delay, tit = "No Delay", trim = 0.99, legend = "none", nn = 6)
dev.off()

### Final really gross bit that should never be replicated but its a one off plot so whatever
pdf("~/dissertation/defense/img/compare_bar_plot.pdf", width = 4.5, height = 3.5)
biasBarPlot2(fit_sac_no_delay, fit_sac_weibull,
             sim_no_delay, sim_weibull, tit = "Look Onset, No Delay vs. Weibull Delay",
             trim = 0.99, legend = "none", nn = 6)
dev.off()

## With diff color (since not doing onset vs prop)
# https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
pdf("~/dissertation/defense/img/compare_bar_plot2.pdf", width = 4.5, height = 3.5)
biasBarPlot2(fit_sac_no_delay, fit_sac_weibull,
             sim_no_delay, sim_weibull, tit = "Look Onset, No Delay vs. Weibull Delay",
             trim = 0.99, legend = "none", nn = 6)
dev.off()

biasBarPlot2 <- function(fix, sac, sim1, sim2, tit, trim = 0,  legend = NULL, nn = 10) {
  idx1 <- getRmvIdx(fix)
  idx2 <- getRmvIdx(sac)
  idx <- setdiff(seq_len(nrow(fix)), c(idx1, idx2))

  print(length(idx) / nrow(fix))

  fsim1 <- subsetSim(sim1, idx)
  fsim2 <- subsetSim(sim2, idx)
  ff <- fix[idx, ]
  ss <- sac[idx, ]

  bb <- getParBias(fsim1, ff)
  bb2 <- getParBias(fsim2, ss)

  ## If trim is not 0, we keep that quantile
  if (trim) {
    bb <- lapply(split(bb, by = "variable"), function(x) {
      qq <- quantile(x$value, probs = c(1-trim, trim))
      x <- x[value > qq[1] & value < qq[2], ]
    }) |> rbindlist()
    bb2 <- lapply(split(bb2, by = "variable"), function(x) {
      qq <- quantile(x$value, probs = c(1-trim, trim))
      x <- x[value > qq[1] & value < qq[2], ]
    }) |> rbindlist()
  }


  bb$Method <- "No Delay"
  bb2$Method <- "Weibull Delay"
  dat <- rbind(bb2, bb)
  dat$Bias <- dat$value
  dat[, variable := stringr::str_to_title(variable)]
  dat$variable <- ifelse(dat$variable == "Cross", "Crossover", dat$variable)
  dat$nvar <- factor(dat$variable, levels = c("Mini", "Peak", "Slope", "Crossover"))
  #dat$Method <- factor(dat$Method, levels = c("No Delay", "Weibull Delay"))
  dat$Method <- factor(dat$Method, levels = c("Weibull Delay", "No Delay"))

  pp <- ggplot(dat, aes(x = Bias, y = Method, fill = Method)) +
    geom_boxplot() +
    geom_vline(xintercept = 0, color = 'red', linetype = 'dashed') +
    facet_wrap(~nvar, scales = "free") + theme_bw() +
    scale_fill_manual(values = c("#619CFF", "#F8766D"),
                      breaks = c("No Delay", "Weibull Delay")) +
    ggtitle(tit)

  if (legend == "none") {
    pp <- pp + theme(legend.position = "none", text = element_text(size = nn))
  }
  return(pp)

}



######### Difference plots
diffCurvePlot <- function(fix, sac, sim, tit) {
  idx1 <- getRmvIdx(fix)
  idx2 <- getRmvIdx(sac)
  idx <- setdiff(seq_len(nrow(fix)), c(idx1, idx2))[11:16]
  # get pars
  truep <- subsetSim(sim, idx)$subPars$pars[, 2:5] |> as.matrix()
  fixp <- coef(fix[idx, ])
  sacp <- coef(sac[idx, ])

  getCurve <- function(pp, type) {
    pp <- split(pp, seq_len(nrow(pp)))
    rrr <- 0:1600
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

  fixCurve$curve <- fixCurve$curve - trueCurve$curve
  sacCurve$curve <- sacCurve$curve - trueCurve$curve

  plotty <- rbindlist(list(fixCurve, sacCurve))


  pp <- ggplot(plotty, aes(time, curve, color = Curve, linetype = Curve)) +
    geom_line(linewidth = 1) +
    scale_linetype_manual(values = c("solid", "solid")) +
    scale_color_manual(values = c("#619CFF", "#00BA38")) +
    facet_wrap(~id, nrow = 2) + theme_bw() + labs(y = "Activation", x = "Time") +
    ggtitle(paste0("Representative Error Curves, ", tit)) +
    suppressWarnings(scale_x_discrete(limits = c(0, 750, 1500))) + theme(legend.position = "bottom")

  pp
}

## Starting with fixed
pp1 <- diffCurvePlot(fit_fix_no_delay,
                     fit_sac_no_delay,
                     sim_no_delay,
                     tit = "No Delay")
pdf("error_no_delay.pdf", width = 4.5, height = 3.5)
pp1
dev.off()

pp3 <- diffCurvePlot(fit_fix_weibull,
                     fit_sac_weibull,
                     sim_weibull,
                     tit = "Weibull Delay")
pdf("error_weibull_delay.pdf", width = 4.5, height = 3.5)
pp3
dev.off()



###################################################
## Time for representative curves for each group ##
###################################################

## Starting with fixed
pp1 <- sampleCurvePlot(fit_fix_no_delay,
                       fit_sac_no_delay,
                       sim_no_delay,
                       tit = "No Delay")
pp1d <- diffCurvePlot(fit_fix_no_delay,
                     fit_sac_no_delay,
                     sim_no_delay,
                     tit = "No Delay")

pdf("~/dissertation/writing/saccade/img/rep_and_diff_no_delay.pdf", width = 7, height = 7)
ggpubr::ggarrange(pp1, pp1d, nrow = 2, ncol = 1,
                  common.legend = TRUE, legend = "bottom")
dev.off()



pp3 <- sampleCurvePlot(fit_fix_weibull,
                       fit_sac_weibull,
                       sim_weibull,
                       tit = "Weibull Delay")
pp3d <- diffCurvePlot(fit_fix_weibull,
                       fit_sac_weibull,
                       sim_weibull,
                       tit = "Weibull Delay")
pdf("~/dissertation/writing/saccade/img/rep_and_diff_weibull_delay.pdf", width = 7, height = 7)
ggpubr::ggarrange(pp3, pp3d, nrow = 2, ncol = 1,
                  common.legend = TRUE, legend = "bottom")
dev.off()


pp4 <- sampleCurvePlot(fit_fix_normal,
                       fit_sac_normal,
                       sim_normal,
                       tit = "Normal Delay")
pp4d <- diffCurvePlot(fit_fix_normal,
                       fit_sac_normal,
                       sim_normal,
                       tit = "Normal Delay")
pdf("~/dissertation/writing/saccade/img/rep_and_diff_normal_delay.pdf", width = 7, height = 7)
ggpubr::ggarrange(pp4, pp4d, nrow = 2, ncol = 1,
                  common.legend = TRUE, legend = "bottom")
dev.off()
