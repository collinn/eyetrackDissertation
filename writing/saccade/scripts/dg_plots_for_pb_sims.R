
# need to check bias of parameters
# need to look at representative curves
# also look at average mise between each group

# Maybe I'll try this again with FBS+T, the saccade clearly wins but by
# less than it did previously

library(bdots)
library(ggplot2)
library(gridExtra)
library(eyetrackSim)

load("~/packages/eyetrackSim/analysis/dg_pb_data.RData")


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
  tp <- as.matrix(ss$subPars$pars[, 2:7])
  fp <- coef(ff)
  bb <- suppressWarnings(melt(as.data.table(fp-tp))) #obs bias
}

getRmvIdx <- function(ff) {
  rr <- coef(ff)
  idx <- which(rr[,3] < 10 | rr[,4] < 10 | rr[,2] < rr[,5] | rr[,2] < rr[,6])
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
  idx <- setdiff(seq_len(nrow(fix)), c(idx1, idx2))[1:6]
  # get pars
  truep <- subsetSim(sim, idx)$subPars$pars[, 2:7] |> as.matrix()
  fixp <- coef(fix[idx, ])
  sacp <- coef(sac[idx, ])

  getCurve <- function(pp, type) {
    pp <- split(pp, seq_len(nrow(pp)))
    rrr <- 0:2000
    ff <- function(p) doubleGauss_f(p, rrr)
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

makeMiseTable <- function(fix, sac, sim, tit, r2 = FALSE) {

  mise <- function(fp, tp) {
    times <- 0:2000
    fp <- split(fp, 1:nrow(fp))
    tp <- split(tp, 1:nrow(tp))

    mv <- Map(function(x, y) {
      g <- function(tt) {
        (doubleGauss_f(x, tt) - doubleGauss_f(y, tt))^2
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
      yy <- doubleGauss_f(y, times)
      yh <- doubleGauss_f(x, times)
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
  truep <- subsetSim(sim, idx)$subPars$pars[, 2:7] |> as.matrix()
  fixp <- coef(fix[idx, ])
  sacp <- coef(sac[idx, ])

  if (r2 == TRUE) {
    rr1 <- get_r2(fixp, truep)
    rr2 <- get_r2(sacp, truep)
  } else {
    rr1 <- mise(fixp, truep)
    rr2 <- mise(sacp, truep)
  }

  ss1 <- setnames(transpose(data.table(as.numeric(summary(rr1)))), names(summary(rr1)))
  ss2 <- setnames(transpose(data.table(as.numeric(summary(rr2)))), names(summary(rr2)))
  ss <- rbindlist(list(ss1, ss2))

  nn <- data.table(Curve = c("Proportion", "Look Onset"),
                   Delay = rep(tit, 2))
  cbind(nn, ss)
}



biasBarPlot <- function(fix, sac, sim, tit, trim = 0) {
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
  dat$variable <- ifelse(dat$variable == "Ht", "Height", dat$variable)
  dat$variable <- ifelse(dat$variable == "Sig1", "Sigma1", dat$variable)
  dat$variable <- ifelse(dat$variable == "Sig2", "Sigma2", dat$variable)
  #dat$nvar <- factor(dat$variable, levels = c("Mini", "Peak", "Slope", "Crossover"))
  #dat$Method <- factor(dat$Method, levels = c("Look Onset", "Proportion"))
  dat$Method <- factor(dat$Method, levels = c("Proportion", "Look Onset"))

  ggplot(dat, aes(x = Bias, y = Method, fill = Method)) +
    geom_boxplot() +
    geom_vline(xintercept = 0, color = 'red', linetype = 'dashed') +
    facet_wrap(~variable, scales = "free") + theme_bw() +
    scale_fill_manual(values = c("#619CFF", "#00BA38"),
                      breaks = c("Look Onset", "Proportion")) +
    ggtitle(tit)
  #scale_fill_manual(values = c("#C77CFF", "#00BFC4"))
}




############### END MAKING FUNCTIONS #################################
# for testing
fix <- dg_fit_fix_no_delay
sac <- dg_fit_sac_no_delay
sim <- dg_sim_no_delay
tit <- ""


####### Let's make tables, yo

ndtab <- makeMiseTable(dg_fit_fix_no_delay,
                       dg_fit_sac_no_delay,
                       dg_sim_no_delay,
                       tit = "No Delay", r2 = FALSE)

nmtab <- makeMiseTable(dg_fit_fix_normal,
                        dg_fit_sac_normal,
                        dg_sim_normal,
                        tit = "Normal Delay", r2 = FALSE)

wbtab <- makeMiseTable(dg_fit_fix_weibull,
                       dg_fit_sac_weibull,
                       dg_sim_weibull,
                       tit = "Weibull Delay", r2 = FALSE)


misetab <- rbindlist(list(ndtab, nmtab, wbtab))[order(Curve), ]
misetab

print(xtable::xtable(misetab), include.rownames=FALSE)
print(xtable::xtable(misetab[, c(1:2, 4:5, 7)]), include.rownames=FALSE)

ndtab <- makeMiseTable(dg_fit_fix_no_delay,
                       dg_fit_sac_no_delay,
                       dg_sim_no_delay,
                       tit = "No Delay", r2 = TRUE)

nmtab <- makeMiseTable(dg_fit_fix_normal,
                       dg_fit_sac_normal,
                       dg_sim_normal,
                       tit = "Normal Delay", r2 = TRUE)

wbtab <- makeMiseTable(dg_fit_fix_weibull,
                       dg_fit_sac_weibull,
                       dg_sim_weibull,
                       tit = "Weibull Delay", r2 = TRUE)


misetab <- rbindlist(list(ndtab, nmtab, wbtab))[order(Curve), ]
misetab

print(xtable::xtable(misetab), include.rownames=FALSE)
print(xtable::xtable(misetab[, c(1:2, 4:5, 7)]), include.rownames=FALSE)




## No delay
pdf("../img/dg_no_delay_bar_plot.pdf", width = 7, height = 4)
biasBarPlot(dg_fit_fix_no_delay,
            dg_fit_sac_no_delay,
            dg_sim_no_delay,
            tit = "No Delay",
            trim = 0.99)
dev.off()

## Normal delay
pdf("../img/dg_normal_delay_bar_plot.pdf", width = 7, height = 4)
biasBarPlot(dg_fit_fix_normal,
            dg_fit_sac_normal,
            dg_sim_normal,
            tit = "Normal Delay",
            trim = 0.99)
dev.off()

pdf("../img/dg_weibull_delay_bar_plot.pdf", width = 7, height = 4)
biasBarPlot(dg_fit_fix_weibull,
            dg_fit_sac_weibull,
            dg_sim_weibull,
            tit = "Weibull Delay",
            trim = 0.99)
dev.off()



###################################################
## Time for representative curves for each group ##
###################################################

## NOTE: F < G < S fucks up ordering on the legend

## Starting with fixed
pp1 <- sampleCurvePlot(dg_fit_fix_no_delay,
                      dg_fit_sac_no_delay,
                      dg_sim_no_delay,
                      tit = "No Delay")
pdf("../img/dg_rep_curves_no_delay.pdf", width = 7, height = 4)
pp1
dev.off()

pp2 <- sampleCurvePlot(dg_fit_fix_normal,
                      dg_fit_sac_normal,
                      dg_sim_normal,
                      tit = "Normal Delay")
pdf("../img/dg_rep_curves_normal_delay.pdf", width = 7, height = 4)
pp2
dev.off()

pp3 <- sampleCurvePlot(dg_fit_fix_weibull,
                      dg_fit_sac_weibull,
                      dg_sim_weibull,
                      tit = "Weibull Delay")
pdf("../img/dg_rep_curves_weibull_delay.pdf", width = 7, height = 4)
pp3
dev.off()


######### Differences yo



###################################################
## Time for representative curves for each group ##
###################################################


diffCurvePlot <- function(fix, sac, sim, tit) {
  idx1 <- getRmvIdx(fix)
  idx2 <- getRmvIdx(sac)
  idx <- setdiff(seq_len(nrow(fix)), c(idx1, idx2))[1:6]
  # get pars
  truep <- subsetSim(sim, idx)$subPars$pars[, 2:7] |> as.matrix()
  fixp <- coef(fix[idx, ])
  sacp <- coef(sac[idx, ])

  getCurve <- function(pp, type) {
    pp <- split(pp, seq_len(nrow(pp)))
    rrr <- 0:2000
    ff <- function(p) doubleGauss_f(p, rrr)
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
pp1 <- sampleCurvePlot(dg_fit_fix_no_delay,
                       dg_fit_sac_no_delay,
                       dg_sim_no_delay,
                       tit = "No Delay")
pp1d <- diffCurvePlot(dg_fit_fix_no_delay,
                      dg_fit_sac_no_delay,
                      dg_sim_no_delay,
                      tit = "No Delay")

pdf("~/dissertation/writing/saccade/img/dg_rep_and_diff_no_delay.pdf", width = 7, height = 7)
ggpubr::ggarrange(pp1, pp1d, nrow = 2, ncol = 1,
                  common.legend = TRUE, legend = "bottom")
dev.off()



pp3 <- sampleCurvePlot(dg_fit_fix_weibull,
                       dg_fit_sac_weibull,
                       dg_sim_weibull,
                       tit = "Weibull Delay")
pp3d <- diffCurvePlot(dg_fit_fix_weibull,
                      dg_fit_sac_weibull,
                      dg_sim_weibull,
                      tit = "Weibull Delay")
pdf("~/dissertation/writing/saccade/img/dg_rep_and_diff_weibull_delay.pdf", width = 7, height = 7)
ggpubr::ggarrange(pp3, pp3d, nrow = 2, ncol = 1,
                  common.legend = TRUE, legend = "bottom")
dev.off()


pp4 <- sampleCurvePlot(dg_fit_fix_normal,
                       dg_fit_sac_normal,
                       dg_sim_normal,
                       tit = "Normal Delay")
pp4d <- diffCurvePlot(dg_fit_fix_normal,
                      dg_fit_sac_normal,
                      dg_sim_normal,
                      tit = "Normal Delay")
pdf("~/dissertation/writing/saccade/img/dg_rep_and_diff_normal_delay.pdf", width = 7, height = 7)
ggpubr::ggarrange(pp4, pp4d, nrow = 2, ncol = 1,
                  common.legend = TRUE, legend = "bottom")
dev.off()
