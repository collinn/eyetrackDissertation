library(eyetrackSim)
library(data.table)
library(knitr)
library(kableExtra)

## I know, stupid place to keep this, needs reorganized
load("~/eyetrack/matlab_eye_sim/R_sim/analysis/allSims2.RData")

mise <- function(ff) {
  curves <- ff$curves
  times <- ff$times

  cc <- lapply(ff$bfits, coef)
  cc[["true_f"]] <- ff$true_coef

  misev <- vapply(cc, function(x) {
    ## tt = time
    g <- function(tt) {
      (eyetrackSim:::logistic_f(x, tt) - eyetrackSim:::logistic_f(ff$true_coef, tt))^2
    }
    integrate(g, lower = min(times), upper = max(times))$value
  }, 1)

  if (length(misev) == 3) {
    names(misev) <- c("Aggregate -- Shifted", "Saccade -- Shifted", "Underlying")
  } else {
    names(misev) <- c("Aggregate", "Aggregate -- Shifted","Saccade", "Saccade -- Shifted", "Underlying")
  }
  misev
}

plotBfits <- function(ff, mm = NULL) {
  obs <- ff$obs_ag
  times <- obs$times
  curves <- ff$curves
  sac <- ff$raw_sac
  #par(mfrow = c(2, 1))

  if (length(curves) == 5) {
    plot(obs, lty = 2, col = 'gray', ylim = c(0, 1),
         main = mm, xlab = "time", ylab = "activation")
    c_lty <- c("longdash", "solid", "longdash", "solid", "solid")
    c_lwd <- c(2, 3, 2, 3, 4)
    c_col <- c("firebrick1", "firebrick1", "chartreuse", "chartreuse", " darkorchid1")
    for (i in rev(seq_along(curves))) {
      lines(times, curves[[i]], lty = c_lty[i], col = c_col[i], lwd = c_lwd[i])
    }
    legend(1250, 0.4, col = c("gray", c_col), lwd = c(1, c_lwd), lty = c("solid", c_lty),
           legend = c("Obs aggregate", "Aggregated fit", "Aggregated fit -- shifted",
                      "Saccade fit", "Saccade fit -- shifted", "Underlying Curve"))
  } else {
    plot(obs, lty = 2, col = 'gray', ylim = c(0, 1),
         main = mm)
    c_lty <- c("solid", "solid", "solid")
    c_lwd <- c(3, 3, 4)
    c_col <- c("firebrick1", "chartreuse", " darkorchid1")
    for (i in rev(seq_along(curves))) {
      lines(times, curves[[i]], lty = c_lty[i], col = c_col[i], lwd = c_lwd[i])
    }
    legend(1250, 0.5, col = c("gray", c_col), lwd = c(1, c_lwd), lty = c("solid", c_lty),
           legend = c("Obs aggregate",  "Aggregated fit -- shifted",
                      "Saccade fit -- shifted", "Underlying Curve"))
  }
  #hist(sac$starttime, main = "Saccades", xlab = "time")
}


miseFit <- function(ff) {
  mm <- lapply(ff, function(x) {
    mise(x[['fit']])
  })
  mm[[5]] <- c(NA, mm[[5]][1], NA, mm[[5]][2:3])
  rr <- Reduce(cbind, mm)
  colnames(rr) <- c("Standard", "Early", "Mid", "Late", "N")
  rr <- rr[c(1, 3, 2, 4, 5), ]
  #kable(rr, caption = "MISE") |> kable_styling(full_width = FALSE)
}


### Plots
## Second simulation, fbst
ff <- res_69_fbst

old <- par()$mar
old2 <- par(mar = c(3.1, 2.1, 2.1, 2.1))

## Table
library(xtable)
rr <- miseFit(ff)

setwd("~/dissertation/eyetrackDissertation/prospectus/img/")

png("reg_fit.png", width = 600, height = 480)
plotBfits(ff$reg$fit, mm = "Aggregate vs Saccade, N = 300")
dev.off()

xtable(t(rr[, 1]), caption = "MISE")

png("early_fit.png", width = 600)
plotBfits(ff$early$fit, mm = "Early Window (100-400), N = 300")
dev.off()
xtable(t(rr[, 2]))

png("mid_fit.png", width = 600)
plotBfits(ff$mid$fit, mm = "Mid Window (700-1000), N = 300")
dev.off()
xtable(t(rr[, 3]))

png("late_fit.png", width = 600)
plotBfits(ff$late$fit, mm = "Late Window (1400-1700), N = 300")
dev.off()
xtable(t(rr[, 4]))

png("asy_fit.png", width = 600)
plotBfits(ff$nlist$fit, mm = "Aggregate vs Saccade, N = 100,000")
dev.off()
xtable(t(rr[, 5]))


## Plot all!
xtable(rr)
## nope, don't really fit
# png("plot_all.png")
# par(mfrow = c(2,2))
# plotBfits(ff$reg$fit, mm = "Aggregate vs Saccade, N = 300")
# plotBfits(ff$early$fit, mm = "Early Window (100-400), N = 300")
# plotBfits(ff$mid$fit, mm = "Mid Window (700-1000), N = 300")
# plotBfits(ff$late$fit, mm = "Late Window (1400-1700), N = 300")
# dev.off()
