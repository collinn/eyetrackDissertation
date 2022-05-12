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
         main = mm, xlab = "time", ylab = "probability")
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
         main = mm, xlab = "time", ylab = "probability")
    c_lty <- c("solid", "solid", "solid")
    c_lwd <- c(3, 3, 4)
    c_col <- c("firebrick1", "chartreuse", " darkorchid1")
    for (i in rev(seq_along(curves))) {
      lines(times, curves[[i]], lty = c_lty[i], col = c_col[i], lwd = c_lwd[i])
    }
    legend(1050, 0.5, col = c("gray", c_col), lwd = c(1, c_lwd), lty = c("solid", c_lty),
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

# png("reg_fit.png", width = 600, height = 480)
# plotBfits(ff$reg$fit, mm = "Aggregate vs Saccade, N = 300")
# dev.off()
#
# xtable(t(rr[, 1]), caption = "MISE")
#
# png("early_fit.png", width = 600)
# plotBfits(ff$early$fit, mm = "Early Window (100-400), N = 300")
# dev.off()
# xtable(t(rr[, 2]))
#
# png("mid_fit.png", width = 600)
# plotBfits(ff$mid$fit, mm = "Mid Window (700-1000), N = 300")
# dev.off()
# xtable(t(rr[, 3]))
#
# png("late_fit.png", width = 600)
# plotBfits(ff$late$fit, mm = "Late Window (1400-1700), N = 300")
# dev.off()
# xtable(t(rr[, 4]))
#
# png("asy_fit.png", width = 600)
# plotBfits(ff$nlist$fit, mm = "Aggregate vs Saccade, N = 100,000")
# dev.off()
# xtable(t(rr[, 5]))


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

######################33

## For mouse data
library(bdots)
dat <- fread("~/projects/tumr/Analytic-DeIdentified_GJZ16-091_.csv")

## Define user curve
expCurve <- function(dat, y, time, params = NULL, ...) {
  estExpPars <- function(dat, y, time) {
    tt <- lm(log(dat[[y]]) ~ dat[[time]])
    x0 <- exp(coef(tt)[1])
    k <- coef(tt)[2]
    names(x0) <- names(k) <- NULL
    return(c(x0 = x0, k = k))
  }

  if (is.null(params)) {
    params <- estExpPars(dat, y, time)
  } else {
    # put checks here
  }
  y <- str2lang(y)
  time <- str2lang(time)
  ff <- bquote(.(y) ~ x0 * exp(.(time) * k))
  attr(ff, "parnames") <- names(params)
  return(list(formula = ff, params = params))
}

## Issues with values at 0, so remove
dat <- dat[Volume != 0, ]
fit <- bdotsFit(data = dat,
                subject = "ID",
                time = "Day",
                y = "Volume",
                group = "Treatment",
                curveType = expCurve())

bootAB <- bdotsBoot(formula = Volume ~ Treatment(A, B),
                  bdObj = fit)
bootBC <- bdotsBoot(formula = Volume ~ Treatment(B, C),
                  bdObj = fit)
bootCD <- bdotsBoot(formula = Volume ~ Treatment(C, D),
                  bdObj = fit)
bootDE <- bdotsBoot(formula = Volume ~ Treatment(D, E),
                  bdObj = fit)
bootAE <- bdotsBoot(formula = Volume ~ Treatment(A, E),
                  bdObj = fit)


png("img/boot_AB.png")
tt <- plot(boot, plotDiffs = FALSE)
dev.off()

library(gridExtra)
tt1 <- plot(bootBC, plotDiffs = FALSE)$bootPlot
tt2 <- plot(bootCD, plotDiffs = FALSE)$bootPlot
tt3 <- plot(bootDE, plotDiffs = FALSE)$bootPlot
tt4 <- plot(bootAE, plotDiffs = FALSE)$bootPlot

png("img/four_boot.png")
grid.arrange(tt1, tt2, tt3, tt4, ncol = 2)
dev.off()


## Fuck everything about these plots

load("~/eyetrack/matlab_eye_sim/R_sim/analysis/allSims2.RData")
ff <- res_69_fbst$reg$fit


obs <- ff$obs_ag
times <- obs$times
curves <- ff$curves
sac <- ff$raw_sac

mm <- "Aggregate Fit (N = 300)"
png("img/aggregate_fit_only.png", width = 600)
plot(obs, lty = 2, col = 'gray', ylim = c(0.2, 1),
     main = mm, xlab = "time", ylab = "activation")
lines(times, curves[["true_f"]], lty = "solid", lwd = 4, col = "darkorchid1")
lines(times, curves[["ag_f2"]], lty = "solid", lwd = 3, col = "firebrick1")
lines(times, curves[["ag_f"]], lty = "longdash", lwd = 2, col = "firebrick1")
legend(1050, 0.5, col = c("gray", "darkorchid1", "firebrick1", "firebrick1"),
       lty = c("solid", "solid","longdash", "solid"), lwd = 3,
       legend = c("Obs aggregate", "Underlying Curve", "Aggregated Fit",
                  "Aggregated -- Shifted"))
dev.off()

mm <- "Aggregate vs Saccade (N = 300)"
png("img/aggregate_sac_1.png")
plot(obs, lty = 2, col = 'gray', ylim = c(0, 1),
     main = mm, xlab = "time", ylab = "activation")
lines(times, curves[["true_f"]], lty = "solid", lwd = 4, col = "darkorchid1")
lines(times, curves[["ag_f2"]], lty = "solid", lwd = 3, col = "firebrick1")
lines(times, curves[["ag_f"]], lty = "longdash", lwd = 2, col = "firebrick1")
legend(1050, 0.5, col = c("gray", "darkorchid1", "firebrick1", "firebrick1"),
       lty = c("solid", "solid","longdash", "solid"), lwd = 3,
       legend = c("Obs aggregate", "Underlying Curve", "Aggregated Fit",
                  "Aggregated -- Shifted"))
dev.off()


png("img/aggregate_sac_2.png")
plot(obs, lty = 2, col = 'gray', ylim = c(0, 1),
     main = mm, xlab = "time", ylab = "activation")
lines(times, curves[["true_f"]], lty = "solid", lwd = 4, col = "darkorchid1")
lines(times, curves[["ag_f2"]], lty = "solid", lwd = 3, col = "firebrick1")
lines(times, curves[["ag_f"]], lty = "longdash", lwd = 2, col = "firebrick1")
lines(times, curves[["sac_f2"]], lty = "solid", lwd = 3, col = "chartreuse")
lines(times, curves[["sac_f"]], lty = "longdash", lwd = 2, col = "chartreuse")
legend(1050, 0.5, col = c("gray", "darkorchid1", "firebrick1", "firebrick1", "chartreuse", "chartreuse"),
       lty = c("solid", "solid","longdash", "solid", "longdash", "solid"), lwd = 3,
       legend = c("Obs aggregate", "Underlying Curve", "Aggregated Fit",
                  "Aggregated -- Shifted", "Saccade Fit", "Saccade -- Shifted"))
dev.off()


## Fuck these things still


# Since allSims3 apparently has plotBfits that I don't want
plotBfits <- function(ff, mm = NULL, yy = c(0, 1)) {
  obs <- ff$obs_ag
  times <- obs$times
  curves <- ff$curves
  sac <- ff$raw_sac
  #par(mfrow = c(2, 1))

  if (length(curves) == 5) {
    plot(obs, lty = 2, col = 'gray', ylim = yy,
         main = mm, xlab = "time", ylab = "probability")
    c_lty <- c("longdash", "solid", "longdash", "solid", "solid")
    c_lwd <- c(2, 3, 2, 3, 4)
    c_col <- c("firebrick1", "firebrick1", "chartreuse", "chartreuse", " darkorchid1")
    for (i in rev(seq_along(curves))) {
      lines(times, curves[[i]], lty = c_lty[i], col = c_col[i], lwd = c_lwd[i])
    }
    legend(1050, 0.4, col = c("gray", c_col), lwd = c(1, c_lwd), lty = c("solid", c_lty),
           legend = c("Obs aggregate", "Aggregated fit", "Aggregated fit -- shifted",
                      "Saccade fit", "Saccade fit -- shifted", "Underlying Curve"))
  } else {
    plot(obs, lty = 2, col = 'gray', ylim = yy,
         main = mm, xlab = "time", ylab = "probability")
    c_lty <- c("solid", "solid", "solid")
    c_lwd <- c(3, 3, 4)
    c_col <- c("firebrick1", "chartreuse", " darkorchid1")
    for (i in rev(seq_along(curves))) {
      lines(times, curves[[i]], lty = c_lty[i], col = c_col[i], lwd = c_lwd[i])
    }
    legend(1050, 0.5, col = c("gray", c_col), lwd = c(1, c_lwd), lty = c("solid", c_lty),
           legend = c("Obs aggregate",  "Aggregated fit -- shifted",
                      "Saccade fit -- shifted", "Underlying Curve"))
  }
  #hist(sac$starttime, main = "Saccades", xlab = "time")
}

load("~/packages/eyetrackSim/analysis/allSims3.RData")
ff <- res_6969_fbst

png("img/new_delay1.png", width = 600)
plotBfits(ff$reg$fit, mm = "Aggregate vs Saccade (N = 300)", yy = c(0.15, 0.85))
dev.off()
png("img/new_delay2.png", width = 600)
plotBfits(ff$nlist$fit, mm = "Aggregate vs Saccade (N = 100,000)", yy = c(0.15, 0.85))
dev.off()


## Fuck this too
dat <- data.table(cohort_unrelated)
dat$Curve <- ifelse(dat$LookType == "Cohort", 1, 2)
library(bdots0)

fit0 <- doubleGauss.fit(
  data = dat, # Requires columns "Subject", "Time", and "Group
  col = 4, # Specify outcome with numeric position
  concave = TRUE, # argument tied to curve function
  diffs = TRUE) # Requires column "Curve" with values 1,2

fit <- bdotsFit(data = dat,
                subject = "Subject",
                time = "Time",
                y = "Fixations",
                group = c("Group", "LookType"),
                curveType = doubleGauss(concave = TRUE))
