
## Aggregate fbst and shift only
library(eyetrackSim)
library(data.table)
library(bdots)
set.seed(69)
sim <- runSub(ntrials = 300, fbst = TRUE)
true_coef <- sim$subInfo$pars
bfit <- makeJointFits(sim)


ff <- bfit

ff <- res_69_fbst$reg$fit



obs <- ff$obs_ag
times <- obs$times
curves <- ff$curves
sac <- ff$raw_sac

mm <- "Aggregate Fit (N = 300)"
png("img/aggregate_fit_only.png")
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

library(xtable)
df <- as.data.frame(mise(bfit))
xtable(t(df))

load("~/eyetrack/matlab_eye_sim/R_sim/analysis/allSims2.RData")
ff <- res_69_fbst

png("img/asy_fit.png", width = 600)
plotBfits(ff$nlist$fit, mm = "Aggregate vs Saccade (N = 100,000)")
dev.off()


load("~/packages/eyetrackSim/analysis/allSims3.RData")
ff <- res_6969_fbst

plotBfits(ff$reg$fit, mm = "Aggregate vs Saccade (N = 300)")

plotBfits(ff$nlist$fit, mm = "Aggregate vs Saccade (N = 100,000)")
