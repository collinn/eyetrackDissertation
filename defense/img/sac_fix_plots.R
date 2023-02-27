library(eyetrackSim)
library(data.table)
library(knitr)
library(kableExtra)
library(ggplot2)

# ## I know, stupid place to keep this, needs reorganized
# load("~/eyetrack/matlab_eye_sim/R_sim/analysis/allSims2.RData")
#
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
#
# plotBfits <- function(ff, mm = NULL) {
#   obs <- ff$obs_ag
#   times <- obs$times
#   curves <- ff$curves
#   sac <- ff$raw_sac
#   #par(mfrow = c(2, 1))
#
#   if (length(curves) == 5) {
#     plot(obs, lty = 2, col = 'gray', ylim = c(0, 1),
#          main = mm, xlab = "time", ylab = "probability")
#     c_lty <- c("longdash", "solid", "longdash", "solid", "solid")
#     c_lwd <- c(2, 3, 2, 3, 4)
#     c_col <- c("firebrick1", "firebrick1", "chartreuse", "chartreuse", " darkorchid1")
#     for (i in rev(seq_along(curves))) {
#       lines(times, curves[[i]], lty = c_lty[i], col = c_col[i], lwd = c_lwd[i])
#     }
#     legend(1250, 0.4, col = c("gray", c_col), lwd = c(1, c_lwd), lty = c("solid", c_lty),
#            legend = c("Obs aggregate", "Aggregated fit", "Aggregated fit -- shifted",
#                       "Saccade fit", "Saccade fit -- shifted", "Underlying Curve"))
#   } else {
#     plot(obs, lty = 2, col = 'gray', ylim = c(0, 1),
#          main = mm, xlab = "time", ylab = "probability")
#     c_lty <- c("solid", "solid", "solid")
#     c_lwd <- c(3, 3, 4)
#     c_col <- c("firebrick1", "chartreuse", " darkorchid1")
#     for (i in rev(seq_along(curves))) {
#       lines(times, curves[[i]], lty = c_lty[i], col = c_col[i], lwd = c_lwd[i])
#     }
#     legend(1050, 0.5, col = c("gray", c_col), lwd = c(1, c_lwd), lty = c("solid", c_lty),
#            legend = c("Obs aggregate",  "Aggregated fit -- shifted",
#                       "Saccade fit -- shifted", "Underlying Curve"))
#   }
#   #hist(sac$starttime, main = "Saccades", xlab = "time")
# }
#
#
# miseFit <- function(ff) {
#   mm <- lapply(ff, function(x) {
#     mise(x[['fit']])
#   })
#   mm[[5]] <- c(NA, mm[[5]][1], NA, mm[[5]][2:3])
#   rr <- Reduce(cbind, mm)
#   colnames(rr) <- c("Standard", "Early", "Mid", "Late", "N")
#   rr <- rr[c(1, 3, 2, 4, 5), ]
#   #kable(rr, caption = "MISE") |> kable_styling(full_width = FALSE)
# }
#
#
#
# load("~/eyetrack/matlab_eye_sim/R_sim/analysis/allSims2.RData")
# ff <- res_69_fbst$reg$fit
#
#
# obs <- ff$obs_ag
# times <- obs$times
# curves <- ff$curves
# sac <- ff$raw_sac
#
# plot(obs, lty = 2, col = 'gray', ylim = c(0, 1),
#      main = mm, xlab = "time", ylab = "activation")
# lines(times, curves[["true_f"]], lty = "solid", lwd = 4, col = "darkorchid1")
# lines(times, curves[["ag_f2"]], lty = "solid", lwd = 3, col = "firebrick1")
# lines(times, curves[["ag_f"]], lty = "longdash", lwd = 2, col = "firebrick1")
# lines(times, curves[["sac_f2"]], lty = "solid", lwd = 3, col = "chartreuse")
# lines(times, curves[["sac_f"]], lty = "longdash", lwd = 2, col = "chartreuse")
# legend(1050, 0.5, col = c("gray", "darkorchid1", "firebrick1", "firebrick1", "chartreuse", "chartreuse"),
#        lty = c("solid", "solid","longdash", "solid", "longdash", "solid"), lwd = 3,
#        legend = c("Obs aggregate", "Underlying Curve", "Aggregated Fit",
#                   "Aggregated -- Shifted", "Saccade Fit", "Saccade -- Shifted"))
#
# obs$cond <- "Observed"
#
# und <- data.table(times = times,
#                   looks = curves[["true_f"]],
#                   cond = "Underlying")
#
# agg <- data.table(times = times,
#                   looks = curves[["ag_f2"]],
#                   cond = "Aggregated")
#
# sac <- data.table(times = times,
#                   looks = curves[["sac_f2"]],
#                   cond = "Saccade")
#
# dat <- rbindlist(list(und, agg, sac))
# names(dat) <- c("Time", "Prob", "Curve")
# names(obs) <- c("Time", "Prob", "Curve")


saveRDS(list(dat = dat, obs = obs), "research_data_for_plots.rds")

tt <- readRDS("research_data_for_plots.rds")
dat <- tt$dat
obs <- tt$obs

png("sac_agg.png", width = 1.4*480)
ggplot(dat, aes(Time, Prob, color = Curve)) +
  geom_line(size = 1.5) + theme_bw(base_size=22) + ylab("Activation") +
  geom_point(data = obs, aes(Time, Prob), color = "gray", shape = 1) +
  scale_color_manual(
    values = c("Aggregated" = "tomato", "Saccade" = "steelblue",
               "Underlying" = "gray20", "Observed" = "gray")) +
  theme(legend.position = "bottom")
dev.off()


dd <- split(dat, by = "Curve")
