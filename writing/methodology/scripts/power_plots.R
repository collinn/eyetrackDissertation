
## Plots for power

library(ggplot2)

time <- seq(-1, 1, length.out = 501)

dt1 <- data.table(Condition = "Effect",
                  Time = time)
dt1[, y := pline(c(0, 0.005), time, "A")]

dt2 <- data.table(Condition = "No Effect",
                  Time = time)
dt2[, y := pline(c(0, 0), time, "B")]
dt <- rbindlist(list(dt1, dt2))

pdf("~/dissertation/writing/methodology/img/power_plot.pdf",
    width = 6, height = 3)
ggplot(dt, aes(Time, y, color = Condition)) +
  geom_line(linewidth = 1, alpha = 0.75) + theme_bw()
dev.off()

## Ok but let's see how those bitches look with distribution
## THAT IS, we are looking at distribution of TRUTH without error (so ar assumption not releavnt)
library(eyetrackSim)

makeDatPlotReadyCI <- function(dat) {
  # get rid of this
  dat[, groupMean := mean(true), by = .(time, group)]
  dat[, `:=`(groupLowCI = quantile(true, probs = 0.025),
             groupHighCI = quantile(true,  probs = 0.975)), by = .(time, group)]

  ## Remove non unique
  dat[, `:=`(id = NULL, true = NULL, fixations = NULL)]
  dat <- unique(dat)
  dat$Condition <- ifelse(dat$group == "A","Effect", "No Effect")
  dat
}

## Paired (original), sig = 0.025, no AR
dat1 <- createPlineData2(n = 1000, TIME = seq(-1, 1, length.out = 501))
dat1 <- makeDatPlotReadyCI(dat1)

pdf("~/dissertation/writing/methodology/img/paired_many_means_high_sig_dist_plot.pdf",
    width = 6, height = 5)
ggplot(dat1, aes(time, groupMean, color = Condition)) +
  geom_point(alpha = 0.5) +
  geom_ribbon(aes(ymin = groupLowCI, ymax = groupHighCI), alpha = 0.1) +
  theme_bw() + theme(legend.position = "bottom")
dev.off()


## Paired (original), sig = 0.025, no AR
dat2 <- createPlineData2(n = 1000, TIME = seq(-1, 1, length.out = 501), manymeans = FALSE)
dat2 <- makeDatPlotReadyCI(dat2)

pdf("~/dissertation/writing/methodology/img/paired_single_means_high_sig_dist_plot.pdf",
    width = 6, height = 5)
ggplot(dat2, aes(time, groupMean, color = Condition)) +
  geom_point(alpha = 0.5) +
  geom_ribbon(aes(ymin = groupLowCI, ymax = groupHighCI), alpha = 0.1) +
  theme_bw() + theme(legend.position = "bottom")
dev.off()


## Now with smaller sig
dat3 <- createPlineData2(n = 1000, TIME = seq(-1, 1, length.out = 501), distSig = 0.005)
dat3 <- makeDatPlotReadyCI(dat3)

pdf("~/dissertation/writing/methodology/img/paired_many_means_low_sig_dist_plot.pdf",
    width = 6, height = 5)
ggplot(dat3, aes(time, groupMean, color = Condition)) +
  geom_point(alpha = 0.5) +
  geom_ribbon(aes(ymin = groupLowCI, ymax = groupHighCI), alpha = 0.1) +
  theme_bw() + theme(legend.position = "bottom")
dev.off()


## Paired (original), sig = 0.025, no AR
dat4 <- createPlineData2(n = 1000, TIME = seq(-1, 1, length.out = 501), manymeans = FALSE,
                         distSig = 0.005)
dat4 <- makeDatPlotReadyCI(dat4)

pdf("~/dissertation/writing/methodology/img/paired_single_means_low_sig_dist_plot.pdf",
    width = 6, height = 5)
ggplot(dat4, aes(time, groupMean, color = Condition)) +
  geom_point(alpha = 0.5) +
  geom_ribbon(aes(ymin = groupLowCI, ymax = groupHighCI), alpha = 0.1) +
  theme_bw() + theme(legend.position = "bottom")
dev.off()



## Now with smaller sig
dat5 <- createPlineData3(n = 1000, TIME = seq(-1, 1, length.out = 501), distSig = 0.025)
dat5 <- makeDatPlotReadyCI(dat5)

pdf("~/dissertation/writing/methodology/img/unpaired_many_means_high_sig_dist_plot.pdf",
    width = 6, height = 5)
ggplot(dat5, aes(time, groupMean, color = Condition)) +
  geom_point(alpha = 0.5) +
  geom_ribbon(aes(ymin = groupLowCI, ymax = groupHighCI), alpha = 0.1) +
  theme_bw() + theme(legend.position = "bottom")
dev.off()


## Paired (original), sig = 0.025, no AR
dat6 <- createPlineData3(n = 1000, TIME = seq(-1, 1, length.out = 501), manymeans = FALSE,
                         distSig = 0.025)
dat6 <- makeDatPlotReadyCI(dat6)

pdf("~/dissertation/writing/methodology/img/unpaired_single_means_high_sig_dist_plot.pdf",
    width = 6, height = 5)
ggplot(dat6, aes(time, groupMean, color = Condition)) +
  geom_point(alpha = 0.5) +
  geom_ribbon(aes(ymin = groupLowCI, ymax = groupHighCI), alpha = 0.1) +
  theme_bw() + theme(legend.position = "bottom")
dev.off()
