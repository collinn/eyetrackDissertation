

library(bdots)
library(ggplot2)

looks <- fread("~/dissertation/data/bob_trace_data/human_looks_rt_cut.csv")
sac <- fread("~/dissertation/data/bob_trace_data/human_saccades_rt_cut.csv")

looks$Protocol <- ifelse(looks$Protocol == "CF", "SCI", looks$Protocol)
sac$Protocol <- ifelse(sac$Protocol == "CF", "SCI", sac$Protocol)

looks[, Time := time]
looks[, Target := target]
sac[, Time := starttime]
sac[, Target := target]

getRmvIdx <- function(ff) {
  rr <- coef(ff)
  idx <- which(rr[,4] <= 0 | rr[,2] < rr[,1] | rr[,3] < 0)
  idx
}

sac <- sac[starttime > -156, ]


fit_l <- bdotsFit(data = looks,
                  y = "Target",
                  time = "Time",
                  subject = "subject",
                  group = "Protocol",
                  curveType = logistic_sac(startSamp = 15))


fit_s <- bdotsFit(data = sac,
                  y = "Target",
                  time = "Time",
                  subject = "subject",
                  group = "Protocol",
                  curveType = logistic_sac(startSamp = 15))


save.image("~/dissertation/analysis/irl_comparison/fit_data.rds")
load("~/dissertation/analysis/irl_comparison/fit_data.rds")

load("~/dissertation/analysis/irl_comparison/irl_comparison.rds")
## 6 different formulas we can look at
#f1 <- y ~ Protocol(SCI, N)
f2 <- y ~ Protocol(SCI, NLI)
#f3 <- y ~ Protocol(SCI, SLI)
f4 <- y ~ Protocol(N, NLI)
#f5 <- y ~ Protocol(N, SLI)
#f6 <- y ~ Protocol(SLI, NLI)


## Round 1
# boot_l1 <- bdotsBoot(f1, fit_l, permutation = TRUE)
# boot_s1 <- bdotsBoot(f1, fit_s, permutation = TRUE)

## Round 2
boot_l2 <- bdotsBoot(f2, fit_l, permutation = TRUE)
boot_s2 <- bdotsBoot(f2, fit_s, permutation = TRUE)

p1 <- plot(boot_l2, plotDiffs = FALSE)$bootPlot
p2 <- plot(boot_s2, plotDiffs = FALSE)$bootPlot

ss <- 4

pdf("~/dissertation/writing/saccade/img/irl_data_prop_1.pdf", width = ss, height = ss+1)
p1 <- p1 + ggtitle("Bootstrapped Fits, Proportion of Fixations")
p1
dev.off()
pdf("~/dissertation/writing/saccade/img/irl_data_onset_1.pdf", width = ss, height = ss+1)
p2 <- p2 + ggtitle("Bootstrapped Fits, Look Onset")
p2
dev.off()


## Round 3
# boot_l3 <- bdotsBoot(f3, fit_l, permutation = TRUE)
# boot_s3 <- bdotsBoot(f3, fit_s, permutation = TRUE)

## Round 4
boot_l4 <- bdotsBoot(f4, fit_l, permutation = TRUE)
boot_s4 <- bdotsBoot(f4, fit_s, permutation = TRUE)

p3 <- plot(boot_l4, plotDiffs = FALSE)$bootPlot
p4 <- plot(boot_s4, plotDiffs = FALSE)$bootPlot

pdf("~/dissertation/writing/saccade/img/irl_data_prop_2.pdf", width = ss, height = ss+1)
p3 <- p3 + ggtitle("Bootstrapped Fits, Proportion of Fixations")
p3
dev.off()
pdf("~/dissertation/writing/saccade/img/irl_data_onset_2.pdf", width = ss, height = ss+1)
p4 <- p4 + ggtitle("Bootstrapped Fits, Look Onset")
p4
dev.off()

## Round 5
# boot_l5 <- bdotsBoot(f5, fit_l, permutation = TRUE)
# boot_s5 <- bdotsBoot(f5, fit_s, permutation = TRUE)
#
# ## Round 6
# boot_l6 <- bdotsBoot(f6, fit_l, permutation = TRUE)
# boot_s6 <- bdotsBoot(f6, fit_s, permutation = TRUE)

save.image("~/dissertation/analysis/irl_comparison/irl_comparison.rds")
