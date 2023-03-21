

library(bdots)

looks <- fread("~/dissertation/data/bob_trace_data/human_looks_rt_cut.csv")
sac <- fread("~/dissertation/data/bob_trace_data/human_saccades_rt_cut.csv")

getRmvIdx <- function(ff) {
  rr <- coef(ff)
  idx <- which(rr[,4] <= 0 | rr[,2] < rr[,1] | rr[,3] < 0)
  idx
}

sac <- sac[starttime > -156, ]

# fit_l <- bdotsFit(data = looks,
#                   y = "target",
#                   time = "time",
#                   subject = "subject",
#                   group = "Protocol",
#                   curveType = logistic())
#
# fit_l <- bdotsRefit(fit_l, fitCode = 5)
#
# fit_s <- bdotsFit(data = sac,
#                   y = "target",
#                   time = "starttime",
#                   subject = "subject",
#                   group = "Protocol",
#                   curveType = logistic())


fit_l <- bdotsFit(data = looks,
                  y = "target",
                  time = "time",
                  subject = "subject",
                  group = "Protocol",
                  curveType = logistic_sac(startSamp = 15))

#fit_l <- bdotsRefit(fit_l, fitCode = 5)

fit_s <- bdotsFit(data = sac,
                  y = "target",
                  time = "starttime",
                  subject = "subject",
                  group = "Protocol",
                  curveType = logistic_sac(startSamp = 15))


save.image("~/dissertation/analysis/irl_comparison/fit_data.rds")
load("~/dissertation/analysis/irl_comparison/fit_data.rds")


## 6 different formulas we can look at
f1 <- y ~ Protocol(CF, N)
f2 <- y ~ Protocol(CF, NLI)
f3 <- y ~ Protocol(CF, SLI)
f4 <- y ~ Protocol(N, NLI)
f5 <- y ~ Protocol(N, SLI)
f6 <- y ~ Protocol(SLI, NLI)


## Round 1
boot_l1 <- bdotsBoot(f1, fit_l, permutation = TRUE)
boot_s1 <- bdotsBoot(f1, fit_s, permutation = TRUE)

## Round 2
boot_l2 <- bdotsBoot(f2, fit_l, permutation = TRUE)
boot_s2 <- bdotsBoot(f2, fit_s, permutation = TRUE)

plot(boot_l2)
plot(boot_s2)

## Round 3
boot_l3 <- bdotsBoot(f3, fit_l, permutation = TRUE)
boot_s3 <- bdotsBoot(f3, fit_s, permutation = TRUE)

## Round 4
boot_l4 <- bdotsBoot(f4, fit_l, permutation = TRUE)
boot_s4 <- bdotsBoot(f4, fit_s, permutation = TRUE)

plot(boot_l4)
plot(boot_s4)

## Round 5
boot_l5 <- bdotsBoot(f5, fit_l, permutation = TRUE)
boot_s5 <- bdotsBoot(f5, fit_s, permutation = TRUE)

## Round 6
boot_l6 <- bdotsBoot(f6, fit_l, permutation = TRUE)
boot_s6 <- bdotsBoot(f6, fit_s, permutation = TRUE)

save.image("~/dissertation/analysis/irl_comparison/irl_comparison.rds")
