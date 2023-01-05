
## Taking what I know from bob, lets try to do this whole analysis with trace right here
# though there are likely a few ways this can go, we will choose the one that looks best
library(data.table)
library(ggplot2)
library(gridExtra)
library(bdots)
library(eyetrackSim)

setwd("~/dissertation/analysis/")
## Relevant data
looksrt <- fread("../data/bob_trace_data/human_looks_rt_cut.csv")
sacsrt <- fread("../data/bob_trace_data/human_saccades_rt_cut.csv")

looks <- fread("../data/bob_trace_data/human_looks_rt_nocut.csv")
sacs <- fread("../data/bob_trace_data/human_saccades_rt_nocut.csv")

## Should consider making group niot necessary in bdots
looksrt[, group := "A"]
sacsrt[, group := "A"]
looks[, group := "A"]
sacs[, group := "A"]


if (file.exists("~/dissertation/data/saccade_look_fits/target_fits_bdots.RData")) {
  load("~/dissertation/data/saccade_look_fits/target_fits_bdots.RData")
} else {
  fit_looks <- bdotsFit(data = looks, 
                        subject = "subject", 
                        time = "time", 
                        y = "target", 
                        group = "group", 
                        curveType = logistic())
  
  fit_looks_rt <- bdotsFit(data = looksrt, 
                         subject = "subject", 
                         time = "time", 
                         y = "target", 
                         group = "group", 
                         curveType = logistic())
  
  #sacs <- sacs[starttime <= 2000, ]
  
  fit_sacs  <- bdotsFit(data = sacs, 
                        subject = "subject", 
                        time = "starttime", 
                        y = "target", 
                        group = "group", 
                        curveType = logistic())  
  fit_sacs_rt  <- bdotsFit(data = sacsrt, 
                         subject = "subject", 
                         time = "starttime", 
                         y = "target", 
                         group = "group", 
                         curveType = logistic())  
  
  save(fit_looks, fit_looks_rt, fit_sacs, fit_sacs_rt, 
       file = "~/dissertation/data/saccade_look_fits/target_fits_bdots.RData")
}

## Actually I should remove all of the same subjects

# Remove these from looks
idxrm0 <- which(fit_looks$fitCode >= 5 | fit_looks$fitCode >= 5)
idxrm1 <- which(fit_looks_rt$fitCode >= 5 | fit_looks_rt$fitCode >= 5)

# Remove these from saccades 
qq <- coef(fit_sacs)
idxrm2 <- which(qq[,3] < 0 | qq[,4] < 0 | qq[,1] > qq[,2])

## Remove these from  saccades rt
qq <- coef(fit_sacs_rt)
idxrm3 <- which(qq[,3] < 0 | qq[,4] < 0 | qq[,1] > qq[,2])

idxrm <- Reduce(union, list(idxrm0, idxrm1, idxrm2, idxrm3))
idx <- setdiff(1:40, idxrm)

fit_looks <- fit_looks[idx, ]
fit_looks_rt <- fit_looks_rt[idx, ]

fit_sacs <- fit_sacs[idx, ]
fit_sacs_rt <- fit_sacs_rt[idx, ]

mm1 <- colMeans(coef(fit_looks))
mm2 <- colMeans(coef(fit_looks_rt))
mm3 <- colMeans(coef(fit_sacs))
mm4 <- colMeans(coef(fit_sacs_rt))


## 1780 is how far trace goes out
f1 <- logistic_f(mm1, 0:1780)
f2 <- logistic_f(mm2, 0:1780)
f3 <- logistic_f(mm3, 0:1780)
f4 <- logistic_f(mm4, 0:1780)
time <- 0:1780
plot(time, f1, type = 'l', col = 'red', ylim = c(0, 1), ylab = "proportions")
lines(time, f2, type = 'l', col = 'blue')
lines(time, f3, type = 'l', col = 'green', lwd=3)
lines(time, f4, type = 'l', col = 'purple')
legend(1000, .250, legend = c("look w rt", "look wo rt", "sac w rt", "sac wo rt"), 
       col = c("red", "blue", "green", "purple"), lwd = c(1,1,1,1))


## Pretty sure I figured out trace
trace_luce <- fread("~/dissertation/data/bob_trace_data/trace_scaled_4.5.csv")
trace_luce <- fread("~/dissertation/data/bob_trace_data/trace_scaled_3.csv")


plot(trace_luce$time, trace_luce$targ2,type = 'l', col = 'red')
lines(trace_luce$time, trace_luce$targ_bp, col = 'brown', lty = 2)
legend(x = 1200, y = 0.2, legend = c("using p, b", "using 0, 1"), col = c("blue", "red"), lty = 1)


## Comparing fits with tracec
time <- 0:1780
plot(time, f1, type = 'l', col = 'red', ylim = c(0, 1), ylab = "proportions")
lines(time, f2, type = 'l', col = 'blue')
lines(time, f3, type = 'l', col = 'green')
lines(time, f4, type = 'l', col = 'purple')
legend(1000, .250, legend = c("look w rt cut", "look wo rt", "sac w rt cut", "sac wo rt"), 
       col = c("red", "blue", "green", "purple"), lwd = 1)

lines(trace_luce$time, trace_luce$targ1, col = 'orange', lwd = 2)


##################################3
## Ok, time to repeat with cohort data (gulp)

# # 
# # fit_looks_g <- bdotsFit(data = looks, 
# #                       subject = "subject", 
# #                       time = "time", 
# #                       y = "cohort", 
# #                       group = "group", 
# #                       curveType = doubleGauss())
# 
# fit_looks2_g <- bdotsFit(data = looks2, 
#                        subject = "subject", 
#                        time = "time", 
#                        y = "cohort", 
#                        group = "group", 
#                        curveType = doubleGauss())
# 
# 
# # fit_sacs_g  <- bdotsFit(data = sacs, 
# #                       subject = "subject", 
# #                       time = "starttime", 
# #                       y = "cohort", 
# #                       group = "group", 
# #                       curveType = doubleGauss2())  
# fit_sacs2_g  <- bdotsFit(data = sacs2, 
#                        subject = "subject", 
#                        time = "starttime", 
#                        y = "cohort", 
#                        group = "group", 
#                        curveType = doubleGauss())  
# 
# 
# colMeans(coef(fit_looks2_g[c(1:37, 39:40)]))
# 
# 
# 
