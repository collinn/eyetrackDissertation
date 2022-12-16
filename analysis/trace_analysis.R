
## Taking what I know from bob, lets try to do this whole analysis with trace right here
# though there are likely a few ways this can go, we will choose the one that looks best
library(data.table)
library(ggplot2)
library(gridExtra)
library(bdots)
library(eyetrackSim)

setwd("~/dissertation/analysis/")
## Relevant data
looks <- fread("../data/bob_trace_data/human_looks.csv")
sacs <- fread("../data/bob_trace_data/human_saccades.csv")
looks2 <- fread("../data/bob_trace_data/human_looks2.csv")
sacs2 <- fread("../data/bob_trace_data/human_saccades2.csv")

## Should consider making group not necessary in bdots
looks[, group := "A"]
sacs[, group := "A"]
looks2[, group := "A"]
sacs2[, group := "A"]




fit_looks <- bdotsFit(data = looks, 
                      subject = "subject", 
                      time = "time", 
                      y = "target", 
                      group = "group", 
                      curveType = logistic())

fit_looks2 <- bdotsFit(data = looks2, 
                      subject = "subject", 
                      time = "time", 
                      y = "target", 
                      group = "group", 
                      curveType = logistic())

#sacs <- sacs[starttime <= 2000, ]


# ll <- as.numeric(sort(unique(sacs$subject)))
# i <- 1
# 
# # i == 8, 15, 17, 34, 38
# for (i in seq_along(ll)) {
#   i <- 1
#   fit_sacs  <- bdotsFit(data = sacs[subject == 120, ], 
#                         subject = "subject", 
#                         time = "starttime", 
#                         y = "target", 
#                         group = "group", 
#                         curveType = logistic())  
#   
#   i <- i+1
#   print(i)
# }
# 
# ll[c(8, 15, 17, 34, 38)]
# # 120, 37, 43, 87, 97

fit_sacs  <- bdotsFit(data = sacs, 
                      subject = "subject", 
                      time = "starttime", 
                      y = "target", 
                      group = "group", 
                      curveType = logistic())  
fit_sacs2  <- bdotsFit(data = sacs2, 
                      subject = "subject", 
                      time = "starttime", 
                      y = "target", 
                      group = "group", 
                      curveType = logistic())  

## This makes sense why this happens now
(mm <- colMeans(coef(fit_looks[fitCode >= 5, ])))
(mm2 <- colMeans(coef(fit_looks2)))

## Remove wrong ones (R2 and fitcode useless here)
qq <- coef(fit_sacs)
qq <- qq[qq[, 3] > 0, ]
qq <- qq[qq[, 4] > 0, ]
qq <- qq[qq[, 1] < qq[, 2], ]
mm3 <- colMeans(qq)
#(mm3 <- colMeans(coef(fit_sacs)))

qq <- coef(fit_sacs2)
qq <- qq[qq[, 4] > 0, ]
qq <- qq[qq[, 3] > 0, ]
qq <- qq[qq[, 1] < qq[, 2], ]
#(mm4 <- colMeans(coef(fit_sacs2)[coef(fit_sacs2)[, 4] > 0, ]))
mm4 <- colMeans(qq)

## 1780 is how far trace goes out
f1 <- logistic_f(mm, 0:1780)
f2 <- logistic_f(mm2, 0:1780)
f3 <- logistic_f(mm3, 0:1780)
f4 <- logistic_f(mm4, 0:1780)
time <- 0:1780
plot(time, f1, type = 'l', col = 'red', ylim = c(0, 1), ylab = "proportions")
lines(time, f2, type = 'l', col = 'blue')
lines(time, f3, type = 'l', col = 'green')
lines(time, f4, type = 'l', col = 'purple')
legend(1000, .250, legend = c("look w rt cut", "look wo rt", "sac w rt cut", "sac wo rt"), 
       col = c("red", "blue", "green", "purple"), lwd = 1)



### TRACE stuff
trace <- fread("../data/bob_trace_data/trace_curves.csv")
# (also looks like bob did this on activation not luce choice rule transform)
scaler <- function(a, b = 0, p = 1) { #a is max activation
  s <- 4
  w <- 0.0001
  cc <- 0.25
  den <- (1 + w * exp(-s * (a - cc)))^(1/w)
  (ss <- (p-b)/den + b)
}

lucer <- function(l, tau = 3.5) {
  expl <- lapply(l, function(x) {
    exp(tau * x)
  })
  ss <- Reduce(`+`, expl)
  rr <- lapply(expl, function(x) as.data.table(x / ss))
  rr
}
l <- as.list(trace[, -"time", with = FALSE])
trace_luce <- Reduce(`cbind`, (lucer(l)))
names(trace_luce) <- names(l)
trace_luce <- cbind(trace[, .(time)], trace_luce)

## Prep trace 
trace_luce[, maxact := max(target, cohort, rhyme, ur), by = time]
trace_luce[, ss := scaler(maxact, 0, 1)] # 0, 1 looks better and makes more sense
trace_luce[, sst := scaler(maxact, min(target), max(target))]
trace_luce[, targets := target * ss]
trace_luce[, targetss := target * sst]
# lines(trace_luce$time, trace_luce$ss, col = "black", lty = 2)

#trace <- trace_luce
# plot(trace_luce$time, trace_luce$ss, type = 'l', ylim = c(0, 1))
# lines(trace_luce$time, trace_luce$targets, col = 'red', lty = 2)
# lines(trace$time, trace$target*trace_luce$ss, col = 'seagreen', lty = 2)

trace[, maxact_trace := max(target, cohort, rhyme, ur), by = time]
trace[, ss_trace := scaler(maxact_trace, 0, 1)]

plot(time, f1, type = 'l', col = 'red', ylim = c(0, 1), ylab = "proportions")
lines(time, f2, type = 'l', col = 'blue')
lines(time, f3, type = 'l', col = 'green')
lines(time, f4, type = 'l', col = 'purple')
legend(1000, .250, legend = c("look w rt cut", "look wo rt", "sac w rt cut", "sac wo rt"), 
       col = c("red", "blue", "green", "purple"), lwd = 1)
lines(trace$time,  trace$ss_trace, lwd = 2, lty = 2)
lines(trace$time,  trace_luce$targets, lwd = 2, lty = 2, col = 'red')
lines(trace$time,  trace_luce$targetss, lwd = 2, lty = 2, col = 'blue')
