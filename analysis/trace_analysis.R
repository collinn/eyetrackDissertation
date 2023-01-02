
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


## Pretty sure I figured out trace
trace <- fread("~/dissertation/data/bob_trace_data/trace_curves.csv")

scaler <- function(a, b = 0, p = 1) { #a is max activation
  s <- 4
  w <- 0.0001
  cc <- 0.25
  den <- (1 + w * exp(-s * (a - cc)))^(1/w)
  (ss <- (p-b)/den + b)
}

# Range tau = 2-4.5
lucer <- function(l, tau = 4) {
  expl <- lapply(l, function(x) {
    exp(tau * x)
  })
  ss <- Reduce(`+`, expl)
  rr <- lapply(expl, function(x) as.data.table(x / ss))
  rr
}

## Implement luce
l <- as.list(trace[, -"time", with = FALSE])
trace_luce <- Reduce(`cbind`, (lucer(l)))
names(trace_luce) <- names(l)
trace_luce <- cbind(trace[, .(time)], trace_luce)

## Compute scaling term, using both p/b and 0/1
# for target specifically
bb_t <- trace_luce[1, target] # first
pp_t <- trace_luce[108, target] # last

## Once using the correct p/b
trace_luce[, ss_targ := scaler(max(target, cohort, rhyme, ur), bb_t, pp_t), by = time]
trace_luce[, ss_targ2 := scaler(max(target, cohort, rhyme, ur), 0, 1), by = time]

trace_luce[, `:=`(targ1 = target * ss_targ, 
                  targ2 = target * ss_targ2)]

plot(trace_luce$time, trace_luce$targ2,type = 'l', col = 'red')
lines(trace_luce$time, trace_luce$targ1, col = 'blue')
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
