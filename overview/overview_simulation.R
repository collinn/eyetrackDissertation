library(data.table)
library(eyetrackSim)
library(bdots)

## For logistic
params <- c(0.05, .9, 0.0019, 969.3)

## No delay
sub1 <- runSub(pars = params, saccadeDelay = 0)

## 200ms delay
sub2 <- runSub(pars = params, saccadeDelay = 200)

## Gamma
sub3 <- runSub(pars = params, saccadeDelay = NULL)

## Test
tt <- buildSaccadeSub(sub1)

## Weird error I should investigate
time <- 0:2000
tt <- lapply(list(sub1, sub2, sub3), function(x) {
  y <- buildSaccadeSub(x)
  y$ID <- 1
  y$group <- 1
  #y <- y[order(starttime), ]
  fit <- bdotsFit(data = y,
                  subject = "ID",
                  group = "group",
                  y = "looks", time = "starttime",
                  curveType = logistic())
  cc <- coef(fit)
  fv <- eyetrackSim:::logistic_f(p = cc, t = time)
  fv <- data.table(time = time, fit = fv)
  return(list("coef" = cc, "fit" = fv))
})





tt <- rbindlist(tt)

tt <- buildSaccadeSub(sub1)
tt$ID <- 1
tt$group <- "a"
fit <- bdotsFit(data = tt,
                y = "looks",
                time = "starttime",
                subject = "ID",
                group = "group",
                curveType = logistic())


qq <- aggregateSub(sub1)
