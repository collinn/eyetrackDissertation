
library(bdots)
library(eyetrackSim)
library(mvtnorm)
library(MASS)

## First, fit actual data that we have
load("~/packages/bdots/data/ci.rda")
ci <- as.data.table(ci)
ci <- ci[LookType == "Target", ]
res.l <- bdotsFit(data = ci,
                  subject = "Subject",
                  time = "Time",
                  y = "Fixations",
                  group = "protocol",
                  curveType = logistic(),
                  cor = TRUE,
                  numRefits = 2)


## Take subsets for new starting parameters
sub1 <- res.l[Subject == 2, ]$fit[[1]]
sub2 <- res.l[Subject == 38, ]$fit[[1]]

## Create new subjects based on this
createSets <- function(x, n = 20, time = seq(0, 2000, by = 4), group = "A") {
  set.seed(69)
  pars <- coef(x)
  vv <- vcov(x)
  vv <- 15*diag(nrow(vv)) %*% vv
  #vv <- 15*vv
  mm <- mvrnorm(n, mu = pars, Sigma = vv)
  mmF <- apply(mm, 1, function(x) {
    eyetrackSim:::logistic_f(x, time)
  })
  #matplot(mmF, type = 'l')
  dt <- data.table(sub = rep(1:n, each = length(time)),
                   time = time,
                   group = group,
                   fixation = as.numeric(mmF))
  return(list(pars = mm, fits = mmF, dt = dt))
}

dt1 <- createSets(sub1)
dt2 <- createSets(sub2, group = "B")
dt2$dt[, sub := sub + 20]
dt <- rbindlist(list(dt1$dt, dt2$dt))

## Fit new subjects
res <- bdotsFit(data = dt,
                y = "fixation",
                group = "group",
                subject = "sub",
                time = "time",
                curveType = logistic(),
                cores = 7)

## Create bootstrap function that
# 1. Resamples w replacement from each group
# 2. Gets mean and var matrix
# 3. Redraw new parameters
# 4. combine and fit curve
#' @param x A bdotsObj
bsFunction <- function(x) {
 xs <- split(x, by = "group")
 newpars <- lapply(xs, function(y) {
   idx <- sample(seq_len(nrow(y)), replace = TRUE)
   yn <- y[idx, ]
   yn$splitvar <- seq_len(nrow(y))
   yns <- split(yn, by = "splitvar")
   ypar <- vapply(yns, function(z) {
     rmvnorm(1, coef(z), vcov(z$fit[[1]]))
   }, numeric(4)) |> t()
   colMeans(ypar)
 })
}

bootstrapCurves <- function(x, n = 100) {
  tt <- replicate(n, bsFunction(x), simplify = FALSE)
  A <- sapply(tt, `[[`, 1) |> t()
  B <- sapply(tt, `[[`, 2) |> t()
  time <- attr(x, "time")
  ac <- apply(A, 1, function (x) {
    eyetrackSim:::logistic_f(x, time)
  })
  bc <- apply(B, 1, function (x) {
    eyetrackSim:::logistic_f(x, time)
  })
  parMat <- list("A" = A, "B" = B)
  curves <- list("A" = ac, "B" = bc)
  list(pars = parMat, curves = curves)
}

tt <- bootstrapCurves(res, 1000)

ac <- tt$curves$B
matplot(ac, type = 'l')
lines(eyetrackSim:::logistic_f(coef(sub1), time))

mainA <- eyetrackSim:::logistic_f(coef(sub2), time)

qm <- apply(ac, 1, function(x) quantile(x, probs = c(0.025, 0.975)))
mmat <- apply(ac, 1, mean)
mm2 <- cbind(qm[1, ], mmat, qm[2, ])
matplot(mm2, type = 'l', lty = c(2,1,2), col = c("red", "black", "red"),
        main = "quantile")
lines(mainA, col = 'blue')
