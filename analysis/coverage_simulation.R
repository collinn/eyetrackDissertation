
library(data.table)
library(bdots)
library(eyetrackSim)
library(mvtnorm)



## Let's start by creating distribution for each group
ci <- as.data.table(ci)
ci <- ci[LookType == "Target", ]
fit <- bdotsFit(data = ci,
                y = "Fixations",
                subject = "Subject",
                time = "Time",
                group = "protocol", curveType = logistic())

groupDist <- lapply(split(fit, by = "protocol"), function(x) {
  cc <- coef(x)
  vv <- var(cc)
  cc <- colMeans(cc)
  # Got get these in right spot
  cc[1] <- abs(cc[1])
  cc[2] <- pmin(cc[2], 1)
  list(mean = cc, sigma = vv)
})


# createData <- function(n = 25, trials = 10, pars, gp = "A") {
#   ## Constant
#   time <- seq(0, 2000, by = 4)
#   newpars <- do.call(rmvnorm, as.list(c(n, pars)))
#   ## no negatives or greater than one please
#   newpars[, 1] <- abs(newpars[, 1])
#   newpars[, 2] <- pmin(newpars[, 2], 1)
#   spars <- split(newpars, row(newpars))
#   dts <- lapply(seq_len(n), function(x) {
#     pp <- spars[[x]]
#     dt <- data.table(id = x,
#                      time = time,
#                      group = gp,
#                      true = eyetrackSim:::logistic_f(pp, time))
#     dt[, fixations := mean(rbinom(trials, 1, true)), by = time]
#   })
#   dts <- rbindlist(dts)
#   # ggplot(dts, aes(time, true, group = id)) + geom_line(aes(color = as.character(id)))
#   return(list(dts = dts, pars = newpars))
# }

# dts <- Map(function(x, y) {
#   createData(n = 10, trials = 20, pars = x, gp = y)
# }, x = groupDist, y = list("A", "B"))
# 
# ## Change ID
# dts[[2]]$dts$id <- dts[[2]]$dts$id + 25
# dt <- rbindlist(lapply(dts, `[[`, 1))
# 
# ## starting parameters
# parsA <- groupDist$CI$mean
# parsB <- groupDist$NH$mean
# 
# ## Ok bdots
# fit <- bdotsFit(data = dt,
#                 group = "group",
#                 y = "fixations",
#                 time = "time",
#                 subject = "id",
#                 curveType = logistic())
# 
# 
# boot <- bdotsBoot(formula = fixations ~ group(A,B), fit)


################################################################
## Above first attempt, but with two groups. Lets just do one ##
################################################################
ci <- as.data.table(ci)
ci <- ci[LookType == "Target", ]
fit <- bdotsFit(data = ci,
                y = "Fixations",
                subject = "Subject",
                time = "Time",
                group = "protocol", curveType = logistic())

groupDist <- lapply(split(fit, by = "protocol"), function(x) {
  cc <- coef(x)
  vv <- var(cc)
  cc <- colMeans(cc)
  # Got get these in right spot
  cc[1] <- abs(cc[1])
  cc[2] <- pmin(cc[2], 2 - cc[2])
  list(mean = cc, sigma = vv)
})


## For a single group?
#' @param n number of subjects
#' @param trials number of trials
#' @param pars list of mean and sigma
createData <- function(n = 25, trials = 10, pars, gp = "A") {
  ## Constant
  time <- seq(0, 2000, by = 4)
  newpars <- do.call(rmvnorm, as.list(c(n, pars)))
  ## no negatives or greater than one please
  newpars[, 1] <- abs(newpars[, 1])
  newpars[, 2] <- pmin(newpars[, 2], 1)
  spars <- split(newpars, row(newpars))
  dts <- lapply(seq_len(n), function(x) {
    pp <- spars[[x]]
    dt <- data.table(id = x,
                     time = time,
                     group = gp,
                     true = eyetrackSim:::logistic_f(pp, time))
    dt[, fixations := mean(rbinom(trials, 1, true)), by = time]
  })
  dts <- rbindlist(dts)
  # ggplot(dts, aes(time, true, group = id)) + geom_line(aes(color = as.character(id)))
  return(list(dts = dts, pars = newpars))
}
# 
# dts <- Map(function(x, y) {
#   createData(n = 10, trials = 20, pars = x, gp = y)
# }, x = groupDist, y = list("A", "B"))
# 
# dat <- createData(n = 25, trials = 10, pars = groupDist[[1]], gp = "A")
# 
# ## Ok bdots
# fit <- bdotsFit(data = dat[[1]],
#                 group = "group",
#                 y = "fixations",
#                 time = "time",
#                 subject = "id",
#                 curveType = logistic())

## Ok, let's collect our bootstraps

# x is fitted object, b is bootstrap
bsPars <- function(x, b = 1000) {
  bsPars2 <- function(x) {
    idx <- sample(seq_len(nrow(x)), replace = TRUE)
    xn <- x[idx, ]
    xn$splitvar <- seq_len(nrow(x))
    xns <- split(xn, by = "splitvar")
    xpar <- vapply(xns, function(z) {
      rmvnorm(1, coef(z), vcov(z$fit[[1]]))
    }, numeric(4))
    rowMeans(xpar)
  }
  tt <- replicate(b, bsPars2(x), simplify = TRUE) |> t()
}




### Here we make consideration for a single simulated run
# tp <- groupDist[[1]]$mean
# x <- bsPars(fit, b = 1000)

## How do the pars fit in here
## This returns quantile of each parameter
parQuants <- function(x, tp) {
  qq <- Map(function(y, p) {
    ff <- ecdf(y)
    ff(p)
  }, y = split(x, col(x)), p = tp)
  setNames(unlist(qq), names(tp))
}
# parQuants(x, tp)

curveQuants <- function(x, tp) {
  time <- seq(0, 2000, by = 4)
  cc <- apply(x, 1, function(y) {
    eyetrackSim:::logistic_f(y, time)
  })
  actualCurve <- eyetrackSim:::logistic_f(tp, time)
  cc_ecdf <- apply(cc, 1, ecdf)
  tt <- Map(function(ac, ec) {
    ec(ac)
  }, ac = actualCurve, ec = cc_ecdf) |> unlist()
  tt
}

## groupDist[[1]] is are set of starting parameters
runSingleSim <- function(n = 25, m = 100, b = 100, pars = groupDist[[1]]) {

  ## First, create data
  dat <- createData(n = n, trials = m, pars = groupDist[[1]], gp = "A")

  ## Ok bdots
  fit <- bdotsFit(data = dat[[1]],
                  group = "group",
                  y = "fixations",
                  time = "time",
                  subject = "id",
                  curveType = logistic())

  ## Ok, let's collect our bootstraps
  x <- bsPars(fit, b = b)
  tp <- groupDist[[1]]$mean

  ## Parameter info first
  pq <- parQuants(x, tp)
  cq <- curveQuants(x, tp)
  list(pq = pq, cq = cq)
}



runSim <- function(N = 100, n = 25, m = 100, b = 1000, pars = groupDist[[1]]) {
  tt <- replicate(N, runSingleSim(n, m, b, pars))
  pp <- tt[1, ] |> unlist() |> matrix(ncol = 4, byrow = TRUE)
  cc <- tt[2, ] |> unlist() |> matrix(ncol = 501, byrow = TRUE) |> t()
  list(parQuants = pp, curveQuants = cc)
}

gg <- expand.grid(N = 100,
            n = 25,
            m = c(10, 25, 50, 75, 100),
            b = 1000)

gg <- split(gg, row(gg))


results <- lapply(gg, function(arg) {
  do.call(runSim, arg)
})
# beepr::beep(3)
saveRDS(results, file = "../data/results.rds")

## Ok, let's practice on a single instance of this (only one that worked, fuck)
# rr <- results[[1]]
# 
# pp <- lapply(results, `[[`, 1)
# cc <- lapply(results, `[[`, 2)
# 
# ## Parameters first, let's say 90 percent coverage
# pp <- lapply(pp, function(x) {
#   parCoverage <- apply(x, 2, function(y) sum(y > 0.05 & y < 0.95) / length(y))
# })
# 
# mini <- lapply(pp, `[[`, 1) |> unlist()
# peak <- lapply(pp, `[[`, 2) |> unlist()
# slope <- lapply(pp, `[[`, 3) |> unlist()
# cross <- lapply(pp, `[[`, 4) |> unlist()
# 
# par(mfrow = c(2,2))
# plot(mini, ylim = c(0, 1.1), main = "mini", type = 'b', xaxt = 'n', xlab = "# Trials")
# abline(h = 0.9, col = 'red', lty = 2, ylab = "Coverage")
# axis(1, at = 1:5, labels = c(10, 25, 50, 75, 100))
# 
# plot(peak, ylim = c(0, 1.1), main = "peak", type = 'b', xaxt = 'n', xlab = "# Trials")
# abline(h = 0.9, col = 'red', lty = 2, ylab = "Coverage")
# axis(1, at = 1:5, labels = c(10, 25, 50, 75, 100))
# 
# plot(slope, ylim = c(0, 1.1), main = "slope", type = 'b', xaxt = 'n', xlab = "# Trials")
# abline(h = 0.9, col = 'red', lty = 2, ylab = "Coverage")
# axis(1, at = 1:5, labels = c(10, 25, 50, 75, 100))
# 
# plot(cross, ylim = c(0, 1.1), main = "cross", type = 'b', xaxt = 'n', xlab = "# Trials")
# abline(h = 0.9, col = 'red', lty = 2, ylab = "Coverage")
# axis(1, at = 1:5, labels = c(10, 25, 50, 75, 100))

# ## This takes average across time points, then average across trials, then average total
# cc_cover <- lapply(cc, function(x) {
#   ## Here we just do full band for now
#   mm <- apply(x, 1, function(y) sum(y > 0.05 & y < 0.95) / length(y))
#   mean(mm)
# }) |> unlist()
# #cc_cover <- Reduce(`+`, cc_cover) / length(cc_cover)
# par(mfrow = c(1,1))
# plot(cc_cover, ylim = c(0, 1.1), main = "Average pointwise coverage", type = 'b',
#      xaxt = 'n', xlab = "# Trials", ylab = "Coverage")
# abline(h = 0.9, col = 'red', lty = 2)
# axis(1, at = 1:5, labels = c(10, 25, 50, 75, 100))


#################################################
## ok lets look at coverage at each time point ##
#################################################

# ## Practice with the first
# cc_time <- lapply(cc, function(x) {
#   ## Here we just do full band for now
#   mm <- apply(x, 1, function(y) sum(y > 0.05 & y < 0.95) / length(y))
#   #mean(mm)
# }) #|> unlist()
# 
# par(mfrow = c(2, 3))
# mtrials <- c(10, 25, 50, 75, 100)
# for (i in seq_along(cc_time)) {
#   x <- cc_time[[i]]
#   plot(x, ylim = c(0.5, 1.05), xlab = "Time", xaxt = 'n', type = 'l',
#        ylab = 'coverage', main = paste(mtrials[i], "Trials"))
#   abline(h = 0.9, col = 'red', lty = 2)
#   axis(1, at = seq(0, 2000, by = 50), labels = seq(0, 2000, by = 50))
# }




#################################################
##  ok lets look at coverage for entire curve  ##
#################################################

# cc_band <- lapply(cc, function(x) {
#   ## Go through each trial and determine what quantile
#   # needed to cover entire band
#   mm <- apply(x, 2, function(y) max(max(y), 1 - min(y)))
#   quantile(mm, 0.9)
# })

# par(mfrow = c(1,1))
# plot(unlist(cc_band), ylim = c(0.9, 1.0), main = "Required quantile for 90% coverage of entire band", type = 'b',
#      xaxt = 'n', xlab = "# Trials", ylab = "Coverage")
# abline(h = 0.9, col = 'red', lty = 2)
# axis(1, at = 1:5, labels = c(10, 25, 50, 75, 100))
# text(1:5, unlist(cc_band) + 0.005, unlist(cc_band), cex = 0.8)
# 
# ## For example, here is a band of quantiles needed to cover. Which is 90?
# quantile(mm, 0.9)


################################################################
################################################################
##     Time for simulation for motherfucking bdots, bitch     ##
################################################################
################################################################


groupDist <- lapply(split(fit, by = "protocol"), function(x) {
  cc <- coef(x)
  vv <- var(cc)
  cc <- colMeans(cc)
  # Got get these in right spot
  cc[1] <- abs(cc[1])
  cc[2] <- pmin(cc[2], 1)
  list(mean = cc, sigma = vv)
})


## For a single group?
#' @param n number of subjects
#' @param trials number of trials
#' @param pars list of mean and sigma
createData <- function(n = 25, trials = 10, pars, gp = "A") {
  ## Constant
  time <- seq(0, 2000, by = 4)
  newpars <- do.call(rmvnorm, as.list(c(n, pars)))
  ## no negatives or greater than one please
  newpars[, 1] <- abs(newpars[, 1])
  newpars[, 2] <- pmin(newpars[, 2], 1)
  spars <- split(newpars, row(newpars))
  dts <- lapply(seq_len(n), function(x) {
    pp <- spars[[x]]
    dt <- data.table(id = x,
                     time = time,
                     group = gp,
                     true = eyetrackSim:::logistic_f(pp, time))
    dt[, fixations := mean(rbinom(trials, 1, true)), by = time]
  })
  dts <- rbindlist(dts)
  # ggplot(dts, aes(time, true, group = id)) + geom_line(aes(color = as.character(id)))
  return(list(dts = dts, pars = newpars))
}


## This function needs to be different from the previous, whereas
# the one for parameters does not
## Unlike previous, does not return quantiles or use ecdf, only binary coverage in 90% CI
curveQuants_boot <- function(cl, tp, alpha) {
  tv <- stats::qt(1 - alpha / 2, cl[['n']] - 1)
  fit <- cl[['fit']]
  sd <- cl[['sd']]
  cc <- eyetrackSim:::logistic_f(tp, t = seq(0, 2000, by = 4))
  mm <- data.table(l = fit - sd * tv,
                   y   = cc,
                   u = fit + sd * tv)
  mm[, cvr := y > l & y < u]
  as.integer(mm$cvr)
}

## This one does not use the modified alpha, as it shouldn't
curveQuants <- function(x, tp) {
  time <- seq(0, 2000, by = 4)
  cc <- apply(x, 1, function(y) {
    eyetrackSim:::logistic_f(y, time)
  })
  actualCurve <- eyetrackSim:::logistic_f(tp, time)
  cc_ecdf <- apply(cc, 1, ecdf)
  tt <- Map(function(ac, ec) {
    ec(ac)
  }, ac = actualCurve, ec = cc_ecdf) |> unlist()
  tt
}


runSingleSim_bdots <- function(n = 25, m = 100, b = 1000, pars = groupDist) {

  ## First, create data
  dts <- Map(function(x, y) {
    createData(n, trials = m, pars = x, gp = y)
  }, x = groupDist, y = list("A", "B"))

  ## Change ID
  dts[[2]]$dts$id <- dts[[2]]$dts$id + n

  ## Only a single subject for group B, because we don't care
  dt <- rbindlist(list(dts[[1]][[1]], dts[[2]][[1]][id == n + 1]))

  ## Ok bdots
  fit <- bdots0::bdotsFit(data = dt,
                  group = "group",
                  y = "fixations",
                  time = "time",
                  subject = "id",
                  curveType = logistic())

  ## Now bootstrap
  bb <- bdots0::bdotsBoot(formula = fixations ~ group(A,B), fit, alpha = 0.1, Niter = b)

  ## Get parameters
  x <- bb$curveList$A$parMat
  tp <- groupDist[[1]]$mean

  ## Either quantile or coverage
  pq <- parQuants(x, tp)
  #cq <- curveQuants_boot(bb$curveList$A, tp, bb$adjalpha)
  cq <- curveQuants(bb$curveList$A$parMat, tp)

  list(pq = pq, cq = cq)
}



runSim_bdots <- function(N = 100, n = 25, m = 100, b = 1000, pars = groupDist) {
  tt <- replicate(N, runSingleSim_bdots(n, m, b, pars))
  pp <- tt[1, ] |> unlist() |> matrix(ncol = 4, byrow = TRUE)
  cc <- tt[2, ] |> unlist() |> matrix(ncol = 501, byrow = TRUE) |> t()
  list(parQuants = pp, curveQuants = cc)
}

gg <- expand.grid(N = 100,
                  n = 25,
                  m = c(10, 25, 50, 75, 100),
                  b = 1000)

gg <- split(gg, row(gg))


results <- lapply(gg, function(arg) {
  do.call(runSim_bdots, arg)
})
# beepr::beep(3)
saveRDS(results, file = "../data/bdots_coverage_results.rds")

results_bdots <- results






# 
# 
# ####### Delete this below
# pp <- lapply(results, `[[`, 1)
# cc <- lapply(results, `[[`, 2)
# 
# ## Parameters first, let's say 90 percent coverage
# pp <- lapply(pp, function(x) {
#   parCoverage <- apply(x, 2, function(y) sum(y > 0.05 & y < 0.95) / length(y))
# })
# 
# mini <- lapply(pp, `[[`, 1) |> unlist()
# peak <- lapply(pp, `[[`, 2) |> unlist()
# slope <- lapply(pp, `[[`, 3) |> unlist()
# cross <- lapply(pp, `[[`, 4) |> unlist()
# 
# par(mfrow = c(2,2))
# plot(mini, ylim = c(0, 1.1), main = "mini", type = 'b', xaxt = 'n', xlab = "# Trials")
# abline(h = 0.9, col = 'red', lty = 2, ylab = "Coverage")
# axis(1, at = 1:5, labels = c(10, 25, 50, 75, 100))
# 
# plot(peak, ylim = c(0, 1.1), main = "peak", type = 'b', xaxt = 'n', xlab = "# Trials")
# abline(h = 0.9, col = 'red', lty = 2, ylab = "Coverage")
# axis(1, at = 1:5, labels = c(10, 25, 50, 75, 100))
# 
# plot(slope, ylim = c(0, 1.1), main = "slope", type = 'b', xaxt = 'n', xlab = "# Trials")
# abline(h = 0.9, col = 'red', lty = 2, ylab = "Coverage")
# axis(1, at = 1:5, labels = c(10, 25, 50, 75, 100))
# 
# plot(cross, ylim = c(0, 1.1), main = "cross", type = 'b', xaxt = 'n', xlab = "# Trials")
# abline(h = 0.9, col = 'red', lty = 2, ylab = "Coverage")
# axis(1, at = 1:5, labels = c(10, 25, 50, 75, 100))
# 
# ## This takes average across time points, then average across trials, then average total
# cc_cover <- lapply(cc, function(x) {
#   ## Here we just do full band for now
#   mm <- apply(x, 1, function(y) mean(y))
#   mean(mm)
# }) |> unlist()
# #cc_cover <- Reduce(`+`, cc_cover) / length(cc_cover)
# par(mfrow = c(1,1))
# plot(cc_cover, ylim = c(0, 1.1), main = "Average pointwise coverage", type = 'b',
#      xaxt = 'n', xlab = "# Trials", ylab = "Coverage")
# abline(h = 0.9, col = 'red', lty = 2)
# axis(1, at = 1:5, labels = c(10, 25, 50, 75, 100))
# 
# cc2 <- lapply(results_bdots, `[[`, 2)
# cc_cover2 <- lapply(cc2, function(x) {
#   ## Here we just do full band for now
#   mm <- apply(x, 1, function(y) sum(y > 0.05 & y < 0.95) / length(y))
#   mean(mm)
# }) |> unlist()
# 
# #################################################
# ## ok lets look at coverage at each time point ##
# #################################################
# 
# ## Practice with the first
# cc_time <- lapply(cc, function(x) {
#   ## Here we just do full band for now
#   mm <- apply(x, 1, function(y) sum(y > 0.05 & y < 0.95) / length(y))
#   #mean(mm)
# }) #|> unlist()
# 
# par(mfrow = c(2, 3))
# mtrials <- c(10, 25, 50, 75, 100)
# for (i in seq_along(cc_time)) {
#   x <- cc_time[[i]]
#   plot(x, ylim = c(0.5, 1.05), xlab = "Time", xaxt = 'n', type = 'l',
#        ylab = 'coverage', main = paste(mtrials[i], "Trials"))
#   abline(h = 0.9, col = 'red', lty = 2)
#   axis(1, at = seq(0, 2000, by = 50), labels = seq(0, 2000, by = 50))
# }
# 
# 
# 
# 
# 








