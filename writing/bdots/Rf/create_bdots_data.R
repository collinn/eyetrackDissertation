
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
  return(dts)
  #return(list(dts = dts, pars = newpars))
}

## Create new distribution for groups
fullDist <- vector("list",  length = 8L)
vv <- groupDist[[1]]$sigma
for (i in seq_along(fullDist)) {
  pp <- do.call(rmvnorm, as.list(c(1, groupDist[[1]])))
  #vv1 <- apply(vv, c(1, 2), jitter)
  fullDist[[i]] <- list(mean = pp, sigma = vv)
}

# for (i in seq_along(fullDist[5:8])) {
#   pp <- do.call(rmvnorm, as.list(c(1, groupDist[[2]])))
#   #vv1 <- apply(vv, c(1, 2), jitter)
#   fullDist[[i]] <- list(mean = pp, sigma = vv)
# }

## Creating 8 groups for disseration bdots paper
dts <- Map(function(x, y) {
  createData(n = 25, trials = 50, pars = x, gp = y)
}, x = fullDist, y = as.list(LETTERS[1:8]))

#####################################
### Need to update IDs for things ###
#####################################

## Classes
cc <- expand.grid(Origin = c("foreign", "domestic"), 
            Vehicle = c("car", "truck"), 
            Color = c("red", "blue"))
cc[] <- lapply(cc, as.character)
cc <- split(cc, row(cc))

## ids for pairs
id <- list(1:25, 1:25 ,
           26:50, 26:50, 
           51:75, 51:75, 
           76:100, 76:100)

dts <- Map(function(x, y, i) {
  x$Origin <- y[1]
  x$Vehicle <- y[2]
  x$Color <- y[3]
  id <- rep(i, each = nrow(x[id == 1, ]))
  x$id <- id
  x
}, x = dts, y = cc, i = id)

dts <- rbindlist(dts)


fit <- bdotsFit(data = dts,
                y = "fixations",
                subject = "id",
                time = "time",
                group = c("Origin", "Vehicle", "Color"), 
                curveType = logistic())

refit <- bdotsRefit(fit, fitCode = 2)

saveRDS(fit, "~/packages/bdots/btest/eightgrpfit.rds")
