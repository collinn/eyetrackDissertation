
library(bdots)
library(eyetrackSim)
library(ggplot2)

## This compute overall TIE across all time points
getFWER <- function(y) {
  tt <- readRDS(y)
  sigs <- lapply(tt,  function(y) {
    y <- lapply(y, `[[`, 1)
  })

  sm <- sapply(sigs, function(x) !is.null(x[[1]])) |> mean()
  mm <- sapply(sigs, function(x) !is.null(x[[2]])) |> mean()
  pm <- sapply(sigs, function(x) !is.null(x[[3]])) |> mean()

  smt <- sapply(sigs, function(x) timetie(x[[1]])) |> rowMeans()
  mmt <- sapply(sigs, function(x) timetie(x[[2]])) |> rowMeans()
  pmt <- sapply(sigs, function(x) timetie(x[[3]])) |> rowMeans()

  fwer <- data.table(sm = sm, mm = mm, pm = pm)
  tsmat <- matrix(c(smt, mmt, pmt), byrow = TRUE, nrow = 3,
                  dimnames = list(c("sm", "mm", "pm"), NULL))
  return(list(fwer = fwer, timeSliceMat = tsmat))
}

## This takes the list returned by getfwer
timeSliceFwer <- function(y, f = mean) {
  nn <- length(y)
  y <- lapply(y, `[[`, 2)
  y <- lapply(y, function(z) {
    apply(z, 1, f)
  })
  y <- Reduce(rbind, y)
  #y <- sapply(y, rowMeans)
  #rownames(y) <- paste0("sim", 1:8)
  y
}

## Given signifiance mat, need to return length 401 bool vector
timetie <- function(mm) {
  time <- seq(0, 1600, 4)
  vec <- vector("numeric", length = length(time))

  if (is.null(dim(mm))) {
    vec <- as.logical(vec)
    return(vec)
  }

  # each of these into a list
  sm <- split(mm, row(mm))
  bv <- lapply(sm, function(m) {
    sigt <- do.call(seq, as.list(c(m, 4)))
  })
  bv <- Reduce(union, bv)
  rr <- time %in% bv
  return(rr)
}




ff <- list.files(path = "~/dissertation/writing/methodology/scripts/argon/rds_boot", pattern = "rds", full.names = TRUE)

## OH MY GOD WRONG ORDER
ff <- ff[c(1, 9:16, 2:8)]

simDataSettings <- expand.grid(manymeans = c(TRUE, FALSE),
                               paired = c(TRUE, FALSE),
                               ar1 = c(TRUE, FALSE),
                               bdotscor = c(TRUE, FALSE))


gg <- lapply(ff, getFWER)
fwer <- rbindlist(lapply(gg, `[[`, 1))
tfwer <- timeSliceFwer(gg, median)

fwer <- as.data.table(cbind(simDataSettings, fwer))[order(rev(manymeans), ar1, decreasing = TRUE), ]
tfwer <- as.data.table(cbind(simDataSettings, tfwer))[order(rev(manymeans), ar1, decreasing = TRUE)]


## Get the median comparison rate

y <- ff[5]
makeTimeHist <- function(y, tit = NULL) {
  tt <- readRDS(y)
  sigs <- lapply(tt,  function(y) {
    y <- lapply(y, `[[`, 1)
  })

  smt <- sapply(sigs, function(x) timetie(x[[1]]))
  mmt <- sapply(sigs, function(x) timetie(x[[2]]))
  pmt <- sapply(sigs, function(x) timetie(x[[3]]))

  smt <- apply(smt, 2, function(z)  TIME[z])
  smt <- Reduce("c", smt)
  mmt <- apply(mmt, 2, function(z)  TIME[z])
  mmt <- Reduce("c", mmt)
  pmt <- apply(pmt, 2, function(z)  TIME[z])
  pmt <- Reduce("c", pmt)

  if (is.null(smt)) smt <- 0
  if (is.null(mmt)) mmt <- 0
  if (is.null(pmt)) pmt <- 0


  ## Whichever has largest count should be plotted first
  s1 <- hist(smt, breaks = seq(0,1600, 32), plot = FALSE)$counts |> max()
  m1 <- hist(mmt, breaks = seq(0,1600, 32), plot = FALSE)$counts |> max()
  p1 <- hist(pmt, breaks = seq(0,1600, 32), plot = FALSE)$counts |> max()
  yy <- max(c(s1, m1, p1))
  idx <- which.max(c(s1, m1, p1))
  idxm <- which.min(c(s1, m1, p1))

  ## stupid way of doing
  mh <- function(x, co, a = TRUE, tt = NULL) {
    if (identical(x, 0)) return()
    hist(x, breaks = seq(0, 1600, 32),
         add = a, col = co, ylim = c(0, yy), main = tt, xlab = "Time")
  }
  if (idx == 1) {
    mh(smt, co = alpha("chartreuse4", alpha = 0.5), a = FALSE, tit)
    if (idxm == 2) {
      mh(pmt, co = alpha("tomato", alpha = 0.5))
      mh(mmt, co = alpha("steelblue", alpha = 0.5))
    } else {
      mh(mmt, co = alpha("steelblue", alpha = 0.5))
      mh(pmt, co = alpha("tomato", alpha = 0.5))
    }
  } else if (idx == 2) {
    mh(mmt, co = alpha("steelblue", alpha = 0.5), a = FALSE, tit)
    if (idxm == 3) {
      mh(smt, co = alpha("chartreuse4", alpha = 0.5))
      mh(pmt, co = alpha("tomato", alpha = 0.5))
    } else {
      mh(pmt, co = alpha("tomato", alpha = 0.5))
      mh(smt, co = alpha("chartreuse4", alpha = 0.5))
    }
  } else {
    mh(pmt, co = alpha("tomato", alpha = 0.5), a = FALSE, tit)
    if (idxm == 1) {
      mh(mmt, co = alpha("steelblue", alpha = 0.5))
      mh(smt, co = alpha("chartreuse4", alpha = 0.5))
    } else {
      mh(smt, co = alpha("chartreuse4", alpha = 0.5))
      mh(mmt, co = alpha("steelblue", alpha = 0.5))
    }
  }
  ## Obviously this a retarded thing to do
  if (ii == 1) {
    legend("topright", legend = c("Bad Bootstrap", "Good Bootstrap", "Permutation"),
           fill = c(alpha("chartreuse4", alpha = 0.5),
                    alpha("steelblue", alpha = 0.5),
                    alpha("tomato", alpha = 0.5)))
  }


  # fwer <- data.table(sm = sm, mm = mm, pm = pm)
  # tsmat <- matrix(c(smt, mmt, pmt), byrow = TRUE, nrow = 3,
  #                 dimnames = list(c("sm", "mm", "pm"), NULL))
  # return(list(fwer = fwer, timeSliceMat = tsmat))
}



## Ok, lets just do a page of this for now
ff <- ff[c(4, 12, 8, 16, 3, 11, 7, 15)]


pdf("~/dissertation/writing/methodology/img/TEMP_histogram.pdf",
    width = 6.5, height = 7)
par(mfrow = c(4,2))
ii <- 1
makeTimeHist(ff[ii], paste("Simulation", ii)); ii <- ii + 1
makeTimeHist(ff[ii], paste("Simulation", ii)); ii <- ii + 1
makeTimeHist(ff[ii], paste("Simulation", ii)); ii <- ii + 1
makeTimeHist(ff[ii], paste("Simulation", ii)); ii <- ii + 1
makeTimeHist(ff[ii], paste("Simulation", ii)); ii <- ii + 1
makeTimeHist(ff[ii], paste("Simulation", ii)); ii <- ii + 1
makeTimeHist(ff[ii], paste("Simulation", ii)); ii <- ii + 1
makeTimeHist(ff[ii], paste("Simulation", ii)); ii <- ii + 1
dev.off()
