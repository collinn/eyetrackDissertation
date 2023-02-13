library(bdots)
library(eyetrackSim)
library(ggplot2)

simDataSettings <- data.table(mm = c(F, T, T),
                              ar = c(T, T, F),
                              bcor = c(T, F, F))
simDataSettings <- rbind(simDataSettings, simDataSettings)
simDataSettings$sigVal <- rep(c(0.005, 0.025), each = 3)
simDataSettings <- rbind(simDataSettings, simDataSettings)
simDataSettings$slope <- rep(c(0.005, 0.025), each = 6)

## Goal here is to make plot in time showing percentage of times flagged diff
ff <- list.files("rds_files", full.names = TRUE)
ff <- ff[c(1, 5:12, 2:4)]

rr <- readRDS(ff[3])

mm <- rr[[1]][[1]]

## Given signifiance mat, need to return length 401 bool vector
timetiePower <- function(mm) {
  time <- seq(-0.1, 1, length.out = 401)
  vec <- vector("numeric", length = length(time))

  if (is.null(dim(mm))) {
    vec <- as.logical(vec)
    return(vec)
  }

  # each of these into a list
  sm <- split(mm, row(mm))
  bv <- lapply(sm, function(m) {
    #sigt <- do.call(seq, as.list(c(m, by = 0.005)))
    time[time > m[1] & time < m[2] + 0.005] # for rounding
  })
  bv <- Reduce(union, bv)
  rr <- time %in% bv #round(time,3) %in% round(bv, 3)
  return(rr)
}

getDiffSlices <- function(ff) {
  rr <- readRDS(ff)

  sm <- lapply(rr, `[[`, 1)
  mm <- lapply(rr, `[[`, 2)
  pm <- lapply(rr, `[[`, 3)

  smm <- sapply(sm, timetiePower) |> rowSums()
  mmm <- sapply(mm, timetiePower) |> rowSums()
  pmm <- sapply(pm, timetiePower) |> rowSums()
  time <- seq(-1, 1, length.out = 401)
  plot(time, smm, type = 'l', col = 'green', main = paste0(attributes(rr)))
  lines(time, mmm, type = 'l', col = 'black')
  lines(time, pmm, type = 'l', col = 'blue')
  lines(seq(-1, 0, 0.1), rep(5,11), col = 'red', lty = 2, lwd = 2)
  lines(c(0,0), c(0, 100), col = 'red', lty = 2, lwd = 2)
  legend("topleft", legend = c("single", "many", "perm"),
         col = c("green", "black", "blue"), lty = 1, lwd = 1)
  #abline(v = 0, col = 'red', lty = 2,  lwd = 2, ylim = c(5, 100))
}



i <- 1

getDiffSlices(ff[i])
i <- i+1
