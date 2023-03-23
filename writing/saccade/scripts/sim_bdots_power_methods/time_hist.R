
# tt <- replicate(n = N, expr = runSim_lg(idx = 1))
# tt2 <- replicate(n = N, expr = runSim_lg(idx = 2))
# tt3 <- replicate(n = N, expr = runSim_lg(idx = 3))
#
# tt4 <- replicate(n = N, expr = runSim_dg(idx = 4))
# tt5 <- replicate(n = N, expr = runSim_dg(idx = 5))
# tt6 <- replicate(n = N, expr = runSim_dg(idx = 6))


library(bdots)
library(eyetrackSim)

ff <- list.files("rds_files", full.names = TRUE, pattern = "rds")
ff <- list.files("alt_rds_files", full.names = TRUE, pattern = "rds")


##
y <- readRDS(ff[5])

onset <- y[1,]
fix <- y[2, ]

sapply(fix, is.null) |> mean()
sapply(onset, is.null) |> mean()


##########################



powerHist <- function(y, tit) {
  y <- readRDS(y)
  ons <- y[1, ]
  fix <- y[2, ]

  timesig <- function(mm) {
    time <- seq(0, 2000, 4)
    vec <- vector("numeric", length = length(time))

    if (is.null(dim(mm))) {
      vec <- as.logical(vec)
      return(vec)
    }

    sm <- split(mm, row(mm))
    bv <- lapply(sm, function(m) {
      sigt <- do.call(seq, as.list(c(m, 4)))
    })
    bv <- Reduce(union, bv)
    rr <- time %in% bv # logical vec
    return(rr)
  }

  TIME <- seq(0, 2000, 4)

  ons_mat <- sapply(ons, timesig)
  ons_mat <- apply(ons_mat, 2, function(z) TIME[z])
  ons_mat <- Reduce("c", ons_mat)

  fix_mat <- sapply(fix, timesig)
  fix_mat <- apply(fix_mat, 2, function(z) TIME[z])
  fix_mat <- Reduce("c", fix_mat)

  s1 <- hist(ons_mat, breaks = seq(0, 2000, 40), plot = FALSE)$counts
  s2 <- hist(fix_mat, breaks = seq(0, 2000, 40), plot = FALSE)$counts

  yy <- max(s1, s2)
  hist(ons_mat, breaks = seq(0, 2000, 40), ylim = c(0, yy), main = tit,
       xlab = "Time", col = alpha("steelblue", alpha = 0.5))
  hist(fix_mat, breaks = seq(0, 2000, 40), ylim = c(0, yy), main = tit,
       xlab = "Time", col = alpha("tomato", alpha = 0.5), add = TRUE)
}

powerHist(ff[1], "no delay lg")
powerHist(ff[2], "norm delay lg")
powerHist(ff[3], "weib delay lg")

powerHist(ff[4], "no delay dg")
powerHist(ff[5], "norm delay dg")
powerHist(ff[6], "weib delay dg")
