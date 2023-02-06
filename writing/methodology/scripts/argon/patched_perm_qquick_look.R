
library(bdots)


ff <- list.files(path = "~/dissertation/writing/methodology/scripts/argon/patched_boot", pattern = "rds", full.names = TRUE)

## OH MY GOD WRONG ORDER
ff <- ff[c(1, 9:16, 2:8)]

simDataSettings <- expand.grid(manymeans = c(TRUE, FALSE),
                               paired = c(TRUE, FALSE),
                               ar1 = c(TRUE, FALSE),
                               bdotscor = c(TRUE, FALSE))

kpidx <- c(12, 16, 4, 8)

#ff <- ff[kpidx]
simDataSettings <- simDataSettings[kpidx, ]

tt <- lapply(ff, readRDS)
tt <- lapply(tt, function(y) {
  y <- lapply(y, `[[`, 1)
  y <- lapply(y, `[[`, 1)
  y <- sapply(y, function(z) !is.null(z)) |> mean()
})

sigs <- lapply(tt,  function(y) {
  y <- lapply(y, `[[`, 1)
})
sigs <- lapply(sigs, `[[`, 3)

fwerhist <- function(y) {
  tt <- readRDS(y)
  sigs <- lapply(tt,  function(y) {
    y <- lapply(y, `[[`, 1)
  })

  psigs

  smt <- sapply(sigs, function(x) timetie(x[[1]]))
  mmt <- sapply(sigs, function(x) timetie(x[[2]]))
  pmt <- sapply(sigs, function(x) timetie(x[[3]]))

  pmtt <- apply(pmt, 2, function(x) TIME[x])
  rr <- Reduce(c, pmtt)
  hist(rr, breaks = seq(0, 1600, 16))
  hist(rr, breaks = seq(0, 1600, 8))
  hist(rr, breaks = seq(0, 1600, 4))
  density(rr, bw = "SJ") |> plot()
}
