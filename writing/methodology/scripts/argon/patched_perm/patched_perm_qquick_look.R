
library(bdots)


ff <- list.files(path = "~/dissertation/writing/methodology/scripts/argon/patched_perm/patched_boot", pattern = "rds", full.names = TRUE)

## OH MY GOD WRONG ORDER
#ff <- ff[c(1, 9:16, 2:8)]
ff <- ff[c(1, 5:12, 2:4)]

simDataSettings <- expand.grid(manymeans = c(TRUE, FALSE),
                               paired = c(TRUE, FALSE),
                               ar1 = c(TRUE, FALSE),
                               bdotscor = c(TRUE, FALSE))
simDataSettings <- simDataSettings[c(1L, 3L, 4L, 5L, 7L, 8L, 9L, 11L, 12L, 13L, 15L, 16L), ]
simDataSettings <- as.data.table(simDataSettings)


tt <- lapply(ff, readRDS)
tie <- sapply(tt, function(y) {
  y <- lapply(y, `[[`, 1)
  y <- lapply(y, `[[`, 1)
  y <- sapply(y, function(z) !is.null(z)) |> mean()
})

simDataSettings$tie <- tie

sds <- simDataSettings[order(ar1, decreasing = TRUE), ][order(manymeans)][order(paired),]

sigs <- lapply(tt,  function(y) {
  y <- lapply(y, `[[`, 1)
})
sigs <- lapply(sigs, `[[`, 1)
sigs <- lapply(sigs, `[[`, 1)

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
