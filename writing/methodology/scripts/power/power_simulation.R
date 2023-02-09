
library(bdots)
library(eyetrackSim)

# Only doing power on these three things yay
simDataSettings <- data.table(mm = c(F, T, T),
                              ar = c(T, T, F),
                              bcor = c(T, F, F))
simDataSettings <- rbind(simDataSettings, simDataSettings)
simDataSettings$timetog <- c(1, 1, 1, 2, 2, 2)

idx <- as.numeric(commandArgs(TRUE))

sidx <- simDataSettings[idx, ]

createFits <- function(sidx, nit = 500) {

  if (sidx$timetog == 2) {
    tttime <- seq(-2, 2, length.out = 501)
  } else {
   tttime <- seq(-1, 1, length.out = 401)
  }


  dat <- createPlineData2(manymeans = sidx$mm,
                         ar1 = sidx$ar,
                         TIME = tttime, distSig = 0.025)

  fit <- bdotsFit(data = dat,
                  y = "fixations",
                  group = "group",
                  subject = "id",
                  time = "time",
                  curveType = plinePars(),
                  cores = detectCores() - 1L,
                  cor = sidx$bcor)


  sm <- bdotsBoot(formula = fixations ~ group(A, B),
                  bdObj = fit, singleMeans = TRUE, Niter = nit)$sigTime
  mm <- bdotsBoot(formula = fixations ~ group(A, B),
                  bdObj = fit, Niter = nit)$sigTime
  pm <- suppressMessages(bdotsBoot(formula = fixations ~ group(A, B),
                  bdObj = fit, skipDist = TRUE, Niter = nit,
                  permutation = TRUE)$sigTime)
  list(singlemean = sm,
       manymean = mm,
       permutation = pm)
}

N <- 100
sims <- vector("list", length = N)
nn <- paste0("sim", idx)
sf <- paste0("prog_txt/", nn, ".txt")
rf <- paste0("rds_files/", nn, ".rds")


sink(file = sf)
print(paste0("starting index: ", idx))
for (i in seq_len(N)) {
  sims[[i]] <- createFits(sidx)
  if (i %% 10 == 0) {
    msg <- paste0("index: ", idx, ", iteration: ", i)
    print(msg)
    #saveRDS(object = sims, file = rf)
  }
}
saveRDS(sims, rf)
