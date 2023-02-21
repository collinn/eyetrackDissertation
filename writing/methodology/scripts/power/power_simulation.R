
library(bdots)
library(eyetrackSim)

# Only doing power on these three things yay
sds <- data.table(mm = c(F, T, T),
                  ar = c(T, T, F),
                  bcor = c(T, F, F),
                  sigVal = 0.05,
                  slope = 0.25)

idx <- as.numeric(commandArgs(TRUE))

sidx <- sds[idx, ]

createFits <- function(sidx, nit = 1000) {

  slp <- sidx$slope
  ppars <- c(0, slp)

  ## only setting one of the times
  dat <- createPlineData(manymeans = sidx$mm,
                         ar1 = sidx$ar,
                         distSig = sidx$sigVal,
                         paired = FALSE,
                         pars = ppars,
                         TIME = seq(-1, 1, length.out = 401))

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

N <- 50
sims <- vector("list", length = N)
attr(sims, "simsettings") <- sidx
nn <- paste0("2_sim", idx)
sf <- paste0("prog_txt/", nn, ".txt")
rf <- paste0("tue_new_dist_rds_files/", nn, ".rds")


sink(file = sf)
print(paste0("starting index: ", idx))
for (i in seq_len(N)) {
  sims[[i]] <- createFits(sidx)
  if (i %% 20 == 0) {
    msg <- paste0("index: ", idx, ", iteration: ", i)
    print(msg)
  }
}
saveRDS(sims, rf)
