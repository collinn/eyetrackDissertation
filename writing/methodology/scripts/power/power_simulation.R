
library(bdots)
library(eyetrackSim)

sds <- data.table(mm = rep(c(F, T, T, T, T), 2),
                  ar = rep(c(T, T, T, F, F), 2),
                  bcor = rep(c(T, T, F, T, F), 2),
                  sigVal = 0.05,
                  slope = rep(c(0.025, 0.25), each = 5))

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
                         TIME = seq(-1,1, by = 0.005))

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

N <- 1000
sims <- vector("list", length = N)
attr(sims, "simsettings") <- sidx
nn <- paste0("sim", idx)
sf <- paste0("prog_txt/", nn, ".txt")
rf <- paste0("negative_one_to_one_rds_files/", nn, ".rds")


sink(file = sf)
print(paste0("starting index: ", idx))
for (i in seq_len(N)) {
  sims[[i]] <- createFits(sidx)
  if (i %% 10 == 0) {
    msg <- paste0("index: ", idx, ", iteration: ", i)
    print(msg)
  }
}
saveRDS(sims, rf)
