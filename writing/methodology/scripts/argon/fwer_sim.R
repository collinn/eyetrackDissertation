library(eyetrackSim)
library(bdots)

## Ok, what would it look like to create all these fits
# only n = 25 for time, taking us from 64 to 16
simDataSettings <- expand.grid(manymeans = c(TRUE, FALSE),
                               paired = c(TRUE, FALSE),
                               ar1 = c(TRUE, FALSE),
                               bdotscor = c(TRUE, FALSE))


idx <- as.numeric(commandArgs(TRUE))

sidx <- simDataSettings[idx, ]

createFits <- function(sidx) {
  dat <- createData(paired = sidx$paired,
                    ar1 = sidx$ar1,
                    manymeans = sidx$manymeans)
  fit <- bdotsFit(data = dat$dts,
                  y = "fixations",
                  group = "group",
                  subject = "id",
                  time = "time",
                  curveType = logistic(),
                  cores = detectCores() - 1L,
                  cor = sidx$bdotscor)
  list(fit = fit, data = dat)
}

N <- 100
res <- vector("list", length = N)
nn <- paste0("sim", idx)
sf <- paste0("prog_txt/", nn, ".txt")
rf <- paste0("rds_files/", nn, ".rds")


sink(file = sf)
print(paste0("starting index: ", idx))
for (i in seq_len(N)) {
  fit <- createFits(sidx)

  sm <- bdotsBoot(formula = fixations ~ group(A, B),
                  bdObj = fit, singleMeans = TRUE,
                  cores = detectCores() - 1L)$sigTime

  mm <- bdotsBoot(formula = fixations ~ group(A, B),
                  bdObj = fit, singleMeans = FALSE,
                  cores = detectCores() - 1L)$sigTime

  pm <- suppressMessages(bdotsBoot(formula = fixations ~ group(A, B),
                                   bdObj = fit, permutation = TRUE, skipDist = TRUE,
                                   cores = detectCores() - 1L))$sigTime

  res[[i]] <- list(singleMeans = sm,
                   manyMeans = mm,
                   permutation = pm)

  if (i %% 10 == 0) {
    msg <- paste0("index: ", idx, ", iteration: ", i)
    print(msg)
  }
}
saveRDS(res, rf)
