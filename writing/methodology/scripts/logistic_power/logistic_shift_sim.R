library(eyetrackSim)
library(bdots)

sds <- expand.grid(paired = c(TRUE, FALSE),
                   xosd = c(60, 120),
                   shift = c(50, 150))

idx <- as.numeric(commandArgs(TRUE))

sidx <- sds[idx, ]

ccores <- 4

createFits <- function(sidx) {
  dat <- createDataShiftLogistic(paired = sidx$paired,
                                 dd = sidx$shift,
                                 xosd = sidx$xosd)
  fit <- bdotsFit(data = dat$dts,
                  y = "fixations",
                  group = "group",
                  subject = "id",
                  time = "time",
                  curveType = logistic(),
                  cores = ccores,
                  cor = FALSE)
  fit
}

N <- 250
res <- vector("list", length = N)
attr(res, "settings") <- sidx
nn <- paste0("sim", idx)
sf <- paste0("prog_txt/", nn, ".txt")
rf <- paste0("rds_files/", nn, ".rds")


sink(file = sf)
print(paste0("starting index: ", idx))
for (i in seq_len(N)) {
  fit <- createFits(sidx)

  sm <- bdotsBoot(formula = fixations ~ group(A, B),
                  bdObj = fit, singleMeans = TRUE,
                  cores = ccores)$sigTime

  mm <- bdotsBoot(formula = fixations ~ group(A, B),
                  bdObj = fit, singleMeans = FALSE,
                  cores = ccores)$sigTime

  pm <- suppressMessages(bdotsBoot(formula = fixations ~ group(A, B),
                                   bdObj = fit, permutation = TRUE, skipDist = TRUE,
                                   cores = ccores))$sigTime

  res[[i]] <- list(singleMeans = sm,
                   manyMeans = mm,
                   permutation = pm)

  if (i %% 10 == 0) {
    msg <- paste0("index: ", idx, ", iteration: ", i)
    print(msg)
  }
}
saveRDS(res, rf)
