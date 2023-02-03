library(bdots)
library(parallel)

idx <- as.numeric(commandArgs(TRUE))

ff <- paste0("rds_files/sim", idx, ".rds")

tt <- readRDS(ff)

# get bdotsFits
bf <- lapply(tt,`[[`, 1)

res <- vector("list", length = length(bf))

nn <- paste0("boot", idx)
sf <- paste0("prog_boot/", nn, ".txt")
rf <- paste0("full_boot/", nn, ".rds")

sink(sf)

for (i in seq_along(bf)) {
  sm <- bdotsBoot(formula = fixations ~ group(A, B),
                  bdObj = bf[[i]], singleMeans = TRUE,
                  cores = detectCores() - 1L)

  mm <- bdotsBoot(formula = fixations ~ group(A, B),
                  bdObj = bf[[i]], singleMeans = FALSE,
                  cores = detectCores() - 1L)

  pm <- suppressMessages(bdotsBoot(formula = fixations ~ group(A, B),
                  bdObj = bf[[i]], permutation = TRUE, skipDist = TRUE,
                  cores = detectCores() - 1L))

  # sm <- list(sigTime = sm$sigTime,
  #            parA = sm$curveList$A$parMat,
  #            parB = sm$curveList$B$parMat)
  # mm <- list(sigTime = mm$sigTime,
  #            parA = mm$curveList$A$parMat,
  #            parB = mm$curveList$B$parMat)
  # pm <- list(sigTime = pm$sigTime)

  res[[i]] <- list(singleMeans = sm,
                   manyMeans = mm,
                   permutation = pm)

  if (i %% 10 == 0) {
    msg <- paste0("index: ", idx, ", iteration: ", i)
    print(msg)
  }
}
saveRDS(object = res, file = rf)
sink()
