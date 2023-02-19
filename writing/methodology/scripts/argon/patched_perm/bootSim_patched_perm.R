library(bdots)
library(parallel)

idx <- as.numeric(commandArgs(TRUE))

ff <- list.files("../rds_files", full.names = TRUE)
ff <- ff[c(1L, 3L, 4L, 5L, 7L, 8L, 9L, 11L, 12L, 13L, 15L, 16L)]

simDataSettings <- expand.grid(manymeans = c(TRUE, FALSE),
                               paired = c(TRUE, FALSE),
                               ar1 = c(TRUE, FALSE),
                               bdotscor = c(TRUE, FALSE))
simDataSettings <- simDataSettings[c(1L, 3L, 4L, 5L, 7L, 8L, 9L, 11L, 12L, 13L, 15L, 16L), ]

ff <- ff[idx]
sidx <- simDataSettings[idx, ]


tt <- readRDS(ff)

# get bdotsFits
bf <- lapply(tt,`[[`, 1)

res <- vector("list", length = length(bf))
attr(res, "settings") <- sidx

nn <- paste0("boot", idx)
sf <- paste0("patched_txt/", nn, ".txt")
rf <- paste0("patched_boot/", nn, ".rds")

sink(sf)

for (i in seq_along(bf)) {
  # sm <- bdotsBoot(formula = fixations ~ group(A, B),
  #                 bdObj = bf[[i]], singleMeans = TRUE,
  #                 cores = detectCores() - 1L)
  #
  # mm <- bdotsBoot(formula = fixations ~ group(A, B),
  #                 bdObj = bf[[i]], singleMeans = FALSE,
  #                 cores = detectCores() - 1L)

  pm <- suppressMessages(bdotsBoot(formula = fixations ~ group(A, B),
                  bdObj = bf[[i]], permutation = TRUE, skipDist = TRUE,
                  cores = detectCores() - 1L))

  # sm <- list(sigTime = sm$sigTime,
  #            parA = sm$curveList$A$parMat,
  #            parB = sm$curveList$B$parMat)
  # mm <- list(sigTime = mm$sigTime,
  #            parA = mm$curveList$A$parMat,
  #            parB = mm$curveList$B$parMat)
  pm <- list(sigTime = pm$sigTime)
  res[[i]] <- list(permutation = pm)

  # res[[i]] <- list(singleMeans = sm,
  #                  manyMeans = mm,
  #                  permutation = pm)

  if (i %% 10 == 0) {
    msg <- paste0("index: ", idx, ", iteration: ", i)
    print(msg)
  }
}
saveRDS(object = res, file = rf)
sink()
