library(eyetrackSim)
library(bdots)



sds <- expand.grid(bdotscor = c(TRUE, FALSE),
                   ar1 = c(TRUE, FALSE),
                   manymeans = c(FALSE, TRUE),
                   paired = c(FALSE, TRUE))
sds <- as.data.table(sds)
sds <- sds[!(manymeans == FALSE & paired == TRUE), ]

idx <- as.numeric(commandArgs(TRUE))

sidx <- sds[idx, ]

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
                  cores = ccores,
                  cor = sidx$bdotscor)
  fit
}

N <- 250
res <- vector("list", length = N)
attr(res, "settings") <- sidx
nn <- paste0("sim", idx)
sf <- paste0("prog_txt/", nn, ".txt")
rf <- paste0("rds_files/", nn, ".rds")

ccores <- 4


sink(file = sf)
print(paste0("starting index: ", idx))
for (i in seq_len(N)) {
  fit <- createFits(sidx)

  sm <- bdotsBoot(formula = fixations ~ group(A, B),
                  bdObj = fit, singleMeans = TRUE,
                  cores = ccores)#$sigTime

  mm <- bdotsBoot(formula = fixations ~ group(A, B),
                  bdObj = fit, singleMeans = FALSE,
                  cores = ccores)#$sigTime

  pm <- suppressMessages(bdotsBoot(formula = fixations ~ group(A, B),
                                   bdObj = fit, permutation = TRUE, skipDist = FALSE,
                                   cores = ccores))#$sigTime

  res[[i]] <- list(singleMeans = sm,
                   manyMeans = mm,
                   permutation = pm)

  if (i %% 10 == 0) {
    msg <- paste0("index: ", idx, ", iteration: ", i)
    print(msg)
  }
}
saveRDS(res, rf)
