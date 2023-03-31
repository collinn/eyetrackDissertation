library(eyetrackSim)
library(bdots)



sds <- expand.grid(bdotscor = c(TRUE, FALSE),
                   ar1 = c(TRUE, FALSE),
                   manymeans = c(FALSE, TRUE),
                   paired = c(FALSE, TRUE),
                   pairedType = 1)
sds <- as.data.table(sds)
sds <- sds[!(manymeans == FALSE & paired == TRUE), ]

## Now need to make a pairedType 1/2
dat <- sds[paired == TRUE, ]
dat1 <- copy(dat)
dat1$pairedType <- 2

sds <- rbind(sds, dat1)


idx <- as.numeric(commandArgs(TRUE))

sidx <- sds[idx, ]

createFits <- function(sidx) {
  dat <- createData_feb(paired = sidx$paired,
                    ar1 = sidx$ar1,
                    manymeans = sidx$manymeans,
                    pairedType = sidx$pairedType) # this is ignored if not paired

  ## Make them less reliable
  newtime <- seq(0, 1600, by = 40)
  dat$dts <- dat$dts[time %in% newtime, ]

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

N <- 25
res <- vector("list", length = N)
attr(res, "settings") <- sidx
nn <- paste0("sim", idx)
sf <- paste0("prog_txt/", nn, ".txt")
rf <- paste0("40_rds_files/", nn, ".rds")

ccores <- 4


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
                                   bdObj = fit, permutation = TRUE, skipDist = FALSE,
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
