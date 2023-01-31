## Here we are checking without parired to see how much parameter choice has impact
# mostly consequence of discrepancy between this and jake paper

## Let's create here the data that's going to be used in all our simulations
library(eyetrackSim)
library(bdots)

## Ok, what would it look like to create all these fits
# only n = 25 for time, taking us from 64 to 16
simDataSettings <- expand.grid(manymeans = c(TRUE, FALSE),
                               #paired = c(TRUE, FALSE),
                               ar1 = c(TRUE, FALSE),
                               bdotscor = c(TRUE, FALSE))


idx <- as.numeric(commandArgs(TRUE))

sidx <- simDataSettings[idx, ]

createFits <- function(sidx) {
  dat <- createData(ar1 = sidx$ar1,
                    manymeans = sidx$manymeans,
                    pars = PARS)
  fit <- bdotsFit(data = dat$dts,
                  y = "fixations",
                  group = "group",
                  subject = "id",
                  time = "time",
                  curveType = logistic(),
                  cores = 4,
                  cor = sidx$bdotscor)
  list(fit = fit, data = dat)
}


## Change starting parameters a la oleson 2017
PARS <- list(mean = c(mini = 0, peak = 0.75, slope = 0.0025, cross = 200),
           sigma = structure(c(0.000175969499369012,
                               0.0000487830783643044, 0.00000313632100316084, 0.00364081890233776,
                               0.0000487830783643044, 0.00696428035548042, 0.0000240045450371927,
                               -1.75318934674301, 0.00000313632100316084, 0.0000240045450371927,
                               0.000000180059372175171, -0.0135790777489765, 0.00364081890233776,
                               -1.75318934674301, -0.0135790777489765, 3513.62904635378),
                             dim = c(4L, 4L), dimnames = list(c("mini", "peak", "slope", "cross"),
                                                              c("mini","peak", "slope", "cross"))))

N <- 100
sims <- vector("list", length = N)
nn <- paste0("sim", idx, "_altpars")
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



